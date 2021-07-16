;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace
  (:require
   [app.common.data :as d]
   [app.common.geom.align :as gal]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.proportions :as gpr]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.pages :as cp]
   [app.common.pages.helpers :as cph]
   [app.common.pages.spec :as spec]
   [app.common.spec :as us]
   [app.common.transit :as t]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.util.globals :as ug]
   [app.main.data.messages :as dm]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.drawing :as dwd]
   [app.main.data.workspace.groups :as dwg]
   [app.main.data.workspace.libraries :as dwl]
   [app.main.data.workspace.notifications :as dwn]
   [app.main.data.workspace.path :as dwdp]
   [app.main.data.workspace.persistence :as dwp]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.svg-upload :as svg]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.undo :as dwu]
   [app.main.repo :as rp]
   [app.main.streams :as ms]
   [app.main.worker :as uw]
   [app.util.http :as http]
   [app.util.i18n :as i18n]
   [app.util.router :as rt]
   [app.util.webapi :as wapi]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [clojure.set :as set]
   [cuerdas.core :as str]
   [potok.core :as ptk]))

;; (log/set-level! :trace)
;; --- Specs

(s/def ::shape-attrs ::cp/shape-attrs)
(s/def ::set-of-string
  (s/every string? :kind set?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare file-initialized)

;; --- Initialize Workspace

(s/def ::layout-flag
  #{:sitemap
    :layers
    :comments
    :assets
    :document-history
    :colorpalette
    :element-options
    :rules
    :display-grid
    :snap-grid
    :scale-text
    :dynamic-alignment})

(s/def ::layout-flags (s/coll-of ::layout-flag))

(def default-layout
  #{:sitemap
    :layers
    :element-options
    :rules
    :display-grid
    :snap-grid
    :dynamic-alignment})

(def layout-names
  {:assets
   {:del #{:sitemap :layers :document-history }
    :add #{:assets}}

   :document-history
   {:del #{:assets :layers :sitemap}
    :add #{:document-history}}

   :layers
   {:del #{:document-history :assets}
    :add #{:sitemap :layers}}})

(s/def ::options-mode #{:design :prototype})

(def workspace-local-default
  {:zoom 1
   :flags #{}
   :selected (d/ordered-set)
   :expanded {}
   :tooltip nil
   :options-mode :design
   :draw-interaction-to nil
   :left-sidebar? true
   :right-sidebar? true
   :color-for-rename nil
   :selected-palette-colorpicker :recent
   :selected-palette :recent
   :selected-palette-size :big
   :assets-files-open {}
   :picking-color? false
   :picked-color nil
   :picked-color-select false})

(declare ensure-layout)

(defn initialize-layout
  [layout-name]
  (us/verify (s/nilable ::us/keyword) layout-name)
  (ptk/reify ::initialize-layout
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-layout
              (fn [layout]
                (or layout default-layout))))

    ptk/WatchEvent
    (watch [_ _ _]
      (if (and layout-name (contains? layout-names layout-name))
        (rx/of (ensure-layout layout-name))
        (rx/of (ensure-layout :layers))))))

(defn initialize-file
  [project-id file-id]
  (us/verify ::us/uuid project-id)
  (us/verify ::us/uuid file-id)

  (ptk/reify ::initialize-file
    ptk/UpdateEvent
    (update [_ state]
      (assoc state
             :current-file-id file-id
             :current-project-id project-id
             :workspace-presence {}))

    ptk/WatchEvent
    (watch [_ _ stream]
      (rx/merge
       (rx/of (dwp/fetch-bundle project-id file-id))

       ;; Initialize notifications (websocket connection) and the file persistence
       (->> stream
            (rx/filter (ptk/type? ::dwp/bundle-fetched))
            (rx/take 1)
            (rx/map deref)
            (rx/mapcat (fn [bundle]
                         (rx/merge
                          (rx/of (dwn/initialize file-id)
                                 (dwp/initialize-file-persistence file-id)
                                 (dwc/initialize-indices bundle))

                          (->> stream
                               (rx/filter #(= ::dwc/index-initialized %))
                               (rx/first)
                               (rx/map #(file-initialized bundle)))))))))

    ptk/EffectEvent
    (effect [_ _ _]
      (let [name (str "workspace-" file-id)]
        (unchecked-set ug/global "name" name)))))

(defn- file-initialized
  [{:keys [file users project libraries] :as bundle}]
  (ptk/reify ::file-initialized
    ptk/UpdateEvent
    (update [_ state]
      (assoc state
             :current-team-id (:team-id project)
             :users (d/index-by :id users)
             :workspace-undo {}
             :workspace-project project
             :workspace-file (assoc file :initialized true)
             :workspace-data (:data file)
             :workspace-libraries (d/index-by :id libraries)))

    ptk/WatchEvent
    (watch [_ _ _]
      (let [file-id       (:id file)
            ignore-until  (:ignore-sync-until file)
            needs-update? (some #(and (> (:modified-at %) (:synced-at %))
                                      (or (not ignore-until)
                                          (> (:modified-at %) ignore-until)))
                                libraries)]
        (when needs-update?
          (rx/of (dwl/notify-sync-file file-id)))))))

(defn finalize-file
  [_project-id file-id]
  (ptk/reify ::finalize
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state
              :workspace-file
              :workspace-project
              :workspace-media-objects
              :workspace-persistence))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwn/finalize file-id)
             ::dwp/finalize))))

(defn initialize-page
  [page-id]
  (us/assert ::us/uuid page-id)
  (ptk/reify ::initialize-page
    ptk/UpdateEvent
    (update [_ state]
      (let [;; we maintain a cache of page state for user convenience
            ;; with the exception of the selection; when user abandon
            ;; the current page, the selection is lost

            page    (get-in state [:workspace-data :pages-index page-id])
            local   (-> state
                        (get-in [:workspace-cache page-id] workspace-local-default)
                        (assoc :selected (d/ordered-set)))]
        (-> state
            (assoc :current-page-id page-id)
            (assoc :trimmed-page (select-keys page [:id :name]))
            (assoc :workspace-local local)
            (update-in [:route :params :query] assoc :page-id (str page-id)))))))

(defn finalize-page
  [page-id]
  (us/assert ::us/uuid page-id)
  (ptk/reify ::finalize-page
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id (or page-id (get-in state [:workspace-data :pages 0]))
            local   (-> (:workspace-local state)
                        (dissoc :edition)
                        (dissoc :edit-path)
                        (dissoc :selected))]
        (-> state
            (assoc-in [:workspace-cache page-id] local)
            (dissoc :current-page-id :workspace-local :trimmed-page :workspace-drawing))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace Page CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-page
  [{:keys [file-id]}]
  (let [id (uuid/next)]
    (ptk/reify ::create-page
      IDeref
      (-deref [_]
        {:id id :file-id file-id})

      ptk/WatchEvent
      (watch [it state _]
        (let [pages   (get-in state [:workspace-data :pages-index])
              unames  (dwc/retrieve-used-names pages)
              name    (dwc/generate-unique-name unames "Page")

              rchange {:type :add-page
                       :id id
                       :name name}
              uchange {:type :del-page
                       :id id}]
          (rx/of (dch/commit-changes {:redo-changes [rchange]
                                      :undo-changes [uchange]
                                      :origin it})))))))

(defn duplicate-page
  [page-id]
  (ptk/reify ::duplicate-page
    ptk/WatchEvent
    (watch [this state _]
      (let [id      (uuid/next)
            pages   (get-in state [:workspace-data :pages-index])
            unames  (dwc/retrieve-used-names pages)
            page    (get-in state [:workspace-data :pages-index page-id])
            name    (dwc/generate-unique-name unames (:name page))

            page (-> page (assoc :name name :id id))

            rchange {:type :add-page
                     :page page}
            uchange {:type :del-page
                     :id id}]
        (rx/of (dch/commit-changes {:redo-changes [rchange]
                                    :undo-changes [uchange]
                                    :origin this}))))))

(s/def ::rename-page
  (s/keys :req-un [::id ::name]))

(defn rename-page
  [id name]
  (us/verify ::us/uuid id)
  (us/verify string? name)
  (ptk/reify ::rename-page
    ptk/WatchEvent
    (watch [it state _]
      (let [page (get-in state [:workspace-data :pages-index id])
            rchg {:type :mod-page
                  :id id
                  :name name}
            uchg {:type :mod-page
                  :id id
                  :name (:name page)}]
        (rx/of (dch/commit-changes {:redo-changes [rchg]
                                    :undo-changes [uchg]
                                    :origin it}))))))

(declare purge-page)
(declare go-to-file)

;; TODO: properly handle positioning on undo.

(defn delete-page
  [id]
  (ptk/reify ::delete-page
    ptk/WatchEvent
    (watch [it state _]
      (let [page (get-in state [:workspace-data :pages-index id])
            rchg {:type :del-page
                  :id id}
            uchg {:type :add-page
                  :page page}]
        (rx/of (dch/commit-changes {:redo-changes [rchg]
                                    :undo-changes [uchg]
                                    :origin it})
               (when (= id (:current-page-id state))
                 go-to-file))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORKSPACE File Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rename-file
  [id name]
  {:pre [(uuid? id) (string? name)]}
  (ptk/reify ::rename-file
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-file :name] name))

    ptk/WatchEvent
    (watch [_ _ _]
      (let [params {:id id :name name}]
        (->> (rp/mutation :rename-file params)
             (rx/ignore))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace State Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Viewport Sizing

(declare zoom-to-fit-all)

(defn initialize-viewport
  [{:keys [width height] :as size}]
  (letfn [(update* [{:keys [vport] :as local}]
            (let [wprop (/ (:width vport) width)
                  hprop (/ (:height vport) height)]
              (-> local
                  (assoc :vport size)
                  (update :vbox (fn [vbox]
                                    (-> vbox
                                        (update :width #(/ % wprop))
                                        (update :height #(/ % hprop))))))))

          (initialize [state local]
            (let [page-id (:current-page-id state)
                  objects (wsh/lookup-page-objects state page-id)
                  shapes  (cp/select-toplevel-shapes objects {:include-frames? true})
                  srect   (gsh/selection-rect shapes)
                  local   (assoc local :vport size :zoom 1)]
              (cond
                (or (not (mth/finite? (:width srect)))
                    (not (mth/finite? (:height srect))))
                (assoc local :vbox (assoc size :x 0 :y 0 :left-offset 0))

                (or (> (:width srect) width)
                    (> (:height srect) height))
                (let [srect (gal/adjust-to-viewport size srect {:padding 40})
                      zoom  (/ (:width size) (:width srect))]
                  (-> local
                      (assoc :zoom zoom)
                      (update :vbox merge srect)))

                :else
                (assoc local :vbox (assoc size
                                          :x (+ (:x srect) (/ (- (:width srect) width) 2))
                                          :y (+ (:y srect) (/ (- (:height srect) height) 2)))))))

          (setup [state local]
            (if (:vbox local)
              (update* local)
              (initialize state local)))]

    (ptk/reify ::initialize-viewport
      ptk/UpdateEvent
      (update [_ state]
        (update state :workspace-local
                (fn [local]
                  (setup state local)))))))

(defn update-viewport-position
  [{:keys [x y] :or {x identity y identity}}]
  (us/assert fn? x)
  (us/assert fn? y)
  (ptk/reify ::update-viewport-position
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:workspace-local :vbox]
                 (fn [vbox]
                   (-> vbox
                       (update :x x)
                       (update :y y)))))))

(defn update-viewport-size
  [{:keys [width height] :as size}]
  (ptk/reify ::update-viewport-size
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local
              (fn [{:keys [vport left-sidebar? zoom] :as local}]
                (if (or (mth/almost-zero? width) (mth/almost-zero? height))
                  ;; If we have a resize to zero just keep the old value
                  local
                  (let [wprop (/ (:width vport) width)
                        hprop (/ (:height vport) height)
                        left-offset (if left-sidebar? 0 (/ (* -1 15 16) zoom))]
                    (-> local         ;; This matches $width-settings-bar
                        (assoc :vport size) ;; in frontend/resources/styles/main/partials/sidebar.scss
                        (update :vbox (fn [vbox]
                                        (-> vbox
                                            (update :width #(/ % wprop))
                                            (update :height #(/ % hprop))
                                            (assoc :left-offset left-offset))))))))))))


(defn start-panning []
  (ptk/reify ::start-panning
    ptk/WatchEvent
    (watch [_ state stream]
      (let [stopper (->> stream (rx/filter (ptk/type? ::finish-panning)))
            zoom (-> (get-in state [:workspace-local :zoom]) gpt/point)]
        (when-not (get-in state [:workspace-local :panning])
          (rx/concat
           (rx/of #(-> % (assoc-in [:workspace-local :panning] true)))
           (->> stream
                (rx/filter ms/pointer-event?)
                (rx/filter #(= :delta (:source %)))
                (rx/map :pt)
                (rx/take-until stopper)
                (rx/map (fn [delta]
                          (let [delta (gpt/divide delta zoom)]
                            (update-viewport-position {:x #(- % (:x delta))
                                                       :y #(- % (:y delta))})))))))))))

(defn finish-panning []
  (ptk/reify ::finish-panning
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update :workspace-local dissoc :panning)))))


;; --- Toggle layout flag

(defn ensure-layout
  [layout-name]
  (assert (contains? layout-names layout-name)
          (str "unexpected layout name: " layout-name))
  (ptk/reify ::ensure-layout
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-layout
              (fn [stored]
                (let [todel (get-in layout-names [layout-name :del] #{})
                      toadd (get-in layout-names [layout-name :add] #{})]
                  (-> stored
                      (set/difference todel)
                      (set/union toadd))))))))

(defn toggle-layout-flags
  [& flags]
  (ptk/reify ::toggle-layout-flags
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-layout
              (fn [stored]
                (reduce (fn [flags flag]
                          (if (contains? flags flag)
                            (disj flags flag)
                            (conj flags flag)))
                        stored
                        (into #{} flags)))))))

;; --- Set element options mode

(defn set-options-mode
  [mode]
  (us/assert ::options-mode mode)
  (ptk/reify ::set-options-mode
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :options-mode] mode))))

;; --- Tooltip

(defn assign-cursor-tooltip
  [content]
  (ptk/reify ::assign-cursor-tooltip
    ptk/UpdateEvent
    (update [_ state]
      (if (string? content)
        (assoc-in state [:workspace-local :tooltip] content)
        (assoc-in state [:workspace-local :tooltip] nil)))))

;; --- Zoom Management

(defn- impl-update-zoom
  [{:keys [vbox] :as local} center zoom]
  (let [vbox (update vbox :x + (:left-offset vbox))
        new-zoom (if (fn? zoom) (zoom (:zoom local)) zoom)
        old-zoom (:zoom local)
        center (if center center (gsh/center-rect vbox))
        scale (/ old-zoom new-zoom)
        mtx  (gmt/scale-matrix (gpt/point scale) center)
        vbox' (gsh/transform-rect vbox mtx)
        vbox' (update vbox' :x - (:left-offset vbox))]
    (-> local
        (assoc :zoom new-zoom)
        (update :vbox merge (select-keys vbox' [:x :y :width :height])))))

(defn increase-zoom
  [center]
  (ptk/reify ::increase-zoom
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local
              #(impl-update-zoom % center (fn [z] (min (* z 1.3) 200)))))))

(defn decrease-zoom
  [center]
  (ptk/reify ::decrease-zoom
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local
              #(impl-update-zoom % center (fn [z] (max (/ z 1.3) 0.01)))))))

(def reset-zoom
  (ptk/reify ::reset-zoom
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local
              #(impl-update-zoom % nil 1)))))

(def zoom-to-fit-all
  (ptk/reify ::zoom-to-fit-all
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            shapes  (cp/select-toplevel-shapes objects {:include-frames? true})
            srect   (gsh/selection-rect shapes)]

        (if (or (mth/nan? (:width srect))
                (mth/nan? (:height srect)))
          state
          (update state :workspace-local
                  (fn [{:keys [vport] :as local}]
                    (let [srect (gal/adjust-to-viewport vport srect {:padding 40})
                          zoom  (/ (:width vport) (:width srect))]
                      (-> local
                          (assoc :zoom zoom)
                          (update :vbox merge srect))))))))))

(def zoom-to-selected-shape
  (ptk/reify ::zoom-to-selected-shape
    ptk/UpdateEvent
    (update [_ state]
      (let [selected (wsh/lookup-selected state)]
        (if (empty? selected)
          state
          (let [page-id (:current-page-id state)
                objects (wsh/lookup-page-objects state page-id)
                srect   (->> selected
                             (map #(get objects %))
                             (gsh/selection-rect))]
            (update state :workspace-local
                    (fn [{:keys [vport] :as local}]
                      (let [srect (gal/adjust-to-viewport vport srect {:padding 40})
                            zoom  (/ (:width vport) (:width srect))]
                        (-> local
                            (assoc :zoom zoom)
                            (update :vbox merge srect)))))))))))

;; --- Update Shape Attrs

(defn update-shape
  [id attrs]
  (us/verify ::us/uuid id)
  (us/verify ::shape-attrs attrs)
  (ptk/reify ::update-shape
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dch/update-shapes [id] #(merge % attrs))))))

(defn start-rename-shape
  [id]
  (us/verify ::us/uuid id)
  (ptk/reify ::start-rename-shape
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :shape-for-rename] id))))

(defn end-rename-shape
  []
  (ptk/reify ::end-rename-shape
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local dissoc :shape-for-rename))))

;; --- Update Selected Shapes attrs

(defn update-selected-shapes
  [attrs]
  (us/verify ::shape-attrs attrs)
  (ptk/reify ::update-selected-shapes
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/from (map #(update-shape % attrs) selected))))))

;; --- Delete Selected

(def delete-selected
  "Deselect all and remove all selected shapes."
  (ptk/reify ::delete-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/of (dwc/delete-shapes selected)
               (dws/deselect-all))))))

;; --- Shape Vertical Ordering

(s/def ::loc  #{:up :down :bottom :top})

(defn vertical-order-selected
  [loc]
  (us/verify ::loc loc)
  (ptk/reify ::vertical-order-selected
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            selected (wsh/lookup-selected state)
            rchanges (mapv (fn [id]
                             (let [obj (get objects id)
                                   parent (get objects (:parent-id obj))
                                   shapes (:shapes parent)
                                   cindex (d/index-of shapes id)
                                   nindex (case loc
                                            :top (count shapes)
                                            :down (max 0 (- cindex 1))
                                            :up (min (count shapes) (+ (inc cindex) 1))
                                            :bottom 0)]
                               {:type :mov-objects
                                :parent-id (:parent-id obj)
                                :frame-id (:frame-id obj)
                                :page-id page-id
                                :index nindex
                                :shapes [id]}))
                           selected)

             uchanges (mapv (fn [id]
                             (let [obj (get objects id)]
                               {:type :mov-objects
                                :parent-id (:parent-id obj)
                                :frame-id (:frame-id obj)
                                :page-id page-id
                                :shapes [id]
                                :index (cp/position-on-parent id objects)}))
                            selected)]
        ;; TODO: maybe missing the :reg-objects event?
        (rx/of (dch/commit-changes {:redo-changes rchanges
                                    :undo-changes uchanges
                                    :origin it}))))))


;; --- Change Shape Order (D&D Ordering)

(defn relocate-shapes-changes [objects parents parent-id page-id to-index ids
                               groups-to-delete groups-to-unmask shapes-to-detach
                               shapes-to-reroot shapes-to-deroot shapes-to-unconstraint]
  (let [;; Changes to the shapes that are being move
        r-mov-change
        [{:type :mov-objects
          :parent-id parent-id
          :page-id page-id
          :index to-index
          :shapes (vec (reverse ids))}]

        u-mov-change
        (map (fn [id]
               (let [obj (get objects id)]
                 {:type :mov-objects
                  :parent-id (:parent-id obj)
                  :page-id page-id
                  :index (cp/position-on-parent id objects)
                  :shapes [id]}))
             (reverse ids))

        ;; Changes deleting empty groups
        r-del-change
        (map (fn [group-id]
               {:type :del-obj
                :page-id page-id
                :id group-id})
             groups-to-delete)

        u-del-change
        (d/concat
         []
         ;; Create the groups
         (map (fn [group-id]
                (let [group (get objects group-id)]
                  {:type :add-obj
                   :page-id page-id
                   :parent-id parent-id
                   :frame-id (:frame-id group)
                   :id group-id
                   :obj (-> group
                            (assoc :shapes []))}))
              groups-to-delete)
         ;; Creates the hierarchy
         (map (fn [group-id]
                (let [group (get objects group-id)]
                  {:type :mov-objects
                   :page-id page-id
                   :parent-id (:id group)
                   :shapes (:shapes group)}))
              groups-to-delete))

        ;; Changes removing the masks from the groups without mask shape
        r-mask-change
        (map (fn [group-id]
               {:type :mod-obj
                :page-id page-id
                :id group-id
                :operations [{:type :set
                              :attr :masked-group?
                              :val false}]})
             groups-to-unmask)

        u-mask-change
        (map (fn [group-id]
               (let [group (get objects group-id)]
                 {:type :mod-obj
                  :page-id page-id
                  :id group-id
                  :operations [{:type :set
                                :attr :masked-group?
                                :val (:masked-group? group)}]}))
             groups-to-unmask)

        ;; Changes to the components metadata

        detach-keys [:component-id :component-file :component-root? :remote-synced? :shape-ref :touched]

        r-detach-change
        (map (fn [id]
               {:type :mod-obj
                :page-id page-id
                :id id
                :operations (mapv #(hash-map :type :set :attr % :val nil) detach-keys)})
             shapes-to-detach)

        u-detach-change
        (map (fn [id]
               (let [obj (get objects id)]
                 {:type :mod-obj
                  :page-id page-id
                  :id id
                  :operations (mapv #(hash-map :type :set :attr % :val (get obj %)) detach-keys)}))
             shapes-to-detach)

        r-deroot-change
        (map (fn [id]
               {:type :mod-obj
                :page-id page-id
                :id id
                :operations [{:type :set
                              :attr :component-root?
                              :val nil}]})
             shapes-to-deroot)

        u-deroot-change
        (map (fn [id]
               {:type :mod-obj
                :page-id page-id
                :id id
                :operations [{:type :set
                              :attr :component-root?
                              :val true}]})
             shapes-to-deroot)

        r-reroot-change
        (map (fn [id]
               {:type :mod-obj
                :page-id page-id
                :id id
                :operations [{:type :set
                              :attr :component-root?
                              :val true}]})
             shapes-to-reroot)

        u-reroot-change
        (map (fn [id]
               {:type :mod-obj
                :page-id page-id
                :id id
                :operations [{:type :set
                              :attr :component-root?
                              :val nil}]})
             shapes-to-reroot)


        ;; Changes resetting constraints

        r-unconstraint-change
        (map (fn [id]
               (let [obj      (get objects id)
                     parent   (get objects parent-id)
                     frame-id (if (= (:type parent) :frame)
                                (:id parent)
                                (:frame-id parent))]
                 {:type :mod-obj
                  :page-id page-id
                  :id id
                  :operations [{:type :set
                                :attr :constraints-h
                                :val (spec/default-constraints-h
                                       (assoc obj :parent-id parent-id :frame-id frame-id))}
                               {:type :set
                                :attr :constraints-v
                                :val (spec/default-constraints-v
                                       (assoc obj :parent-id parent-id :frame-id frame-id))}]}))
             shapes-to-unconstraint)

        u-unconstraint-change
        (map (fn [id]
               (let [obj (get objects id)]
                 {:type :mod-obj
                  :page-id page-id
                  :id id
                  :operations [{:type :set
                                :attr :constraints-h
                                :val (:constraints-h obj)}
                               {:type :set
                                :attr :constraints-v
                                :val (:constraints-v obj)}]}))
             shapes-to-unconstraint)

        r-reg-change
        [{:type :reg-objects
          :page-id page-id
          :shapes (vec parents)}]

        u-reg-change
        [{:type :reg-objects
          :page-id page-id
          :shapes (vec parents)}]

        rchanges (d/concat []
                           r-mov-change
                           r-del-change
                           r-mask-change
                           r-detach-change
                           r-deroot-change
                           r-reroot-change
                           r-unconstraint-change
                           r-reg-change)

        uchanges (d/concat []
                           u-del-change
                           u-reroot-change
                           u-deroot-change
                           u-detach-change
                           u-mask-change
                           u-mov-change
                           u-unconstraint-change
                           u-reg-change)]
    [rchanges uchanges]))

(defn relocate-shapes
  [ids parent-id to-index]
  (us/verify (s/coll-of ::us/uuid) ids)
  (us/verify ::us/uuid parent-id)
  (us/verify number? to-index)

  (ptk/reify ::relocate-shapes
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)

            ;; Ignore any shape whose parent is also intented to be moved
            ids (cp/clean-loops objects ids)

            ;; If we try to move a parent into a child we remove it
            ids (filter #(not (cp/is-parent? objects parent-id %)) ids)

            parents (reduce (fn [result id]
                              (conj result (cp/get-parent id objects)))
                            #{parent-id} ids)

            groups-to-delete
            (loop [current-id (first parents)
                   to-check (rest parents)
                   removed-id? (set ids)
                   result #{}]

              (if-not current-id
                ;; Base case, no next element
                result

                (let [group (get objects current-id)]
                  (if (and (not= :frame (:type group))
                           (not= current-id parent-id)
                           (empty? (remove removed-id? (:shapes group))))

                    ;; Adds group to the remove and check its parent
                    (let [to-check (d/concat [] to-check [(cp/get-parent current-id objects)]) ]
                      (recur (first to-check)
                             (rest to-check)
                             (conj removed-id? current-id)
                             (conj result current-id)))

                    ;; otherwise recur
                    (recur (first to-check)
                           (rest to-check)
                           removed-id?
                           result)))))

            groups-to-unmask
            (reduce (fn [group-ids id]
                      ;; When a masked group loses its mask shape, because it's
                      ;; moved outside the group, the mask condition must be
                      ;; removed, and it must be converted to a normal group.
                      (let [obj (get objects id)
                            parent (get objects (:parent-id obj))]
                        (if (and (:masked-group? parent)
                                 (= id (first (:shapes parent)))
                                 (not= (:id parent) parent-id))
                          (conj group-ids (:id parent))
                          group-ids)))
                    #{}
                    ids)

            ;; Sets the correct components metadata for the moved shapes
            ;; `shapes-to-detach` Detach from a component instance a shape that was inside a component and is moved outside
            ;; `shapes-to-deroot` Removes the root flag from a component instance moved inside another component
            ;; `shapes-to-reroot` Adds a root flag when a nested component instance is moved outside
            [shapes-to-detach shapes-to-deroot shapes-to-reroot]
            (reduce (fn [[shapes-to-detach shapes-to-deroot shapes-to-reroot] id]
                      (let [shape          (get objects id)
                            instance-part? (and (:shape-ref shape)
                                                (not (:component-id shape)))
                            instance-root? (:component-root? shape)
                            sub-instance?  (and (:component-id shape)
                                                (not (:component-root? shape)))

                            parent                 (get objects parent-id)
                            component-shape        (cph/get-component-shape shape objects)
                            component-shape-parent (cph/get-component-shape parent objects)

                            detach? (and instance-part? (not= (:id component-shape)
                                                              (:id component-shape-parent)))
                            deroot? (and instance-root? component-shape-parent)
                            reroot? (and sub-instance? (not component-shape-parent))

                            ids-to-detach (when detach?
                                            (cons id (cph/get-children id objects)))]

                        [(cond-> shapes-to-detach detach? (into ids-to-detach))
                         (cond-> shapes-to-deroot deroot? (conj id))
                         (cond-> shapes-to-reroot reroot? (conj id))]))
                    [[] [] []]
                    ids)

            [rchanges uchanges]
            (relocate-shapes-changes objects
                                     parents
                                     parent-id
                                     page-id
                                     to-index
                                     ids
                                     groups-to-delete
                                     groups-to-unmask
                                     shapes-to-detach
                                     shapes-to-reroot
                                     shapes-to-deroot
                                     ids)]

        (rx/of (dch/commit-changes {:redo-changes rchanges
                                    :undo-changes uchanges
                                    :origin it})
               (dwc/expand-collapse parent-id))))))

(defn relocate-selected-shapes
  [parent-id to-index]
  (ptk/reify ::relocate-selected-shapes
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/of (relocate-shapes selected parent-id to-index))))))


(defn start-editing-selected
  []
  (ptk/reify ::start-editing-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (if-not (= 1 (count selected))
          (rx/empty)

          (let [objects (wsh/lookup-page-objects state)
                {:keys [id type shapes]} (get objects (first selected))]

            (case type
              :text
              (rx/of (dwc/start-edition-mode id))

              :group
              (rx/of (dwc/select-shapes (into (d/ordered-set) [(last shapes)])))

              (rx/of (dwc/start-edition-mode id)
                     (dwdp/start-path-edit id)))))))))


;; --- Change Page Order (D&D Ordering)

(defn relocate-page
  [id index]
  (ptk/reify ::relocate-pages
    ptk/WatchEvent
    (watch [it state _]
      (let [cidx (-> (get-in state [:workspace-data :pages])
                     (d/index-of id))
            rchg {:type :mov-page
                  :id id
                  :index index}
            uchg {:type :mov-page
                  :id id
                  :index cidx}]
        (rx/of (dch/commit-changes {:redo-changes [rchg]
                                    :undo-changes [uchg]
                                    :origin it}))))))

;; --- Shape / Selection Alignment and Distribution

(declare align-object-to-frame)
(declare align-objects-list)

(defn align-objects
  [axis]
  (us/verify ::gal/align-axis axis)
  (ptk/reify :align-objects
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            selected (wsh/lookup-selected state)
            moved    (if (= 1 (count selected))
                       (align-object-to-frame objects (first selected) axis)
                       (align-objects-list objects selected axis))

            moved-objects (->> moved (group-by :id))
            ids (keys moved-objects)
            update-fn (fn [shape] (first (get moved-objects (:id shape))))]

        (rx/of (dch/update-shapes ids update-fn {:reg-objects? true}))))))

(defn align-object-to-frame
  [objects object-id axis]
  (let [object (get objects object-id)
        frame (get objects (:frame-id object))]
    (gal/align-to-rect object frame axis objects)))

(defn align-objects-list
  [objects selected axis]
  (let [selected-objs (map #(get objects %) selected)
        rect (gsh/selection-rect selected-objs)]
    (mapcat #(gal/align-to-rect % rect axis objects) selected-objs)))

(defn distribute-objects
  [axis]
  (us/verify ::gal/dist-axis axis)
  (ptk/reify :align-objects
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            selected (wsh/lookup-selected state)
            moved    (-> (map #(get objects %) selected)
                         (gal/distribute-space axis objects))

            moved-objects (->> moved (group-by :id))
            ids (keys moved-objects)
            update-fn (fn [shape] (first (get moved-objects (:id shape))))]
        (rx/of (dch/update-shapes ids update-fn {:reg-objects? true}))))))

;; --- Shape Proportions

(defn set-shape-proportion-lock
  [id lock]
  (ptk/reify ::set-shape-proportion-lock
    ptk/WatchEvent
    (watch [_ _ _]
      (letfn [(assign-proportions [shape]
                (if-not lock
                  (assoc shape :proportion-lock false)
                  (-> (assoc shape :proportion-lock true)
                      (gpr/assign-proportions))))]
        (rx/of (dch/update-shapes [id] assign-proportions))))))

;; --- Update Shape Flags

(defn update-shape-flags
  [id {:keys [blocked hidden] :as flags}]
  (s/assert ::us/uuid id)
  (s/assert ::shape-attrs flags)
  (ptk/reify ::update-shape-flags
    ptk/WatchEvent
    (watch [_ state _]
      (let [update-fn
            (fn [obj]
              (cond-> obj
                (boolean? blocked) (assoc :blocked blocked)
                (boolean? hidden) (assoc :hidden hidden)))

            objects (wsh/lookup-page-objects state)
            ids (d/concat [id] (cp/get-children id objects))]
        (rx/of (dch/update-shapes ids update-fn))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn navigate-to-project
  [project-id]
  (ptk/reify ::navigate-to-project
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-ids (get-in state [:projects project-id :pages])
            params {:project project-id :page (first page-ids)}]
        (rx/of (rt/nav :workspace/page params))))))

(defn go-to-page
  ([]
   (ptk/reify ::go-to-page
     ptk/WatchEvent
     (watch [_ state _]
       (let [project-id (:current-project-id state)
             file-id    (:current-file-id state)
             page-id    (get-in state [:workspace-data :pages 0])

             pparams    {:file-id file-id :project-id project-id}
             qparams    {:page-id page-id}]
         (rx/of (rt/nav' :workspace pparams qparams))))))
  ([page-id]
   (us/verify ::us/uuid page-id)
   (ptk/reify ::go-to-page
     ptk/WatchEvent
     (watch [_ state _]
       (let [project-id (:current-project-id state)
             file-id    (:current-file-id state)
             pparams    {:file-id file-id :project-id project-id}
             qparams    {:page-id page-id}]
         (rx/of (rt/nav :workspace pparams qparams)))))))

(defn go-to-layout
  [layout]
  (us/verify ::layout-flag layout)
  (ptk/reify ::go-to-layout
    ptk/WatchEvent
    (watch [_ state _]
      (let [project-id (get-in state [:workspace-project :id])
            file-id    (get-in state [:workspace-file :id])
            page-id    (get state :current-page-id)
            pparams    {:file-id file-id :project-id project-id}
            qparams    {:page-id page-id :layout (name layout)}]
        (rx/of (rt/nav :workspace pparams qparams))))))

(def go-to-file
  (ptk/reify ::go-to-file
    ptk/WatchEvent
    (watch [_ state _]
      (let [{:keys [id project-id data] :as file} (:workspace-file state)
            page-id (get-in data [:pages 0])
            pparams {:project-id project-id :file-id id}
            qparams {:page-id page-id}]
        (rx/of (rt/nav :workspace pparams qparams))))))

(defn go-to-viewer
  ([] (go-to-viewer {}))
  ([{:keys [file-id page-id]}]
   (ptk/reify ::go-to-viewer
     ptk/WatchEvent
     (watch [_ state _]
       (let [{:keys [current-file-id current-page-id]} state
             params {:file-id (or file-id current-file-id)
                     :page-id (or page-id current-page-id)}]
         (rx/of ::dwp/force-persist
                (rt/nav-new-window :viewer params {:index 0})))))))

(defn go-to-dashboard
  ([] (go-to-dashboard nil))
  ([{:keys [team-id]}]
   (ptk/reify ::go-to-dashboard
     ptk/WatchEvent
     (watch [_ state _]
       (when-let [team-id (or team-id (:current-team-id state))]
         (rx/of ::dwp/force-persist
                (rt/nav :dashboard-projects {:team-id team-id})))))))

(defn go-to-dashboard-fonts
  []
   (ptk/reify ::go-to-dashboard
     ptk/WatchEvent
     (watch [_ state _]
       (let [team-id (:current-team-id state)]
         (rx/of ::dwp/force-persist
                (rt/nav :dashboard-fonts {:team-id team-id}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def ::point gpt/point?)

(defn show-context-menu
  [{:keys [position shape] :as params}]
  (us/verify ::point position)
  (us/verify (s/nilable ::cp/minimal-shape) shape)
  (ptk/reify ::show-context-menu
    ptk/UpdateEvent
    (update [_ state]
      (let [mdata (cond-> params
                    (some? shape)
                    (assoc :selected
                           (wsh/lookup-selected state)))]
        (assoc-in state [:workspace-local :context-menu] mdata)))))

(defn show-shape-context-menu
  [{:keys [position shape] :as params}]
  (us/verify ::point position)
  (us/verify ::cp/minimal-shape shape)
  (ptk/reify ::show-shape-context-menu
    ptk/WatchEvent
    (watch [_ state _]
      (let [selected (wsh/lookup-selected state)]
        (rx/concat
          (when-not (selected (:id shape))
            (rx/of (dws/select-shape (:id shape))))
          (rx/of (show-context-menu params)))))))

(def hide-context-menu
  (ptk/reify ::hide-context-menu
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :context-menu] nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn copy-selected
  []
  (letfn [;; Sort objects so they have the same relative ordering
          ;; when pasted later.
          (sort-selected [state data]
            (let [selected (:selected data)
                  page-id (:current-page-id state)
                  objects (get-in state [:workspace-data
                                         :pages-index
                                         page-id
                                         :objects])]
              (->> (uw/ask! {:cmd :selection/query-z-index
                             :page-id page-id
                             :objects objects
                             :ids selected})
                   (rx/map (fn [z-indexes]
                             (assoc data :selected
                                    (->> (d/zip selected z-indexes)
                                         (sort-by second)
                                         (map first)
                                         (into (d/ordered-set)))))))))

          ;; Retrieve all ids of selected shapes with corresponding
          ;; children; this is needed because each shape should be
          ;; processed one by one because of async events (data url
          ;; fetching).
          (collect-object-ids [objects res id]
            (let [obj (get objects id)]
              (reduce (partial collect-object-ids objects)
                      (assoc res id obj)
                      (:shapes obj))))

          ;; Prepare the shape object. Mainly needed for image shapes
          ;; for retrieve the image data and convert it to the
          ;; data-url.
          (prepare-object [objects selected {:keys [type] :as obj}]
            (let [obj (maybe-translate obj objects selected)]
              (if (= type :image)
                (let [url (cfg/resolve-file-media (:metadata obj))]
                  (->> (http/send! {:method :get
                                    :uri url
                                    :response-type :blob})
                       (rx/map :body)
                       (rx/mapcat wapi/read-file-as-data-url)
                       (rx/map #(assoc obj ::data %))
                       (rx/take 1)))
                (rx/of obj))))

          ;; Collects all the items together and split images into a
          ;; separated data structure for a more easy paste process.
          (collect-data [res {:keys [id metadata] :as item}]
            (let [res (update res :objects assoc id (dissoc item ::data))]
              (if (= :image (:type item))
                (let [img-part {:id   (:id metadata)
                                :name (:name item)
                                :file-data (::data item)}]
                  (update res :images conj img-part))
                res)))

          (maybe-translate [shape objects selected]
            (if (and (not= (:type shape) :frame)
                     (not (contains? selected (:frame-id shape))))
              ;; When the parent frame is not selected we change to relative
              ;; coordinates
              (let [frame (get objects (:frame-id shape))]
                (gsh/translate-to-frame shape frame))
              shape))

          (on-copy-error [error]
            (js/console.error "Clipboard blocked:" error)
            (rx/empty))]

    (ptk/reify ::copy-selected
      ptk/WatchEvent
      (watch [_ state _]
        (let [objects  (wsh/lookup-page-objects state)
              selected (->> (wsh/lookup-selected state)
                            (cp/clean-loops objects))
              pdata    (reduce (partial collect-object-ids objects) {} selected)
              initial  {:type :copied-shapes
                        :file-id (:current-file-id state)
                        :selected selected
                        :objects {}
                        :images #{}}]
          (->> (rx/from (seq (vals pdata)))
               (rx/merge-map (partial prepare-object objects selected))
               (rx/reduce collect-data initial)
               (rx/mapcat (partial sort-selected state))
               (rx/map t/encode-str)
               (rx/map wapi/write-to-clipboard)
               (rx/catch on-copy-error)
               (rx/ignore)))))))

(declare paste-shape)
(declare paste-text)
(declare paste-image)
(declare paste-svg)

(def paste
  (ptk/reify ::paste
    ptk/WatchEvent
    (watch [_ _ _]
      (try
        (let [clipboard-str (wapi/read-from-clipboard)

              paste-transit-str
              (->> clipboard-str
                   (rx/filter t/transit?)
                   (rx/map t/decode-str)
                   (rx/filter #(= :copied-shapes (:type %)))
                   (rx/map #(select-keys % [:selected :objects]))
                   (rx/map paste-shape))

              paste-plain-text-str
              (->> clipboard-str
                   (rx/filter (comp not empty?))
                   (rx/map paste-text))

              paste-image-str
              (->> (wapi/read-image-from-clipboard)
                   (rx/map paste-image))]

          (->> (rx/concat paste-transit-str
                          paste-plain-text-str
                          paste-image-str)
               (rx/first)
               (rx/catch
                   (fn [err]
                     (js/console.error "Clipboard error:" err)
                     (rx/empty)))))
        (catch :default e
          (let [data (ex-data e)]
            (if (:not-implemented data)
              (rx/of (dm/warn (i18n/tr "errors.clipboard-not-implemented")))
              (js/console.error "ERROR" e))))))))

(defn paste-from-event
  [event in-viewport?]
  (ptk/reify ::paste-from-event
    ptk/WatchEvent
    (watch [_ state _]
      (try
        (let [objects (wsh/lookup-page-objects state)
              paste-data    (wapi/read-from-paste-event event)
              image-data    (wapi/extract-images paste-data)
              text-data     (wapi/extract-text paste-data)
              decoded-data  (and (t/transit? text-data)
                                 (t/decode-str text-data))

              edit-id (get-in state [:workspace-local :edition])
              is-editing-text? (and edit-id (= :text (get-in objects [edit-id :type])))]

          (cond
            (and (string? text-data)
                 (str/includes? text-data "<svg"))
            (rx/of (paste-svg text-data))

            (seq image-data)
            (rx/from (map paste-image image-data))

            (coll? decoded-data)
            (->> (rx/of decoded-data)
                 (rx/filter #(= :copied-shapes (:type %)))
                 (rx/map #(paste-shape % in-viewport?)))

            ;; Some paste events can be fired while we're editing a text
            ;; we forbid that scenario so the default behaviour is executed
            (and (string? text-data) (not is-editing-text?))
            (rx/of (paste-text text-data))

            :else
            (rx/empty)))
        (catch :default err
          (js/console.error "Clipboard error:" err))))))

(defn selected-frame? [state]
  (let [selected (wsh/lookup-selected state)
        objects  (wsh/lookup-page-objects state)]
    (and (= 1 (count selected))
         (= :frame (get-in objects [(first selected) :type])))))

(defn- paste-shape
  [{:keys [selected objects images] :as data} in-viewport?] ;; TODO: perhaps rename 'objects' to 'shapes', because it contains only
  (letfn [;; Given a file-id and img (part generated by the ;;       the shapes to paste, not the whole page tree of shapes
          ;; copy-selected event), uploads the new media.
          (upload-media [file-id imgpart]
            (->> (http/send! {:uri (:file-data imgpart)
                              :response-type :blob
                              :method :get})
                 (rx/map :body)
                 (rx/map
                  (fn [blob]
                    {:name (:name imgpart)
                     :file-id file-id
                     :content blob
                     :is-local true}))
                 (rx/mapcat #(rp/mutation! :upload-file-media-object %))
                 (rx/map (fn [media]
                           (assoc media :prev-id (:id imgpart))))))

          ;; Analyze the rchange and replace staled media and
          ;; references to the new uploaded media-objects.
          (process-rchange [media-idx item]
            (if (= :image (get-in item [:obj :type]))
              (update-in item [:obj :metadata]
                         (fn [{:keys [id] :as mdata}]
                           (if-let [mobj (get media-idx id)]
                             (assoc mdata
                                    :id (:id mobj)
                                    :path (:path mobj))
                             mdata)))
              item))

          (calculate-paste-position [state mouse-pos in-viewport?]
            (let [page-objects  (wsh/lookup-page-objects state)
                  selected-objs (map #(get objects %) selected)
                  has-frame? (d/seek #(= (:type %) :frame) selected-objs)
                  page-selected (wsh/lookup-selected state)
                  wrapper       (gsh/selection-rect selected-objs)
                  orig-pos      (gpt/point (:x1 wrapper) (:y1 wrapper))]
              (cond
                (and (selected-frame? state) (not has-frame?))
                (let [frame-id (first page-selected)
                      delta    (get page-objects frame-id)]
                  [frame-id frame-id delta])

                (empty? page-selected)
                (let [frame-id (cp/frame-id-by-position page-objects mouse-pos)
                      delta    (gpt/subtract mouse-pos orig-pos)]
                  [frame-id frame-id delta])

                :else
                (let [base (cp/get-base-shape page-objects page-selected)
                      index (cp/position-on-parent (:id base) page-objects)
                      frame-id (:frame-id base)
                      parent-id (:parent-id base)
                      delta (if in-viewport?
                              (gpt/subtract mouse-pos orig-pos)
                              (gpt/subtract (gpt/point (:selrect base)) orig-pos))]
                  [frame-id parent-id delta index]))))

          ;; Change the indexes if the paste is done with an element selected
          (change-add-obj-index [objects selected index change]
            (let [set-index (fn [[result index] id]
                              [(assoc result id index) (inc index)])

                  map-ids (when index
                            (->> (vals objects)
                                 (filter #(not (selected (:parent-id %))))
                                 (map :id)
                                 (reduce set-index [{} (inc index)])
                                 first))]
              (if (and (= :add-obj (:type change))
                       (contains? map-ids (:old-id change)))
                (assoc change :index (get map-ids (:old-id change)))
                change)))

          ;; Check if the shape is an instance whose master is defined in a
          ;; library that is not linked to the current file
          (foreign-instance? [shape objects state]
            (let [root         (cph/get-root-shape shape objects)
                  root-file-id (:component-file root)]
              (and (some? root)
                   (not= root-file-id (:current-file-id state))
                   (nil? (get-in state [:workspace-libraries root-file-id])))))

          ;; Procceed with the standard shape paste procediment.
          (do-paste [it state mouse-pos media]
            (let [media-idx     (d/index-by :prev-id media)

                  ;; Calculate position for the pasted elements
                  [frame-id parent-id delta index] (calculate-paste-position state mouse-pos in-viewport?)

                  objects   (->> objects
                                 (d/mapm (fn [_ shape]
                                           (-> shape
                                               (assoc :frame-id frame-id)
                                               (assoc :parent-id parent-id)

                                               (cond->
                                                  ;; if foreign instance, detach the shape
                                                 (foreign-instance? shape objects state)
                                                 (dissoc :component-id
                                                         :component-file
                                                         :component-root?
                                                         :remote-synced?
                                                         :shape-ref
                                                         :touched))))))

                  page-id   (:current-page-id state)
                  unames    (-> (wsh/lookup-page-objects state page-id)
                                (dwc/retrieve-used-names)) ;; TODO: move this calculation inside prepare-duplcate-changes?

                  rchanges  (->> (dws/prepare-duplicate-changes objects page-id unames selected delta)
                                 (mapv (partial process-rchange media-idx))
                                 (mapv (partial change-add-obj-index objects selected index)))

                  uchanges  (mapv #(array-map :type :del-obj :page-id page-id :id (:id %))
                                  (reverse rchanges))

                  ;; Adds a reg-objects operation so the groups are updated. We add all the new objects
                  new-objects-ids (->> rchanges (filter #(= (:type %) :add-obj)) (mapv :id))

                  rchanges (conj rchanges {:type :reg-objects
                                           :page-id page-id
                                           :shapes new-objects-ids})

                  selected  (->> rchanges
                                 (filter #(selected (:old-id %)))
                                 (map #(get-in % [:obj :id]))
                                 (into (d/ordered-set)))]

              (rx/of (dch/commit-changes {:redo-changes rchanges
                                          :undo-changes uchanges
                                          :origin it})
                     (dwc/select-shapes selected))))]
    (ptk/reify ::paste-shape
      ptk/WatchEvent
      (watch [it state _]
        (let [file-id   (:current-file-id state)
              mouse-pos (deref ms/mouse-position)]
          (if (= file-id (:file-id data))
            (do-paste it state mouse-pos [])
            (->> (rx/from images)
                 (rx/merge-map (partial upload-media file-id))
                 (rx/reduce conj [])
                 (rx/mapcat (partial do-paste it state mouse-pos)))))))))


(defn as-content [text]
  (let [paragraphs (->> (str/lines text)
                        (map str/trim)
                        (mapv #(hash-map :type "paragraph"
                                         :children [{:text %}])))]
    {:type "root"
     :children [{:type "paragraph-set" :children paragraphs}]}))

(defn paste-text
  [text]
  (s/assert string? text)
  (ptk/reify ::paste-text
    ptk/WatchEvent
    (watch [_ state _]
      (let [id (uuid/next)
            {:keys [x y]} @ms/mouse-position
            width (max 8 (min (* 7 (count text)) 700))
            height 16
            page-id (:current-page-id state)
            frame-id (-> (wsh/lookup-page-objects state page-id)
                         (cp/frame-id-by-position @ms/mouse-position))
            shape (gsh/setup-selrect
                   {:id id
                    :type :text
                    :name "Text"
                    :x x
                    :y y
                    :frame-id frame-id
                    :width width
                    :height height
                    :grow-type (if (> (count text) 100) :auto-height :auto-width)
                    :content (as-content text)})]
        (rx/of (dwu/start-undo-transaction)
               (dws/deselect-all)
               (dwc/add-shape shape)
               (dwu/commit-undo-transaction))))))

(defn- paste-svg
  [text]
  (s/assert string? text)
  (ptk/reify ::paste-svg
    ptk/WatchEvent
    (watch [_ state _]
      (let [position (deref ms/mouse-position)
            file-id  (:current-file-id state)]
        (->> (dwp/parse-svg ["svg" text])
             (rx/map #(svg/svg-uploaded % file-id position)))))))

(defn- paste-image
  [image]
  (ptk/reify ::paste-bin-impl
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (get-in state [:workspace-file :id])
            params  {:file-id file-id
                     :blobs [image]
                     :position @ms/mouse-position}]
        (rx/of (dwp/upload-media-workspace params))))))

(defn toggle-distances-display [value]
  (ptk/reify ::toggle-distances-display

    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :show-distances?] value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare move-create-interaction)
(declare finish-create-interaction)

(defn start-create-interaction
  []
  (ptk/reify ::start-create-interaction
    ptk/WatchEvent
    (watch [_ state stream]
      (let [initial-pos @ms/mouse-position
            selected (wsh/lookup-selected state)
            stopper (rx/filter ms/mouse-up? stream)]
        (when (= 1 (count selected))
          (rx/concat
            (->> ms/mouse-position
                 (rx/take-until stopper)
                 (rx/map #(move-create-interaction initial-pos %)))
            (rx/of (finish-create-interaction initial-pos))))))))

(defn move-create-interaction
  [initial-pos position]
  (ptk/reify ::move-create-interaction
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            selected-shape-id (-> state wsh/lookup-selected first)
            selected-shape (get objects selected-shape-id)
            selected-shape-frame-id (:frame-id selected-shape)
            start-frame (get objects selected-shape-frame-id)
            end-frame   (dwc/get-frame-at-point objects position)]
        (cond-> state
          (not= position initial-pos) (assoc-in [:workspace-local :draw-interaction-to] position)
          (not= start-frame end-frame) (assoc-in [:workspace-local :draw-interaction-to-frame] end-frame))))))

(defn finish-create-interaction
  [initial-pos]
  (ptk/reify ::finish-create-interaction
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-local :draw-interaction-to] nil)
          (assoc-in [:workspace-local :draw-interaction-to-frame] nil)))

    ptk/WatchEvent
    (watch [_ state _]
      (let [position @ms/mouse-position
            page-id  (:current-page-id state)
            objects  (wsh/lookup-page-objects state page-id)
            frame    (dwc/get-frame-at-point objects position)

            shape-id (-> state wsh/lookup-selected first)
            shape    (get objects shape-id)]

        (when-not (= position initial-pos)
          (if (and frame shape-id
                   (not= (:id frame) (:id shape))
                   (not= (:id frame) (:frame-id shape)))
            (rx/of (update-shape shape-id
                                 {:interactions [{:event-type :click
                                                  :action-type :navigate
                                                  :destination (:id frame)}]}))
            (rx/of (update-shape shape-id
                                 {:interactions []}))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CANVAS OPTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-canvas-color
  [color]
  (ptk/reify ::change-canvas-color
    ptk/WatchEvent
    (watch [it state _]
      (let [page-id (get state :current-page-id)
            options (wsh/lookup-page-options state page-id)
            previus-color  (:background options)]
        (rx/of (dch/commit-changes
                {:redo-changes [{:type :set-option
                                 :page-id page-id
                                 :option :background
                                 :value (:color color)}]
                 :undo-changes [{:type :set-option
                                 :page-id page-id
                                 :option :background
                                 :value previus-color}]
                 :origin it}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transform

(d/export dwt/start-resize)
(d/export dwt/update-dimensions)
(d/export dwt/start-rotate)
(d/export dwt/increase-rotation)
(d/export dwt/start-move-selected)
(d/export dwt/move-selected)
(d/export dwt/update-position)
(d/export dwt/flip-horizontal-selected)
(d/export dwt/flip-vertical-selected)

;; Persistence

(d/export dwp/set-file-shared)
(d/export dwp/fetch-shared-files)
(d/export dwp/link-file-to-library)
(d/export dwp/unlink-file-from-library)
(d/export dwp/upload-media-asset)
(d/export dwp/upload-media-workspace)
(d/export dwp/clone-media-object)
(d/export dwc/image-uploaded)

;; Selection

(d/export dws/select-shape)
(d/export dws/deselect-shape)
(d/export dws/select-all)
(d/export dws/deselect-all)
(d/export dwc/select-shapes)
(d/export dws/shift-select-shapes)
(d/export dws/duplicate-selected)
(d/export dws/handle-area-selection)
(d/export dws/select-inside-group)
(d/export dwd/select-for-drawing)
(d/export dwc/clear-edition-mode)
(d/export dwc/add-shape)
(d/export dwc/start-edition-mode)

;; Groups

(d/export dwg/mask-group)
(d/export dwg/unmask-group)
(d/export dwg/group-selected)
(d/export dwg/ungroup-selected)
