;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.texts
  (:require
   [app.common.attrs :as attrs]
   [app.common.data :as d]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.pages :as cp]
   [app.common.text :as txt]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.util.router :as rt]
   [app.util.text-editor :as ted]
   [app.util.timers :as ts]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn update-editor
  [editor]
  (ptk/reify ::update-editor
    ptk/UpdateEvent
    (update [_ state]
      (if (some? editor)
        (assoc state :workspace-editor editor)
        (dissoc state :workspace-editor)))))

(defn focus-editor
  []
  (ptk/reify ::focus-editor
    ptk/EffectEvent
    (effect [_ state _]
      (when-let [editor (:workspace-editor state)]
        (ts/schedule #(.focus ^js editor))))))

(defn update-editor-state
  [{:keys [id] :as shape} editor-state]
  (ptk/reify ::update-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (if (some? editor-state)
        (update state :workspace-editor-state assoc id editor-state)
        (update state :workspace-editor-state dissoc id)))))

(defn finalize-editor-state
  [{:keys [id] :as shape}]
  (ptk/reify ::finalize-editor-state
    ptk/WatchEvent
    (watch [_ state _]
      (let [content (-> (get-in state [:workspace-editor-state id])
                        (ted/get-editor-current-content))]

        (if (ted/content-has-text? content)
          (let [content (d/merge (ted/export-content content)
                                 (dissoc (:content shape) :children))]
            (rx/merge
             (rx/of (update-editor-state shape nil))
             (when (and (not= content (:content shape))
                        (some? (:current-page-id state)))
               (rx/of
                (dch/update-shapes [id] #(assoc % :content content))
                (dwu/commit-undo-transaction)))))
          (rx/of (dws/deselect-shape id)
                 (dwc/delete-shapes #{id})))))))

(defn initialize-editor-state
  [{:keys [id content] :as shape} decorator]
  (ptk/reify ::initialize-editor-state
    ptk/UpdateEvent
    (update [_ state]
      (let [text-state (some->> content ted/import-content)
            attrs (get-in state [:workspace-local :defaults :font])

            editor (cond-> (ted/create-editor-state text-state decorator)
                     (and (nil? content) (some? attrs))
                     (ted/update-editor-current-block-data attrs))]
        (-> state
            (assoc-in [:workspace-editor-state id] editor))))

    ptk/WatchEvent
    (watch [_ _ stream]
      ;; We need to finalize editor on two main events: (1) when user
      ;; explicitly navigates to other section or page; (2) when user
      ;; leaves the editor.
      (->> (rx/merge
            (rx/filter (ptk/type? ::rt/navigate) stream)
            (rx/filter #(= ::finalize-editor-state %) stream))
           (rx/take 1)
           (rx/map #(finalize-editor-state shape))))))

(defn select-all
  "Select all content of the current editor. When not editor found this
  event is noop."
  [{:keys [id] :as shape}]
  (ptk/reify ::editor-select-all
    ptk/UpdateEvent
    (update [_ state]
      (d/update-in-when state [:workspace-editor-state id] ted/editor-select-all))))

(defn cursor-to-end
  [{:keys [id] :as shape}]
  (ptk/reify ::cursor-to-end
    ptk/UpdateEvent
    (update [_ state]
      (d/update-in-when state [:workspace-editor-state id] ted/cursor-to-end))))

;; --- Helpers

(defn- shape-current-values
  [shape pred attrs]
  (let [root  (:content shape)
        nodes (->> (txt/node-seq pred root)
                   (map #(if (txt/is-text-node? %)
                           (merge txt/default-text-attrs %)
                           %)))]
    (attrs/get-attrs-multi nodes attrs)))

(defn current-root-values
  [{:keys [attrs shape]}]
  (shape-current-values shape txt/is-root-node? attrs))

(defn current-paragraph-values
  [{:keys [editor-state attrs shape]}]
  (if editor-state
    (-> (ted/get-editor-current-block-data editor-state)
        (select-keys attrs))
    (shape-current-values shape txt/is-paragraph-node? attrs)))

(defn current-text-values
  [{:keys [editor-state attrs shape]}]
  (if editor-state
    (-> (ted/get-editor-current-inline-styles editor-state)
        (select-keys attrs))
    (shape-current-values shape txt/is-text-node? attrs)))


;; --- TEXT EDITION IMPL

(defn- update-shape
  [shape pred-fn merge-fn attrs]
  (let [merge-attrs #(merge-fn % attrs)
        transform   #(txt/transform-nodes pred-fn merge-attrs %)]
    (update shape :content transform)))

(defn update-root-attrs
  [{:keys [id attrs]}]
  (ptk/reify ::update-root-attrs
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects   (wsh/lookup-page-objects state)
            shape     (get objects id)

            update-fn #(update-shape % txt/is-root-node? attrs/merge attrs)
            shape-ids (cond (= (:type shape) :text)  [id]
                            (= (:type shape) :group) (cp/get-children id objects))]

        (rx/of (dch/update-shapes shape-ids update-fn))))))

(defn update-paragraph-attrs
  [{:keys [id attrs]}]
  (let [attrs (d/without-nils attrs)]
    (ptk/reify ::update-paragraph-attrs
      ptk/UpdateEvent
      (update [_ state]
        (d/update-in-when state [:workspace-editor-state id] ted/update-editor-current-block-data attrs))

      ptk/WatchEvent
      (watch [_ state _]
        (when-not (some? (get-in state [:workspace-editor-state id]))
          (let [objects   (wsh/lookup-page-objects state)
                shape     (get objects id)

                merge-fn  (fn [node attrs]
                            (reduce-kv (fn [node k v]
                                         (if (= (get node k) v)
                                           (dissoc node k)
                                           (assoc node k v)))
                                       node
                                       attrs))

                update-fn #(update-shape % txt/is-paragraph-node? merge-fn attrs)
                shape-ids (cond (= (:type shape) :text)  [id]
                                (= (:type shape) :group) (cp/get-children id objects))]

            (rx/of (dch/update-shapes shape-ids update-fn))))))))

(defn update-text-attrs
  [{:keys [id attrs]}]
  (ptk/reify ::update-text-attrs
    ptk/UpdateEvent
    (update [_ state]
      (d/update-in-when state [:workspace-editor-state id] ted/update-editor-current-inline-styles attrs))

    ptk/WatchEvent
    (watch [_ state _]
      (when-not (some? (get-in state [:workspace-editor-state id]))
        (let [objects   (wsh/lookup-page-objects state)
              shape     (get objects id)
              update-node? (fn [node]
                             (or (txt/is-text-node? node)
                                 (txt/is-paragraph-node? node)))

              update-fn #(update-shape % update-node? attrs/merge attrs)
              shape-ids (cond (= (:type shape) :text)  [id]
                              (= (:type shape) :group) (cp/get-children id objects))]
          (rx/of (dch/update-shapes shape-ids update-fn)))))))

;; --- RESIZE UTILS

(defn update-overflow-text [id value]
  (ptk/reify ::update-overflow-text
    ptk/UpdateEvent
    (update [_ state]
      (let [page-id (:current-page-id state)]
        (update-in state [:workspace-data :pages-index page-id :objects id] assoc :overflow-text value)))))


(def start-edit-if-selected
  (ptk/reify ::start-edit-if-selected
    ptk/UpdateEvent
    (update [_ state]
      (let [objects  (wsh/lookup-page-objects state)
            selected (->> state wsh/lookup-selected (mapv #(get objects %)))]
        (cond-> state
          (and (= 1 (count selected))
               (= (-> selected first :type) :text))
          (assoc-in [:workspace-local :edition] (-> selected first :id)))))))

(defn not-changed? [old-dim new-dim]
  (> (mth/abs (- old-dim new-dim)) 0.1))

(defn resize-text-batch [changes]
  (ptk/reify ::resize-text-batch
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id  (:current-page-id state)
            objects (get-in state [:workspace-data :pages-index page-id :objects])]
        (if-not (every? #(contains? objects(first %)) changes)
          (rx/empty)

          (let [changes-map (->> changes (into {}))
                ids (keys changes-map)
                update-fn
                (fn [shape]
                  (let [[new-width new-height] (get changes-map (:id shape))
                        {:keys [selrect grow-type overflow-text]} (gsh/transform-shape shape)
                        {shape-width :width shape-height :height} selrect

                        modifier-width (gsh/resize-modifiers shape :width new-width)
                        modifier-height (gsh/resize-modifiers shape :height new-height)]

                    (cond-> shape
                      (and overflow-text (not= :fixed grow-type))
                      (assoc :overflow-text false)

                      (and (= :fixed grow-type) (not overflow-text) (> new-height shape-height))
                      (assoc :overflow-text true)

                      (and (= :fixed grow-type) overflow-text (<= new-height shape-height))
                      (assoc :overflow-text false)

                      (and (not-changed? shape-width new-width) (= grow-type :auto-width))
                      (-> (assoc :modifiers modifier-width)
                          (gsh/transform-shape))

                      (and (not-changed? shape-height new-height)
                           (or (= grow-type :auto-height) (= grow-type :auto-width)))
                      (-> (assoc :modifiers modifier-height)
                          (gsh/transform-shape)))))]

            (rx/of (dch/update-shapes ids update-fn {:reg-objects? true}))))))))

;; When a resize-event arrives we start "buffering" for a time
;; after that time we invoke `resize-text-batch` with all the changes
;; together. This improves the performance because we only re-render the
;; resized components once even if there are changes that applies to
;; lots of texts like changing a font
(defn resize-text
  [id new-width new-height]
  (ptk/reify ::resize-text
    IDeref
    (-deref [_]
      {:id id :width new-width :height new-height})

    ptk/WatchEvent
    (watch [_ state stream]
      (let [;; This stream aggregates the events of "resizing"
            resize-events
            (rx/merge
             (->> (rx/of (resize-text id new-width new-height)))
             (->> stream (rx/filter (ptk/type? ::resize-text))))

            ;; Stop buffering after time without resizes
            stop-buffer (->> resize-events (rx/debounce 100))

            ;; Agregates the resizes so only send the resize when the sizes are stable
            resize-batch
            (->> resize-events
                 (rx/take-until stop-buffer)
                 (rx/reduce (fn [acc event]
                              (assoc acc (:id @event) [(:width @event) (:height @event)]))
                            {id [new-width new-height]})
                 (rx/map #(resize-text-batch %)))

            ;; This stream retrieves the changes of page so we cancel the agregation
            change-page
            (->> stream
                 (rx/filter (ptk/type? :app.main.data.workspace/finalize-page))
                 (rx/take 1)
                 (rx/ignore))]

        (if-not (::handling-texts state)
          (->> (rx/concat
                (rx/of #(assoc % ::handling-texts true))
                (rx/race resize-batch change-page)
                (rx/of #(dissoc % ::handling-texts))))
          (rx/empty))))))

(defn save-font
  [data]
  (ptk/reify ::save-font
    ptk/UpdateEvent
    (update [_ state]
      (let [multiple? (->> data vals (d/seek #(= % :multiple)))]
        (cond-> state
          (not multiple?)
          (assoc-in [:workspace-local :defaults :font] data))))))

