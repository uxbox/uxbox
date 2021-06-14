;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.pages.changes
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.geom.shapes :as gsh]
   [app.common.pages.common :refer [component-sync-attrs]]
   [app.common.pages.helpers :as cph]
   [app.common.pages.init :as init]
   [app.common.pages.spec :as spec]
   [app.common.spec :as us]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page Transformation Changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Changes Processing Impl

(defmulti process-change (fn [_ change] (:type change)))
(defmulti process-operation (fn [_ op] (:type op)))

(defn process-changes
  ([data items] (process-changes data items true))
  ([data items verify?]
   ;; When verify? false we spec the schema validation. Currently used to make just
   ;; 1 validation even if the changes are applied twice
   (when verify?
     (us/assert ::spec/changes items))

   (let [pages  (into #{} (map :page-id) items)
         result (->> items
                     (reduce #(or (process-change %1 %2) %1) data))]

     ;; Validate result shapes (only on the backend)
     #?(:clj
        (doseq [page-id pages]
          (let [page (get-in result [:pages-index page-id])]
            (doseq [[id shape] (:objects page)]
              (when-not (= shape (get-in data [:pages-index page-id :objects id]))
                ;; If object has change verify is correct
                (us/verify ::spec/shape shape))))))

     result)))

(defmethod process-change :set-option
  [data {:keys [page-id option value]}]
  (d/update-in-when data [:pages-index page-id]
                    (fn [data]
                      (let [path (if (seqable? option) option [option])]
                        (if value
                          (assoc-in data (into [:options] path) value)
                          (assoc data :options (d/dissoc-in (:options data) path)))))))

(defmethod process-change :add-obj
  [data {:keys [id obj page-id component-id frame-id parent-id
                index ignore-touched]}]
  (letfn [(update-fn [data]
            (let [parent-id (or parent-id frame-id)
                  objects   (:objects data)
                  obj (assoc obj
                             :frame-id frame-id
                             :parent-id parent-id
                             :id id)]
              (if (and (or (nil? parent-id) (contains? objects parent-id))
                       (or (nil? frame-id) (contains? objects frame-id)))
                (-> data
                    (update :objects assoc id obj)
                    (update-in [:objects parent-id :shapes]
                               (fn [shapes]
                                 (let [shapes (or shapes [])]
                                   (cond
                                     (some #{id} shapes)
                                     shapes

                                     (nil? index)
                                     (if (= :frame (:type obj))
                                       (d/concat [id] shapes)
                                       (conj shapes id))

                                     :else
                                     (cph/insert-at-index shapes index [id])))))

                    (cond-> (and (:shape-ref (get-in data [:objects parent-id]))
                                 (not= parent-id frame-id)
                                 (not ignore-touched))
                      (->
                        (update-in [:objects parent-id :touched]
                                   cph/set-touched-group :shapes-group)
                        (d/dissoc-in [:objects parent-id :remote-synced?]))))
                data)))]
    (if page-id
      (d/update-in-when data [:pages-index page-id] update-fn)
      (d/update-in-when data [:components component-id] update-fn))))

(defmethod process-change :mod-obj
  [data {:keys [id page-id component-id operations]}]
  (let [update-fn (fn [objects]
                    (if-let [obj (get objects id)]
                      (let [result (reduce process-operation obj operations)]
                        (assoc objects id result))
                      objects))]
    (if page-id
      (d/update-in-when data [:pages-index page-id :objects] update-fn)
      (d/update-in-when data [:components component-id :objects] update-fn))))

(defmethod process-change :del-obj
  [data {:keys [page-id component-id id ignore-touched]}]
  (letfn [(delete-object [objects id]
            (if-let [target (get objects id)]
              (let [parent-id (cph/get-parent id objects)
                    frame-id  (:frame-id target)
                    parent    (get objects parent-id)
                    objects   (dissoc objects id)]
                (cond-> objects
                  (and (not= parent-id frame-id)
                       (#{:group :svg-raw} (:type parent)))
                  (update-in [parent-id :shapes] (fn [s] (filterv #(not= % id) s)))

                  (and (:shape-ref parent) (not ignore-touched))
                  (->
                    (update-in [parent-id :touched] cph/set-touched-group :shapes-group)
                    (d/dissoc-in [parent-id :remote-synced?]))

                  (contains? objects frame-id)
                  (update-in [frame-id :shapes] (fn [s] (filterv #(not= % id) s)))

                  (seq (:shapes target))   ; Recursive delete all
                                           ; dependend objects
                  (as-> $ (reduce delete-object $ (:shapes target)))))
              objects))]
    (if page-id
      (d/update-in-when data [:pages-index page-id :objects] delete-object id)
      (d/update-in-when data [:components component-id :objects] delete-object id))))

;; reg-objects operation "regenerates" the geometry and selrect of the parent groups
(defmethod process-change :reg-objects
  [data {:keys [page-id component-id shapes]}]
  (letfn [(reg-objects [objects]
            (reduce #(d/update-when %1 %2 update-group %1) objects
                    (sequence (comp
                               (mapcat #(cons % (cph/get-parents % objects)))
                               (map #(get objects %))
                               (filter #(= (:type %) :group))
                               (map :id)
                               (distinct))
                              shapes)))
          (set-mask-selrect [group children]
            (let [mask (first children)]
              (-> group
                  (merge (select-keys mask [:selrect :points]))
                  (assoc :x (-> mask :selrect :x)
                         :y (-> mask :selrect :y)
                         :width (-> mask :selrect :width)
                         :height (-> mask :selrect :height)
                         :flip-x (-> mask :flip-x)
                         :flip-y (-> mask :flip-y)))))
          (update-group [group objects]
            (let [children (->> group :shapes (map #(get objects %)))]
              (cond
                ;; If the group is empty we don't make any changes. Should be removed by a later process
                (empty? children)
                group

                (:masked-group? group)
                (set-mask-selrect group children)

                :else
                (gsh/update-group-selrect group children))))]

    (if page-id
      (d/update-in-when data [:pages-index page-id :objects] reg-objects)
      (d/update-in-when data [:components component-id :objects] reg-objects))))

(defmethod process-change :mov-objects
  [data {:keys [parent-id shapes index page-id component-id ignore-touched]}]
  (letfn [(is-valid-move? [objects shape-id]
            (let [invalid-targets (cph/calculate-invalid-targets shape-id objects)]
              (and (not (invalid-targets parent-id))
                   (cph/valid-frame-target shape-id parent-id objects))))

          (insert-items [prev-shapes index shapes]
            (let [prev-shapes (or prev-shapes [])]
              (if index
                (cph/insert-at-index prev-shapes index shapes)
                (cph/append-at-the-end prev-shapes shapes))))

          (check-insert-items [prev-shapes parent index shapes]
            (if-not (:masked-group? parent)
              (insert-items prev-shapes index shapes)
              ;; For masked groups, the first shape is the mask
              ;; and it cannot be moved.
              (let [mask-id (first prev-shapes)
                    other-ids (rest prev-shapes)
                    not-mask-shapes (strip-id shapes mask-id)
                    new-index (if (nil? index) nil (max (dec index) 0))
                    new-shapes (insert-items other-ids new-index not-mask-shapes)]
                (d/concat [mask-id] new-shapes))))

          (strip-id [coll id]
            (filterv #(not= % id) coll))

          (add-to-parent [parent index shapes]
            (cond-> parent
              true
              (update :shapes check-insert-items parent index shapes)

              (and (:shape-ref parent) (= (:type parent) :group) (not ignore-touched))
              (->
                (update :touched cph/set-touched-group :shapes-group)
                (dissoc :remote-synced?))))

          (remove-from-old-parent [cpindex objects shape-id]
            (let [prev-parent-id (get cpindex shape-id)]
              ;; Do nothing if the parent id of the shape is the same as
              ;; the new destination target parent id.
              (if (= prev-parent-id parent-id)
                objects
                (let [sid shape-id
                      pid prev-parent-id
                      obj (get objects pid)
                      component? (and (:shape-ref obj)
                                      (= (:type obj) :group)
                                      (not ignore-touched))]

                  (-> objects
                      (d/update-in-when [pid :shapes] strip-id sid)

                      (cond-> component?
                        (d/update-when
                         pid
                         #(-> %
                              (update :touched cph/set-touched-group :shapes-group)
                              (dissoc :remote-synced?)))))))))

          (update-parent-id [objects id]
            (-> objects
                (d/update-when id assoc :parent-id parent-id)))

          ;; Updates the frame-id references that might be outdated
          (assign-frame-id [frame-id objects id]
            (let [objects (-> objects
                              (d/update-when id assoc :frame-id frame-id))
                  obj     (get objects id)]
              (cond-> objects
                ;; If we moving frame, the parent frame is the root
                ;; and we DO NOT NEED update children because the
                ;; children will point correctly to the frame what we
                ;; are currently moving
                (not= :frame (:type obj))
                (as-> $$ (reduce (partial assign-frame-id frame-id) $$ (:shapes obj))))))

          (move-objects [objects]
            (let [valid?   (every? (partial is-valid-move? objects) shapes)

                  ;; Create a index of shape ids pointing to the
                  ;; corresponding parents; used mainly for update old
                  ;; parents after move operation.
                  cpindex  (reduce (fn [index id]
                                     (let [obj (get objects id)]
                                       (assoc! index id (:parent-id obj))))
                                   (transient {})
                                   (keys objects))
                  cpindex  (persistent! cpindex)

                  parent   (get objects parent-id)
                  frame-id (if (= :frame (:type parent))
                             (:id parent)
                             (:frame-id parent))]

              (if (and valid? (seq shapes))
                (as-> objects $
                  ;; Add the new shapes to the parent object.
                  (d/update-when $ parent-id #(add-to-parent % index shapes))

                  ;; Update each individual shape link to the new parent
                  (reduce update-parent-id $ shapes)

                  ;; Analyze the old parents and clear the old links
                  ;; only if the new parrent is different form old
                  ;; parent.
                  (reduce (partial remove-from-old-parent cpindex) $ shapes)

                  ;; Ensure that all shapes of the new parent has a
                  ;; correct link to the topside frame.
                  (reduce (partial assign-frame-id frame-id) $ shapes))
              objects)))]

    (if page-id
      (d/update-in-when data [:pages-index page-id :objects] move-objects)
      (d/update-in-when data [:components component-id :objects] move-objects))))

(defmethod process-change :add-page
  [data {:keys [id name page]}]
  (cond
    (and (string? name) (uuid? id))
    (let [page (assoc init/empty-page-data
                      :id id
                      :name name)]
      (-> data
          (update :pages conj id)
          (update :pages-index assoc id page)))

    (map? page)
    (-> data
        (update :pages conj (:id page))
        (update :pages-index assoc (:id page) page))

    :else
    (ex/raise :type :conflict
              :hint "name or page should be provided, never both")))

(defmethod process-change :mod-page
  [data {:keys [id name]}]
  (d/update-in-when data [:pages-index id] assoc :name name))

(defmethod process-change :del-page
  [data {:keys [id]}]
  (-> data
      (update :pages (fn [pages] (filterv #(not= % id) pages)))
      (update :pages-index dissoc id)))

(defmethod process-change :mov-page
  [data {:keys [id index]}]
  (update data :pages cph/insert-at-index index [id]))

(defmethod process-change :add-color
  [data {:keys [color]}]
  (update data :colors assoc (:id color) color))

(defmethod process-change :mod-color
  [data {:keys [color]}]
  (d/assoc-in-when data [:colors (:id color)] color))

(defmethod process-change :del-color
  [data {:keys [id]}]
  (update data :colors dissoc id))

(defmethod process-change :add-recent-color
  [data {:keys [color]}]
  ;; Moves the color to the top of the list and then truncates up to 15
  (update data :recent-colors (fn [rc]
                                (let [rc (conj (filterv (comp not #{color}) (or rc [])) color)]
                                  (if (> (count rc) 15)
                                    (subvec rc 1)
                                    rc)))))

;; -- Media

(defmethod process-change :add-media
  [data {:keys [object]}]
  (update data :media assoc (:id object) object))

(defmethod process-change :mod-media
  [data {:keys [object]}]
  (d/update-in-when data [:media (:id object)] merge object))

(defmethod process-change :del-media
  [data {:keys [id]}]
  (update data :media dissoc id))

;; -- Components

(defmethod process-change :add-component
  [data {:keys [id name path shapes]}]
  (assoc-in data [:components id]
            {:id id
             :name name
             :path path
             :objects (d/index-by :id shapes)}))

(defmethod process-change :mod-component
  [data {:keys [id name path objects]}]
  (update-in data [:components id]
             #(cond-> %
                  (some? name)
                  (assoc :name name)

                  (some? path)
                  (assoc :path path)

                  (some? objects)
                  (assoc :objects objects))))

(defmethod process-change :del-component
  [data {:keys [id]}]
  (d/dissoc-in data [:components id]))

;; -- Typography

(defmethod process-change :add-typography
  [data {:keys [typography]}]
  (update data :typographies assoc (:id typography) typography))

(defmethod process-change :mod-typography
  [data {:keys [typography]}]
  (d/update-in-when data [:typographies (:id typography)] merge typography))

(defmethod process-change :del-typography
  [data {:keys [id]}]
  (update data :typographies dissoc id))

;; -- Operations

(defmethod process-operation :set
  [shape op]
  (let [attr       (:attr op)
        val        (:val op)
        ignore     (:ignore-touched op)
        shape-ref  (:shape-ref shape)
        group      (get component-sync-attrs attr)
        root-name? (and (= group :name-group)
                        (:component-root? shape))]

    ;; (js/console.log "**********shape" (:name shape))
    ;; (js/console.log "attr" (clj->js attr))
    ;; (js/console.log "ignore-geometry" (clj->js ignore-geometry))
    (cond-> shape
      (and shape-ref group (not ignore) (not= val (get shape attr))
           (not root-name?)
           ;; FIXME: it's difficult to tell if the geometry changes affect
           ;;        an individual shape inside the component, or are for
           ;;        the whole component (in which case we shouldn't set
           ;;        touched). For the moment we disable geometry touched
           ;;        except width and height that seems to work well.
           (or (not= group :geometry-group) (#{:width :height} attr)))
      (->
        (update :touched cph/set-touched-group group)
        (dissoc :remote-synced?))

      (nil? val)
      (dissoc attr)

      (some? val)
      (assoc attr val))))

(defmethod process-operation :set-touched
  [shape op]
  (let [touched (:touched op)
        shape-ref (:shape-ref shape)]
    (if (or (nil? shape-ref) (nil? touched) (empty? touched))
      (dissoc shape :touched)
      (assoc shape :touched touched))))

(defmethod process-operation :set-remote-synced
  [shape op]
  (let [remote-synced? (:remote-synced? op)
        shape-ref (:shape-ref shape)]
    (if (or (nil? shape-ref) (not remote-synced?))
      (dissoc shape :remote-synced?)
      (assoc shape :remote-synced? true))))

(defmethod process-operation :default
  [_ op]
  (ex/raise :type :not-implemented
            :code :operation-not-implemented
            :context {:type (:type op)}))

