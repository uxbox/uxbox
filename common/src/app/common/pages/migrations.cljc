;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.pages.migrations
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.path :as gsp]
   [app.common.math :as mth]
   [app.common.pages :as cp]
   [app.common.uuid :as uuid]))

;; TODO: revisit this and rename to file-migrations

(defmulti migrate :version)

(defn migrate-data
  ([data]
   (if (= (:version data) cp/file-version)
     data
     (reduce #(migrate-data %1 %2 (inc %2))
             data
             (range (:version data 0) cp/file-version))))

  ([data _ to-version]
   (-> data
       (assoc :version to-version)
       (migrate))))

(defn migrate-file
  [file]
  (-> file
      (update :data assoc :id (:id file))
      (update :data migrate-data)))

;; Default handler, noop
(defmethod migrate :default [data] data)

;; -- MIGRATIONS --

;; Ensure that all :shape attributes on shapes are vectors.
(defmethod migrate 2
  [data]
  (letfn [(update-object [_ object]
            (d/update-when object :shapes
                           (fn [shapes]
                             (if (seq? shapes)
                               (into [] shapes)
                               shapes))))

          (update-page [_ page]
            (update page :objects #(d/mapm update-object %)))]

    (update data :pages-index #(d/mapm update-page %))))

;; Changes paths formats
(defmethod migrate 3
  [data]
  (letfn [(migrate-path [shape]
            (if-not (contains? shape :content)
              (let [content (gsp/segments->content (:segments shape) (:close? shape))
                    selrect (gsh/content->selrect content)
                    points  (gsh/rect->points selrect)]
                (-> shape
                    (dissoc :segments)
                    (dissoc :close?)
                    (assoc :content content)
                    (assoc :selrect selrect)
                    (assoc :points points)))
              ;; If the shape contains :content is already in the new format
              shape))

          (fix-frames-selrects [frame]
            (if (= (:id frame) uuid/zero)
              frame
              (let [frame-rect (select-keys frame [:x :y :width :height])]
                (-> frame
                    (assoc :selrect (gsh/rect->selrect frame-rect))
                    (assoc :points (gsh/rect->points frame-rect))))))

          (fix-empty-points [shape]
            (let [shape (cond-> shape
                          (empty? (:selrect shape)) (gsh/setup-selrect))]
              (cond-> shape
                (empty? (:points shape))
                (assoc :points (gsh/rect->points (:selrect shape))))))

          (update-object [_ object]
            (cond-> object
              (= :curve (:type object))
              (assoc :type :path)

              (#{:curve :path} (:type object))
              (migrate-path)

              (= :frame (:type object))
              (fix-frames-selrects)

              (and (empty? (:points object)) (not= (:id object) uuid/zero))
              (fix-empty-points)

              :always
              (->
               ;; Setup an empty transformation to re-calculate selrects
               ;; and points data
               (assoc :modifiers {:displacement (gmt/matrix)})
               (gsh/transform-shape))

              ))

          (update-page [_ page]
            (update page :objects #(d/mapm update-object %)))]

    (update data :pages-index #(d/mapm update-page %))))

;; We did rollback version 4 migration.
;; Keep this in order to remember the next version to be 5
(defmethod migrate 4 [data] data)

;; Put the id of the local file in :component-file in instances of local components
(defmethod migrate 5
  [data]
  (letfn [(update-object [_ object]
            (if (and (some? (:component-id object))
                     (nil? (:component-file object)))
              (assoc object :component-file (:id data))
              object))

          (update-page [_ page]
            (update page :objects #(d/mapm update-object %)))]

    (update data :pages-index #(d/mapm update-page %))))

(defn fix-line-paths
  "Fixes issues with selrect/points for shapes with width/height = 0 (line-like paths)"
  [_ shape]
  (if (= (:type shape) :path)
    (let [{:keys [width height]} (gsh/points->rect (:points shape))]
      (if (or (mth/almost-zero? width) (mth/almost-zero? height))
        (let [selrect (gsh/content->selrect (:content shape))
              points (gsh/rect->points selrect)
              transform (gmt/matrix)
              transform-inv (gmt/matrix)]
          (assoc shape
                 :selrect selrect
                 :points points
                 :transform transform
                 :transform-inverse transform-inv))
        shape))
    shape))


(defmethod migrate 6
  [data]
  (letfn [(update-container [_ container]
            (-> container
                (update :objects #(d/mapm fix-line-paths %))))]

    (-> data
        (update :components  #(d/mapm update-container %))
        (update :pages-index #(d/mapm update-container %)))))


;; Remove interactions pointing to deleted frames
(defmethod migrate 7
  [data]
  (letfn [(update-object [page _ object]
            (d/update-when object :interactions
              (fn [interactions]
                (filterv #(get-in page [:objects (:destination %)])
                         interactions))))

          (update-page [_ page]
            (update page :objects #(d/mapm (partial update-object page) %)))]

    (update data :pages-index #(d/mapm update-page %))))


;; Remove groups without any shape, both in pages and components

(defmethod migrate 8
  [data]
  (letfn [(clean-parents [obj deleted?]
            (d/update-when obj :shapes
                           (fn [shapes]
                             (into [] (remove deleted?) shapes))))

          (obj-is-empty? [obj]
            (and (= (:type obj) :group)
                 (or (empty? (:shapes obj))
                     (nil? (:selrect obj)))))

          (clean-objects [objects]
            (loop [entries (seq objects)
                   deleted #{}
                   result  objects]
              (let [[id obj :as entry] (first entries)]
                (if entry
                  (if (obj-is-empty? obj)
                    (recur (rest entries)
                           (conj deleted id)
                           (dissoc result id))
                    (recur (rest entries)
                           deleted
                           result))
                  [(count deleted)
                   (d/mapm #(clean-parents %2 deleted) result)]))))

          (clean-container [_ container]
            (loop [n       0
                   objects (:objects container)]
              (let [[deleted objects] (clean-objects objects)]
                (if (and (pos? deleted) (< n 1000))
                  (recur (inc n) objects)
                  (assoc container :objects objects)))))]

    (-> data
        (update :pages-index #(d/mapm clean-container %))
        (d/update-when :components #(d/mapm clean-container %)))))

(defmethod migrate 9
  [data]
  (letfn [(find-empty-groups [objects]
            (->> (vals objects)
                 (filter (fn [shape]
                           (and (= :group (:type shape))
                                (or (empty? (:shapes shape))
                                    (every? (fn [child-id]
                                              (not (contains? objects child-id)))
                                            (:shapes shape))))))
                 (map :id)))

          (calculate-changes [[page-id page]]
            (let [objects (:objects page)
                  eids    (find-empty-groups objects)]

              (map (fn [id]
                     {:type :del-obj
                      :page-id page-id
                      :id id})
                   eids)))]

    (loop [data data]
      (let [changes (mapcat calculate-changes (:pages-index data))]
        (if (seq changes)
          (recur (cp/process-changes data changes))
          data)))))

(defmethod migrate 10
  [data]
  (letfn [(update-page [_ page]
            (d/update-in-when page [:objects uuid/zero] dissoc :points :selrect))]
    (update data :pages-index #(d/mapm update-page %))))

(defmethod migrate 11
  [data]
  (letfn [(update-object [objects _id shape]
            (if (= :frame (:type shape))
              (d/update-when shape :shapes (fn [shapes]
                                             (filterv (fn [id] (contains? objects id)) shapes)))
              shape))

          (update-page [_ page]
            (update page :objects #(d/mapm (partial update-object %) %)))]

    (update data :pages-index #(d/mapm update-page %))))
