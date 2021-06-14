;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.transforms
  "Events related with shapes transformations"
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.math :as mth]
   [app.common.pages :as cp]
   [app.common.spec :as us]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.selection :as dws]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.undo :as dwu]
   [app.main.refs :as refs]
   [app.main.snap :as snap]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [app.util.path.shapes-to-path :as ups]
   [beicon.core :as rx]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [potok.core :as ptk]))

;; -- Declarations

(declare set-modifiers)
(declare set-rotation)
(declare apply-modifiers)

;; -- Helpers

;; For each of the 8 handlers gives the modifier for resize
;; for example, right will only grow in the x coordinate and left
;; will grow in the inverse of the x coordinate
(def ^:private handler-modifiers
  {:right        [ 1  0]
   :bottom       [ 0  1]
   :left         [-1  0]
   :top          [ 0 -1]
   :top-right    [ 1 -1]
   :top-left     [-1 -1]
   :bottom-right [ 1  1]
   :bottom-left  [-1  1]})

;; Given a handler returns the coordinate origin for resizes
;; this is the opposite of the handler so for right we want the
;; left side as origin of the resize
;; sx, sy => start x/y
;; mx, my => middle x/y
;; ex, ey => end x/y
(defn- handler-resize-origin [{sx :x sy :y :keys [width height]} handler]
  (let [mx (+ sx (/ width 2))
        my (+ sy (/ height 2))
        ex (+ sx width)
        ey (+ sy height)

        [x y] (case handler
                :right [sx my]
                :bottom [mx sy]
                :left [ex my]
                :top [mx ey]
                :top-right [sx ey]
                :top-left [ex ey]
                :bottom-right [sx sy]
                :bottom-left [ex sy])]
    (gpt/point x y)))

(defn- fix-init-point
  "Fix the initial point so the resizes are accurate"
  [initial handler shape]
  (let [{:keys [x y width height]} (:selrect shape)
        {:keys [rotation]} shape
        rotation (or rotation 0)]
    (if (= rotation 0)
      (cond-> initial
        (contains? #{:left :top-left :bottom-left} handler)
        (assoc :x x)

        (contains? #{:right :top-right :bottom-right} handler)
        (assoc :x (+ x width))

        (contains? #{:top :top-right :top-left} handler)
        (assoc :y y)

        (contains? #{:bottom :bottom-right :bottom-left} handler)
        (assoc :y (+ y height)))
      initial)))

(defn finish-transform []
  (ptk/reify ::finish-transform
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local dissoc :transform))))

;; -- RESIZE
(defn start-resize
  [handler initial ids shape]
  (letfn [(resize [shape initial resizing-shapes layout [point lock? point-snap]]
            (let [{:keys [width height]} (:selrect shape)
                  {:keys [rotation]} shape
                  rotation (or rotation 0)

                  initial (fix-init-point initial handler shape)

                  shapev (-> (gpt/point width height))

                  scale-text (:scale-text layout)

                  ;; Force lock if the scale text mode is active
                  lock? (or lock? scale-text)

                  ;; Vector modifiers depending on the handler
                  handler-modif (let [[x y] (handler-modifiers handler)] (gpt/point x y))

                  ;; Difference between the origin point in the coordinate system of the rotation
                  deltav (-> (gpt/to-vec initial (if (= rotation 0) point-snap point))
                             (gpt/transform (gmt/rotate-matrix (- rotation)))
                             (gpt/multiply handler-modif))

                  ;; Resize vector
                  scalev (gpt/divide (gpt/add shapev deltav) shapev)

                  scalev (if lock?
                           (let [v (cond
                                     (#{:right :left} handler) (:x scalev)
                                     (#{:top :bottom} handler) (:y scalev)
                                     :else (max (:x scalev) (:y scalev)))]
                             (gpt/point v v))

                           scalev)

                  shape-transform (:transform shape (gmt/matrix))
                  shape-transform-inverse (:transform-inverse shape (gmt/matrix))

                  shape-center (gsh/center-shape shape)

                  ;; Resize origin point given the selected handler
                  origin  (-> (handler-resize-origin (:selrect shape) handler)
                              (gsh/transform-point-center shape-center shape-transform))]

              (rx/of (set-modifiers ids
                                    {:resize-vector scalev
                                     :resize-origin origin
                                     :resize-transform shape-transform
                                     :resize-scale-text scale-text
                                     :resize-transform-inverse shape-transform-inverse}))))

          ;; Unifies the instantaneous proportion lock modifier
          ;; activated by Shift key and the shapes own proportion
          ;; lock flag that can be activated on element options.
          (normalize-proportion-lock [[point shift?]]
            (let [proportion-lock? (:proportion-lock shape)]
              [point (or proportion-lock? shift?)]))]
    (reify
      ptk/UpdateEvent
      (update [_ state]
        (-> state
            (assoc-in [:workspace-local :transform] :resize)))

      ptk/WatchEvent
      (watch [it state stream]
        (let [initial-position @ms/mouse-position
              stoper  (rx/filter ms/mouse-up? stream)
              layout  (:workspace-layout state)
              page-id (:current-page-id state)
              zoom    (get-in state [:workspace-local :zoom] 1)
              objects (wsh/lookup-page-objects state page-id)
              resizing-shapes (map #(get objects %) ids)
              text-shapes-ids (->> resizing-shapes
                                   (filter #(= :text (:type %)))
                                   (map :id))]
          (rx/concat
           (rx/of (dch/update-shapes text-shapes-ids #(assoc % :grow-type :fixed)))
           (->> ms/mouse-position
                (rx/with-latest vector ms/mouse-position-shift)
                (rx/map normalize-proportion-lock)
                (rx/switch-map (fn [[point :as current]]
                               (->> (snap/closest-snap-point page-id resizing-shapes layout zoom point)
                                    (rx/map #(conj current %)))))
                (rx/mapcat (partial resize shape initial-position resizing-shapes layout))
                (rx/take-until stoper))
           (rx/of (apply-modifiers ids)
                  (finish-transform))))))))


(defn start-rotate
  [shapes]
  (ptk/reify ::start-rotate
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-local :transform] :rotate)))

    ptk/WatchEvent
    (watch [it state stream]
      (let [stoper          (rx/filter ms/mouse-up? stream)
            group           (gsh/selection-rect shapes)
            group-center    (gsh/center-selrect group)
            initial-angle   (gpt/angle @ms/mouse-position group-center)
            calculate-angle (fn [pos ctrl?]
                              (let [angle (- (gpt/angle pos group-center) initial-angle)
                                    angle (if (neg? angle) (+ 360 angle) angle)
                                    modval (mod angle 45)
                                    angle (if ctrl?
                                            (if (< 22.5 modval)
                                              (+ angle (- 45 modval))
                                              (- angle modval))
                                            angle)
                                    angle (if (= angle 360)
                                            0
                                            angle)]
                                angle))]
        (rx/concat
         (->> ms/mouse-position
              (rx/with-latest vector ms/mouse-position-ctrl)
              (rx/map (fn [[pos ctrl?]]
                        (let [delta-angle (calculate-angle pos ctrl?)]
                          (set-rotation delta-angle shapes group-center))))
              (rx/take-until stoper))
         (rx/of (apply-modifiers (map :id shapes))
                (finish-transform)))))))

;; -- MOVE

(declare start-move)
(declare start-move-duplicate)
(declare start-local-displacement)
(declare clear-local-transform)

(defn start-move-selected
  []
  (ptk/reify ::start-move-selected
    ptk/WatchEvent
    (watch [it state stream]
      (let [initial  (deref ms/mouse-position)
            selected (wsh/lookup-selected state {:omit-blocked? true})
            stopper  (rx/filter ms/mouse-up? stream)]
        (when-not (empty? selected)
          (->> ms/mouse-position
               (rx/take-until stopper)
               (rx/map #(gpt/to-vec initial %))
               (rx/map #(gpt/length %))
               (rx/filter #(> % 1))
               (rx/take 1)
               (rx/with-latest vector ms/mouse-position-alt)
               (rx/mapcat
                (fn [[_ alt?]]
                  (if alt?
                    ;; When alt is down we start a duplicate+move
                    (rx/of (start-move-duplicate initial)
                           dws/duplicate-selected)
                    ;; Otherwise just plain old move
                    (rx/of (start-move initial selected)))))))))))

(defn start-move-duplicate [from-position]
  (ptk/reify ::start-move-selected
    ptk/WatchEvent
    (watch [it state stream]
      (->> stream
           (rx/filter (ptk/type? ::dws/duplicate-selected))
           (rx/first)
           (rx/map #(start-move from-position))))))

(defn calculate-frame-for-move [ids]
  (ptk/reify ::calculate-frame-for-move
    ptk/WatchEvent
    (watch [it state stream]
      (let [position @ms/mouse-position
            page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            frame-id (cp/frame-id-by-position objects position)

            moving-shapes (->> ids
                               (cp/clean-loops objects)
                               (map #(get objects %))
                               (remove #(or (nil? %)
                                            (= (:frame-id %) frame-id))))

            rch [{:type :mov-objects
                  :page-id page-id
                  :parent-id frame-id
                  :shapes (mapv :id moving-shapes)}]


            uch (->> moving-shapes
                     (reverse)
                     (mapv (fn [shape]
                             {:type :mov-objects
                              :page-id page-id
                              :parent-id (:parent-id shape)
                              :index (cp/get-index-in-parent objects (:id shape))
                              :shapes [(:id shape)]})))]

        (when-not (empty? uch)
          (rx/of dwu/pop-undo-into-transaction
                 (dch/commit-changes {:redo-changes rch
                                      :undo-changes uch
                                      :origin it})
                 (dwu/commit-undo-transaction)
                 (dwc/expand-collapse frame-id)))))))

(defn start-move
  ([from-position] (start-move from-position nil))
  ([from-position ids]
   (ptk/reify ::start-move
     ptk/UpdateEvent
     (update [_ state]
       (-> state
           (assoc-in [:workspace-local :transform] :move)))

     ptk/WatchEvent
     (watch [it state stream]
       (let [page-id (:current-page-id state)
             objects (wsh/lookup-page-objects state page-id)
             selected (wsh/lookup-selected state {:omit-blocked? true})
             ids     (if (nil? ids) selected ids)
             shapes  (mapv #(get objects %) ids)
             stopper (rx/filter ms/mouse-up? stream)
             layout  (get state :workspace-layout)
             zoom    (get-in state [:workspace-local :zoom] 1)


             position (->> ms/mouse-position
                           (rx/take-until stopper)
                           (rx/map #(gpt/to-vec from-position %)))

             snap-delta (rx/concat
                         ;; We send the nil first so the stream is not waiting for the first value
                         (rx/of nil)
                         (->> position
                              (rx/throttle 20)
                              (rx/switch-map
                               (fn [pos]
                                 (->> (snap/closest-snap-move page-id shapes objects layout zoom pos)
                                      (rx/map #(vector pos %)))))))]
         (if (empty? shapes)
           (rx/empty)
           (rx/concat
            (->> position
                 (rx/with-latest vector snap-delta)
                 (rx/map snap/correct-snap-point)
                 (rx/map start-local-displacement))

            (rx/of (apply-modifiers ids {:set-modifiers? true})
                   (calculate-frame-for-move ids)
                   (finish-transform)))))))))

(defn- get-displacement-with-grid
  "Retrieve the correct displacement delta point for the
  provided direction speed and distances thresholds."
  [shape direction options]
  (let [grid-x (:grid-x options 10)
        grid-y (:grid-y options 10)
        x-mod (mod (:x shape) grid-x)
        y-mod (mod (:y shape) grid-y)]
    (case direction
      :up (gpt/point 0 (- (if (zero? y-mod) grid-y y-mod)))
      :down (gpt/point 0 (- grid-y y-mod))
      :left (gpt/point (- (if (zero? x-mod) grid-x x-mod)) 0)
      :right (gpt/point (- grid-x x-mod) 0))))

(defn- get-displacement
  "Retrieve the correct displacement delta point for the
  provided direction speed and distances thresholds."
  [direction]
  (case direction
    :up (gpt/point 0 (- 1))
    :down (gpt/point 0 1)
    :left (gpt/point (- 1) 0)
    :right (gpt/point 1 0)))

(s/def ::direction #{:up :down :right :left})

(defn move-selected
  [direction shift?]
  (us/verify ::direction direction)
  (us/verify boolean? shift?)

  (let [same-event (js/Symbol "same-event")]
    (ptk/reify ::move-selected
      IDeref
      (-deref [_] direction)

      ptk/UpdateEvent
      (update [_ state]
        (if (nil? (get-in state [:workspace-local :current-move-selected]))
          (-> state
              (assoc-in [:workspace-local :transform] :move)
              (assoc-in [:workspace-local :current-move-selected] same-event))
          state))

      ptk/WatchEvent
      (watch [it state stream]
        (if (= same-event (get-in state [:workspace-local :current-move-selected]))
          (let [selected (wsh/lookup-selected state {:omit-blocked? true})
                move-events (->> stream
                                 (rx/filter (ptk/type? ::move-selected))
                                 (rx/filter #(= direction (deref %))))
                stopper (->> move-events
                             (rx/debounce 100)
                             (rx/first))
                scale (if shift? (gpt/point 10) (gpt/point 1))
                mov-vec (gpt/multiply (get-displacement direction) scale)]

            (rx/concat
             (rx/merge
              (->> move-events
                   (rx/take-until stopper)
                   (rx/scan #(gpt/add %1 mov-vec) (gpt/point 0 0))
                   (rx/map start-local-displacement))
              (rx/of (move-selected direction shift?)))

             (rx/of (apply-modifiers selected {:set-modifiers? true})
                    (finish-transform))))
            (rx/empty))))))


;; -- Apply modifiers

(defn- set-modifiers-recursive
  [modif-tree objects shape modifiers]
  (let [children (->> (get shape :shapes [])
                      (map #(get objects %)))

        resized-shape (when (seq children) ; <- don't calculate it if not needed
                        (gsh/transform-shape
                          (assoc shape :modifiers (select-keys modifiers
                                                               [:resize-origin
                                                                :resize-vector]))))

        set-child (fn [modif-tree child]
                    (let [child-modifiers (gsh/calc-child-modifiers shape
                                                                    resized-shape
                                                                    child
                                                                    modifiers)]
                      (set-modifiers-recursive modif-tree
                                               objects
                                               child
                                               child-modifiers)))]

    (reduce set-child
            (update-in modif-tree [(:id shape) :modifiers] #(merge % modifiers))
            children)))

(defn set-modifiers
  ([ids] (set-modifiers ids nil))
  ([ids modifiers]
   (us/verify (s/coll-of uuid?) ids)
   (ptk/reify ::set-modifiers
     ptk/UpdateEvent
     (update [_ state]
       (let [modifiers (or modifiers (get-in state [:workspace-local :modifiers] {}))
             page-id (:current-page-id state)
             objects (wsh/lookup-page-objects state page-id)

             ids (->> ids (into #{} (remove #(get-in objects [% :blocked] false))))]

         (reduce (fn [state id]
                   (update state :workspace-modifiers
                           #(set-modifiers-recursive %
                                                     objects
                                                     (get objects id)
                                                     modifiers)))
                 state
                 ids))))))

;; Set-rotation is custom because applies different modifiers to each
;; shape adjusting their position.

(defn set-rotation
  ([angle shapes]
   (set-rotation angle shapes (-> shapes gsh/selection-rect gsh/center-selrect)))

  ([angle shapes center]
   (ptk/reify ::set-rotation
     ptk/UpdateEvent
     (update [_ state]
       (let [objects (wsh/lookup-page-objects state)
             id->obj #(get objects %)
             get-children (fn [shape] (map id->obj (cp/get-children (:id shape) objects)))

             shapes (->> shapes (into [] (remove #(get % :blocked false))))

             shapes (->> shapes (mapcat get-children) (concat shapes))

             update-shape
             (fn [modifiers shape]
               (let [rotate-modifiers (gsh/rotation-modifiers shape center angle)]
                 (assoc-in modifiers [(:id shape) :modifiers] rotate-modifiers)))]
         (-> state
             (update :workspace-modifiers
                     #(reduce update-shape % shapes))))))))

(defn increase-rotation [ids rotation]
  (ptk/reify ::increase-rotation
    ptk/WatchEvent
    (watch [it state stream]

      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            rotate-shape (fn [shape]
                           (let [delta (- rotation (:rotation shape))]
                             (set-rotation delta [shape])))]
        (rx/concat
         (rx/from (->> ids (map #(get objects %)) (map rotate-shape)))
         (rx/of (apply-modifiers ids)))))))

(defn apply-modifiers
  ([ids]
   (apply-modifiers ids nil))

  ([ids {:keys [set-modifiers?]
         :or   {set-modifiers? false}}]
   (us/verify (s/coll-of uuid?) ids)
   (ptk/reify ::apply-modifiers
     ptk/WatchEvent
     (watch [it state stream]
       (let [objects (wsh/lookup-page-objects state)
             children-ids (->> ids (mapcat #(cp/get-children % objects)))
             ids-with-children (d/concat [] children-ids ids)

             state (if set-modifiers?
                     (ptk/update (set-modifiers ids) state)
                     state)
             object-modifiers (get state :workspace-modifiers)]

         (rx/of (dwu/start-undo-transaction)
                (dch/update-shapes
                 ids-with-children
                 (fn [shape]
                   (-> shape
                       (merge (get object-modifiers (:id shape)))
                       (gsh/transform-shape)))
                 {:reg-objects? true
                  ;; Attributes that can change in the transform. This way we don't have to check
                  ;; all the attributes
                  :attrs [:selrect :points
                          :x :y
                          :width :height
                          :content
                          :transform
                          :transform-inverse
                          :rotation
                          :flip-x
                          :flip-y]
                  })
                (clear-local-transform)
                (dwu/commit-undo-transaction)))))))

;; --- Update Dimensions

;; Event mainly used for handling user modification of the size of the
;; object from workspace sidebar options inputs.

(defn update-dimensions
  [ids attr value]
  (us/verify (s/coll-of ::us/uuid) ids)
  (us/verify #{:width :height} attr)
  (us/verify ::us/number value)
  (ptk/reify ::update-dimensions
    ptk/UpdateEvent
    (update [_ state]

      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)

            update-children
            (fn [objects ids modifiers]
              (reduce #(assoc-in %1 [%2 :modifiers] modifiers) objects ids))

            ;; For each shape updates the modifiers given as arguments
            update-shape
            (fn [objects shape-id]
              (let [shape (get objects shape-id)
                    modifier (gsh/resize-modifiers shape attr value)]
                (set-modifiers-recursive objects objects shape modifier)))]

        (d/update-in-when
         state
         [:workspace-data :pages-index page-id :objects]
         #(reduce update-shape % ids))))

    ptk/WatchEvent
    (watch [it state stream]
      (let [page-id (:current-page-id state)
            objects (wsh/lookup-page-objects state page-id)
            ids (d/concat [] ids (mapcat #(cp/get-children % objects) ids))]
        (rx/of (apply-modifiers ids))))))

(defn flip-horizontal-selected []
  (ptk/reify ::flip-horizontal-selected
    ptk/WatchEvent
    (watch [it state stream]
      (let [objects  (wsh/lookup-page-objects state)
            selected (wsh/lookup-selected state {:omit-blocked? true})
            shapes   (map #(get objects %) selected)
            selrect  (gsh/selection-rect (->> shapes (map gsh/transform-shape)))
            origin   (gpt/point (:x selrect) (+ (:y selrect) (/ (:height selrect) 2)))]

        (rx/of (set-modifiers selected
                              {:resize-vector (gpt/point -1.0 1.0)
                               :resize-origin origin
                               :displacement (gmt/translate-matrix (gpt/point (- (:width selrect)) 0))})
               (apply-modifiers selected))))))

(defn flip-vertical-selected []
  (ptk/reify ::flip-vertical-selected
    ptk/WatchEvent
    (watch [it state stream]
      (let [objects  (wsh/lookup-page-objects state)
            selected (wsh/lookup-selected state {:omit-blocked? true})
            shapes   (map #(get objects %) selected)
            selrect  (gsh/selection-rect (->> shapes (map gsh/transform-shape)))
            origin   (gpt/point (+ (:x selrect) (/ (:width selrect) 2)) (:y selrect))]

        (rx/of (set-modifiers selected
                              {:resize-vector (gpt/point 1.0 -1.0)
                               :resize-origin origin
                               :displacement (gmt/translate-matrix (gpt/point 0 (- (:height selrect))))})
               (apply-modifiers selected))))))

(defn start-local-displacement [point]
  (ptk/reify ::start-local-displacement
    ptk/UpdateEvent
    (update [_ state]
      (let [mtx (gmt/translate-matrix point)]
        (-> state
            (assoc-in [:workspace-local :modifiers] {:displacement mtx}))))))

(defn clear-local-transform []
  (ptk/reify ::clear-local-transform
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (dissoc :workspace-modifiers)
          (update :workspace-local dissoc :modifiers :current-move-selected)))))

(defn selected-to-path
  []
  (ptk/reify ::selected-to-path
    ptk/WatchEvent
    (watch [_ state stream]
      (let [ids (wsh/lookup-selected state {:omit-blocked? true})]
        (rx/of (dch/update-shapes ids ups/convert-to-path))))))
