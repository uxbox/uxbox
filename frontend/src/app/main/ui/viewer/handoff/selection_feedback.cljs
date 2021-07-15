;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.selection-feedback
  (:require
   [app.common.geom.shapes :as gsh]
   [app.main.store :as st]
   [app.main.ui.measurements :refer [selection-guides size-display measurement]]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

;; ------------------------------------------------
;; CONSTANTS
;; ------------------------------------------------

(def select-color "#1FDEA7")
(def selection-rect-width 1)
(def select-guide-width 1)
(def select-guide-dasharray 5)

;; ------------------------------------------------
;; LENSES
;; ------------------------------------------------

(defn make-selected-shapes-iref
  "Creates a lens to the current selected shapes"
  []
  (let [selected->shapes
        (fn [state]
          (let [selected (get-in state [:viewer-local :selected])
                objects  (get-in state [:viewer-data :objects])
                resolve-shape #(get objects %)]
            (->> selected (map resolve-shape) (filterv (comp not nil?)))))]
    #(l/derived selected->shapes st/state)))


(defn make-hover-shapes-iref
  "Creates a lens to the shapes the user is making hover"
  []
  (let [hover->shapes
        (fn [state]
          (let [hover   (get-in state [:viewer-local :hover])
                objects (get-in state [:viewer-data :objects])]
            (get objects hover)))]
    #(l/derived hover->shapes st/state)))


(defn- resolve-shapes
  [objects ids]
  (let [resolve-shape #(get objects %)]
    (into [] (comp (map resolve-shape)
                   (filter some?))
          ids)))


(def selected-zoom
  (l/derived (l/in [:viewer-local :zoom]) st/state))

;; ------------------------------------------------
;; HELPERS
;; ------------------------------------------------

(defn frame->bounds [frame]
  {:x 0
   :y 0
   :width (:width frame)
   :height (:height frame)})

;; ------------------------------------------------
;; COMPONENTS
;; ------------------------------------------------

(mf/defc selection-rect [{:keys [selrect zoom]}]
  (let [{:keys [x y width height]} selrect
        selection-rect-width (/ selection-rect-width zoom)]
    [:g.selection-rect
     [:rect {:x x
             :y y
             :width width
             :height height
             :style {:fill "none"
                     :stroke select-color
                     :stroke-width selection-rect-width}}]]))

(mf/defc selection-feedback
  [{:keys [frame local objects]}]
  (let [{:keys [hover selected zoom]} local

        ;; hover-shapes-ref (mf/use-memo (make-hover-shapes-iref))
        ;; hover-shape (-> (or (mf/deref hover-shapes-ref) frame)
        ;;                 (gsh/translate-to-frame frame))

        ;; selected-shapes-ref (mf/use-memo (make-selected-shapes-iref))
        ;; selected-shapes (->> (mf/deref selected-shapes-ref)
        ;;                      (map #(gsh/translate-to-frame % frame)))

        hover-shape     (or (first (resolve-shapes objects [hover])) frame)
        selected-shapes (resolve-shapes objects selected)

        selrect         (gsh/selection-rect selected-shapes)
        bounds          (frame->bounds frame)]

    (when (seq selected-shapes)
      [:g.selection-feedback {:pointer-events "none"}
       [:g.selected-shapes
        [:& selection-guides {:bounds bounds :selrect selrect :zoom zoom}]
        [:& selection-rect {:selrect selrect :zoom zoom}]
        [:& size-display {:selrect selrect :zoom zoom}]]

       [:& measurement {:bounds bounds
                        :selected-shapes selected-shapes
                        :hover-shape hover-shape
                        :zoom zoom}]])))
