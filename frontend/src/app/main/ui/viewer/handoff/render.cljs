;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.render
  "The main container for a frame in handoff mode"
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as geom]
   [app.common.pages :as cp]
   [app.main.data.viewer :as dv]
   [app.main.store :as st]
   [app.main.ui.shapes.circle :as circle]
   [app.main.ui.shapes.frame :as frame]
   [app.main.ui.shapes.group :as group]
   [app.main.ui.shapes.image :as image]
   [app.main.ui.shapes.path :as path]
   [app.main.ui.shapes.rect :as rect]
   [app.main.ui.shapes.shape :refer [shape-container]]
   [app.main.ui.shapes.svg-raw :as svg-raw]
   [app.main.ui.shapes.text :as text]
   [app.main.ui.viewer.handoff.selection-feedback :refer [selection-feedback]]
   [app.main.ui.viewer.interactions :refer [prepare-objects]]
   [app.util.dom :as dom]
   [app.util.object :as obj]
   [rumext.alpha :as mf]))

(declare shape-container-factory)

(defn handle-hover-shape
  [{:keys [type id]} hover?]
  (fn [event]
    (when-not (#{:group :frame} type)
      (dom/prevent-default event)
      (dom/stop-propagation event)
      (st/emit! (dv/hover-shape id hover?)))))

(defn select-shape [{:keys [type id]}]
  (fn [event]
    (when-not (#{:group :frame} type)
      (dom/stop-propagation event)
      (dom/prevent-default event)
      (cond
        (.-shiftKey ^js event)
        (st/emit! (dv/toggle-selection id))

        :else
        (st/emit! (dv/select-shape id))))))

(defn shape-wrapper-factory
  [component]
  (mf/fnc shape-wrapper
    {::mf/wrap-props false}
    [props]
    (let [shape (unchecked-get props "shape")
          childs (unchecked-get props "childs")
          frame  (unchecked-get props "frame")
          render-wrapper? (or (not= :svg-raw (:type shape))
                              (svg-raw/graphic-element? (get-in shape [:content :tag])))]

      (if render-wrapper?
        [:> shape-container {:shape shape
                             :on-mouse-enter (handle-hover-shape shape true)
                             :on-mouse-leave (handle-hover-shape shape false)
                             :on-click (select-shape shape)}
         [:& component {:shape shape
                        :frame frame
                        :childs childs
                        :is-child-selected? true}]]

        ;; Don't wrap svg elements inside a <g> otherwise some can break
        [:& component {:shape shape
                       :frame frame
                       :childs childs}]))))

(defn frame-container-factory
  [objects]
  (let [shape-container (shape-container-factory objects)
        frame-shape     (frame/frame-shape shape-container)
        frame-wrapper   (shape-wrapper-factory frame-shape)]
    (mf/fnc frame-container
      {::mf/wrap-props false}
      [props]
      (let [shape (unchecked-get props "shape")
            childs (mapv #(get objects %) (:shapes shape))
            shape  (geom/transform-shape shape)

            props (-> (obj/new)
                      (obj/merge! props)
                      (obj/merge! #js {:shape shape
                                       :childs childs}))]
        [:> frame-wrapper props]))))

(defn group-container-factory
  [objects]
  (let [shape-container (shape-container-factory objects)
        group-shape     (group/group-shape shape-container)
        group-wrapper   (shape-wrapper-factory group-shape)]
    (mf/fnc group-container
      {::mf/wrap-props false}
      [props]
      (let [shape  (unchecked-get props "shape")
            childs (mapv #(get objects %) (:shapes shape))
            props (-> (obj/new)
                      (obj/merge! props)
                      (obj/merge! #js {:childs childs}))]
        [:> group-wrapper props]))))

(defn svg-raw-container-factory
  [objects]
  (let [shape-container (shape-container-factory objects)
        svg-raw-shape   (svg-raw/svg-raw-shape shape-container)
        svg-raw-wrapper (shape-wrapper-factory svg-raw-shape)]
    (mf/fnc group-container
      {::mf/wrap-props false}
      [props]
      (let [shape  (unchecked-get props "shape")
            childs (mapv #(get objects %) (:shapes shape))
            props (-> (obj/new)
                      (obj/merge! props)
                      (obj/merge! #js {:childs childs}))]
        [:> svg-raw-wrapper props]))))

(defn shape-container-factory
  [objects]
  (let [path-wrapper   (shape-wrapper-factory path/path-shape)
        text-wrapper   (shape-wrapper-factory text/text-shape)
        rect-wrapper   (shape-wrapper-factory rect/rect-shape)
        image-wrapper  (shape-wrapper-factory image/image-shape)
        circle-wrapper (shape-wrapper-factory circle/circle-shape)]
    (mf/fnc shape-container
      {::mf/wrap-props false}
      [props]
      (let [shape (unchecked-get props "shape")
            frame (unchecked-get props "frame")
            group-container (mf/use-memo
                             (mf/deps objects)
                             #(group-container-factory objects))
            svg-raw-container (mf/use-memo
                               (mf/deps objects)
                               #(svg-raw-container-factory objects))]
        (when (and shape (not (:hidden shape)))
          (let [shape (-> (geom/transform-shape shape)
                          (geom/translate-to-frame frame))
                opts #js {:shape shape
                          :frame frame}]
            (case (:type shape)
              :text    [:> text-wrapper opts]
              :rect    [:> rect-wrapper opts]
              :path    [:> path-wrapper opts]
              :image   [:> image-wrapper opts]
              :circle  [:> circle-wrapper opts]
              :group   [:> group-container opts]
              :svg-raw [:> svg-raw-container opts])))))))

(mf/defc render-frame-svg
  [{:keys [page zoom frame local]}]

  (let [objects (mf/use-memo
                 (mf/deps page frame)
                 (prepare-objects page frame))


        ;; Retrieve frame again with correct modifier
        frame   (get objects (:id frame))

        zoom    (:zoom local 1)
        width   (* (:width frame) zoom)
        height  (* (:height frame) zoom)
        vbox    (str "0 0 " (:width frame 0) " " (:height frame 0))

        render  (mf/use-memo
                 (mf/deps objects)
                 #(frame-container-factory objects))]

    [:svg
     {:id "svg-frame"
      :view-box vbox
      :width width
      :height height
      :version "1.1"
      :xmlnsXlink "http://www.w3.org/1999/xlink"
      :xmlns "http://www.w3.org/2000/svg"}

     [:& render {:shape frame :view-box vbox}]
     [:& selection-feedback
      {:frame frame
       :objects objects
       :local local}]]))

