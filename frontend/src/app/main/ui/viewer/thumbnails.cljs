;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.thumbnails
  (:require
   [app.common.data :as d]
   [app.main.data.viewer :as dv]
   [app.main.exports :as exports]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown']]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [goog.object :as gobj]
   [rumext.alpha :as mf]))

(mf/defc thumbnails-content
  [{:keys [children expanded? total] :as props}]
  (let [container (mf/use-ref)
        width     (mf/use-var (.. js/document -documentElement -clientWidth))
        element-width (mf/use-var 152)

        offset (mf/use-state 0)

        on-left-arrow-click
        (fn [_]
          (swap! offset (fn [v]
                          (if (pos? v)
                            (dec v)
                            v))))

        on-right-arrow-click
        (fn [_]
          (let [visible (/ @width @element-width)
                max-val (- total visible)]
            (swap! offset (fn [v]
                            (if (< v max-val)
                              (inc v)
                              v)))))

        on-scroll
        (fn [event]
          (if (pos? (.. event -nativeEvent -deltaY))
            (on-right-arrow-click event)
            (on-left-arrow-click event)))

        on-mount
        (fn []
          (let [dom (mf/ref-val container)]
            (reset! width (gobj/get dom "clientWidth"))))]

    (mf/use-effect on-mount)
    (if expanded?
      [:div.thumbnails-content
       [:div.thumbnails-list-expanded children]]
      [:div.thumbnails-content
       [:div.left-scroll-handler {:on-click on-left-arrow-click} i/arrow-slide]
       [:div.right-scroll-handler {:on-click on-right-arrow-click} i/arrow-slide]
       [:div.thumbnails-list {:ref container :on-wheel on-scroll}
        [:div.thumbnails-list-inside {:style {:right (str (* @offset 152) "px")}}
         children]]])))

(mf/defc thumbnails-summary
  [{:keys [on-toggle-expand on-close total] :as props}]
  [:div.thumbnails-summary
   [:span.counter (tr "labels.num-of-frames" (i18n/c total))]
   [:span.buttons
    [:span.btn-expand {:on-click on-toggle-expand} i/arrow-down]
    [:span.btn-close {:on-click on-close} i/close]]])

(mf/defc thumbnail-item
  [{:keys [selected? frame on-click index objects] :as props}]
  [:div.thumbnail-item {:on-click #(on-click % index)}
   [:div.thumbnail-preview
    {:class (dom/classnames :selected selected?)}
    [:& exports/frame-svg {:frame frame :objects objects}]]
   [:div.thumbnail-info
    [:span.name {:title (:name frame)} (:name frame)]]])

(mf/defc thumbnails-panel
  [{:keys [frames page index] :as props}]
  (let [expanded? (mf/use-state false)
        container (mf/use-ref)

        frame     (get frames index)

        objects   (:objects page)
        index     (:index frame)

        on-close  #(st/emit! dv/toggle-thumbnails-panel)
        selected  (mf/use-var false)

        on-mouse-leave
        (fn [_]
          (when @selected
            (on-close)))

        on-item-click
        (fn [_ index]
          (compare-and-set! selected false true)
          (st/emit! (dv/go-to-frame-by-index index))
          (when @expanded?
            (on-close)))]

    [:& dropdown' {:on-close on-close
                   :container container
                   :show true}
     [:section.viewer-thumbnails
      {:class (dom/classnames :expanded @expanded?)
       :ref container
       :on-mouse-leave on-mouse-leave}

      [:& thumbnails-summary {:on-toggle-expand #(swap! expanded? not)
                              :on-close on-close
                              :total (count frames)}]
      [:& thumbnails-content {:expanded? @expanded?
                              :total (count frames)}
       (for [[i frame] (d/enumerate frames)]
         [:& thumbnail-item {:key i
                             :index i
                             :frame frame
                             :objects objects
                             :on-click on-item-click
                             :selected? (= i index)}])]]]))
