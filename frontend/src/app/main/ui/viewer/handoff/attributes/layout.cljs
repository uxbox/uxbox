;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.attributes.layout
  (:require
   [app.common.math :as mth]
   [app.main.ui.components.copy-button :refer [copy-button]]
   [app.util.code-gen :as cg]
   [app.util.i18n :refer [t]]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(def properties [:width :height :x :y :radius :rx :r1])

(def params
  {:to-prop {:x "left"
             :y "top"
             :rotation "transform"
             :rx "border-radius"
             :r1 "border-radius"}
   :format  {:rotation #(str/fmt "rotate(%sdeg)" %)
             :r1 #(apply str/fmt "%spx, %spx, %spx, %spx" %)}
   :multi   {:r1 [:r1 :r2 :r3 :r4]}})

(defn copy-data
  ([shape]
   (apply copy-data shape properties))
  ([shape & properties]
   (cg/generate-css-props shape properties params)))

(mf/defc layout-block
  [{:keys [shape locale]}]
  (let [selrect (:selrect shape)
        {:keys [width height x y]} selrect]
    [:*
     [:div.attributes-unit-row
      [:div.attributes-label (t locale "handoff.attributes.layout.width")]
      [:div.attributes-value (mth/precision width 2) "px"]
      [:& copy-button {:data (copy-data selrect :width)}]]

     [:div.attributes-unit-row
      [:div.attributes-label (t locale "handoff.attributes.layout.height")]
      [:div.attributes-value (mth/precision height 2) "px"]
      [:& copy-button {:data (copy-data selrect :height)}]]

     (when (not= (:x shape) 0)
       [:div.attributes-unit-row
        [:div.attributes-label (t locale "handoff.attributes.layout.left")]
        [:div.attributes-value (mth/precision x 2) "px"]
        [:& copy-button {:data (copy-data selrect :x)}]])

     (when (not= (:y shape) 0)
       [:div.attributes-unit-row
        [:div.attributes-label (t locale "handoff.attributes.layout.top")]
        [:div.attributes-value (mth/precision y 2) "px"]
        [:& copy-button {:data (copy-data selrect :y)}]])

     (when (and (:rx shape) (not= (:rx shape) 0))
       [:div.attributes-unit-row
        [:div.attributes-label (t locale "handoff.attributes.layout.radius")]
        [:div.attributes-value (mth/precision (:rx shape) 2) "px"]
        [:& copy-button {:data (copy-data shape :rx)}]])

     (when (and (:r1 shape)
                (or (not= (:r1 shape) 0)
                    (not= (:r2 shape) 0)
                    (not= (:r3 shape) 0)
                    (not= (:r4 shape) 0)))
       [:div.attributes-unit-row
        [:div.attributes-label (t locale "handoff.attributes.layout.radius")]
        [:div.attributes-value (mth/precision (:r1 shape) 2) ", "
         (mth/precision (:r2 shape) 2) ", "
         (mth/precision (:r3 shape) 2) ", "
         (mth/precision (:r4 shape) 2) "px"]
        [:& copy-button {:data (copy-data shape :r1)}]])

     (when (not= (:rotation shape 0) 0)
       [:div.attributes-unit-row
        [:div.attributes-label (t locale "handoff.attributes.layout.rotation")]
        [:div.attributes-value (mth/precision (:rotation shape) 2) "deg"]
        [:& copy-button {:data (copy-data shape :rotation)}]])]))


(mf/defc layout-panel
  [{:keys [shapes locale]}]
  [:div.attributes-block
   [:div.attributes-block-title
    [:div.attributes-block-title-text (t locale "handoff.attributes.layout")]
    (when (= (count shapes) 1)
      [:& copy-button {:data (copy-data (first shapes))}])]

   (for [shape shapes]
     [:& layout-block {:shape shape
                       :locale locale}])])
