;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.attributes.shadow
  (:require
   [app.common.data :as d]
   [app.main.ui.components.copy-button :refer [copy-button]]
   [app.main.ui.viewer.handoff.attributes.common :refer [color-row]]
   [app.util.code-gen :as cg]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(defn has-shadow? [shape]
  (:shadow shape))

(defn shape-copy-data [shape]
  (cg/generate-css-props
   shape
   :shadow
   {:to-prop "box-shadow"
    :format #(str/join ", " (map cg/shadow->css (:shadow shape)))}))

(defn shadow-copy-data [shadow]
  (cg/generate-css-props
   shadow
   :style
   {:to-prop "box-shadow"
    :format #(cg/shadow->css shadow)}))

(mf/defc shadow-block [{:keys [shadow]}]
  (let [color-format (mf/use-state :hex)]
    [:div.attributes-shadow-block
     [:div.attributes-shadow-row
      [:div.attributes-label (->> shadow :style d/name (str "handoff.attributes.shadow.style.") (tr))]
      [:div.attributes-shadow
       [:div.attributes-label (tr "handoff.attributes.shadow.shorthand.offset-x")]
       [:div.attributes-value (str (:offset-x shadow))]]

      [:div.attributes-shadow
       [:div.attributes-label (tr "handoff.attributes.shadow.shorthand.offset-y")]
       [:div.attributes-value (str (:offset-y shadow))]]

      [:div.attributes-shadow
       [:div.attributes-label (tr "handoff.attributes.shadow.shorthand.blur")]
       [:div.attributes-value (str (:blur shadow))]]

      [:div.attributes-shadow
       [:div.attributes-label (tr "handoff.attributes.shadow.shorthand.spread")]
       [:div.attributes-value (str (:spread shadow))]]

      [:& copy-button {:data (shadow-copy-data shadow)}]]

     [:& color-row {:color (:color shadow)
                    :format @color-format
                    :on-change-format #(reset! color-format %)}]]))

(mf/defc shadow-panel [{:keys [shapes]}]
  (let [shapes (->> shapes (filter has-shadow?))]
    (when (seq shapes)
      [:div.attributes-block
       [:div.attributes-block-title
        [:div.attributes-block-title-text (tr "handoff.attributes.shadow")]
        (when (= (count shapes) 1)
          [:& copy-button {:data (shape-copy-data (first shapes))}])]

       [:div.attributes-shadow-blocks
        (for [shape shapes]
          (for [shadow (:shadow shape)]
            [:& shadow-block {:shape shape
                              :shadow shadow}]))]])))
