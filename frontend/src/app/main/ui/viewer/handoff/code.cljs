;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.code
  (:require
   ["js-beautify" :as beautify]
   [app.common.geom.shapes :as gsh]
   [app.main.ui.components.code-block :refer [code-block]]
   [app.main.ui.components.copy-button :refer [copy-button]]
   [app.main.ui.icons :as i]
   [app.util.code-gen :as cg]
   [app.util.dom :as dom]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))

(defn generate-markup-code [_type shapes]
  (let [frame (dom/query js/document "#svg-frame")
        markup-shape
        (fn [shape]
          (let [selector (str "#shape-" (:id shape) (when (= :text (:type shape)) " .root"))]
            (when-let [el (and frame (dom/query frame selector))]
              (str
               (str/fmt "<!-- %s -->" (:name shape))
               (.-outerHTML el)))))]
    (->> shapes
         (map markup-shape )
         (remove nil?)
         (str/join "\n\n"))))

(defn format-code [code type]
  (let [code (-> code
                 (str/replace "<defs></defs>" "")
                 (str/replace "><" ">\n<"))]
    (cond-> code
      (= type "svg") (beautify/html #js {"indent_size" 2}))))

(mf/defc code
  [{:keys [shapes frame on-expand]}]
  (let [style-type (mf/use-state "css")
        markup-type (mf/use-state "svg")
        shapes (->> shapes
                    (map #(gsh/translate-to-frame % frame)))

        style-code (-> (cg/generate-style-code @style-type shapes)
                       (format-code "css"))

        markup-code (-> (mf/use-memo (mf/deps shapes) #(generate-markup-code @markup-type shapes))
                        (format-code "svg"))]
    [:div.element-options
     [:div.code-block
      [:div.code-row-lang
       [:select.code-selection
        [:option {:value "css"} "CSS"]
        #_[:option {:value "sass"} "SASS"]
        #_[:option {:value "less"} "Less"]
        #_[:option {:value "stylus"} "Stylus"]]

       [:button.expand-button
        {:on-click on-expand }
        i/full-screen]

       [:& copy-button { :data style-code }]]

      [:div.code-row-display
       [:& code-block {:type @style-type
                       :code style-code}]]]

     [:div.code-block
      [:div.code-row-lang
       [:select.code-selection
        [:option "SVG"]
        [:option "HTML"]]

       [:button.expand-button
        {:on-click on-expand}
        i/full-screen]

       [:& copy-button { :data markup-code }]]

      [:div.code-row-display
       [:& code-block {:type @markup-type
                       :code markup-code}]]]

     ]))
