;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.attributes.svg
  (:require
   #_[app.common.math :as mth]
   #_[app.main.ui.icons :as i]
   #_[app.util.code-gen :as cg]
   [app.common.data :as d]
   [app.main.ui.components.copy-button :refer [copy-button]]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.alpha :as mf]))


(defn map->css [attr]
  (->> attr
       (map (fn [[attr-key attr-value]] (str (d/name attr-key) ":" attr-value)))
       (str/join "; ")))

(mf/defc svg-attr [{:keys [attr value]}]
  (if (map? value)
    [:*
     [:div.attributes-block-title
      [:div.attributes-block-title-text (d/name attr)]
      [:& copy-button {:data (map->css value)}]]

     (for [[attr-key attr-value] value]
       [:& svg-attr {:attr  attr-key :value attr-value}])]

    [:div.attributes-unit-row
     [:div.attributes-label (d/name attr)]
     [:div.attributes-value (str value)]
     [:& copy-button {:data (d/name value)}]]))

(mf/defc svg-block
  [{:keys [shape]}]
  [:*
   (for [[attr-key attr-value] (:svg-attrs shape)]
     [:& svg-attr {:attr  attr-key :value attr-value}])]  )


(mf/defc svg-panel
  [{:keys [shapes]}]

  (let [shape (first shapes)]
    (when (seq (:svg-attrs shape))
      [:div.attributes-block
       [:div.attributes-block-title
        [:div.attributes-block-title-text (tr "workspace.sidebar.options.svg-attrs.title")]]
       [:& svg-block {:shape shape}]])))
