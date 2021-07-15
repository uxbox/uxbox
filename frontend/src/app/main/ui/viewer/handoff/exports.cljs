;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.exports
  (:require
   [app.common.data :as d]
   [app.main.data.messages :as dm]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.menus.exports :as we]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [beicon.core :as rx]
   [rumext.alpha :as mf]))

(mf/defc exports
  [{:keys [shape page-id file-id] :as props}]
  (let [exports  (mf/use-state (:exports shape []))
        loading? (mf/use-state false)

        on-download
        (mf/use-callback
         (mf/deps shape @exports)
         (fn [event]
           (dom/prevent-default event)
           (swap! loading? not)
           (->> (we/request-export (assoc shape :page-id page-id :file-id file-id) @exports)
                (rx/subs
                 (fn [{:keys [status body] :as response}]
                   (js/console.log status body)
                   (if (= status 200)
                     (dom/trigger-download (:name shape) body)
                     (st/emit! (dm/error (tr "errors.unexpected-error")))))
                 (constantly nil)
                 (fn []
                   (swap! loading? not))))))

        add-export
        (mf/use-callback
         (mf/deps shape)
         (fn []
           (let [xspec {:type :png
                        :suffix ""
                        :scale 1}]
             (swap! exports conj xspec))))

        delete-export
        (mf/use-callback
         (mf/deps shape)
         (fn [index]
           (swap! exports (fn [exports]
                            (let [[before after] (split-at index exports)]
                              (d/concat [] before (rest after)))))))

        on-scale-change
        (mf/use-callback
         (mf/deps shape)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (d/parse-double value)]
             (swap! exports assoc-in [index :scale] value))))

        on-suffix-change
        (mf/use-callback
         (mf/deps shape)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)]
             (swap! exports assoc-in [index :suffix] value))))

        on-type-change
        (mf/use-callback
         (mf/deps shape)
         (fn [index event]
           (let [target  (dom/get-target event)
                 value   (dom/get-value target)
                 value   (keyword value)]
             (swap! exports assoc-in [index :type] value))))]

    (mf/use-effect
     (mf/deps shape)
     (fn []
       (reset! exports (:exports shape []))))

    [:div.element-set.exports-options
     [:div.element-set-title
      [:span (tr "workspace.options.export")]
      [:div.add-page {:on-click add-export} i/close]]

     (when (seq @exports)
       [:div.element-set-content
        (for [[index export] (d/enumerate @exports)]
          [:div.element-set-options-group
           {:key index}
           [:select.input-select {:on-change (partial on-scale-change index)
                                  :value (:scale export)}
            [:option {:value "0.5"}  "0.5x"]
            [:option {:value "0.75"} "0.75x"]
            [:option {:value "1"} "1x"]
            [:option {:value "1.5"} "1.5x"]
            [:option {:value "2"} "2x"]
            [:option {:value "4"} "4x"]
            [:option {:value "6"} "6x"]]

           [:input.input-text {:on-change (partial on-suffix-change index)
                               :value (:suffix export)}]
           [:select.input-select {:on-change (partial on-type-change index)
                                  :value (d/name (:type export))}
            [:option {:value "png"} "PNG"]
            [:option {:value "jpeg"} "JPEG"]
            [:option {:value "svg"} "SVG"]]

           [:div.delete-icon {:on-click (partial delete-export index)}
            i/minus]])

        [:div.btn-icon-dark.download-button
         {:on-click (when-not @loading? on-download)
          :class (dom/classnames :btn-disabled @loading?)
          :disabled @loading?}
         (if @loading?
           (tr "workspace.options.exporting-object")
           (tr "workspace.options.export-object"))]])]))

