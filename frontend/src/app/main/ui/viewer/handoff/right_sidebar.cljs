;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.right-sidebar
  (:require
   [app.common.data :as d]
   [app.main.store :as st]
   [app.main.ui.components.tab-container :refer [tab-container tab-element]]
   [app.main.ui.viewer.handoff.attributes :refer [attributes]]
   [app.main.ui.viewer.handoff.code :refer [code]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.layers :refer [element-icon]]
   [app.util.i18n :refer [t] :as i18n]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(defn make-selected-shapes-iref
  []
  (let [selected->shapes
        (fn [state]
          (let [selected (get-in state [:viewer-local :selected])
                objects (get-in state [:viewer-data :page :objects])
                resolve-shape #(get objects %)]
            (mapv resolve-shape selected)))]
    #(l/derived selected->shapes st/state)))

(mf/defc right-sidebar
  [{:keys [frame page-id file-id]}]
  (let [expanded (mf/use-state false)
        locale (mf/deref i18n/locale)
        section (mf/use-state :info #_:code)
        selected-ref (mf/use-memo (make-selected-shapes-iref))
        shapes (mf/deref selected-ref)
        selected-type (-> shapes first (:type :not-found))]
    [:aside.settings-bar.settings-bar-right {:class (when @expanded "expanded")}
     [:div.settings-bar-inside
      (when (seq shapes)
        [:div.tool-window
         [:div.tool-window-bar.big
          (if (> (count shapes) 1)
            [:*
             [:span.tool-window-bar-icon i/layers]
             [:span.tool-window-bar-title (t locale "handoff.tabs.code.selected.multiple" (count shapes))]]
            [:*
             [:span.tool-window-bar-icon
              [:& element-icon {:shape (-> shapes first)}]]
             [:span.tool-window-bar-title (->> selected-type d/name (str "handoff.tabs.code.selected.") (t locale))]])
          ]
         [:div.tool-window-content
          [:& tab-container {:on-change-tab #(do
                                               (reset! expanded false)
                                               (reset! section %))
                             :selected @section}
           [:& tab-element {:id :info :title (t locale "handoff.tabs.info")}
            [:& attributes {:page-id page-id
                            :file-id file-id
                            :frame frame
                            :shapes shapes}]]

           [:& tab-element {:id :code :title (t locale "handoff.tabs.code")}
            [:& code {:frame frame
                      :shapes shapes
                      :on-expand #(swap! expanded not)}]]]]])]]))
