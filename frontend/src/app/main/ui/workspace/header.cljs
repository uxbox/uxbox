;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.header
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.config :as cfg]
   [app.main.data.modal :as modal]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.repo :as rp]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.presence :refer [active-sessions]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.router :as rt]
   [beicon.core :as rx]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

;; --- Zoom Widget

(def workspace-persistence-ref
  (l/derived :workspace-persistence st/state))

(mf/defc persistence-state-widget
  {::mf/wrap [mf/memo]}
  []
  (let [data (mf/deref workspace-persistence-ref)]
    [:div.persistence-status-widget
     (cond
       (= :pending (:status data))
       [:div.pending
        [:span.label (tr "workspace.header.unsaved")]]

       (= :saving (:status data))
       [:div.saving
        [:span.icon i/toggle]
        [:span.label (tr "workspace.header.saving")]]

       (= :saved (:status data))
       [:div.saved
        [:span.icon i/tick]
        [:span.label (tr "workspace.header.saved")]]

       (= :error (:status data))
       [:div.error {:title "There was an error saving the data. Please refresh if this persists."}
        [:span.icon i/msg-warning]
        [:span.label (tr "workspace.header.save-error")]])]))


(mf/defc zoom-widget
  {::mf/wrap [mf/memo]}
  [{:keys [zoom
           on-increase
           on-decrease
           on-zoom-reset
           on-zoom-fit
           on-zoom-selected
           on-fullscreen]
    :as props}]
  (let [show-dropdown? (mf/use-state false)]
    [:div.zoom-widget {:on-click #(reset! show-dropdown? true)}
     [:span.label {} (str (mth/round (* 100 zoom)) "%")]
     [:span.icon i/arrow-down]
     [:& dropdown {:show @show-dropdown?
                   :on-close #(reset! show-dropdown? false)}
      [:ul.dropdown
       [:li {:on-click on-increase}
        "Zoom in" [:span (sc/get-tooltip :increase-zoom)]]
       [:li {:on-click on-decrease}
        "Zoom out" [:span (sc/get-tooltip :decrease-zoom)]]
       [:li {:on-click on-zoom-reset}
        "Zoom to 100%" [:span (sc/get-tooltip :reset-zoom)]]
       [:li {:on-click on-zoom-fit}
        "Zoom to fit all" [:span (sc/get-tooltip :fit-all)]]
       [:li {:on-click on-zoom-selected}
        "Zoom to selected" [:span (sc/get-tooltip :zoom-selected)]]
       (when on-fullscreen
         [:li {:on-click on-fullscreen}
          "Full screen"])]]]))


;; --- Header Users

(mf/defc menu
  [{:keys [layout project file team-id] :as props}]
  (let [show-menu? (mf/use-state false)
        editing? (mf/use-state false)

        edit-input-ref (mf/use-ref nil)

        add-shared-fn
        (st/emitf (dw/set-file-shared (:id file) true))

        del-shared-fn
        (st/emitf (dw/set-file-shared (:id file) false))

        on-add-shared
        (mf/use-fn
         (mf/deps file)
         (st/emitf (modal/show
                    {:type :confirm
                     :message ""
                     :title (tr "modals.add-shared-confirm.message" (:name file))
                     :hint (tr "modals.add-shared-confirm.hint")
                     :cancel-label :omit
                     :accept-label (tr "modals.add-shared-confirm.accept")
                     :accept-style :primary
                     :on-accept add-shared-fn})))

        on-remove-shared
        (mf/use-fn
         (mf/deps file)
         (st/emitf (modal/show
                    {:type :confirm
                     :message ""
                     :title (tr "modals.remove-shared-confirm.message" (:name file))
                     :hint (tr "modals.remove-shared-confirm.hint")
                     :cancel-label :omit
                     :accept-label (tr "modals.remove-shared-confirm.accept")
                     :on-accept del-shared-fn})))


        handle-blur (fn [_]
                      (let [value (-> edit-input-ref mf/ref-val dom/get-value)]
                        (st/emit! (dw/rename-file (:id file) value)))
                      (reset! editing? false))

        handle-name-keydown (fn [event]
                              (when (kbd/enter? event)
                                (handle-blur event)))
        start-editing-name (fn [event]
                             (dom/prevent-default event)
                             (reset! editing? true))

        on-export-files
        (mf/use-callback
         (mf/deps file team-id)
         (fn [_]
           (->> (rx/of file)
                (rx/flat-map
                 (fn [file]
                   (->> (rp/query :file-libraries {:file-id (:id file)})
                        (rx/map #(assoc file :has-libraries? (d/not-empty? %))))))
                (rx/reduce conj [])
                (rx/subs
                 (fn [files]
                   (st/emit!
                    (modal/show
                     {:type :export
                      :team-id team-id
                      :has-libraries? (->> files (some :has-libraries?))
                      :files files})))))))]

    (mf/use-effect
     (mf/deps @editing?)
     #(when @editing?
        (dom/select-text! (mf/ref-val edit-input-ref))))

    [:div.menu-section
     [:div.btn-icon-dark.btn-small {:on-click #(reset! show-menu? true)} i/actions]
     [:div.project-tree {:alt (tr "workspace.sitemap")}
      [:span.project-name
       {:on-click #(st/emit! (rt/navigate :dashboard-files {:team-id team-id
                                                            :project-id (:project-id file)}))}
       (:name project) " /"]
      (if @editing?
        [:input.file-name
         {:type "text"
          :ref edit-input-ref
          :on-blur handle-blur
          :on-key-down handle-name-keydown
          :auto-focus true
          :default-value (:name file "")}]
        [:span
         {:on-double-click start-editing-name}
         (:name file)])]
     (when (:is-shared file)
       [:div.shared-badge i/library])

     [:& dropdown {:show @show-menu?
                   :on-close #(reset! show-menu? false)}
      [:ul.menu
       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :rules))}
        [:span
         (if (contains? layout :rules)
           (tr "workspace.header.menu.hide-rules")
           (tr "workspace.header.menu.show-rules"))]
        [:span.shortcut (sc/get-tooltip :toggle-rules)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :display-grid))}
        [:span
         (if (contains? layout :display-grid)
           (tr "workspace.header.menu.hide-grid")
           (tr "workspace.header.menu.show-grid"))]
        [:span.shortcut (sc/get-tooltip :toggle-grid)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :snap-grid))}
        [:span
         (if (contains? layout :snap-grid)
           (tr "workspace.header.menu.disable-snap-grid")
           (tr "workspace.header.menu.enable-snap-grid"))]
        [:span.shortcut (sc/get-tooltip :toggle-snap-grid)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :sitemap :layers))}
        [:span
         (if (or (contains? layout :sitemap) (contains? layout :layers))
           (tr "workspace.header.menu.hide-layers")
           (tr "workspace.header.menu.show-layers"))]
        [:span.shortcut (sc/get-tooltip :toggle-layers)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :colorpalette))}
        [:span
         (if (contains? layout :colorpalette)
           (tr "workspace.header.menu.hide-palette")
           (tr "workspace.header.menu.show-palette"))]
        [:span.shortcut (sc/get-tooltip :toggle-palette)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :assets))}
        [:span
         (if (contains? layout :assets)
           (tr "workspace.header.menu.hide-assets")
           (tr "workspace.header.menu.show-assets"))]
        [:span.shortcut (sc/get-tooltip :toggle-assets)]]

       [:li {:on-click #(st/emit! (dw/select-all))}
        [:span (tr "workspace.header.menu.select-all")]
        [:span.shortcut (sc/get-tooltip :select-all)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :dynamic-alignment))}
        [:span
         (if (contains? layout :dynamic-alignment)
           (tr "workspace.header.menu.disable-dynamic-alignment")
           (tr "workspace.header.menu.enable-dynamic-alignment"))]
        [:span.shortcut (sc/get-tooltip :toggle-alignment)]]

       [:li {:on-click #(st/emit! (dw/toggle-layout-flags :scale-text))}
        [:span
         (if (contains? layout :scale-text)
           (tr "workspace.header.menu.disable-scale-text")
           (tr "workspace.header.menu.enable-scale-text"))]
        [:span.shortcut (sc/get-tooltip :toggle-scale-text)]]

       (if (:is-shared file)
         [:li {:on-click on-remove-shared}
          [:span (tr "dashboard.remove-shared")]]
         [:li {:on-click on-add-shared}
          [:span (tr "dashboard.add-shared")]])

       [:li.export-file {:on-click on-export-files}
        [:span (tr "dashboard.export-single")]]

       (when cfg/feedback-enabled
         [:li.feedback {:on-click (st/emitf (rt/nav :settings-feedback))}
          [:span (tr "labels.give-feedback")]
          [:span.primary-badge "ALPHA"]])
       ]]]))

;; --- Header Component

(mf/defc header
  [{:keys [file layout project page-id] :as props}]
  (let [team-id  (:team-id project)
        zoom     (mf/deref refs/selected-zoom)
        params   {:page-id page-id :file-id (:id file)}

        go-back
        (mf/use-callback
         (mf/deps project)
         (st/emitf (dw/go-to-dashboard project)))

        go-viewer
        (mf/use-callback
         (mf/deps file page-id)
         (st/emitf (dw/go-to-viewer params)))]

    [:header.workspace-header
     [:div.main-icon
      [:a {:on-click go-back} i/logo-icon]]

     [:& menu {:layout layout
               :project project
               :file file
               :team-id team-id}]

     [:div.users-section
      [:& active-sessions]]

     [:div.options-section
      [:& persistence-state-widget]

      [:& zoom-widget
       {:zoom zoom
        :on-increase #(st/emit! (dw/increase-zoom nil))
        :on-decrease #(st/emit! (dw/decrease-zoom nil))
        :on-zoom-reset #(st/emit! dw/reset-zoom)
        :on-zoom-fit #(st/emit! dw/zoom-to-fit-all)
        :on-zoom-selected #(st/emit! dw/zoom-to-selected-shape)}]

      [:a.btn-icon-dark.btn-small.tooltip.tooltip-bottom-left
       {:alt (tr "workspace.header.viewer" (sc/get-tooltip :open-viewer))
        :on-click go-viewer}
       i/play]]]))

