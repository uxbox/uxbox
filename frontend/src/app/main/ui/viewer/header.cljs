;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.header
  (:require
   [app.common.math :as mth]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.main.data.comments :as dcm]
   [app.main.data.messages :as dm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.components.fullscreen :as fs]
   [app.main.ui.icons :as i]
   [app.main.ui.viewer.comments :refer [comments-menu]]
   [app.main.ui.workspace.header :refer [zoom-widget]]
   [app.main.ui.viewer.interactions :refer [interactions-menu]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.object :as obj]
   [app.util.router :as rt]
   [app.util.webapi :as wapi]
   [rumext.alpha :as mf]))

(mf/defc header-options
  [{:keys [section zoom]}]
  (let [fullscreen (mf/use-ctx fs/fullscreen-context)

        has-permission? true

        toggle-fullscreen
        (mf/use-callback
          (mf/deps fullscreen)
          (fn []
            (if @fullscreen (fullscreen false) (fullscreen true))))]

    [:div.options-zone
     (case section
       :interactions [:& interactions-menu]
       :comments [:& comments-menu]

       [:div.view-options])

     [:& zoom-widget
      {:zoom zoom
       :on-increase (st/emitf dv/increase-zoom)
       :on-decrease (st/emitf dv/decrease-zoom)
       :on-zoom-to-50 (st/emitf dv/zoom-to-50)
       :on-zoom-to-100 (st/emitf dv/reset-zoom)
       :on-zoom-to-200 (st/emitf dv/zoom-to-200)
       :on-fullscreen toggle-fullscreen}]

     [:span.btn-icon-dark.btn-small.tooltip.tooltip-bottom-left
      {:alt (tr "viewer.header.fullscreen")
       :on-click toggle-fullscreen}
      (if @fullscreen
        i/full-screen-off
        i/full-screen)]

     (when has-permission?
       [:span.btn-primary (tr "labels.share-prototype")])

     (when has-permission?
       [:span.btn-text-dark (tr "labels.edit-file")])]))

(mf/defc header-sitemap
  [{:keys [project file page frame] :as props}]
  (let [project-name (:name project)
        file-name    (:name file)
        page-name    (:name page)
        frame-name   (:name frame)

        toggle-thumbnails
        (st/emitf dv/toggle-thumbnails-panel)

        show-dropdown? (mf/use-state false)

        navigate-to
        (fn [page-id]
          (st/emit! (dv/go-to-page page-id))
          (reset! show-dropdown? false))
        ]

     [:div.sitemap-zone {:alt (tr "viewer.header.sitemap")}
      [:div.breadcrumb
       {:on-click #(swap! show-dropdown? not)}
       [:span.project-name project-name]
       [:span "/"]
       [:span.file-name file-name]
       [:span "/"]
       [:span.page-name page-name]
       [:span.icon i/arrow-down]

       [:& dropdown {:show @show-dropdown?
                     :on-close #(swap! show-dropdown? not)}
        [:ul.dropdown
         (for [id (get-in file [:data :pages])]
           [:li {:id (str id)
                 :on-click (partial navigate-to id)}
            (get-in file [:data :pages-index id :name])])]]]

      [:div.current-frame
       {:on-click toggle-thumbnails}
       [:span.label "/"]
       [:span.label frame-name]
       [:span.icon i/arrow-down]]]))

(mf/defc header
  [{:keys [project file page frame zoom section]}]

  (let [

        ;; TODO
        ;; profile    (mf/deref refs/profile)
        ;; teams      (mf/deref refs/teams)
        ;; team-id    (get-in data [:project :team-id])
        ;; has-permission? (and (not= uuid/zero (:id profile))
        ;;                      (contains? teams team-id))

        has-permission? true

        toggle-thumbnails
        (st/emitf dv/toggle-thumbnails-panel)

        go-to-dashboard
        (st/emitf (dv/go-to-dashboard))

        navigate
        (fn [section]
          (st/emit! (dv/go-to-section section)))]

    [:header.viewer-header
     [:div.main-icon
      [:a {:on-click go-to-dashboard
           ;; If the user doesn't have permission we disable the link
           :style {:pointer-events (when-not has-permission? "none")}} i/logo-icon]]

     [:& header-sitemap {:project project :file file :page page :frame frame}]

     [:div.mode-zone
      [:button.mode-zone-button.tooltip.tooltip-bottom
       {:on-click #(navigate :interactions)
        :class (dom/classnames :active (= section :interactions))
        :alt "View mode"}
       i/play]

      (when has-permission?
        [:button.mode-zone-button.tooltip.tooltip-bottom
         {:on-click #(navigate :comments)
          :class (dom/classnames :active (= section :comments))
          :alt "Comments"}
         i/chat])

      [:button.mode-zone-button.tooltip.tooltip-bottom
       {:on-click #(navigate :handoff)
        :class (dom/classnames :active (= section :handoff))
        :alt "Code mode"}
       i/code]]

     [:& header-options {:section section :zoom zoom}]]))

