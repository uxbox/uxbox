;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.handoff
  (:require
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.handoff.left-sidebar :refer [left-sidebar]]
   [app.main.ui.handoff.render :refer [render-frame-svg]]
   [app.main.ui.handoff.right-sidebar :refer [right-sidebar]]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.viewer.header :refer [header]]
   [app.main.ui.viewer.thumbnails :refer [thumbnails-panel]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [t tr]]
   [app.util.keyboard :as kbd]
   [goog.events :as events]
   [rumext.alpha :as mf])
  (:import goog.events.EventType))

(defn handle-select-frame [frame]
  #(do (dom/prevent-default %)
       (dom/stop-propagation %)
       (st/emit! (dv/select-shape (:id frame)))))


(mf/defc viewport
  [{:keys [zoom file page frame]}]
  (let [objects (:objects page)
        page-id (:id page)
        file-id (:id file)]
    [:*
     [:& left-sidebar {:frame frame}]
     [:div.handoff-svg-wrapper {:on-click (handle-select-frame frame)}
      [:div.handoff-svg-container
       [:& render-frame-svg {:frame-id (:id frame)
                             :zoom zoom
                             :objects objects}]]]
     [:& right-sidebar {:frame frame
                        :page-id page-id
                        :file-id file-id}]]))


(mf/defc main-panel
  [{:keys [local file page frames index section]}]
  (let [frame   (get frames index)
        objects (:objects page)
        page-id (:id page)
        file-id (:id file)]
    (mf/use-effect
     (mf/deps index)
     (fn []
       (st/emit! (dv/set-current-frame (:id frame))
                 (dv/select-shape (:id frame)))))

    [:section.viewer-preview
     (cond
       (empty? frames)
       [:section.empty-state
        [:span (tr "viewer.empty-state")]]

       (nil? frame)
       [:section.empty-state
        [:span (tr "viewer.frame-not-found")]]

       :else
       [:& viewport {:zoom (:zoom local)
                     :file file
                     :frame frame
                     :page page}])]))

(mf/defc handoff-content
  ;; [{:keys [data state index page-id file-id] :as props}]
  [{:keys [params file page frames]}]

  (let [local   (mf/deref refs/viewer-local)
        project (mf/deref refs/viewer-project)
        index   (:index params)
        section (:section params)

        on-mouse-wheel
        (fn [event]
          (when (or (kbd/ctrl? event) (kbd/meta? event))
            (dom/prevent-default event)
            (let [event (.getBrowserEvent ^js event)
                  delta (+ (.-deltaY ^js event)
                           (.-deltaX ^js event))]
              (if (pos? delta)
                (st/emit! dv/decrease-zoom)
                (st/emit! dv/increase-zoom)))))

        on-mount
        (fn []
          ;; bind with passive=false to allow the event to be cancelled
          ;; https://stackoverflow.com/a/57582286/3219895
          (let [key1 (events/listen goog/global EventType.WHEEL
                                    on-mouse-wheel #js {"passive" false})]
            (fn []
              (events/unlistenByKey key1))))]

    (mf/use-effect on-mount)
    (hooks/use-shortcuts ::handoff sc/shortcuts)

    [:div.handoff-layout {:class (dom/classnames :force-visible (:show-thumbnails local))}
     [:& header
      {:page page
       :frames frames
       :file file
       :project project
       :local local
       :section section
       :index index}]

     [:div.viewer-content
      (when (:show-thumbnails local)
        [:& thumbnails-panel {:frames frames
                              :index index
                              :page page}])
      [:& main-panel {:frames frames
                      :page page
                      :file file
                      :local local
                      :index index
                      :section section}]]]))

(mf/defc handoff
  [{:keys [file-id page-id index token] :as props}]

  (mf/use-effect
   (mf/deps file-id page-id token)
   (fn []
     (st/emit! (dv/initialize-file props))))

  (let [data  (mf/deref refs/viewer-data)
        state (mf/deref refs/viewer-local)]

    (mf/use-effect
      (mf/deps (:file data))
      #(when (:file data)
         (dom/set-html-title (tr "title.viewer"
                                 (get-in data [:file :name])))))

    (when (and data state)
      [:& handoff-content
       {:file-id file-id
        :page-id page-id
        :index index
        :state state
        :data data}])))
