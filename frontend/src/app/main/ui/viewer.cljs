;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as geom]
   [app.common.pages :as cp]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.comments :as cmt]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.viewer.header :refer [header]]
   [app.main.ui.viewer.shapes :as shapes]
   [app.main.ui.viewer.thumbnails :refer [thumbnails-panel]]
   [app.main.ui.viewer.comments :refer [comments-layer]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [goog.events :as events]
   [app.util.object :as obj]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(mf/defc viewport
  {::mf/wrap [mf/memo]}
  [{:keys [state data index section] :as props}]
  (let [zoom          (:zoom state)
        objects       (:objects data)

        frame         (get-in data [:frames index])
        frame-id      (:id frame)

        modifier      (-> (gpt/point (:x frame) (:y frame))
                          (gpt/negate)
                          (gmt/translate-matrix))

        update-fn     #(d/update-when %1 %2 assoc-in [:modifiers :displacement] modifier)

        objects       (->> (d/concat [frame-id] (cp/get-children frame-id objects))
                           (reduce update-fn objects))

        interactions? (:interactions-show? state)
        wrapper       (mf/use-memo (mf/deps objects) #(shapes/frame-container-factory objects interactions?))

        ;; Retrieve frame again with correct modifier
        frame         (get objects frame-id)

        width         (* (:width frame) zoom)
        height        (* (:height frame) zoom)
        vbox          (str "0 0 " (:width frame 0) " " (:height frame 0))]

    [:div.viewport-container
     {:style {:width width
              :height height
              :state state
              :position "relative"}}

     (when (= section :comments)
       [:& comments-layer {:width width
                           :height height
                           :frame frame
                           :data data
                           :zoom zoom}])

     [:svg {:view-box vbox
            :width width
            :height height
            :version "1.1"
            :xmlnsXlink "http://www.w3.org/1999/xlink"
            :xmlns "http://www.w3.org/2000/svg"}
      [:& wrapper {:shape frame
                   :show-interactions? interactions?
                   :view-box vbox}]]]))

(mf/defc main-panel
  [{:keys [data state index section]}]
  (let [frames  (:frames data)
        frame   (get frames index)]
    [:section.viewer-preview
     (cond
       (empty? frames)
       [:section.empty-state
        [:span (tr "viewer.empty-state")]]

       (nil? frame)
       [:section.empty-state
        [:span (tr "viewer.frame-not-found")]]

       (some? state)
       [:& viewport
        {:data data
         :section section
         :index index
         :state state
         }])]))

(mf/defc viewer-content
  {::mf/wrap-props false}
  [props]
  (let [index (obj/get props "index")
        state (obj/get props "state")

        on-click
        (fn [event]
          ;; TODO: revisit this
          ;; (dom/stop-propagation event)
          (st/emit! (dcm/close-thread))
          (let [mode (get state :interactions-mode)]
            (when (= mode :show-on-click)
              (st/emit! dv/flash-interactions))))

        on-mouse-wheel
        (fn [event]
          (when (or (kbd/ctrl? event) (kbd/meta? event))
            (dom/prevent-default event)
            (let [event (.getBrowserEvent ^js event)
                  delta (+ (.-deltaY ^js event) (.-deltaX ^js event))]
              (if (pos? delta)
                (st/emit! dv/decrease-zoom)
                (st/emit! dv/increase-zoom)))))

        on-key-down
        (fn [event]
          (when (kbd/esc? event)
            (st/emit! (dcm/close-thread))))

        on-mount
        (fn []
          ;; bind with passive=false to allow the event to be cancelled
          ;; https://stackoverflow.com/a/57582286/3219895
          (let [key1 (events/listen goog/global "wheel" on-mouse-wheel #js {"passive" false})
                key2 (events/listen js/window "keydown" on-key-down)
                key3 (events/listen js/window "click" on-click)]
            (fn []
              (events/unlistenByKey key1)
              (events/unlistenByKey key2)
              (events/unlistenByKey key3))))]

    (mf/use-effect on-mount)
    (hooks/use-shortcuts ::viewer sc/shortcuts)

    [:div.viewer-layout {:class (dom/classnames :force-visible
                                                (:show-thumbnails state))}
     [:> header props]
     [:div.viewer-content {:on-click on-click}
      #_(when (:show-thumbnails state)
        [:& thumbnails-panel {:screen :viewer
                              :index index}])]
     #_[:> main-panel props]]))


(mf/defc viewer
  {::mf/wrap-props false}
  [props]
  (let [state (mf/deref refs/viewer-local)
        file  (mf/deref refs/viewer-file)]

    (mf/use-effect
     (mf/deps (:name file))
     #(when-let [name (:name file)]
        (dom/set-html-title (str "\u25b6 " (tr "title.viewer" name)))))

    (when (and file state)
      [:> viewer-content props])))


;; --- Component: Viewer Page

(mf/defc viewer-page
  {::mf/wrap-props false}
  [props]
  ;; [{:keys [file-id page-id index token section] :as props}]

  (let [file-id (obj/get props "file-id")]
    (mf/use-effect
     (mf/deps file-id)
     (fn []
       (st/emit! (dv/initialize props))))

    [:> viewer props]))

