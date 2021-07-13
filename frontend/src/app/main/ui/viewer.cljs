;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer
  (:require
   [app.common.uuid :as uuid]
   [app.common.data :as d]
   [app.common.spec :as us]
   [cljs.spec.alpha :as s]

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

(defn- prepare-objects
  [page frame]
  (fn []
    (let [objects   (:objects page)
          frame-id  (:id frame)
          modifier  (-> (gpt/point (:x frame) (:y frame))
                        (gpt/negate)
                        (gmt/translate-matrix))

          update-fn #(d/update-when %1 %2 assoc-in [:modifiers :displacement] modifier)]

      (->> (cp/get-children frame-id objects)
           (d/concat [frame-id])
           (reduce update-fn objects)))))

(mf/defc viewport
  {::mf/wrap [mf/memo]}
  [{:keys [local page frame]}]
  (let [zoom          (:zoom local)
        interactions? (:interactions-show? local)

        objects       (mf/use-memo
                       (mf/deps page frame)
                       (prepare-objects page frame))

        wrapper       (mf/use-memo
                       (mf/deps objects)
                       #(shapes/frame-container-factory objects interactions?))

        ;; Retrieve frame again with correct modifier
        frame         (get objects (:id frame))

        width         (* (:width frame) zoom)
        height        (* (:height frame) zoom)
        vbox          (str "0 0 " (:width frame 0) " " (:height frame 0))]

    [:div.viewport-container
     {:style {:width width
              :height height
              :position "relative"}}

     #_(when (= section :comments)
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
  [{:keys [local page frames index section]}]
  (let [frame (get frames index)]
    [:section.viewer-preview
     (cond
       (empty? frames)
       [:section.empty-state
        [:span (tr "viewer.empty-state")]]

       (nil? frame)
       [:section.empty-state
        [:span (tr "viewer.frame-not-found")]]

       (some? frame)
       [:& viewport
        {:frame frame
         :page page
         :section section
         :index index
         :local local
         }])]))

(mf/defc viewer-content
  [{:keys [params file page frames frame]}]
  (let [local   (mf/deref refs/viewer-local)
        project (mf/deref refs/viewer-project)
        index   (:index params)
        section (:section params)

        on-click
        (fn [event]
          ;; TODO: revisit this
          ;; (dom/stop-propagation event)
          (st/emit! (dcm/close-thread))
          (let [mode (:interactions-mode local)]
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

    [:div.viewer-layout {:class (dom/classnames :force-visible (:show-thumbnails local))}
     [:& header {:page page
                 :frames frames
                 :file file
                 :project project
                 :local local
                 :section section
                 :index index
                 }]
     [:div.viewer-content {:on-click on-click}
      (when (:show-thumbnails local)
        [:& thumbnails-panel {:frames frames
                              :index index
                              :page page}])
      [:& main-panel {:frames frames
                      :page page
                      :local local
                      :index index
                      :section index}]]]))

(defn- select-frames
  [{:keys [objects] :as page}]
  (let [root (get objects uuid/zero)]
    (into [] (comp (map #(get objects %))
                   (filter #(= :frame (:type %))))
          (reverse (:shapes root)))))

(mf/defc viewer
  [{:keys [params file]}]

  ;; Set the page title
  (mf/use-effect
   (mf/deps (:name file))
   (fn []
     (let [name (:name file)]
       (dom/set-html-title (str "\u25b6 " (tr "title.viewer" name))))))

  (let [page-id (:page-id params)
        index   (:index params)
        data    (mf/deref refs/viewer-data)

        page    (mf/use-memo
                 (mf/deps data page-id)
                 (fn []
                   (get-in data [:pages-index page-id])))

        frames  (mf/use-memo
                 (mf/deps page)
                 #(select-frames page))]

    [:& viewer-content
     {:params params
      :page page
      :file file
      :frames frames}]))

;; --- Component: Viewer Page

(s/def ::file-id ::us/uuid)
(s/def ::page-id ::us/uuid)
(s/def ::section ::us/keyword)
(s/def ::index ::us/integer)

(s/def ::viewer-page-props
  (s/keys :req-un [::file-id ::page-id ::section ::index]))

(mf/defc viewer-page
  [{:keys [file-id] :as props}]
  (us/assert ::viewer-page-props props)

  (mf/use-effect
   (mf/deps file-id)
   (fn []
     (st/emit! (dv/initialize-file props))
     (fn []
       (st/emit! (dv/finalize-file props)))))

  (when-let [file (mf/deref refs/viewer-file)]
    [:& viewer {:params props
                :file file
                :key (:id file)}]))
