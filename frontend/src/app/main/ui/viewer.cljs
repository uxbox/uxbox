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
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.data.viewer.shortcuts :as sc]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.comments :as cmt]
   [app.main.ui.hooks :as hooks]
   [app.main.ui.viewer.comments :refer [comments-layer]]
   [app.main.ui.viewer.header :refer [header]]
   [app.main.ui.viewer.shapes :as shapes]
   [app.main.ui.viewer.thumbnails :refer [thumbnails-panel]]
   [app.main.ui.viewer.handoff :as handoff]
   [app.main.ui.viewer.interactions :as interactions]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]
   [cljs.spec.alpha :as s]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(mf/defc viewer-layout
  [{:keys [local index section file page project frames children] :as props}]
  (let [frame (get frames index)]
    (hooks/use-shortcuts ::viewer sc/shortcuts)

    [:div {:class (dom/classnames
                   :force-visible (:show-thumbnails local)
                   :viewer-layout (not= section :handoff)
                   :handoff-layout (= section :handoff))}
     [:& header {:page page
                 :frames frames
                 :file file
                 :project project
                 :local local
                 :section section
                 :index index }]
     [:div.viewer-content
      (when (:show-thumbnails local)
        [:& thumbnails-panel {:frames frames
                              :page page
                              :index index}])

      [:section.viewer-preview
       (cond
         (empty? frames)
         [:section.empty-state
          [:span (tr "viewer.empty-state")]]

         (nil? frame)
         [:section.empty-state
          [:span (tr "viewer.frame-not-found")]]

         (some? frame)
         (children))]]]))

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

  (let [{:keys [page-id section index]} params

        data    (mf/deref refs/viewer-data)
        local   (mf/deref refs/viewer-local)
        project (mf/deref refs/viewer-project)

        page    (mf/use-memo
                 (mf/deps data page-id)
                 (fn []
                   (get-in data [:pages-index page-id])))

        frames  (mf/use-memo
                 (mf/deps page)
                 #(select-frames page))

        frame   (get frames index)]

      [:& viewer-layout
       {:section section
        :index index
        :local local
        :page page
        :file file
        :project project
        :frames frames}

       #(mf/html
         (if (= :handoff section)
           [:& handoff/viewport
            {:frame frame
             :file file
             :page page
             :section section
             :local local}]
           [:& interactions/viewport
            {:frame frame
             :file file
             :page page
             :section section
             :local local}]))]))


    ;; (if (= :handoff (:section params))
    ;;   [:& handoff-content
    ;;    {:params params
    ;;     :page page
    ;;     :file file
    ;;     :frames frames}]
    ;;   [:& viewer-content
    ;;    {:params params
    ;;     :page page
    ;;     :file file
    ;;     :frames frames}])))

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
