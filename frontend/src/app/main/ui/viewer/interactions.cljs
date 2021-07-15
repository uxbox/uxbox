;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.interactions
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
   [app.main.ui.handoff :as handoff]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]
   [cljs.spec.alpha :as s]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(defn prepare-objects
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
  [{:keys [local file page frame section]}]
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
        vbox          (str "0 0 " (:width frame 0) " " (:height frame 0))

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

    [:div.viewport-container
     {:style {:width width
              :height height
              :position "relative"}}

     (when (= section :comments)
       [:& comments-layer {:width width
                           :height height
                           :frame frame
                           :page page
                           :file file
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
