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
   [app.common.pages :as cp]
   [app.common.spec :as us]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.ui.icons :as i]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.store :as st]
   [app.main.refs :as refs]
   [app.main.ui.viewer.comments :refer [comments-layer]]
   [app.main.ui.viewer.shapes :as shapes]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
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


;; TODO: refactor, looks like comments stuff is highly coupled to the
;; interactions viewport.

(defn- calculate-size
  [frame zoom]
  {:width  (* (:width frame) zoom)
   :height (* (:height frame) zoom)
   :vbox   (str "0 0 " (:width frame 0) " " (:height frame 0))})


(mf/defc viewport
  {::mf/wrap [mf/memo]}
  [{:keys [local file users page frame section]}]
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
        size          (mf/use-memo
                       (mf/deps frame zoom)
                       (fn [] (calculate-size frame zoom)))

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
            (st/emit! (dcm/close-thread))))]

    (mf/use-effect
     (fn []
       ;; bind with passive=false to allow the event to be cancelled
       ;; https://stackoverflow.com/a/57582286/3219895
       (let [key1 (events/listen goog/global "wheel" on-mouse-wheel #js {"passive" false})
             key2 (events/listen js/window "keydown" on-key-down)
             key3 (events/listen js/window "click" on-click)]
         (fn []
           (events/unlistenByKey key1)
           (events/unlistenByKey key2)
           (events/unlistenByKey key3)))))


    [:div.viewport-container
     {:style {:width (:width size)
              :height (:height size)
              :position "relative"}}

     (when (= section :comments)
       [:& comments-layer {:file file
                           :users users
                           :frame frame
                           :page page
                           :zoom zoom}])

     [:svg {:view-box (:vbox size)
            :width (:width size)
            :height (:height size)
            :version "1.1"
            :xmlnsXlink "http://www.w3.org/1999/xlink"
            :xmlns "http://www.w3.org/2000/svg"}
      [:& wrapper {:shape frame
                   :show-interactions? interactions?
                   :view-box (:vbox size)}]]]))


(mf/defc interactions-menu
  []
  ;; TODO: optimize access to viewer-local
  (let [local           (mf/deref refs/viewer-local)

        ;; TODO: change name?
        mode            (:interactions-mode local)

        show-dropdown?  (mf/use-state false)
        toggle-dropdown (mf/use-fn #(swap! show-dropdown? not))
        hide-dropdown   (mf/use-fn #(reset! show-dropdown? false))

        select-mode
        (mf/use-callback
         (fn [mode]
           (st/emit! (dv/set-interactions-mode mode))))]

    [:div.view-options {:on-click toggle-dropdown}
     [:span.label (tr "viewer.header.interactions")]
     [:span.icon i/arrow-down]
     [:& dropdown {:show @show-dropdown?
                   :on-close hide-dropdown}
      [:ul.dropdown.with-check
       [:li {:class (dom/classnames :selected (= mode :hide))
             :on-click #(select-mode :hide)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.dont-show-interactions")]]

       [:li {:class (dom/classnames :selected (= mode :show))
             :on-click #(select-mode :show)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.show-interactions")]]

       [:li {:class (dom/classnames :selected (= mode :show-on-click))
             :on-click #(select-mode :show-on-click)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.show-interactions-on-click")]]]]]))

