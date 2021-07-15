;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.comments
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
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [t tr]]
   [app.util.keyboard :as kbd]
   [goog.events :as events]
   [okulary.core :as l]
   [rumext.alpha :as mf]))


(mf/defc comments-menu
  []
  (let [{cmode :mode cshow :show} (mf/deref refs/comments-local)

        show-dropdown?  (mf/use-state false)
        toggle-dropdown (mf/use-fn #(swap! show-dropdown? not))
        hide-dropdown   (mf/use-fn #(reset! show-dropdown? false))

        update-mode
        (mf/use-callback
         (fn [mode]
           (st/emit! (dcm/update-filters {:mode mode}))))

        update-show
        (mf/use-callback
         (fn [mode]
           (st/emit! (dcm/update-filters {:show mode}))))]

    [:div.view-options {:on-click toggle-dropdown}
     [:span.label (tr "labels.comments")]
     [:span.icon i/arrow-down]
     [:& dropdown {:show @show-dropdown?
                   :on-close hide-dropdown}
      [:ul.dropdown.with-check
       [:li {:class (dom/classnames :selected (= :all cmode))
             :on-click #(update-mode :all)}
        [:span.icon i/tick]
        [:span.label (tr "labels.show-all-comments")]]

       [:li {:class (dom/classnames :selected (= :yours cmode))
             :on-click #(update-mode :yours)}
        [:span.icon i/tick]
        [:span.label (tr "labels.show-your-comments")]]

       [:hr]

       [:li {:class (dom/classnames :selected (= :pending cshow))
             :on-click #(update-show (if (= :pending cshow) :all :pending))}
        [:span.icon i/tick]
        [:span.label (tr "labels.hide-resolved-comments")]]]]]))


(defn- frame-contains?
  [{:keys [x y width height]} {px :x py :y}]
  (let [x2 (+ x width)
        y2 (+ y height)]
    (and (<= x px x2)
         (<= y py y2))))

(def threads-ref
  (l/derived :comment-threads st/state))

(def comments-local-ref
  (l/derived :comments-local st/state))

(mf/defc comments-layer
  [{:keys [zoom frame data page file] :as props}]
  (let [profile     (mf/deref refs/profile)

        modifier1   (-> (gpt/point (:x frame) (:y frame))
                        (gpt/negate)
                        (gmt/translate-matrix))

        modifier2   (-> (gpt/point (:x frame) (:y frame))
                        (gmt/translate-matrix))

        threads-map (->> (mf/deref threads-ref)
                         (d/mapm #(update %2 :position gpt/transform modifier1)))

        cstate      (mf/deref refs/comments-local)

        mframe      (geom/transform-shape frame)
        threads     (->> (vals threads-map)
                         (dcm/apply-filters cstate profile)
                         (filter (fn [{:keys [position]}]
                                   (frame-contains? mframe position))))

        on-bubble-click
        (mf/use-callback
         (mf/deps cstate)
         (fn [thread]
           (if (= (:open cstate) (:id thread))
             (st/emit! (dcm/close-thread))
             (st/emit! (dcm/open-thread thread)))))

        on-click
        (mf/use-callback
         (mf/deps cstate frame page file)
         (fn [event]
           (prn "on click")
           (dom/stop-propagation event)
           (if (some? (:open cstate))
             (st/emit! (dcm/close-thread))
             (let [event    (.-nativeEvent ^js event)
                   position (-> (dom/get-offset-position event)
                                (gpt/transform modifier2))
                   params   {:position position
                             :page-id (:id page)
                             :file-id (:id file)}]
               (st/emit! (dcm/create-draft params))))))

        on-draft-cancel
        (mf/use-callback
         (mf/deps cstate)
         (st/emitf (dcm/close-thread)))

        on-draft-submit
        (mf/use-callback
         (mf/deps frame)
         (fn [draft]
           (let [params (update draft :position gpt/transform modifier2)]
             (st/emit! (dcm/create-thread params)
                       (dcm/close-thread)))))]

    [:div.comments-section {:on-click on-click}
     [:div.viewer-comments-container
      [:div.threads
       (for [item threads]
         [:& cmt/thread-bubble {:thread item
                                :zoom zoom
                                :on-click on-bubble-click
                                :open? (= (:id item) (:open cstate))
                                :key (:seqn item)}])

       (when-let [id (:open cstate)]
         (when-let [thread (get threads-map id)]
           [:& cmt/thread-comments {:thread thread
                                    :users (:users data)
                                    :zoom zoom}]))

       (when-let [draft (:draft cstate)]
         [:& cmt/draft-thread {:draft (update draft :position gpt/transform modifier1)
                               :on-cancel on-draft-cancel
                               :on-submit on-draft-submit
                               :zoom zoom}])]]]))
