;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.handoff.left-sidebar
  (:require
   [app.common.data :as d]
   [app.main.data.viewer :as dv]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.layers :refer [element-icon layer-name]]
   [app.util.dom :as dom]
   [app.util.keyboard :as kbd]
   [okulary.core :as l]
   [rumext.alpha :as mf]))

(defn- make-collapsed-iref
  [id]
  #(-> (l/in [:viewer-local :collapsed id])
       (l/derived st/state)))

(mf/defc layer-item
  [{:keys [item selected objects disable-collapse?] :as props}]
  (let [id        (:id item)
        selected? (contains? selected id)
        item-ref  (mf/use-ref nil)


        collapsed-iref (mf/use-memo
                        (mf/deps id)
                        (make-collapsed-iref id))

        expanded? (not (mf/deref collapsed-iref))

        toggle-collapse
        (fn [event]
          (dom/stop-propagation event)
          (st/emit! (dv/toggle-collapse id)))

        select-shape
        (fn [event]
          (dom/prevent-default event)
          (let [id (:id item)]
            (cond
              (or (kbd/ctrl? event) (kbd/meta? event))
              (st/emit! (dv/toggle-selection id))

              (kbd/shift? event)
              (st/emit! (dv/shift-select-to id))

              :else
              (st/emit! (dv/select-shape id)))
            ))
        ]

    (mf/use-effect
     (mf/deps selected)
     (fn []
       (when (and (= (count selected) 1) selected?)
         (.scrollIntoView (mf/ref-val item-ref) false))))

    [:li {:ref item-ref
          :class (dom/classnames
                   :component (not (nil? (:component-id item)))
                   :masked (:masked-group? item)
                   :selected selected?)}

     [:div.element-list-body {:class (dom/classnames :selected selected?
                                                     :icon-layer (= (:type item) :icon))
                              :on-click select-shape}
      [:& element-icon {:shape item}]
      [:& layer-name {:shape item}]

      (when (and (not disable-collapse?) (:shapes item))
        [:span.toggle-content
         {:on-click toggle-collapse
          :class (when expanded? "inverse")}
         i/arrow-slide])]

     (when (and (:shapes item) expanded?)
       [:ul.element-children
        (for [[index id] (reverse (d/enumerate (:shapes item)))]
          (when-let [item (get objects id)]
            [:& layer-item
             {:item item
              :selected selected
              :index index
              :objects objects
              :key (:id item)}]))])]))

(mf/defc left-sidebar
  [{:keys [frame page local]}]
  (let [selected (:selected local)
        objects  (:objects page)]

    [:aside.settings-bar.settings-bar-left
     [:div.settings-bar-inside
      [:ul.element-list
       [:& layer-item
        {:item frame
         :selected selected
         :index 0
         :objects objects
         :disable-collapse? true}]]]]))
