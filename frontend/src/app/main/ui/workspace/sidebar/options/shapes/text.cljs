;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.shapes.text
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.texts :as dwt]
   [app.main.refs :as refs]
   [app.main.ui.workspace.sidebar.options.menus.blur :refer [blur-menu]]
   [app.main.ui.workspace.sidebar.options.menus.constraints :refer [constraint-attrs constraints-menu]]
   [app.main.ui.workspace.sidebar.options.menus.fill :refer [fill-menu fill-attrs]]
   [app.main.ui.workspace.sidebar.options.menus.layer :refer [layer-attrs layer-menu]]
   [app.main.ui.workspace.sidebar.options.menus.measures :refer [measure-attrs measures-menu]]
   [app.main.ui.workspace.sidebar.options.menus.shadow :refer [shadow-menu]]
   [app.main.ui.workspace.sidebar.options.menus.text :refer [text-menu text-fill-attrs root-attrs paragraph-attrs text-attrs]]
   [rumext.alpha :as mf]))

(mf/defc options
  [{:keys [shape] :as props}]
  (let [ids    [(:id shape)]
        type   (:type shape)

        state-map    (mf/deref refs/workspace-editor-state)
        editor-state (get state-map (:id shape))

        layer-values (select-keys shape layer-attrs)

        fill-values  (dwt/current-text-values
                      {:editor-state editor-state
                       :shape shape
                       :attrs text-fill-attrs})

        fill-values (d/update-in-when fill-values [:fill-color-gradient :type] keyword)

        fill-values (cond-> fill-values
                      ;; Keep for backwards compatibility
                      (:fill fill-values) (assoc :fill-color (:fill fill-values))
                      (:opacity fill-values) (assoc :fill-opacity (:fill fill-values)))

        text-values (d/merge
                     (select-keys shape [:grow-type])
                     (select-keys shape fill-attrs)
                     (dwt/current-root-values
                      {:shape shape
                       :attrs root-attrs})
                     (dwt/current-paragraph-values
                      {:editor-state editor-state
                       :shape shape
                       :attrs paragraph-attrs})
                     (dwt/current-text-values
                      {:editor-state editor-state
                       :shape shape
                       :attrs text-attrs}))]

    [:*

     [:& measures-menu
      {:ids ids
       :type type
       :values (select-keys shape measure-attrs)}]

     [:& constraints-menu
      {:ids ids
       :values (select-keys shape constraint-attrs)}]

     [:& layer-menu {:ids ids
                     :type type
                     :values layer-values}]

     [:& fill-menu
      {:ids ids
       :type type
       :values fill-values}]

     [:& shadow-menu
      {:ids ids
       :values (select-keys shape [:shadow])}]

     [:& blur-menu
      {:ids ids
       :values (select-keys shape [:blur])}]

     [:& text-menu
      {:ids ids
       :type type
       :values text-values}]]))
