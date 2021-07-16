
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.refs
  "A collection of derived refs."
  (:require
   [app.common.data :as d]
   [app.common.geom.shapes :as gsh]
   [app.common.pages :as cp]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.store :as st]
   [okulary.core :as l]))

;; ---- Global refs

(def route
  (l/derived :route st/state))

(def router
  (l/derived :router st/state))

(def message
  (l/derived :message st/state))

(def profile
  (l/derived :profile st/state))

(def teams
  (l/derived :teams st/state))

(def exception
  (l/derived :exception st/state))

(def threads-ref
  (l/derived :comment-threads st/state))

;; ---- Dashboard refs

(def dashboard-local
  (l/derived :dashboard-local st/state))

(def dashboard-fonts
  (l/derived :dashboard-fonts st/state))

(def dashboard-projects
  (l/derived :dashboard-projects st/state))

(def dashboard-files
  (l/derived :dashboard-files st/state))

(def dashboard-shared-files
  (l/derived :dashboard-shared-files st/state))

(def dashboard-search-result
  (l/derived :dashboard-search-result st/state))

(def dashboard-team
  (l/derived (fn [state]
               (let [team-id (:current-team-id state)]
                 (get-in state [:teams team-id])))
             st/state))

(def dashboard-team-stats
  (l/derived :dashboard-team-stats st/state))

(def dashboard-team-members
  (l/derived :dashboard-team-members st/state))

(def dashboard-selected-project
  (l/derived (fn [state]
               (get-in state [:dashboard-local :selected-project]))
             st/state))

(def dashboard-selected-files
  (l/derived (fn [state]
               (let [get-file #(get-in state [:dashboard-files %])
                     sim-file #(select-keys % [:id :name :project-id :is-shared])
                     selected (get-in state [:dashboard-local :selected-files])
                     xform    (comp (map get-file)
                                    (map sim-file))]
                 (->> (into #{} xform selected)
                      (d/index-by :id))))
             st/state =))

;; ---- Workspace refs

(def workspace-local
  (l/derived :workspace-local st/state))

(def workspace-drawing
  (l/derived :workspace-drawing st/state))

(def selected-shapes
  (l/derived wsh/lookup-selected st/state))

(defn make-selected-ref
  [id]
  (l/derived #(contains? % id) selected-shapes))

(def viewport-data
  (l/derived #(select-keys % [:options-mode
                              :zoom
                              :vport
                              :vbox
                              :edition
                              :edit-path
                              :tooltip
                              :panning
                              :picking-color?
                              :transform
                              :hover
                              :modifiers
                              :selrect
                              :show-distances?])
             workspace-local =))

(def selected-zoom
  (l/derived :zoom workspace-local))

(def selected-drawing-tool
  (l/derived :tool workspace-drawing))

(def current-drawing-shape
  (l/derived :object workspace-drawing))

(def selected-edition
  (l/derived :edition workspace-local))

(def current-transform
  (l/derived :transform workspace-local))

(def options-mode
  (l/derived :options-mode workspace-local))

(def vbox
  (l/derived :vbox workspace-local))

(def current-hover
  (l/derived :hover workspace-local))

(def editors
  (l/derived :editors workspace-local))

(def workspace-layout
  (l/derived :workspace-layout st/state))

(def workspace-file
  (l/derived (fn [state]
               (let [file (:workspace-file state)
                     data (:workspace-data state)]
                 (-> file
                     (dissoc :data)
                     (assoc :pages (:pages data)))))
             st/state =))

(def workspace-file-colors
  (l/derived (fn [state]
               (when-let [file (:workspace-data state)]
                 (->> (:colors file)
                      (d/mapm #(assoc %2 :file-id (:id file))))))
             st/state))

(def workspace-recent-colors
  (l/derived (fn [state]
               (get-in state [:workspace-data :recent-colors] []))
             st/state))

(def workspace-file-typography
  (l/derived (fn [state]
               (when-let [file (:workspace-data state)]
                 (:typographies file)))
             st/state))

(def workspace-project
  (l/derived :workspace-project st/state))

(def workspace-shared-files
  (l/derived :workspace-shared-files st/state))

(def workspace-local-library
  (l/derived (fn [state]
               (select-keys (get state :workspace-data)
                            [:id
                             :colors
                             :media
                             :typographies
                             :components]))
             st/state))

(def workspace-libraries
  (l/derived :workspace-libraries st/state))

(def workspace-presence
  (l/derived :workspace-presence st/state))

(def workspace-snap-data
  (l/derived :workspace-snap-data st/state))

(def workspace-page
  (l/derived (fn [state]
               (let [page-id (:current-page-id state)
                     data    (:workspace-data state)]
                 (get-in data [:pages-index page-id])))
             st/state))

(def workspace-page-objects
  (l/derived wsh/lookup-page-objects st/state =))

(def workspace-modifiers
  (l/derived :workspace-modifiers st/state))

(def workspace-page-options
  (l/derived :options workspace-page))

(def workspace-frames
  (l/derived cp/select-frames workspace-page-objects))

(def workspace-editor
  (l/derived :workspace-editor st/state))

(def workspace-editor-state
  (l/derived :workspace-editor-state st/state))

(defn object-by-id
  [id]
  (l/derived #(get % id) workspace-page-objects))

(defn objects-by-id
  ([ids]
   (objects-by-id ids nil))

  ([ids {:keys [with-modifiers?]
         :or { with-modifiers? false }}]
   (l/derived (fn [state]
                (let [objects (wsh/lookup-page-objects state)
                      modifiers (:workspace-modifiers state)
                      objects (cond-> objects
                                with-modifiers?
                                (gsh/merge-modifiers modifiers))
                      xform (comp (map #(get objects %))
                                  (remove nil?))]
                  (into [] xform ids)))
              st/state =)))

(def selected-data
  (l/derived #(let [selected (wsh/lookup-selected %)
                    objects (wsh/lookup-page-objects %)]
                (hash-map :selected selected
                          :objects objects))
             st/state =))

(defn is-child-selected?
  [id]
  (letfn [(selector [{:keys [selected objects]}]
            (let [children (cp/get-children id objects)]
              (some #(contains? selected %) children)))]
    (l/derived selector selected-data =)))

(def selected-objects
  (letfn [(selector [{:keys [selected objects]}]
            (->> selected
                 (map #(get objects %))
                 (filterv (comp not nil?))))]
    (l/derived selector selected-data =)))

(def selected-shapes-with-children
  (letfn [(selector [{:keys [selected objects]}]
            (let [children (->> selected
                                (mapcat #(cp/get-children % objects))
                                (filterv (comp not nil?)))]
              (into selected children)))]
    (l/derived selector selected-data =)))

(def selected-objects-with-children
  (letfn [(selector [{:keys [selected objects]}]
            (let [children (->> selected
                                (mapcat #(cp/get-children % objects))
                                (filterv (comp not nil?)))
                  shapes   (into selected children)]
              (mapv #(get objects %) shapes)))]
    (l/derived selector selected-data =)))

;; ---- Viewer refs

(def viewer-file
  (l/derived :viewer-file st/state))

(def viewer-project
  (l/derived :viewer-file st/state))

(def viewer-data
  (l/derived :viewer st/state))

(def viewer-state
  (l/derived :viewer st/state))

(def viewer-local
  (l/derived :viewer-local st/state))

(def comment-threads
  (l/derived :comment-threads st/state))

(def comments-local
  (l/derived :comments-local st/state))

(def users
  (l/derived :users st/state))

