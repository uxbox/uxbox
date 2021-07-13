;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.viewer
  (:require
   [app.common.data :as d]
   [app.common.pages :as cp]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.main.constants :as c]
   [app.main.data.comments :as dcm]
   [app.main.data.fonts :as df]
   [app.main.repo :as rp]
   [app.util.router :as rt]
   [beicon.core :as rx]
   [cljs.spec.alpha :as s]
   [potok.core :as ptk]))

;; --- General Specs

(s/def ::id ::us/uuid)
(s/def ::name ::us/string)

(s/def ::project (s/keys :req-un [::id ::name]))
(s/def ::file (s/keys :req-un [::id ::name]))
(s/def ::page ::cp/page)

(s/def ::bundle
  (s/keys :req-un [::project ::file ::page]))


;; --- Local State Initialization

(def ^:private
  default-local-state
  {:zoom 1
   :interactions-mode :hide
   :interactions-show? false
   :comments-mode :all
   :comments-show :unresolved
   :selected #{}
   :collapsed #{}
   :hover nil})

(declare fetch-comment-threads)
(declare fetch-bundle)
(declare bundle-fetched)

(s/def ::page-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::index ::us/integer)
(s/def ::token (s/nilable ::us/string))
(s/def ::section ::us/string)

(s/def ::initialize-params
  (s/keys :req-un [::page-id ::file-id]
          :opt-un [::token]))

(defn initialize-file
  [{:keys [file-id] :as params}]
  (us/assert ::initialize-params params)
  (ptk/reify ::initialize
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc :current-file-id file-id)
          (update :viewer-local
                  (fn [lstate]
                    (if (nil? lstate)
                      default-local-state
                      lstate)))))

    ptk/WatchEvent
    (watch [_ state _]
      (rx/of (fetch-bundle params)
             (fetch-comment-threads params)))))

(defn finalize-file
  [params]
  (ptk/reify ::finalize-file
    ptk/UpdateEvent
    (update [_ state]
      ;; TODO: complete this
      (dissoc state :viewer-file :viewer-project :viewer-local))))

;; --- Select Page

(defn- extract-frames
  [objects]
  (let [root (get objects uuid/zero)]
    (into [] (comp (map #(get objects %))
                   (filter #(= :frame (:type %))))
          (reverse (:shapes root)))))

;; (defn initialize-page
;;   [{:keys [page-id] :as params}]
;;   (ptk/reify ::initialize
;;     ptk/UpdateEvent
;;     (update [_ state]
;;       (let [page    (get-in state [:viewer :data :pages-index page-id])
;;             ;; page    (assoc page :id page-id)
;;             ;; frames  (extract-frames (:objects page))]
;;         (-> state
;;             (assoc :viewer-page page)

;;         (update state :viewer merge
;;                 {:page page :frames frames})))))

;; --- Data Fetching

(s/def ::fetch-bundle-params
  (s/keys :req-un [::page-id ::file-id]
          :opt-un [::token]))

(defn fetch-bundle
  [{:keys [file-id token] :as params}]
  (us/assert ::fetch-bundle-params params)
  (ptk/reify ::fetch-file
    ptk/WatchEvent
    (watch [_ _ _]
      (let [params' (cond-> {:file-id file-id}
                      (string? token) (assoc :token token))]
        (->> (rp/query :view-only-bundle params')
             (rx/mapcat
              (fn [{:keys [fonts] :as bundle}]
                (rx/of (df/fonts-fetched fonts)
                       (bundle-fetched (merge bundle params))))))))))

(defn bundle-fetched
  [{:keys [project file share-links libraries users data] :as bundle}]
  ;; (us/verify ::bundle bundle)
  (ptk/reify ::bundle-fetched
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc :viewer-libraries (d/index-by :id libraries))
          (assoc :viewer-users (d/index-by :id users))
          (assoc :viewer-share-links share-links)
          (assoc :viewer-project project)
          (assoc :viewer-file file)
          (assoc :viewer-data data)))))



      ;; (assoc state :viewer
      ;;        {:libraries (d/index-by :id libraries)
      ;;         :share-links share-links
      ;;         :project project
      ;;         :file file
      ;;         :data data
      ;;         :users (d/index-by :id users)}))))


(defn fetch-comment-threads
  [{:keys [file-id page-id] :as params}]
  (letfn [(fetched [data state]
            (->> data
                 (filter #(= page-id (:page-id %)))
                 (d/index-by :id)
                 (assoc state :comment-threads)))
          (on-error [{:keys [type] :as err}]
            (if (or (= :authentication type)
                    (= :not-found type))
              (rx/empty)
              (rx/throw err)))]

    (ptk/reify ::fetch-comment-threads
      ptk/WatchEvent
      (watch [_ _ _]
        (->> (rp/query :comment-threads {:file-id file-id})
             (rx/map #(partial fetched %))
             (rx/catch on-error))))))

(defn refresh-comment-thread
  [{:keys [id file-id] :as thread}]
  (letfn [(fetched [thread state]
            (assoc-in state [:comment-threads id] thread))]
    (ptk/reify ::refresh-comment-thread
      ptk/WatchEvent
      (watch [_ _ _]
        (->> (rp/query :comment-thread {:file-id file-id :id id})
             (rx/map #(partial fetched %)))))))

(defn fetch-comments
  [{:keys [thread-id]}]
  (us/assert ::us/uuid thread-id)
  (letfn [(fetched [comments state]
            (update state :comments assoc thread-id (d/index-by :id comments)))]
    (ptk/reify ::retrieve-comments
      ptk/WatchEvent
      (watch [_ _ _]
        (->> (rp/query :comments {:thread-id thread-id})
             (rx/map #(partial fetched %)))))))

(defn create-share-link
  []
  (ptk/reify ::create-share-link
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            page-id (:current-page-id state)]
        (->> (rp/mutation! :create-file-share-token {:file-id file-id
                                                     :page-id page-id})
             (rx/map (fn [{:keys [token]}]
                       #(assoc-in % [:viewer-data :token] token))))))))

(defn delete-share-link
  []
  (ptk/reify ::delete-share-link
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            page-id (:current-page-id state)
            token   (get-in state [:viewer-data :token])
            params  {:file-id file-id
                     :page-id page-id
                     :token token}]
        (->> (rp/mutation :delete-file-share-token params)
             (rx/map (fn [_] #(update % :viewer-data dissoc :token))))))))

;; --- Zoom Management

(def increase-zoom
  (ptk/reify ::increase-zoom
    ptk/UpdateEvent
    (update [_ state]
      (let [increase #(nth c/zoom-levels
                           (+ (d/index-of c/zoom-levels %) 1)
                           (last c/zoom-levels))]
        (update-in state [:viewer-local :zoom] (fnil increase 1))))))

(def decrease-zoom
  (ptk/reify ::decrease-zoom
    ptk/UpdateEvent
    (update [_ state]
      (let [decrease #(nth c/zoom-levels
                           (- (d/index-of c/zoom-levels %) 1)
                           (first c/zoom-levels))]
        (update-in state [:viewer-local :zoom] (fnil decrease 1))))))

(def reset-zoom
  (ptk/reify ::reset-zoom
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :zoom] 1))))

(def zoom-to-50
  (ptk/reify ::zoom-to-50
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :zoom] 0.5))))

(def zoom-to-200
  (ptk/reify ::zoom-to-200
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :zoom] 2))))

;; --- Local State Management

(def toggle-thumbnails-panel
  (ptk/reify ::toggle-thumbnails-panel
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:viewer-local :show-thumbnails] not))))

(def select-prev-frame
  (ptk/reify ::select-prev-frame
    ptk/WatchEvent
    (watch [_ state _]
      (let [route   (:route state)
            screen  (-> route :data :name keyword)
            qparams (:query-params route)
            pparams (:path-params route)
            index   (:index qparams)]
        (when (pos? index)
          (rx/of
           (dcm/close-thread)
           (rt/nav screen pparams (assoc qparams :index (dec index)))))))))

(def select-next-frame
  (ptk/reify ::select-prev-frame
    ptk/WatchEvent
    (watch [_ state _]
      (let [route   (:route state)
            screen  (-> route :data :name keyword)
            qparams (:query-params route)
            pparams (:path-params route)
            index   (:index qparams)
            total   (count (get-in state [:viewer-data :frames]))]
        (when (< index (dec total))
          (rx/of
           (dcm/close-thread)
           (rt/nav screen pparams (assoc qparams :index (inc index)))))))))

(s/def ::interactions-mode #{:hide :show :show-on-click})

(defn set-interactions-mode
  [mode]
  (us/verify ::interactions-mode mode)
  (ptk/reify ::set-interactions-mode
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:viewer-local :interactions-mode] mode)
          (assoc-in [:viewer-local :interactions-show?] (case mode
                                                          :hide false
                                                          :show true
                                                          :show-on-click false))))))

(declare flash-done)

(def flash-interactions
  (ptk/reify ::flash-interactions
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :interactions-show?] true))

    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stopper (rx/filter (ptk/type? ::flash-interactions) stream)]
        (->> (rx/of flash-done)
             (rx/delay 500)
             (rx/take-until stopper))))))

(def flash-done
  (ptk/reify ::flash-done
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :interactions-show?] false))))

;; --- Navigation

(defn go-to-frame-by-index
  [index]
  (ptk/reify ::go-to-frame
    ptk/WatchEvent
    (watch [_ state _]
      (let [route   (:route state)
            screen  (-> route :data :name keyword)
            qparams (:query-params route)
            pparams (:path-params route)]
        (rx/of (rt/nav screen pparams (assoc qparams :index index)))))))

(defn go-to-frame
  [frame-id]
  (us/verify ::us/uuid frame-id)
  (ptk/reify ::go-to-frame
    ptk/WatchEvent
    (watch [_ state _]
      (let [frames  (get-in state [:viewer-data :frames])
            index   (d/index-of-pred frames #(= (:id %) frame-id))]
        (when index
          (rx/of (go-to-frame-by-index index)))))))

(defn go-to-section
  [section]
  (ptk/reify ::go-to-section
    ptk/WatchEvent
    (watch [_ state _]
      (let [route   (:route state)
            pparams (:path-params route)
            qparams (:query-params route)]
        (rx/of (rt/nav :viewer pparams (assoc qparams :section section)))))))

(defn set-current-frame [frame-id]
  (ptk/reify ::set-current-frame
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-data :current-frame-id] frame-id))))

(defn deselect-all []
  (ptk/reify ::deselect-all
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :selected] #{}))))

(defn select-shape
  ([id]
   (ptk/reify ::select-shape
     ptk/UpdateEvent
     (update [_ state]
       (-> state
           (assoc-in [:viewer-local :selected] #{id}))))))

(defn toggle-selection
  [id]
  (ptk/reify ::toggle-selection
    ptk/UpdateEvent
    (update [_ state]
      (let [selected (get-in state [:viewer-local :selected])]
        (cond-> state
          (not (selected id)) (update-in [:viewer-local :selected] conj id)
          (selected id)       (update-in [:viewer-local :selected] disj id))))))

(defn shift-select-to
  [id]
  (ptk/reify ::shift-select-to
    ptk/UpdateEvent
    (update [_ state]
      (let [objects (get-in state [:viewer-data :objects])
            selection (-> state
                          (get-in [:viewer-local :selected] #{})
                          (conj id))]
        (-> state
            (assoc-in [:viewer-local :selected]
                      (cp/expand-region-selection objects selection)))))))

(defn select-all
  []
  (ptk/reify ::select-all
    ptk/UpdateEvent
    (update [_ state]
      (let [objects (get-in state [:viewer-data :objects])
            frame-id (get-in state [:viewer-data :current-frame-id])
            selection (->> objects
                           (filter #(= (:frame-id (second %)) frame-id))
                           (map first)
                           (into #{frame-id}))]
        (-> state
            (assoc-in [:viewer-local :selected] selection))))))

(defn toggle-collapse [id]
  (ptk/reify ::toggle-collapse
    ptk/UpdateEvent
    (update [_ state]
      (let [toggled? (contains? (get-in state [:viewer-local :collapsed]) id)]
        (update-in state [:viewer-local :collapsed] (if toggled? disj conj) id)))))

(defn hover-shape [id hover?]
  (ptk/reify ::hover-shape
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:viewer-local :hover] (when hover? id)))))

;; --- NAV

(defn go-to-dashboard
  ([] (go-to-dashboard nil))
  ([{:keys [team-id]}]
   (ptk/reify ::go-to-dashboard
     ptk/WatchEvent
     (watch [_ state _]
       (let [team-id (or team-id (get-in state [:viewer-data :project :team-id]))]
         (rx/of (rt/nav :dashboard-projects {:team-id team-id})))))))

(defn go-to-page
  [page-id]
  (ptk/reify ::go-to-page
    ptk/WatchEvent
     (watch [_ state _]
       (let [route   (:route state)
             qparams (assoc (:query-params route) :index 0)
             pparams (assoc (:path-params route) :page-id page-id)
             rname   (get-in route [:data :name])]
         (rx/of (rt/nav rname pparams qparams))))))
