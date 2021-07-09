;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.rpc.mutations.share-link
  "Share link related rpc mutation methods."
  (:require
   [app.common.exceptions :as ex]
   [app.common.pages :as cp]
   [app.common.pages.migrations :as pmg]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.config :as cf]
   [app.db :as db]
   [app.rpc.permissions :as perms]
   [app.rpc.queries.files :as files]
   [app.rpc.queries.projects :as proj]
   [app.storage.impl :as simpl]
   [app.util.blob :as blob]
   [app.util.services :as sv]
   [app.util.time :as dt]
   [clojure.spec.alpha :as s]))

;; --- Helpers & Specs

(s/def ::id ::us/uuid)
(s/def ::profile-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::flags (s/every ::us/string :kind set?))
(s/def ::pages (s/every ::us/uuid :kind set?))

;; --- Mutation: Create Share Link

(declare create-share-link)

(s/def ::create-share-link
  (s/keys :req-un [::profile-id ::file-id ::flags]
          :opt-un [::pages]))

(sv/defmethod ::create-share-link
  [{:keys [pool] :as cfg} {:keys [profile-id file-id] :as params}]
  (db/with-atomic [conn pool]
    (files/check-edition-permissions! conn profile-id file-id)
    (create-share-link conn params)))

(defn create-share-link
  [conn {:keys [profile-id file-id pages flags]}]
  (let [pages (db/create-array conn "uuid" pages)
        flags (->> (map name flags)
                   (db/create-array conn "text"))]
    (db/insert! conn :share-link
                {:id (uuid/next)
                 :file-id file-id
                 :flags flags
                 :pages pages
                 :owner-id profile-id})))


;; --- Mutation: Delete Share Link

(declare delete-share-link)

(s/def ::delete-share-link
  (s/keys :req-un [::profile-id ::id]))

(sv/defmethod ::delete-share-link
  [{:keys [pool] :as cfg} {:keys [profile-id id] :as params}]
  (db/with-atomic [conn pool]
    (let [slink (db/get-by-id conn :share-link id)]
      (files/check-edition-permissions! conn profile-id (:file-id slink))
      (db/delete! conn :share-link id)
      nil)))
