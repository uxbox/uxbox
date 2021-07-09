;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.migrations
  (:require
   [app.migrations.migration-0023 :as mg0023]
   [app.util.migrations :as mg]
   [integrant.core :as ig]))

(def migrations
  [{:name "0001-add-extensions"
    :fn (mg/resource "app/migrations/sql/0001-add-extensions.sql")}

   {:name "0002-add-profile-tables"
    :fn (mg/resource "app/migrations/sql/0002-add-profile-tables.sql")}

   {:name "0003-add-project-tables"
    :fn (mg/resource "app/migrations/sql/0003-add-project-tables.sql")}

   {:name "0004-add-tasks-tables"
    :fn (mg/resource "app/migrations/sql/0004-add-tasks-tables.sql")}

   {:name "0005-add-libraries-tables"
    :fn (mg/resource "app/migrations/sql/0005-add-libraries-tables.sql")}

   {:name "0006-add-presence-tables"
    :fn (mg/resource "app/migrations/sql/0006-add-presence-tables.sql")}

   {:name "0007-drop-version-field-from-page-table"
    :fn (mg/resource "app/migrations/sql/0007-drop-version-field-from-page-table.sql")}

   {:name "0008-add-generic-token-table"
    :fn (mg/resource "app/migrations/sql/0008-add-generic-token-table.sql")}

   {:name "0009-drop-profile-email-table"
    :fn (mg/resource "app/migrations/sql/0009-drop-profile-email-table.sql")}

   {:name "0010-add-http-session-table"
    :fn (mg/resource "app/migrations/sql/0010-add-http-session-table.sql")}

   {:name "0011-add-session-id-field-to-page-change-table"
    :fn (mg/resource "app/migrations/sql/0011-add-session-id-field-to-page-change-table.sql")}

   {:name "0012-make-libraries-linked-to-a-file"
    :fn (mg/resource "app/migrations/sql/0012-make-libraries-linked-to-a-file.sql")}

   {:name "0013-mark-files-shareable"
    :fn (mg/resource "app/migrations/sql/0013-mark-files-shareable.sql")}

   {:name "0014-refactor-media-storage.sql"
    :fn (mg/resource "app/migrations/sql/0014-refactor-media-storage.sql")}

   {:name "0015-improve-tasks-tables"
    :fn (mg/resource "app/migrations/sql/0015-improve-tasks-tables.sql")}

   {:name "0016-truncate-and-alter-tokens-table"
    :fn (mg/resource "app/migrations/sql/0016-truncate-and-alter-tokens-table.sql")}

   {:name "0017-link-files-to-libraries"
    :fn (mg/resource "app/migrations/sql/0017-link-files-to-libraries.sql")}

   {:name "0018-add-file-trimming-triggers"
    :fn (mg/resource "app/migrations/sql/0018-add-file-trimming-triggers.sql")}

   {:name "0019-add-improved-scheduled-tasks"
    :fn (mg/resource "app/migrations/sql/0019-add-improved-scheduled-tasks.sql")}

   {:name "0020-minor-fixes-to-media-object"
    :fn (mg/resource "app/migrations/sql/0020-minor-fixes-to-media-object.sql")}

   {:name "0021-http-session-improvements"
    :fn (mg/resource "app/migrations/sql/0021-http-session-improvements.sql")}

   {:name "0022-page-file-refactor"
    :fn (mg/resource "app/migrations/sql/0022-page-file-refactor.sql")}

   {:name "0023-adapt-old-pages-and-files"
    :fn mg0023/migrate}

   {:name "0024-mod-profile-table"
    :fn (mg/resource "app/migrations/sql/0024-mod-profile-table.sql")}

   {:name "0025-del-generic-tokens-table"
    :fn (mg/resource "app/migrations/sql/0025-del-generic-tokens-table.sql")}

   {:name "0026-mod-file-library-rel-table-synced-date"
    :fn (mg/resource "app/migrations/sql/0026-mod-file-library-rel-table-synced-date.sql")}

   {:name "0027-mod-file-table-ignore-sync"
    :fn (mg/resource "app/migrations/sql/0027-mod-file-table-ignore-sync.sql")}

   {:name "0028-add-team-project-profile-rel-table"
    :fn (mg/resource "app/migrations/sql/0028-add-team-project-profile-rel-table.sql")}

   {:name "0029-del-project-profile-rel-indexes"
    :fn (mg/resource "app/migrations/sql/0029-del-project-profile-rel-indexes.sql")}

   {:name "0030-mod-file-table-add-missing-index"
    :fn (mg/resource "app/migrations/sql/0030-mod-file-table-add-missing-index.sql")}

   {:name "0031-add-conversation-related-tables"
    :fn (mg/resource "app/migrations/sql/0031-add-conversation-related-tables.sql")}

   {:name "0032-del-unused-tables"
    :fn (mg/resource "app/migrations/sql/0032-del-unused-tables.sql")}

   {:name "0033-mod-comment-thread-table"
    :fn (mg/resource "app/migrations/sql/0033-mod-comment-thread-table.sql")}

   {:name "0034-mod-profile-table-add-props-field"
    :fn (mg/resource "app/migrations/sql/0034-mod-profile-table-add-props-field.sql")}

   {:name "0035-add-storage-tables"
    :fn (mg/resource "app/migrations/sql/0035-add-storage-tables.sql")}

   {:name "0036-mod-storage-referenced-tables"
    :fn (mg/resource "app/migrations/sql/0036-mod-storage-referenced-tables.sql")}

   {:name "0037-del-obsolete-triggers"
    :fn (mg/resource "app/migrations/sql/0037-del-obsolete-triggers.sql")}

   {:name "0038-add-storage-on-delete-triggers"
    :fn (mg/resource "app/migrations/sql/0038-add-storage-on-delete-triggers.sql")}

   {:name "0039-fix-some-on-delete-triggers"
    :fn (mg/resource "app/migrations/sql/0039-fix-some-on-delete-triggers.sql")}

   {:name "0040-add-error-report-tables"
    :fn (mg/resource "app/migrations/sql/0040-add-error-report-tables.sql")}

   {:name "0041-mod-pg-storage-options"
    :fn (mg/resource "app/migrations/sql/0041-mod-pg-storage-options.sql")}

   {:name "0042-add-server-prop-table"
    :fn (mg/resource "app/migrations/sql/0042-add-server-prop-table.sql")}

   {:name "0043-drop-old-tables-and-fields"
    :fn (mg/resource "app/migrations/sql/0043-drop-old-tables-and-fields.sql")}

   {:name "0044-add-storage-refcount"
    :fn (mg/resource "app/migrations/sql/0044-add-storage-refcount.sql")}

   {:name "0045-add-index-to-file-change-table"
    :fn (mg/resource "app/migrations/sql/0045-add-index-to-file-change-table.sql")}

   {:name "0046-add-profile-complaint-table"
    :fn (mg/resource "app/migrations/sql/0046-add-profile-complaint-table.sql")}

   {:name "0047-mod-file-change-table"
    :fn (mg/resource "app/migrations/sql/0047-mod-file-change-table.sql")}

   {:name "0048-mod-storage-tables"
    :fn (mg/resource "app/migrations/sql/0048-mod-storage-tables.sql")}

   {:name "0049-mod-http-session-table"
    :fn (mg/resource "app/migrations/sql/0049-mod-http-session-table.sql")}

   {:name "0050-mod-server-prop-table"
    :fn (mg/resource "app/migrations/sql/0050-mod-server-prop-table.sql")}

   {:name "0051-mod-file-library-rel-table"
    :fn (mg/resource "app/migrations/sql/0051-mod-file-library-rel-table.sql")}

   {:name "0052-del-legacy-user-and-team"
    :fn (mg/resource "app/migrations/sql/0052-del-legacy-user-and-team.sql")}

   {:name "0053-add-team-font-variant-table"
    :fn (mg/resource "app/migrations/sql/0053-add-team-font-variant-table.sql")}

   {:name "0054-add-audit-log-table"
    :fn (mg/resource "app/migrations/sql/0054-add-audit-log-table.sql")}

   {:name "0055-mod-file-media-object-table"
    :fn (mg/resource "app/migrations/sql/0055-mod-file-media-object-table.sql")}

   {:name "0056-add-missing-index-on-deleted-at"
    :fn (mg/resource "app/migrations/sql/0056-add-missing-index-on-deleted-at.sql")}

   {:name "0057-del-profile-on-delete-trigger"
    :fn (mg/resource "app/migrations/sql/0057-del-profile-on-delete-trigger.sql")}

   {:name "0058-del-team-on-delete-trigger"
    :fn (mg/resource "app/migrations/sql/0058-del-team-on-delete-trigger.sql")}

   {:name "0059-mod-audit-log-table"
    :fn (mg/resource "app/migrations/sql/0059-mod-audit-log-table.sql")}

   {:name "0060-mod-file-change-table"
    :fn (mg/resource "app/migrations/sql/0060-mod-file-change-table.sql")}

   {:name "0061-mod-file-table"
    :fn (mg/resource "app/migrations/sql/0061-mod-file-table.sql")}

   {:name "0062-fix-metadata-media"
    :fn (mg/resource "app/migrations/sql/0062-fix-metadata-media.sql")}

   {:name "0062-fix-metadata-media"
    :fn (mg/resource "app/migrations/sql/0062-fix-metadata-media.sql")}

   {:name "0063-add-share-link-table"
    :fn (mg/resource "app/migrations/sql/0063-add-share-link-table.sql")}
   ])


(defmethod ig/init-key ::migrations [_ _] migrations)
