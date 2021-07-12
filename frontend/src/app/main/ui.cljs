;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui
  (:require
   [app.common.exceptions :as ex]
   [app.common.spec :as us]
   [app.config :as cf]
   [app.main.data.events :as ev]
   [app.main.data.messages :as dm]
   [app.main.data.users :as du]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.auth :refer [auth]]
   [app.main.ui.auth.verify-token :refer [verify-token]]
   [app.main.ui.components.fullscreen :as fs]
   [app.main.ui.context :as ctx]
   [app.main.ui.cursors :as c]
   [app.main.ui.dashboard :refer [dashboard]]
   [app.main.ui.handoff :refer [handoff]]
   [app.main.ui.icons :as i]
   [app.main.ui.messages :as msgs]
   [app.main.ui.onboarding]
   [app.main.ui.render :as render]
   [app.main.ui.settings :as settings]
   [app.main.ui.static :as static]
   [app.main.ui.viewer :refer [viewer-page]]
   [app.main.ui.workspace :as workspace]
   [app.util.timers :as ts]
   [cljs.pprint :refer [pprint]]
   [cljs.spec.alpha :as s]
   [cuerdas.core :as str]
   [expound.alpha :as expound]
   [potok.core :as ptk]
   [rumext.alpha :as mf]))

;; --- Routes

(s/def ::page-id ::us/uuid)
(s/def ::file-id ::us/uuid)
(s/def ::section ::us/keyword)
(s/def ::index ::us/integer)
(s/def ::token (s/nilable ::us/not-empty-string))

(s/def ::viewer-path-params
  (s/keys :req-un [::file-id]
          :opt-un [::page-id]))

(s/def ::viewer-query-params
  (s/keys :req-un [::index]
          :opt-un [::token ::section]))

(def routes
  [["/auth"
    ["/login"            :auth-login]
    (when cf/registration-enabled
      ["/register"         :auth-register])
    (when cf/registration-enabled
      ["/register/validate" :auth-register-validate])
    (when cf/registration-enabled
      ["/register/success" :auth-register-success])
    ["/recovery/request" :auth-recovery-request]
    ["/recovery"         :auth-recovery]
    ["/verify-token"     :auth-verify-token]]

   ["/settings"
    ["/profile"  :settings-profile]
    ["/password" :settings-password]
    ["/feedback" :settings-feedback]
    ["/options"  :settings-options]]

   ["/view/:file-id/:page-id"
    {:name :viewer
     :conform
     {:path-params ::viewer-path-params
      :query-params ::viewer-query-params}}]

   (when *assert*
     ["/debug/icons-preview" :debug-icons-preview])

   ;; Used for export
   ["/render-object/:file-id/:page-id/:object-id" :render-object]
   ["/render-sprite/:file-id" :render-sprite]

   ["/dashboard/team/:team-id"
    ["/members"              :dashboard-team-members]
    ["/settings"             :dashboard-team-settings]
    ["/projects"             :dashboard-projects]
    ["/search"               :dashboard-search]
    ["/fonts"                :dashboard-fonts]
    ["/fonts/providers"      :dashboard-font-providers]
    ["/libraries"            :dashboard-libraries]
    ["/projects/:project-id" :dashboard-files]]

   ["/workspace/:project-id/:file-id" :workspace]])

(mf/defc on-main-error
  [{:keys [error] :as props}]
  (mf/use-effect #(ptk/handle-error error))
  [:span "Internal application errror"])

(mf/defc main-page
  {::mf/wrap [#(mf/catch % {:fallback on-main-error})]}
  [{:keys [route] :as props}]
  (let [{:keys [data params]} route]
    [:& (mf/provider ctx/current-route) {:value route}
     (case (:name data)
       (:auth-login
        :auth-register
        :auth-register-validate
        :auth-register-success
        :auth-recovery-request
        :auth-recovery)
       [:& auth {:route route}]

       :auth-verify-token
       [:& verify-token {:route route}]

       (:settings-profile
        :settings-password
        :settings-options
        :settings-feedback)
       [:& settings/settings {:route route}]

       :debug-icons-preview
       (when *assert*
         [:div.debug-preview
          [:h1 "Cursors"]
          [:& c/debug-preview]
          [:h1 "Icons"]
          [:& i/debug-icons-preview]
          ])

       (:dashboard-search
        :dashboard-projects
        :dashboard-files
        :dashboard-libraries
        :dashboard-fonts
        :dashboard-font-providers
        :dashboard-team-members
        :dashboard-team-settings)
       [:*
        #_[:div.modal-wrapper
           [:& app.main.ui.onboarding/release-notes-modal {:version "1.7"}]]
        [:& dashboard {:route route}]]

       :viewer
       (let [index   (get-in route [:query-params :index])
             token   (get-in route [:query-params :token])
             section (get-in route [:query-params :section] :interactions)
             file-id (get-in route [:path-params :file-id])
             page-id (get-in route [:path-params :page-id])]
         [:& fs/fullscreen-wrapper {}
          (if (= section :handoff)
            [:& handoff {:page-id page-id
                         :file-id file-id
                         :index index
                         :token token}]
            [:& viewer-page {:page-id page-id
                             :file-id file-id
                             :section section
                             :index index
                             :token token}])])

       :render-object
       (do
         (let [file-id   (uuid (get-in route [:path-params :file-id]))
               page-id   (uuid (get-in route [:path-params :page-id]))
               object-id (uuid (get-in route [:path-params :object-id]))]
           [:& render/render-object {:file-id file-id
                                     :page-id page-id
                                     :object-id object-id}]))

       :render-sprite
       (do
         (let [file-id      (uuid (get-in route [:path-params :file-id]))
               component-id (get-in route [:query-params :component-id])
               component-id (when (some? component-id) (uuid component-id))]
           [:& render/render-sprite {:file-id file-id
                                     :component-id component-id}]))

       :workspace
       (let [project-id (some-> params :path :project-id uuid)
             file-id    (some-> params :path :file-id uuid)
             page-id    (some-> params :query :page-id uuid)
             layout     (some-> params :query :layout keyword)]
         [:& workspace/workspace {:project-id project-id
                                  :file-id file-id
                                  :page-id page-id
                                  :layout-name layout
                                  :key file-id}])
       nil)]))

(mf/defc app
  []
  (let [route (mf/deref refs/route)
        edata (mf/deref refs/exception)]
    [:& (mf/provider ctx/current-route) {:value route}
     (if edata
       [:& static/exception-page {:data edata}]
       [:*
        [:& msgs/notifications]
        (when route
          [:& main-page {:route route}])])]))

;; --- Error Handling

;; That are special case server-errors that should be treated
;; differently.
(derive :not-found ::exceptional-state)
(derive :bad-gateway ::exceptional-state)
(derive :service-unavailable ::exceptional-state)

(defmethod ptk/handle-error ::exceptional-state
  [error]
  (ts/schedule
   (st/emitf (dm/assign-exception error))))

;; We receive a explicit authentication error; this explicitly clears
;; all profile data and redirect the user to the login page.
(defmethod ptk/handle-error :authentication
  [_]
  (ts/schedule (st/emitf (du/logout))))

;; Error that happens on an active bussines model validation does not
;; passes an validation (example: profile can't leave a team). From
;; the user perspective a error flash message should be visualized but
;; user can continue operate on the application.
(defmethod ptk/handle-error :validation
  [error]
  (ts/schedule
   (st/emitf
    (dm/show {:content "Unexpected validation error."
              :type :error
              :timeout 3000})))

  ;; Print to the console some debug info.
  (js/console.group "Validation Error")
  (ex/ignoring
   (js/console.info
    (with-out-str
      (pprint (dissoc error :explain))))
   (when-let [explain (:explain error)]
     (js/console.error explain)))
  (js/console.groupEnd "Validation Error"))

;; Error on parsing an SVG
(defmethod ptk/handle-error :svg-parser
  [_]
  (ts/schedule
   (st/emitf
    (dm/show {:content "SVG is invalid or malformed"
              :type :error
              :timeout 3000}))))

;; This is a pure frontend error that can be caused by an active
;; assertion (assertion that is preserved on production builds). From
;; the user perspective this should be treated as internal error.
(defmethod ptk/handle-error :assertion
  [{:keys [data stack message hint context] :as error}]
  (let [message (or message hint)
        context (str/fmt "ns: '%s'\nname: '%s'\nfile: '%s:%s'"
                              (:ns context)
                              (:name context)
                              (str cf/public-uri "js/cljs-runtime/" (:file context))
                              (:line context))]
    (ts/schedule
     (st/emitf
      (dm/show {:content "Internal error: assertion."
                :type :error
                :timeout 3000})
      (ptk/event ::ev/event
                 {::ev/type "exception"
                  ::ev/name "assertion-error"
                  :message message
                  :context context
                  :trace stack})))

    ;; Print to the console some debugging info
    (js/console.group message)
    (js/console.info context)
    (js/console.groupCollapsed "Stack Trace")
    (js/console.info stack)
    (js/console.groupEnd "Stack Trace")
    (js/console.error (with-out-str (expound/printer data)))
    (js/console.groupEnd message)))

;; This happens when the backed server fails to process the
;; request. This can be caused by an internal assertion or any other
;; uncontrolled error.
(defmethod ptk/handle-error :server-error
  [{:keys [data hint] :as error}]
  (let [hint (or hint (:hint data) (:message data))
        info (with-out-str (pprint (dissoc data :explain)))
        expl (:explain data)]
    (ts/schedule
     (st/emitf
      (dm/show {:content "Something wrong has happened (on backend)."
                :type :error
                :timeout 3000})
      (ptk/event ::ev/event
                 {::ev/type "exception"
                  ::ev/name "server-error"
                  :hint hint
                  :info info
                  :explain expl})))

    (js/console.group "Internal Server Error:")
    (js/console.error "hint:" hint)
    (js/console.info info)
    (when expl (js/console.error expl))
    (js/console.groupEnd "Internal Server Error:")))

(defmethod ptk/handle-error :default
  [error]
  (if (instance? ExceptionInfo error)
    (ptk/handle-error (ex-data error))
    (let [stack (.-stack error)
          hint  (or (ex-message error)
                    (:hint error)
                    (:message error))]
      (ts/schedule
       (st/emitf
        (dm/assign-exception error)
        (ptk/event ::ev/event
                   {::ev/type "exception"
                    ::ev/name "unexpected-error"
                    :message hint
                    :trace (.-stack error)})))

      (js/console.group "Internal error:")
      (js/console.log "hint:" hint)
      (ex/ignoring
       (js/console.error (clj->js error))
       (js/console.error "stack:" stack))
      (js/console.groupEnd "Internal error:"))))

(defonce uncaught-error-handler
  (letfn [(on-error [event]
            (ptk/handle-error (unchecked-get event "error"))
            (.preventDefault ^js event))]
    (.addEventListener js/window "error" on-error)
    (fn []
      (.removeEventListener js/window "error" on-error))))
