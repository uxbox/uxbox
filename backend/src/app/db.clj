;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.db
  (:require
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.geom.point :as gpt]
   [app.common.spec :as us]
   [app.common.transit :as t]
   [app.common.uuid :as uuid]
   [app.db.sql :as sql]
   [app.metrics :as mtx]
   [app.util.json :as json]
   [app.util.logging :as l]
   [app.util.migrations :as mg]
   [app.util.time :as dt]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [integrant.core :as ig]
   [next.jdbc :as jdbc]
   [next.jdbc.date-time :as jdbc-dt])
  (:import
   com.zaxxer.hikari.HikariConfig
   com.zaxxer.hikari.HikariDataSource
   com.zaxxer.hikari.metrics.prometheus.PrometheusMetricsTrackerFactory
   java.lang.AutoCloseable
   java.sql.Connection
   java.sql.Savepoint
   org.postgresql.PGConnection
   org.postgresql.geometric.PGpoint
   org.postgresql.largeobject.LargeObject
   org.postgresql.largeobject.LargeObjectManager
   org.postgresql.jdbc.PgArray
   org.postgresql.util.PGInterval
   org.postgresql.util.PGobject))

(declare open)
(declare create-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare instrument-jdbc!)

(s/def ::name keyword?)
(s/def ::uri ::us/not-empty-string)
(s/def ::min-pool-size ::us/integer)
(s/def ::max-pool-size ::us/integer)
(s/def ::migrations map?)

(defmethod ig/pre-init-spec ::pool [_]
  (s/keys :req-un [::uri ::name ::min-pool-size ::max-pool-size ::migrations ::mtx/metrics]))

(defmethod ig/init-key ::pool
  [_ {:keys [migrations metrics] :as cfg}]
  (l/info :action "initialize connection pool"
          :name (d/name (:name cfg))
          :uri (:uri cfg))
  (instrument-jdbc! (:registry metrics))
  (let [pool (create-pool cfg)]
    (when (seq migrations)
      (with-open [conn ^AutoCloseable (open pool)]
        (mg/setup! conn)
        (doseq [[name steps] migrations]
          (mg/migrate! conn {:name (d/name name) :steps steps}))))
    pool))

(defmethod ig/halt-key! ::pool
  [_ pool]
  (.close ^HikariDataSource pool))

(defn- instrument-jdbc!
  [registry]
  (mtx/instrument-vars!
   [#'next.jdbc/execute-one!
    #'next.jdbc/execute!]
   {:registry registry
    :type :counter
    :name "database_query_total"
    :help "An absolute counter of database queries."}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API & Impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def initsql
  (str "SET statement_timeout = 120000;\n"
       "SET idle_in_transaction_session_timeout = 120000;"))

(defn- create-datasource-config
  [{:keys [metrics] :as cfg}]
  (let [dburi    (:uri cfg)
        username (:username cfg)
        password (:password cfg)
        config   (HikariConfig.)
        mtf      (PrometheusMetricsTrackerFactory. (:registry metrics))]
    (doto config
      (.setJdbcUrl (str "jdbc:" dburi))
      (.setPoolName (d/name (:name cfg)))
      (.setAutoCommit true)
      (.setReadOnly false)
      (.setConnectionTimeout 8000)  ;; 8seg
      (.setValidationTimeout 8000)  ;; 8seg
      (.setIdleTimeout 120000)      ;; 2min
      (.setMaxLifetime 1800000)     ;; 30min
      (.setMinimumIdle (:min-pool-size cfg 0))
      (.setMaximumPoolSize (:max-pool-size cfg 30))
      (.setMetricsTrackerFactory mtf)
      (.setConnectionInitSql initsql)
      (.setInitializationFailTimeout -1))
    (when username (.setUsername config username))
    (when password (.setPassword config password))
    config))

(defn pool?
  [v]
  (instance? javax.sql.DataSource v))

(s/def ::pool pool?)

(defn pool-closed?
  [pool]
  (.isClosed ^HikariDataSource pool))

(defn- create-pool
  [cfg]
  (let [dsc (create-datasource-config cfg)]
    (jdbc-dt/read-as-instant)
    (HikariDataSource. dsc)))

(defn unwrap
  [conn klass]
  (.unwrap ^Connection conn klass))

(defn lobj-manager
  [conn]
  (let [conn (unwrap conn org.postgresql.PGConnection)]
    (.getLargeObjectAPI ^PGConnection conn)))

(defn lobj-create
  [manager]
  (.createLO ^LargeObjectManager manager LargeObjectManager/READWRITE))

(defn lobj-open
  ([manager oid]
   (lobj-open manager oid {}))
  ([manager oid {:keys [mode] :or {mode :rw}}]
   (let [mode (case mode
                (:r :read) LargeObjectManager/READ
                (:w :write) LargeObjectManager/WRITE
                (:rw :read+write) LargeObjectManager/READWRITE)]
     (.open ^LargeObjectManager manager (long oid) mode))))

(defn lobj-unlink
  [manager oid]
  (.unlink ^LargeObjectManager manager (long oid)))

(extend-type LargeObject
  io/IOFactory
  (make-reader [lobj opts]
    (let [^InputStream is (.getInputStream ^LargeObject lobj)]
      (io/make-reader is opts)))
  (make-writer [lobj opts]
    (let [^OutputStream os (.getOutputStream ^LargeObject lobj)]
      (io/make-writer os opts)))
  (make-input-stream [lobj opts]
    (let [^InputStream is (.getInputStream ^LargeObject lobj)]
      (io/make-input-stream is opts)))
  (make-output-stream [lobj opts]
    (let [^OutputStream os (.getOutputStream ^LargeObject lobj)]
      (io/make-output-stream os opts))))

(defmacro with-atomic
  [& args]
  `(jdbc/with-transaction ~@args))

(defn ^Connection open
  [pool]
  (jdbc/get-connection pool))

(defn exec!
  ([ds sv]
   (exec! ds sv {}))
  ([ds sv opts]
   (jdbc/execute! ds sv (assoc opts :builder-fn sql/as-kebab-maps))))

(defn exec-one!
  ([ds sv] (exec-one! ds sv {}))
  ([ds sv opts]
   (jdbc/execute-one! ds sv (assoc opts :builder-fn sql/as-kebab-maps))))

(defn insert!
  ([ds table params] (insert! ds table params nil))
  ([ds table params opts]
   (exec-one! ds
              (sql/insert table params opts)
              (assoc opts :return-keys true))))

(defn insert-multi!
  ([ds table cols rows] (insert-multi! ds table cols rows nil))
  ([ds table cols rows opts]
   (exec! ds
          (sql/insert-multi table cols rows opts)
          (assoc opts :return-keys true))))

(defn update!
  ([ds table params where] (update! ds table params where nil))
  ([ds table params where opts]
   (exec-one! ds
              (sql/update table params where opts)
              (assoc opts :return-keys true))))

(defn delete!
  ([ds table params] (delete! ds table params nil))
  ([ds table params opts]
   (exec-one! ds
              (sql/delete table params opts)
              (assoc opts :return-keys true))))

(defn- is-deleted?
  [{:keys [deleted-at]}]
  (and (dt/instant? deleted-at)
       (< (inst-ms deleted-at)
          (inst-ms (dt/now)))))

(defn get-by-params
  ([ds table params]
   (get-by-params ds table params nil))
  ([ds table params {:keys [uncheked] :or {uncheked false} :as opts}]
   (let [res (exec-one! ds (sql/select table params opts))]
     (when (and (not uncheked) (or (not res) (is-deleted? res)))
       (ex/raise :type :not-found
                 :table table
                 :hint "database object not found"))
     res)))

(defn get-by-id
  ([ds table id]
   (get-by-params ds table {:id id} nil))
  ([ds table id opts]
   (get-by-params ds table {:id id} opts)))

(defn query
  ([ds table params]
   (query ds table params nil))
  ([ds table params opts]
   (exec! ds (sql/select table params opts))))

(defn pgobject?
  ([v]
   (instance? PGobject v))
  ([v type]
   (and (instance? PGobject v)
        (= type (.getType ^PGobject v)))))

(defn pginterval?
  [v]
  (instance? PGInterval v))

(defn pgpoint?
  [v]
  (instance? PGpoint v))

(defn pgarray?
  ([v] (instance? PgArray v))
  ([v type]
   (and (instance? PgArray v)
        (= type (.getBaseTypeName ^PgArray v)))))

(defn pgarray-of-uuid?
  [v]
  (and (pgarray? v) (= "uuid" (.getBaseTypeName ^PgArray v))))

(defn decode-pgarray
  ([v] (into [] (.getArray ^PgArray v)))
  ([v in] (into in (.getArray ^PgArray v)))
  ([v in xf] (into in xf (.getArray ^PgArray v))))

(defn pgarray->set
  [v]
  (set (.getArray ^PgArray v)))

(defn pgarray->vector
  [v]
  (vec (.getArray ^PgArray v)))

(defn pgpoint
  [p]
  (PGpoint. (:x p) (:y p)))

(defn create-array
  [conn type objects]
  (let [^PGConnection conn (unwrap conn org.postgresql.PGConnection)]
    (if (coll? objects)
      (.createArrayOf conn ^String type (into-array Object objects))
      (.createArrayOf conn ^String type objects))))


(defn decode-pgpoint
  [^PGpoint v]
  (gpt/point (.-x v) (.-y v)))

(defn pginterval
  [data]
  (org.postgresql.util.PGInterval. ^String data))

(defn savepoint
  ([^Connection conn]
   (.setSavepoint conn))
  ([^Connection conn label]
   (.setSavepoint conn (name label))))

(defn rollback!
  ([^Connection conn]
   (.rollback conn))
  ([^Connection conn ^Savepoint sp]
   (.rollback conn sp)))

(defn interval
  [data]
  (cond
    (integer? data)
    (->> (/ data 1000.0)
         (format "%s seconds")
         (pginterval))

    (string? data)
    (pginterval data)

    (dt/duration? data)
    (->> (/ (.toMillis ^java.time.Duration data) 1000.0)
         (format "%s seconds")
         (pginterval))

    :else
    (ex/raise :type :not-implemented)))

(defn decode-json-pgobject
  [^PGobject o]
  (let [typ (.getType o)
        val (.getValue o)]
    (if (or (= typ "json")
            (= typ "jsonb"))
      (json/decode-str val)
      val)))

(defn decode-transit-pgobject
  [^PGobject o]
  (let [typ (.getType o)
        val (.getValue o)]
    (if (or (= typ "json")
            (= typ "jsonb"))
      (t/decode-str val)
      val)))

(defn inet
  [ip-addr]
  (doto (org.postgresql.util.PGobject.)
    (.setType "inet")
    (.setValue (str ip-addr))))

(defn decode-inet
  [^PGobject o]
  (if (= "inet" (.getType o))
    (.getValue o)
    nil))

(defn tjson
  "Encode as transit json."
  [data]
  (doto (org.postgresql.util.PGobject.)
    (.setType "jsonb")
    (.setValue (t/encode-str data {:type :json-verbose}))))

(defn json
  "Encode as plain json."
  [data]
  (doto (org.postgresql.util.PGobject.)
    (.setType "jsonb")
    (.setValue (json/encode-str data))))

;; --- Locks

(defn- xact-check-param
  [n]
  (cond
    (uuid? n) (uuid/get-word-high n)
    (int? n)  n
    :else (throw (IllegalArgumentException. "uuid or number allowed"))))

(defn xact-lock!
  [conn n]
  (let [n (xact-check-param n)]
    (exec-one! conn ["select pg_advisory_xact_lock(?::bigint) as lock" n])
    true))

(defn xact-try-lock!
  [conn n]
  (let [n   (xact-check-param n)
        row (exec-one! conn ["select pg_try_advisory_xact_lock(?::bigint) as lock" n])]
    (:lock row)))
