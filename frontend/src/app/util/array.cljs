;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.util.array
  "A collection of helpers for work with javascript arrays."
  (:refer-clojure :exclude [conj!]))

(defn conj!
  "A conj like function for js arrays."
  [a v]
  (.push ^js a v)
  a)
