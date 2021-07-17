;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.text
  (:require
   [app.common.data :as d]
   [app.common.transit :as t]
   [clojure.walk :as walk]
   [cuerdas.core :as str]))

(def default-text-attrs
  {:typography-ref-file nil
   :typography-ref-id nil
   :font-id "sourcesanspro"
   :font-family "sourcesanspro"
   :font-variant-id "regular"
   :font-size "14"
   :font-weight "400"
   :font-style "normal"
   :line-height "1.2"
   :letter-spacing "0"
   :text-transform "none"
   :text-align "left"
   :text-decoration "none"
   :fill-color nil
   :fill-opacity 1})

(def typography-fields
  [:font-id
   :font-family
   :font-variant-id
   :font-size
   :font-weight
   :font-style
   :line-height
   :letter-spacing
   :text-transform])

(def default-typography
  (merge
   {:name "Source Sans Pro Regular"}
   (select-keys default-text-attrs typography-fields)))

(defn transform-nodes
  ([transform root]
   (transform-nodes identity transform root))
  ([pred transform root]
   (walk/postwalk
    (fn [item]
      (if (and (map? item) (pred item))
        (transform item)
        item))
    root)))

(defn node-seq
  ([root] (node-seq identity root))
  ([match? root]
   (->> (tree-seq map? :children root)
        (filter match?)
        (seq))))

(defn ^boolean is-text-node?
  [node]
  (and (string? (:text node))
       (not= (:text node) "")))

(defn ^boolean is-paragraph-node?
  [node]
  (= "paragraph" (:type node)))

(defn ^boolean is-root-node?
  [node]
  (= "root" (:type node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DraftJS <-> Penpot Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn encode-style-value
  [v]
  (t/encode-str v))

(defn decode-style-value
  [v]
  (t/decode-str v))

(defn encode-style
  [key val]
  (let [k (d/name key)
        v (encode-style-value val)]
    (str "PENPOT$$$" k "$$$" v)))

(defn decode-style
  [style]
  (let [[_ k v] (str/split style "$$$" 3)]
    [(keyword k) (decode-style-value v)]))

(defn attrs-to-styles
  [attrs]
  (reduce-kv (fn [res k v]
               (conj res (encode-style k v)))
             #{}
             attrs))

(defn styles-to-attrs
  [styles]
  (persistent!
   (reduce (fn [result style]
             (if (str/starts-with? style "PENPOT")
               (if (= style "PENPOT_SELECTION")
                 (assoc! result :penpot-selection true)
                 (let [[_ k v] (str/split style "$$$" 3)]
                   (assoc! result (keyword k) (decode-style-value v))))
               result))
           (transient {})
           (seq styles))))

(defn- parse-draft-styles
  "Parses draft-js style ranges, converting encoded style name into a
  key/val pair of data."
  [styles]
  (->> styles
       (filter #(str/starts-with? (get % :style) "PENPOT$$$"))
       (map (fn [item]
              (let [[_ k v] (-> (get item :style)
                                (str/split "$$$" 3))]
                {:key (keyword k)
                 :val (decode-style-value v)
                 :offset (get item :offset)
                 :length (get item :length)})))))

(defn- build-style-index
  "Generates a character based index with associated styles map."
  [length ranges]
  (loop [result (->> (range length)
                     (mapv (constantly {}))
                     (transient))
         ranges (seq ranges)]
    (if-let [{:keys [offset length] :as item} (first ranges)]
      (recur (reduce (fn [result index]
                       (let [prev (get result index)]
                         (assoc! result index (assoc prev (:key item) (:val item)))))
                     result
                     (range offset (+ offset length)))
             (rest ranges))
      (persistent! result))))

(defn- text->code-points
  [text]
  #?(:cljs (into [] (js/Array.from text))
     :clj  (into [] (iterator-seq (.iterator (.codePoints ^String text))))))

(defn- code-points->text
  [cpoints start end]
  #?(:cljs (apply str (subvec cpoints start end))
     :clj  (let [sb (StringBuilder. (- end start))]
             (run! #(.appendCodePoint sb (int %)) (subvec cpoints start end))
             (.toString sb))))

(defn convert-from-draft
  [content]
  (letfn [(extract-text [cpoints part]
            (let [start (ffirst part)
                  end   (inc (first (last part)))
                  text  (code-points->text cpoints start end)
                  attrs (second (first part))]
              (assoc attrs :text text)))

          (split-texts [text styles]
            (let [cpoints  (text->code-points text)
                  children (->> (parse-draft-styles styles)
                                (build-style-index (count cpoints))
                                (d/enumerate)
                                (partition-by second)
                                (mapv #(extract-text cpoints %)))]
              (cond-> children
                (empty? children)
                (conj {:text ""}))))

          (build-paragraph [block]
            (let [key    (get block :key)
                  text   (get block :text)
                  styles (get block :inlineStyleRanges)
                  data   (get block :data)]
              (-> data
                  (assoc :key key)
                  (assoc :type "paragraph")
                  (assoc :children (split-texts text styles)))))]

    {:type "root"
     :children
     [{:type "paragraph-set"
       :children (->> (get content :blocks)
                      (mapv build-paragraph))}]}))

(defn convert-to-draft
  [root]
  (letfn [(process-attr [children ranges [k v]]
            (loop [children (seq children)
                   start    nil
                   offset   0
                   ranges   ranges]
              (if-let [{:keys [text] :as item} (first children)]
                (let [cpoints (text->code-points text)]
                  (if (= v (get item k ::novalue))
                    (recur (rest children)
                           (if (nil? start) offset start)
                           (+ offset (count cpoints))
                           ranges)
                    (if (some? start)
                      (recur (rest children)
                             nil
                             (+ offset (count cpoints))
                             (conj! ranges {:offset start
                                            :length (- offset start)
                                            :style (encode-style k v)}))
                      (recur (rest children)
                             start
                             (+ offset (count cpoints))
                             ranges))))
                (cond-> ranges
                  (some? start)
                  (conj! {:offset start
                          :length (- offset start)
                          :style (encode-style k v)})))))

          (calc-ranges [{:keys [children] :as blok}]
            (let [xform (comp (map #(dissoc % :key :text))
                              (remove empty?)
                              (mapcat vec)
                              (distinct))
                  f  #(process-attr children %1 %2)]
              (persistent!
               (transduce xform (completing f) (transient []) children))))

          (build-block [{:keys [key children] :as paragraph}]
            {:key key
             :depth 0
             :text (apply str (map :text children))
             :data (dissoc paragraph :key :children :type)
             :type "unstyled"
             :entityRanges []
             :inlineStyleRanges (calc-ranges paragraph)})]

    {:blocks (reduce #(conj %1 (build-block %2)) [] (node-seq #(= (:type %) "paragraph") root))
     :entityMap {}}))



