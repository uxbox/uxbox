;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.util.dom
  (:require
    [app.common.exceptions :as ex]
    [app.common.geom.point :as gpt]
    [app.util.globals :as globals]
    [app.util.object :as obj]
    [cuerdas.core :as str]
    [goog.dom :as dom]
    [promesa.core :as p]))

;; --- Deprecated methods

(defn event->inner-text
  [e]
  (.-innerText (.-target e)))

(defn event->value
  [e]
  (.-value (.-target e)))

(defn event->target
  [e]
  (.-target e))

(defn classnames
  [& params]
  (assert (even? (count params)))
  (str/join " " (reduce (fn [acc [k v]]
                          (if (true? (boolean v))
                            (conj acc (name k))
                            acc))
                        []
                        (partition 2 params))))


;; --- New methods

(defn set-html-title
  [title]
  (set! (.-title globals/document) title))

(defn set-page-style
  [style]
  (let [head (first (.getElementsByTagName ^js globals/document "head"))
        style-str (str/join "\n"
                            (map (fn [[k v]]
                                   (str (name k) ": " v ";"))
                                 style))]
    (.insertAdjacentHTML head "beforeend"
                         (str "<style>"
                              "  @page {" style-str "}"
                              "</style>"))))

(defn get-element-by-class
  ([classname]
   (dom/getElementByClass classname))
  ([classname node]
   (dom/getElementByClass classname node)))

(defn get-element
  [id]
  (dom/getElement id))

(defn get-elements-by-tag
  [node tag]
  (.getElementsByTagName node tag))

(defn stop-propagation
  [e]
  (when e
    (.stopPropagation e)))

(defn prevent-default
  [e]
  (when e
    (.preventDefault e)))

(defn get-target
  "Extract the target from event instance."
  [event]
  (.-target event))

(defn get-parent
  [dom]
  (.-parentElement ^js dom))

(defn get-value
  "Extract the value from dom node."
  [node]
  (.-value node))

(defn get-attribute
  "Extract the value of one attribute of a dom node."
  [node attr-name]
  (.getAttribute node attr-name))

(def get-target-val (comp get-value get-target))

(defn click
  "Click a node"
  [node]
  (.click node))

(defn get-files
  "Extract the files from dom node."
  [node]
  (array-seq (.-files node)))

(defn checked?
  "Check if the node that represents a radio
  or checkbox is checked or not."
  [node]
  (.-checked node))

(defn valid?
  "Check if the node that is a form input
  has a valid value, against html5 form validation
  properties (required, min/max, pattern...)."
  [node]
  (.-valid (.-validity node)))

(defn set-validity!
  "Manually set the validity status of a node that
  is a form input. If the state is an empty string,
  the input will be valid. If not, the string will
  be set as the error message."
  [node status]
  (.setCustomValidity node status)
  (.reportValidity node))

(defn clean-value!
  [node]
  (set! (.-value node) ""))

(defn set-value!
  [node value]
  (set! (.-value ^js node) value))

(defn select-text!
  [node]
  (.select ^js node))

(defn ^boolean equals?
  [node-a node-b]
  (.isEqualNode ^js node-a node-b))

(defn get-event-files
  "Extract the files from event instance."
  [event]
  (get-files (get-target event)))

(defn create-element
  ([tag]
   (.createElement globals/document tag))
  ([ns tag]
   (.createElementNS globals/document ns tag)))

(defn set-html!
  [el html]
  (set! (.-innerHTML el) html))

(defn append-child!
  [el child]
  (.appendChild el child))

(defn get-first-child
  [el]
  (.-firstChild el))

(defn get-tag-name
  [el]
  (.-tagName el))

(defn get-outer-html
  [el]
  (.-outerHTML el))

(defn get-inner-text
  [el]
  (.-innerText el))

(defn query
  [el query]
  (.querySelector el query))

(defn get-client-position
  [event]
  (let [x (.-clientX event)
        y (.-clientY event)]
    (gpt/point x y)))

(defn get-offset-position
  [event]
  (let [x (.-offsetX event)
        y (.-offsetY event)]
    (gpt/point x y)))

(defn get-client-size
  [node]
  {:width (.-clientWidth ^js node)
   :height (.-clientHeight ^js node)})

(defn get-bounding-rect
  [node]
  (let [rect (.getBoundingClientRect ^js node)]
    {:left (.-left ^js rect)
     :top (.-top ^js rect)
     :right (.-right ^js rect)
     :bottom (.-bottom ^js rect)
     :width (.-width ^js rect)
     :height (.-height ^js rect)}))

(defn get-window-size
  []
  {:width (.-innerWidth ^js js/window)
   :height (.-innerHeight ^js js/window)})

(defn focus!
  [node]
  (when (some? node)
    (.focus node)))

(defn blur!
  [node]
  (when (some? node)
    (.blur node)))

(defn fullscreen?
  []
  (cond
    (obj/in? globals/document "webkitFullscreenElement")
    (boolean (.-webkitFullscreenElement globals/document))

    (obj/in? globals/document "fullscreenElement")
    (boolean (.-fullscreenElement globals/document))

    :else
    (ex/raise :type :not-supported
              :hint "seems like the current browset does not support fullscreen api.")))

(defn ^boolean blob?
  [v]
  (instance? js/Blob v))

(defn create-blob
  "Create a blob from content."
  ([content]
   (create-blob content "application/octet-stream"))
  ([content mimetype]
   (js/Blob. #js [content] #js {:type mimetype})))

(defn revoke-uri
  [url]
  (js/URL.revokeObjectURL url))

(defn create-uri
  "Create a url from blob."
  [b]
  {:pre [(blob? b)]}
  (js/URL.createObjectURL b))

(defn set-property! [node property value]
  (.setAttribute node property value))

(defn set-text! [node text]
  (set! (.-textContent node) text))

(defn set-css-property [node property value]
  (.setProperty (.-style ^js node) property value))

(defn capture-pointer [event]
  (-> event get-target (.setPointerCapture (.-pointerId event))))

(defn release-pointer [event]
  (-> event get-target (.releasePointerCapture (.-pointerId event))))

(defn get-root []
  (query globals/document "#app"))

(defn ^boolean class? [node class-name]
  (let [class-list (.-classList ^js node)]
    (.contains ^js class-list class-name)))

(defn child? [node1 node2]
  (.contains ^js node2 ^js node1))

(defn get-user-agent []
  (.-userAgent globals/navigator))

(defn get-active []
  (.-activeElement globals/document))

(defn active? [node]
  (= (get-active) node))

(defn get-data [^js node ^string attr]
  (.getAttribute node (str "data-" attr)))

(defn mtype->extension [mtype]
  ;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
  (case mtype
    "image/apng"         "apng"
    "image/avif"         "avif"
    "image/gif"          "gif"
    "image/jpeg"         "jpg"
    "image/png"          "png"
    "image/svg+xml"      "svg"
    "image/webp"         "webp"
    "application/zip"    "zip"
    "application/penpot" "penpot"
    nil))

(defn set-attribute [^js node ^string attr value]
  (.setAttribute node attr value))

(defn remove-attribute [^js node ^string attr]
  (.removeAttribute node attr))

(defn scroll-into-view!
  ([element]
   (.scrollIntoView ^js element false))
  ([element scroll-top]
   (.scrollIntoView ^js element scroll-top)))

(defn is-in-viewport?
  [element]
  (let [rect   (.getBoundingClientRect element)
        height (or (.-innerHeight js/window)
                   (.. js/document -documentElement -clientHeight))
        width  (or (.-innerWidth js/window)
                   (.. js/document -documentElement -clientWidth))]
    (and (>= (.-top rect) 0)
         (>= (.-left rect) 0)
         (<= (.-bottom rect) height)
         (<= (.-right rect) width))))

(defn trigger-download-uri
  [filename mtype uri]
  (let [link (create-element "a")
        extension (mtype->extension mtype)
        filename (if extension
                   (str filename "." extension)
                   filename)]
    (obj/set! link "href" uri)
    (obj/set! link "download" filename)
    (obj/set! (.-style ^js link) "display" "none")
    (.appendChild (.-body ^js js/document) link)
    (.click link)
    (.remove link)))

(defn trigger-download
  [filename blob]
  (trigger-download-uri filename (.-type ^js blob) (create-uri blob)))

(defn save-as
  [uri filename mtype description]

  ;; Only chrome supports the save dialog
  (if (obj/contains? globals/window "showSaveFilePicker")
    (let [extension (mtype->extension mtype)
          opts {:suggestedName (str filename "." extension)
                :types [{:description description
                         :accept { mtype [(str "." extension)]}}]}]

      (-> (p/let [file-system (.showSaveFilePicker globals/window (clj->js opts))
                  writable    (.createWritable file-system)
                  response    (js/fetch uri)
                  blob        (.blob response)
                  _           (.write writable blob)]
            (.close writable))
          (p/catch
              #(when-not (and (= (type %) js/DOMException)
                              (= (.-name %) "AbortError"))
                 (trigger-download-uri filename mtype uri)))))

    (trigger-download-uri filename mtype uri)))

(defn left-mouse? [bevent]
  (let [event  (.-nativeEvent bevent)]
    (= 1 (.-which event))))
