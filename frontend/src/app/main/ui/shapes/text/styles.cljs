;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.shapes.text.styles
  (:require
   [app.common.data :as d]
   [app.common.text :as txt]
   [app.main.fonts :as fonts]
   [app.util.color :as uc]
   [app.util.object :as obj]
   [cuerdas.core :as str]))

(defn generate-root-styles
  [shape node]
  (let [valign (:vertical-align node "top")
        base   #js {:height (or (:height shape) "100%")
                    :width  (or (:width shape) "100%")}]
    (cond-> base
      (= valign "top")     (obj/set! "justifyContent" "flex-start")
      (= valign "center")  (obj/set! "justifyContent" "center")
      (= valign "bottom")  (obj/set! "justifyContent" "flex-end"))))

(defn generate-paragraph-set-styles
  [{:keys [grow-type] :as shape}]
  ;; This element will control the auto-width/auto-height size for the
  ;; shape. The properties try to adjust to the shape and "overflow" if
  ;; the shape is not big enough.
  ;; We `inherit` the property `justify-content` so it's set by the root where
  ;; the property it's known.
  ;; `inline-flex` is similar to flex but `overflows` outside the bounds of the
  ;; parent
  (let [auto-width?  (= grow-type :auto-width)
        auto-height? (= grow-type :auto-height)]
    #js {:display "inline-flex"
         :flexDirection "column"
         :justifyContent "inherit"
         :minHeight (when-not (or auto-width? auto-height?) "100%")
         :minWidth (when-not auto-width? "100%")
         :verticalAlign "top"}))

(defn generate-paragraph-styles
  [shape data]
  (let [line-height (:line-height data)
        text-align  (:text-align data "start")
        grow-type   (:grow-type shape)

        base        #js {:fontSize (str (:font-size data (:font-size txt/default-text-attrs)) "px")
                         :lineHeight (:line-height data (:line-height txt/default-text-attrs))
                         :margin "inherit"}]
    (cond-> base
      (some? line-height)       (obj/set! "lineHeight" line-height)
      (some? text-align)        (obj/set! "textAlign" text-align)
      (= grow-type :auto-width) (obj/set! "whiteSpace" "pre"))))

(defn generate-text-styles
  [data]
  (let [letter-spacing  (:letter-spacing data)
        text-decoration (:text-decoration data)
        text-transform  (:text-transform data)
        line-height     (:line-height data)

        font-id         (:font-id data (:font-id txt/default-text-attrs))
        font-variant-id (:font-variant-id data)

        font-size       (:font-size data)
        fill-color      (:fill-color data)
        fill-opacity    (:fill-opacity data)

        ;; Uncomment this to allow to remove text colors. This could break the texts that already exist
        ;;[r g b a] (if (nil? fill-color)
        ;;            [0 0 0 0] ;; Transparent color
        ;;            (uc/hex->rgba fill-color fill-opacity))

        [r g b a]       (uc/hex->rgba fill-color fill-opacity)
        text-color      (when (and (some? fill-color) (some? fill-opacity))
                          (str/format "rgba(%s, %s, %s, %s)" r g b a))
        fontsdb         (deref fonts/fontsdb)

        base            #js {:textDecoration text-decoration
                             :textTransform text-transform
                             :lineHeight (or line-height "inherit")
                             :color text-color}]

    (when-let [gradient (:fill-color-gradient data)]
      (let [text-color (-> (update gradient :type keyword)
                           (uc/gradient->css))]
        (-> base
            (obj/set! "--text-color" text-color)
            (obj/set! "backgroundImage" "var(--text-color)")
            (obj/set! "WebkitTextFillColor" "transparent")
            (obj/set! "WebkitBackgroundClip" "text"))))

    (when (and (string? letter-spacing)
               (pos? (alength letter-spacing)))
      (obj/set! base "letterSpacing" (str letter-spacing "px")))

    (when (and (string? font-size)
               (pos? (alength font-size)))
      (obj/set! base "fontSize" (str font-size "px")))

    (when (and (string? font-id)
               (pos? (alength font-id)))
      (fonts/ensure-loaded! font-id)
      (let [font         (get fontsdb font-id)
            font-family  (str/quote
                          (or (:family font)
                              (:font-family data)))
            font-variant (d/seek #(= font-variant-id (:id %))
                                 (:variants font))
            font-style   (or (:style font-variant)
                             (:font-style data))
            font-weight  (or (:weight font-variant)
                             (:font-weight data))]
        (obj/set! base "fontFamily" font-family)
        (obj/set! base "fontStyle" font-style)
        (obj/set! base "fontWeight" font-weight)))

    base))
