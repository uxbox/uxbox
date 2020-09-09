;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.ui.shapes.image
  (:require
   [rumext.alpha :as mf]
   [app.config :as cfg]
   [app.common.geom.shapes :as geom]
   [app.main.ui.shapes.attrs :as attrs]
   [app.util.object :as obj]
   [app.main.data.fetch :as df]
   [promesa.core :as p]))

(mf/defc image-shape
  {::mf/wrap-props false}
  [props]

  (let [shape (unchecked-get props "shape")
        {:keys [id x y width height rotation metadata]} shape
        uri (cfg/resolve-media-path (:path metadata))
        data-uri (mf/use-state nil)]

    (mf/use-effect
     (mf/deps shape)
     (fn []
       (-> (df/fetch-as-data-uri uri)
           (p/then #(reset! data-uri (second %))))))

    (let [transform (geom/transform-matrix shape)
          props (-> (attrs/extract-style-attrs shape)
                    (obj/merge!
                     #js {:x x
                          :y y
                          :transform transform
                          :id (str "shape-" id)
                          :width width
                          :height height
                          :preserveAspectRatio "none"}))]
      (if (nil? @data-uri)
        [:> "rect" (obj/merge!
                    props
                    #js {:fill "#E8E9EA"
                         :stroke "#000000"})]
        [:> "image" (obj/merge!
                     props
                     #js {:xlinkHref @data-uri})]))
    

    ))
