;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.workspace.sidebar.options.menus.typography
  (:require
   ["react-virtualized" :as rvt]
   [app.common.data :as d]
   [app.common.exceptions :as ex]
   [app.common.pages :as cp]
   [app.common.text :as txt]
   [app.main.data.shortcuts :as dsc]
   [app.main.fonts :as fonts]
   [app.main.store :as st]
   [app.main.ui.components.editable-select :refer [editable-select]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.options.common :refer [advanced-options]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]
   [app.util.router :as rt]
   [app.util.timers :as tm]
   [cuerdas.core :as str]
   [goog.events :as events]
   [rumext.alpha :as mf]))

(defn- attr->string [value]
  (if (= value :multiple)
    ""
    (str value)))

(defn- get-next-font
  [{:keys [id] :as current} fonts]
  (if (seq fonts)
    (let [index (d/index-of-pred fonts #(= (:id %) id))
          index (or index -1)
          next  (ex/ignoring (nth fonts (inc index)))]
      (or next (first fonts)))
    current))

(defn- get-prev-font
  [{:keys [id] :as current} fonts]
  (if (seq fonts)
    (let [index (d/index-of-pred fonts #(= (:id %) id))
          next  (ex/ignoring (nth fonts (dec index)))]
      (or next (peek fonts)))
    current))

(mf/defc font-item
  {::mf/wrap [mf/memo]}
  [{:keys [font current? on-click style]}]
  (let [item-ref (mf/use-ref)
        on-click (mf/use-callback (mf/deps font) #(on-click font))]

    (mf/use-effect
     (mf/deps current?)
     (fn []
       (when current?
         (let [element (mf/ref-val item-ref)]
           (when-not (dom/is-in-viewport? element)
             (dom/scroll-into-view! element))))))

    [:div.font-item {:ref item-ref
                     :style style
                     :class (when current? "selected")
                     :on-click on-click}
     [:span.icon (when current? i/tick)]
     [:span.label (:name font)]]))

(declare row-renderer)

(defn filter-fonts
  [{:keys [term backends]} fonts]
  (let [xform (cond-> (map identity)
                (seq term)
                (comp (filter #(str/includes? (str/lower (:name %)) term)))

                (seq backends)
                (comp (filter #(contains? backends (:backend %)))))]
    (into [] xform fonts)))

;; (defn- toggle-backend
;;   [backends id]
;;   (if (contains? backends id)
;;     (disj backends id)
;;     (conj backends id)))

(mf/defc font-selector
  [{:keys [on-select on-close current-font] :as props}]
  (let [selected (mf/use-state current-font)
        state    (mf/use-state {:term "" :backends #{}})

        flist    (mf/use-ref)
        input    (mf/use-ref)

        fonts    (mf/use-memo (mf/deps @state) #(filter-fonts @state @fonts/fonts))

        select-next
        (mf/use-callback
         (mf/deps fonts)
         (fn [event]
           (dom/stop-propagation event)
           (dom/prevent-default event)
           (swap! selected get-next-font fonts)))

        select-prev
        (mf/use-callback
         (mf/deps fonts)
         (fn [event]
           (dom/stop-propagation event)
           (dom/prevent-default event)
           (swap! selected get-prev-font fonts)))

        on-key-down
        (mf/use-callback
         (mf/deps fonts)
         (fn [event]
           (cond
             (kbd/up-arrow? event)   (select-prev event)
             (kbd/down-arrow? event) (select-next event)
             (kbd/esc? event)        (on-close)
             (kbd/enter? event)      (on-close)
             :else                   (dom/focus! (mf/ref-val input)))))

        on-filter-change
        (mf/use-callback
         (mf/deps)
         (fn [event]
           (let [value (dom/get-target-val event)]
             (swap! state assoc :term value))))

        on-select-and-close
        (mf/use-callback
         (mf/deps on-select on-close)
         (fn [font]
           (on-select font)
           (on-close)))
        ]

    (mf/use-effect
     (mf/deps fonts)
     (fn []
       (let [key (events/listen js/document "keydown" on-key-down)]
         #(events/unlistenByKey key))))

    (mf/use-effect
     (mf/deps @selected)
     (fn []
       (when-let [inst (mf/ref-val flist)]
         (when-let [index (:index @selected)]
           (.scrollToRow ^js inst index)))))

    (mf/use-effect
     (mf/deps @selected)
     (fn []
       (on-select @selected)))

    (mf/use-effect
     (fn []
       (st/emit! (dsc/push-shortcuts :typography {}))
       (fn []
         (st/emit! (dsc/pop-shortcuts :typography)))))

    (mf/use-effect
     (fn []
       (let [index  (d/index-of-pred fonts #(= (:id %) (:id current-font)))
             inst   (mf/ref-val flist)]
         (tm/schedule
          #(let [offset (.getOffsetForRow ^js inst #js {:alignment "center" :index index})]
             (.scrollToPosition ^js inst offset))))))

    [:div.font-selector
     [:div.font-selector-dropdown
      [:header
       [:input {:placeholder "Search font"
                :value (:term @state)
                :ref input
                :spell-check false
                :on-change on-filter-change}]

       #_[:div.options
          {:on-click #(swap! state assoc :show-options true)
           :class (when (seq (:backends @state)) "active")}
          i/picker-hsv]

       #_[:& dropdown {:show (:show-options @state false)
                       :on-close #(swap! state dissoc :show-options)}
          (let [backends (:backends @state)]
            [:div.backend-filters.dropdown {:ref ddown}
             [:div.backend-filter
              {:class (when (backends :custom) "selected")
               :on-click #(swap! state update :backends toggle-backend :custom)}
              [:div.checkbox-icon i/tick]
              [:div.backend-name (tr "labels.custom-fonts")]]
             [:div.backend-filter
              {:class (when (backends :google) "selected")
               :on-click #(swap! state update :backends toggle-backend :google)}
              [:div.checkbox-icon i/tick]
              [:div.backend-name "Google Fonts"]]])]]

      [:hr]

      [:div.fonts-list
       [:> rvt/AutoSizer {}
        (fn [props]
          (let [width  (obj/get props "width")
                height (obj/get props "height")
                render #(row-renderer fonts @selected on-select-and-close %)]
            (mf/html
             [:> rvt/List #js {:height height
                               :ref flist
                               :width width
                               :rowCount (count fonts)
                               :rowHeight 32
                               :rowRenderer render}])))]]]]))
(defn row-renderer
  [fonts selected on-select props]
  (let [index (obj/get props "index")
        key   (obj/get props "key")
        style (obj/get props "style")
        font  (nth fonts index)]
    (mf/html
     [:& font-item {:key key
                    :font font
                    :style style
                    :on-click on-select
                    :current? (= (:id font) (:id selected))}])))

(mf/defc font-options
  [{:keys [values on-change on-blur] :as props}]
  (let [{:keys [font-id font-size font-variant-id]} values

        font-id         (or font-id (:font-id txt/default-text-attrs))
        font-size       (or font-size (:font-size txt/default-text-attrs))
        font-variant-id (or font-variant-id (:font-variant-id txt/default-text-attrs))

        fonts           (mf/deref fonts/fontsdb)
        font            (get fonts font-id)

        open-selector?  (mf/use-state false)

        change-font
        (mf/use-callback
         (mf/deps on-change fonts)
         (fn [new-font-id]
           (let [{:keys [family] :as font} (get fonts new-font-id)
                 {:keys [id name weight style]} (fonts/get-default-variant font)]
             (on-change {:font-id new-font-id
                         :font-family family
                         :font-variant-id (or id name)
                         :font-weight weight
                         :font-style style}))))

        on-font-size-change
        (mf/use-callback
         (mf/deps on-change)
         (fn [new-font-size]
           (when-not (str/empty? new-font-size)
             (on-change {:font-size (str new-font-size)}))))

        on-font-variant-change
        (mf/use-callback
         (mf/deps font on-change)
         (fn [event]
           (let [new-variant-id (dom/get-target-val event)
                 variant (d/seek #(= new-variant-id (:id %)) (:variants font))]
             (on-change {:font-id (:id font)
                         :font-family (:family font)
                         :font-variant-id new-variant-id
                         :font-weight (:weight variant)
                         :font-style (:style variant)})
             (dom/blur! (dom/get-target event)))))

        on-font-select
        (mf/use-callback
         (mf/deps change-font)
         (fn [font*]
           (when (not= font font*)
             (change-font (:id font*)))

           (when (some? on-blur)
             (on-blur))))

        on-font-selector-close
        (mf/use-callback
         (fn []
           (reset! open-selector? false)
           (when (some? on-blur)
             (on-blur))
           ))]

    [:*
     (when @open-selector?
       [:& font-selector
        {:current-font font
         :on-close on-font-selector-close
         :on-select on-font-select}])

     [:div.row-flex
      [:div.input-select.font-option
       {:on-click #(reset! open-selector? true)}
       (cond
         (= :multiple font-id)
         "--"

         (some? font)
         (:name font)

         :else
         (tr "dashboard.fonts.deleted-placeholder"))]]


     [:div.row-flex
      (let [size-options [8 9 10 11 12 14 18 24 36 48 72]
            size-options (if (= font-size :multiple) (into [""] size-options) size-options)]
        [:& editable-select
         {:value (attr->string font-size)
          :class "input-option size-option"
          :options size-options
          :type "number"
          :placeholder "--"
          :on-change on-font-size-change
          :on-blur on-blur}])

      [:select.input-select.variant-option
       {:disabled (= font-id :multiple)
        :value (attr->string font-variant-id)
        :on-change on-font-variant-change
        :on-blur on-blur}
       (when (or (= font-id :multiple) (= font-variant-id :multiple))
         [:option {:value ""} "--"])
       (for [variant (:variants font)]
         [:option {:value (:id variant)
                   :key (pr-str variant)}
          (:name variant)])]]]))


(mf/defc spacing-options
  [{:keys [values on-change on-blur] :as props}]
  (let [{:keys [line-height
                letter-spacing]} values

        line-height (or line-height "1.2")
        letter-spacing (or letter-spacing "0")

        handle-change
        (fn [event attr]
          (let [new-spacing (dom/get-target-val event)]
            (on-change {attr new-spacing})))]

    [:div.spacing-options
     [:div.input-icon
      [:span.icon-before.tooltip.tooltip-bottom
       {:alt (tr "workspace.options.text-options.line-height")}
       i/line-height]
      [:input.input-text
       {:type "number"
        :step "0.1"
        :min "0"
        :max "200"
        :value (attr->string line-height)
        :placeholder (tr "settings.multiple")
        :on-change #(handle-change % :line-height)
        :on-blur on-blur}]]

     [:div.input-icon
      [:span.icon-before.tooltip.tooltip-bottom
       {:alt (tr "workspace.options.text-options.letter-spacing")}
       i/letter-spacing]
      [:input.input-text
       {:type "number"
        :step "0.1"
        :min "0"
        :max "200"
        :value (attr->string letter-spacing)
        :placeholder (tr "settings.multiple")
        :on-change #(handle-change % :letter-spacing)
        :on-blur on-blur}]]]))

(mf/defc text-transform-options
  [{:keys [values on-change on-blur] :as props}]
  (let [text-transform (or (:text-transform values) "none")
        handle-change
        (fn [_ type]
          (on-change {:text-transform type})
          (when (some? on-blur) (on-blur)))]
    [:div.align-icons
     [:span.tooltip.tooltip-bottom
      {:alt (tr "workspace.options.text-options.none")
       :class (dom/classnames :current (= "none" text-transform))
       :focus #(dom/prevent-default %)
       :on-click #(handle-change % "none")}
      i/minus]
     [:span.tooltip.tooltip-bottom
      {:alt (tr "workspace.options.text-options.uppercase")
       :class (dom/classnames :current (= "uppercase" text-transform))
       :on-click #(handle-change % "uppercase")}
      i/uppercase]
     [:span.tooltip.tooltip-bottom
      {:alt (tr "workspace.options.text-options.lowercase")
       :class (dom/classnames :current (= "lowercase" text-transform))
       :on-click #(handle-change % "lowercase")}
      i/lowercase]
     [:span.tooltip.tooltip-bottom
      {:alt (tr "workspace.options.text-options.titlecase")
       :class (dom/classnames :current (= "capitalize" text-transform))
       :on-click #(handle-change % "capitalize")}
      i/titlecase]]))

(mf/defc typography-options
  [{:keys [ids editor values on-change on-blur]}]
  (let [opts #js {:editor editor
                  :ids ids
                  :values values
                  :on-change on-change
                  :on-blur on-blur}]
    [:div.element-set-content
     [:> font-options opts]
     [:div.row-flex
      [:> spacing-options opts]]
     [:div.row-flex
      [:> text-transform-options opts]]]))


;; TODO: this need to be refactored, right now combines too much logic
;; and has a dropdown that behaves like a modal but is not a modal.
;; In summary, this need to a good UX/UI/IMPL rework.

(mf/defc typography-entry
  [{:keys [typography read-only? selected? on-click on-change on-detach on-context-menu editting? focus-name? file]}]
  (let [open?          (mf/use-state editting?)
        hover-detach   (mf/use-state false)
        name-input-ref (mf/use-ref)

        on-name-blur
        (fn [event]
          (let [content (dom/get-target-val event)]
            (when-not (str/blank? content)
              (on-change {:name content}))))

        handle-go-to-edit
        (fn []
          (let [pparams {:project-id (:project-id file)
                         :file-id (:id file)}]
            (st/emit! (rt/nav :workspace pparams))))]

    (mf/use-effect
     (mf/deps editting?)
     (fn []
       (when editting?
         (reset! open? editting?))))

    (mf/use-effect
     (mf/deps focus-name?)
     (fn []
       (when focus-name?
         (tm/schedule
          #(when-let [node (mf/ref-val name-input-ref)]
             (dom/focus! node)
             (dom/select-text! node))))))

    [:*
     [:div.element-set-options-group.typography-entry
      {:class (when selected? "selected")
       :style {:display (when @open? "none")}}
      [:div.typography-selection-wrapper
       {:class (when on-click "is-selectable")
        :on-click on-click
        :on-context-menu on-context-menu}
       [:div.typography-sample
        {:style {:font-family (:font-family typography)
                 :font-weight (:font-weight typography)
                 :font-style (:font-style typography)}}
        (tr "workspace.assets.typography.sample")]
       [:div.typography-name (:name typography)]]
      [:div.element-set-actions
       (when on-detach
         [:div.element-set-actions-button
          {:on-mouse-enter #(reset! hover-detach true)
           :on-mouse-leave #(reset! hover-detach false)
           :on-click on-detach}
          (if @hover-detach i/unchain i/chain)])

       [:div.element-set-actions-button
        {:on-click #(reset! open? true)}
        i/actions]]]

     [:& advanced-options {:visible? @open?
                           :on-close #(reset! open? false)}
      (if read-only?
        [:div.element-set-content.typography-read-only-data
         [:div.row-flex.typography-name
          [:span (:name typography)]]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.font-id")]
          [:span (:font-id typography)]]

         [:div.element-set-actions-button.actions-inside
          {:on-click #(reset! open? false)}
          i/actions]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.font-variant-id")]
          [:span (:font-variant-id typography)]]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.font-size")]
          [:span (:font-size typography)]]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.line-height")]
          [:span (:line-height typography)]]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.letter-spacing")]
          [:span (:letter-spacing typography)]]

         [:div.row-flex
          [:span.label (tr "workspace.assets.typography.text-transform")]
          [:span (:text-transform typography)]]

         [:div.go-to-lib-button
          {:on-click handle-go-to-edit}
          (tr "workspace.assets.typography.go-to-edit")]]

        [:*
         [:div.element-set-content
          [:div.row-flex
           [:input.element-name.adv-typography-name
            {:type "text"
             :ref name-input-ref
             :default-value (cp/merge-path-item (:path typography) (:name typography))
             :on-blur on-name-blur}]

             [:div.element-set-actions-button
              {:on-click #(reset! open? false)}
             i/actions]]]

         [:& typography-options {:values typography
                                 :on-change on-change}]])]]))
