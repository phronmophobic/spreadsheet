(ns com.phronemophobic.membrane.spreadsheet
  (:require [membrane.ui :as ui
             :refer [vertical-layout
                     horizontal-layout]]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            clojure.set
            [membrane.basic-components :as basic]
            [clojure.data.json :as json]
            [membrane.component :as com
             :refer [defui
                     defeffect]]
            [membrane.skia :as backend]
            [membrane.alpha.stretch :as stretch]
            ;; [membrane.java2d :as backend]
            [com.phronemophobic.viscous :as iv
             :refer [inspect]]
            [com.phronemophobic.membrane.schematic2 :as s2]
            [com.rpl.specter :as spec]
            [clojure.zip :as z]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [membrane.components.code-editor.code-editor :as code-editor]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [liq.buffer :as buffer]

            ;; xtdb.rocksdb
            ;; [xtdb.api :as xt]


            )
  (:import java.io.PushbackReader
           java.util.UUID
           java.time.Instant)
  (:gen-class))

(defn write-edn [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

(defn ->edn [obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false]
    (pr-str obj)))

(defmacro with-refs [ref-names & body]
  `(let ~(into
          []
          cat
          (for [nm ref-names
                :when (symbol? nm)]
            [nm nil]))
     ~@body))

(def ELEMENT-TREE-WALKER
  (spec/recursive-path [] p
                       (spec/stay-then-continue
                        [(spec/must :element/children) spec/ALL p])))

(defprotocol IResult
  (-re-eval [this form])
  (-error [this])
  (-success [this])
  (-val [this]))

(deftype AResult [re-eval error success]
  Object
  (hashCode [_] (System/identityHashCode (or error success)))

  IResult
  (-re-eval [_ form]
    (re-eval form))
  (-error [_]
    error)
  (-success [_]
    success)
  (-val [this]
    (or error success))

  iv/PWrapped
  (-unwrap [_]
    (or error success)
    )

  clojure.lang.IDeref
  (deref [this] (iv/-unwrap this)))

(defn wrap-result [re-eval error success]
  (->AResult re-eval error success))

(defn wrap-result-constant [err success]
  (let [v (atom nil)
        result (wrap-result (fn [& args]
                              @v)
                            err
                            success)]
    (reset! v result)
    result))

(defn genid
  ^java.util.UUID [] (java.util.UUID/randomUUID))

(def read-string-memo (memoize read-string))
(defmulti init-editor :editor)

(defn ->str [buf-or-s]
  (if (map? buf-or-s)
    (buffer/text buf-or-s)
    buf-or-s))

(defn ->buf [x]
  (buffer/buffer (pr-str x) {:mode :insert}))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt
   (+
    (Math/pow (- x2 x1) 2)
    (Math/pow (- y2 y1) 2))))

(defui wrap-drag-and-drop [{:keys [^:membrane.component/contextual
                                   drag
                                   drag-start
                                   drag-init
                                   body]}]
  (dnd/drag-and-drop
   {:$body nil
    :body
    (ui/on
     (ui/wrap-on
      :mouse-down
      (fn [handler pos]
        (let [intents (handler pos)]
          (if-let [intent (some #(when (= (first %) :start-drag)
                                   %)
                                (reverse intents))]
            (concat (remove #(= (first %) :start-drag)
                            intents)
                    [[:set $drag-start pos]
                     [:set $drag-init (second intent)]])
            intents)))
      :mouse-move
      (fn [handler pos]
        (let [intents (handler pos)]
          (if (and drag-start
                   (> (distance drag-start pos) 10))
            (concat intents
                    [[:delete $drag-start]
                     [:delete $drag-init]
                     [:set $drag drag-init]])
            ;; else
            intents)))
      :mouse-up
      (fn [handler pos]
        (if (or drag drag-start drag-init)
          (let [intents (handler pos)]
            (concat
             intents
             [[:set $drag nil]
              [:delete $drag-start]
              [:delete $drag-init]]))
          (handler pos)))
      body))})
  )

(defui wrap-resizing [{:keys [resizing?
                              width
                              height
                              body]}]
  (if-not resizing?
    body
    (ui/on
     :mouse-up
     (fn [_]
       [[:set $resizing? false]])
     :mouse-move-global
     (fn [[x y]]
       [[:set $width (max 1 (int (+ x 5)))]
        [:set $height (max 1 (int (+ y 5)))]])
     (ui/no-events
      [body
       (ui/with-color [0.2 0.2 0.2 0.2]
         (ui/with-style :membrane.ui/style-stroke
           (ui/rectangle width height)))
       ]))))

(defn wrap-properties [elem]
  (into elem
        (map (fn [[k v]]
               [k (try
                    (read-string-memo (->str v))
                    (catch Exception e
                      nil))]))
        (::properties elem)))


(defui radio-button [{:keys [text value selection]}]
  (ui/on
   :mouse-move (constantly nil)
   :mouse-move-global (constantly nil) 
   (basic/button {:hover? (= value selection)
                  :text text
                  :on-click
                  (fn []
                    [[:set $selection value]])})))

(defui move-tool [{:keys [src body]}]
  (let [elements (:elements src)
        mouse-down (:mouse-down src)
        mouse-temp (:mouse-temp src)
        selection (:selection src)]
    (ui/on
     :mouse-down
     (fn [pos]
       
       [[:set $mouse-down pos]])
     :mouse-move
     (fn [pos]
       (when mouse-down
         [[:set $mouse-temp pos]]))
     :mouse-up
     (fn [pos]
       [[:set $mouse-temp nil]
        [:set $mouse-down nil]
        (when selection
          [:update $elements
           (fn [elements]
             (let [ox (- (nth mouse-temp 0)
                         (nth mouse-down 0))
                   oy (- (nth mouse-temp 1)
                         (nth mouse-down 1))]
               (into []
                     (map (fn [elem]
                            (if (selection (:element/id elem))
                              (-> elem
                                  (update-in [::properties :element/x]
                                             (fn [x]
                                               (when x
                                                 (->buf (long (+ ox (read-string (->str x))))))))
                                  (update-in [::properties :element/y]
                                             (fn [y]
                                               (when y
                                                 (->buf (long (+ oy (read-string (->str y)))))))))
                              elem)))
                     elements))
             )])
        ])
     body)))

(defui add-tool [{:keys [src body new-element]}]
  (let [elements (:elements src)]
    (ui/on
     :mouse-down
     (fn [pos]
       [[:update $elements conj
         (merge (new-element pos)
                {:element/id (genid)})]])
     body)))

(defeffect ::group-selection [$elements result selection]
  (when (and result
             (-success result))
    (dispatch! :update
               $elements
               (fn [elements]
                 (let [selected? (fn [elem]
                                   (selection (:element/id elem)))
                       
                       select-pos (comp (filter selected?)
                                        (map wrap-properties)
                                        (map (fn [m]
                                               [(:element/id m)
                                                (into {}
                                                      (map (fn [k]
                                                             [k (-success (-re-eval result (get m k)))]))
                                                      [:element/x :element/y])])))

                       poss (into {}
                                  select-pos
                                  elements)

                       minx (transduce (map #(-> % val :element/x))
                                       min
                                       Long/MAX_VALUE
                                       poss)
                       miny (transduce (map #(-> % val :element/y))
                                       min
                                       Long/MAX_VALUE
                                       poss)
                       
                       group-elems (into []
                                         (comp (filter selected?)
                                               (map (fn [elem]
                                                      (let [eid (:element/id elem)
                                                            original-x (-> (get poss eid)
                                                                           :element/x)
                                                            original-y (-> (get poss eid)
                                                                           :element/y)]
                                                        
                                                        (update elem
                                                                ::properties
                                                                (fn [props]
                                                                  (assoc props
                                                                         :element/x (->buf (- original-x
                                                                                              minx))
                                                                         :element/y (->buf (- original-y
                                                                                              miny)))))))))
                                         elements)

                       
                       group #:element{:type :element/group
                                       :id (genid)
                                       :name "group_"
                                       :children group-elems

                                       ::properties
                                       #:element{
                                                 :for-bindings (->buf nil)
                                                 :layout (->buf nil) 
                                                 :x (->buf (long minx))
                                                 :y (->buf (long miny))
                                                 :fills (->buf [])}}
                       elements (into []
                                      (remove selected?)
                                      elements)]
                   (conj elements group))))))

(defeffect ::add-property [$properties $kw-str kw-str]
  (when-let [prop (try
                    (read-string kw-str)
                    (catch Exception e
                      nil))]
    (when (keyword? prop)
      (dispatch! :update $properties assoc prop (->buf nil))
      (dispatch! :set $kw-str ""))))

(defn new-label [[mx my]]
  (let [id (genid)]
    #:element{:type :element/label
              :id id
              :name "label"
              ::properties
              #:element{
                        :x (->buf (long mx))
                        :y (->buf (long my))
                        :fills (->buf [{}])
                        :font (->buf {:font/size 14})
                        :text (->buf "hello")}}))

(defn new-rect [[mx my]]
  (let [id (genid)]
    #:element{:type :element/shape
              :id id
              :name "rect"
              ::properties
              #:element{
                        :x (->buf (long mx))
                        :y (->buf (long my))
                        :width (->buf 15)
                        :height (->buf 20)
                        :fills (->buf [{}]) 
                        :corner-radius (->buf 3)}}))


(defn new-code [[mx my]]
  (let [id (genid)]
    #:element{:type :element/code
              :id id
              :name "code"
              ::properties
              #:element{
                        :x (->buf (long mx))
                        :y (->buf (long my))
                        :form (->buf '(ui/label ":)"))}}))

(defui wrap-tool [{:keys [width height tool src body]}]
  (basic/scrollview
   {:scroll-bounds [width height]
    :body
    (case tool

      :rect
      (add-tool {:src src
                 :body body
                 :new-element new-rect})

      :label
      (add-tool {:src src
                 :body body
                 :new-element new-label})

      :code
      (add-tool {:src src
                 :body body
                 :new-element new-code})

      :move
      (move-tool
       {:src src
        :body body})
      
      (add-tool {:src src
                 :body body
                 :new-element new-label}))}))


(defn zfind [zip pred]
  (loop [zip zip]
    (if (z/end? zip)
      nil
      (if (pred (z/node zip))
        zip
        (recur (z/next zip))))))

(defn element-zipper [elem]
  (z/zipper (constantly true)
            (fn [elem]
              (if (vector? elem)
                (seq elem)
                (seq (:element/children elem))))
            (fn [orig xs]
              (if (vector? orig)
                (vec xs)
                (assoc orig :element/children (vec xs))))
            elem))

(defeffect ::move-element-to-beginning [$elements eid]
  (dispatch!
   :update $elements
   (fn [elements]
    (let [zip (element-zipper elements)
          zelem (zfind zip #(= (:element/id %) eid))
          elem (z/node zelem)
          elements (-> zelem
                       z/remove
                       z/root)]
      (into [elem] elements)))))

(defeffect ::move-element-after [$elements eid target-id]
  (when (not= eid target-id)
    (dispatch!
     :update $elements
     (fn [elements]
       (let [zip (element-zipper elements)
             zelem (zfind zip #(= eid (:element/id %)))
             elem (z/node zelem)
             elements (-> zelem z/remove z/root)

             zip (element-zipper elements)
             ztarget (zfind zip #(= target-id (:element/id %)))
             ztarget (z/insert-right ztarget elem)]
         (z/root ztarget))))))

(defeffect ::move-element-to-beginning-of [$elements eid target-id]
  (dispatch!
   :update $elements
   (fn [elements]
     (let [zip (element-zipper elements)
           zelem (zfind zip #(= eid (:element/id %)))
           elem (z/node zelem)
           elements (-> zelem z/remove z/root)

           zip (element-zipper elements)
           ztarget (zfind zip #(= target-id (:element/id %)))
           ztarget (z/insert-child ztarget elem)]
       (z/root ztarget)))))

;; need existing defitinion with meta-data
(defui element-selector [{:keys [elements selection
                                 ^:membrane.component/contextual
                                 shift-down?
                                 ^:membrane.component/contextual
                                 drag
                                 ]}])
(defui element-selector [{:keys [elements selection
                                 ^:membrane.component/contextual
                                 shift-down?
                                 ^:membrane.component/contextual
                                 drag]}]
  (apply
   ui/vertical-layout
   (when drag
     (let [hover? (get extra [:hover :before])]
       (ui/on
        :mouse-up
        (fn [_]
          [[::move-element-to-beginning drag]
           [:set $selection #{}]])
        (basic/on-hover
         {:hover? hover?
          :$body nil
          :body (ui/filled-rectangle [0 0 0 (if hover?
                                              1
                                              0.2)]
                                     100 8)}))))
   (for [element elements]
     (let [lbl (ui/on
                :mouse-down
                (fn [_]
                  [[:start-drag (:element/id element)]])
                :mouse-up
                (fn [_]
                  (if shift-down?
                    [[:update $selection conj (:element/id element)]]
                    [[:set $selection #{(:element/id element)}]]))
                (ui/label
                 (or (:element/name element)
                     (:element/id element))))]
       (ui/vertical-layout
        (ui/horizontal-layout
         (ui/on
          :mouse-down
          (fn [_]
            [[:set $selection #{}]
             [:delete $element]])
          (ui/label "X" (assoc (ui/font "Menlo" 10)
                               :weight :bold)))
         (ui/spacer 3 0)
         (vertical-layout
          (if (get selection (:element/id element))
            (ui/with-color [1 0 0]
              lbl)
            lbl)
          (let [children (:element/children element)]
            (when children
              (ui/translate
               5 0
               (ui/on
                ::move-element-to-beginning
                (fn [drag]
                  [[::move-element-to-beginning-of drag (:element/id element)]])
                (element-selector {:elements children
                                   :selection selection})))))))
        (when drag
          (let [hover? (get extra [:hover :after (:element/id element)])]
            (ui/on
             :mouse-up
             (fn [_]
               [[::move-element-after drag (:element/id element)]])
             (basic/on-hover
              {:hover? hover?
               :$body nil
               :body (ui/filled-rectangle [0 0 0 (if hover?
                                                   1
                                                   0.2)]
                                          100 8)}))))))))
  )

(defn matches-id? [id]
  (fn [m]
    (= id (:element/id m))))
(def matches-id?-memo (memoize matches-id?))

(defui canvas-editor [{:keys [src result  tool
                              ^:membrane.component/contextual
                              drag]}]
  
  (let [elements (:elements src)
        mouse-down (:mouse-down src)
        mouse-temp (:mouse-temp src)
        selection (get src :selection #{})

        canvas-width (get extra :canvas-width 400)
        canvas-height (get extra :canvas-height 400)
        resizing? (get extra :resizing? false)]
    (vertical-layout
     (ui/horizontal-layout
      (radio-button {:text "Move"
                     :value :move
                     :selection tool})
      (radio-button {:text "Label"
                     :value :label
                     :selection tool})
      (radio-button {:text "rect"
                     :value :rect
                     :selection tool})
      (radio-button {:text "code"
                     :value :code
                     :selection tool})
      (basic/button {:text "group"
                     :on-click
                     (fn []
                       [[:set $selection #{}]
                        [::group-selection $elements result selection]])})
      (basic/button {:text "resize"
                     :on-click
                     (fn []
                       [[:set $resizing? true]])}))
     
     (ui/horizontal-layout

      (ui/on
       ::move-element-after
       (fn [eid after-id]
         [[::move-element-after $elements eid after-id]])
       ::move-element-to-beginning
       (fn [eid]
         [[::move-element-to-beginning $elements eid]])
       ::move-element-to-beginning-of
       (fn [eid target-id]
         [[::move-element-to-beginning-of $elements eid target-id]])

       (element-selector {:elements elements
                          :selection selection}))
      [(ui/with-style :membrane.ui/style-stroke
         (ui/rectangle canvas-width canvas-height))
       (when result
         (let [body (ui/no-events
                     [(ui/spacer canvas-width canvas-height)
                      (ui/try-draw
                       (try
                         (when-let [result (-success result)]
                           result)
                         (catch Exception e
                           nil))
                       (fn [& args]
                         nil))
                      ])]
           (if resizing?
             (wrap-resizing
              {:width canvas-width
               :height canvas-height
               :resizing? resizing?
               :body body})
             (wrap-tool {:width canvas-width
                         :height canvas-height
                         :tool tool
                         :src src
                         :body body})))
         
         
         )]
      (let [selected-id (first selection)]
        (when selected-id
          (let [path [spec/ALL
                      ELEMENT-TREE-WALKER
                      (matches-id?-memo selected-id)]
                element (spec/select-one path elements)]
            (let [properties (::properties element)
                  kw-str (get extra [$element ::kw-str ""])
                  ]
              (apply
               ui/vertical-layout
               (basic/textarea {:text (:element/name element)})
               (ui/horizontal-layout
                (basic/button {:text "+"
                               :on-click (fn []
                                           (when (seq kw-str)
                                             [[::add-property $properties $kw-str kw-str]]))})
                (basic/textarea {:text kw-str}))
               (for [[k v] properties]
                 (ui/horizontal-layout
                  (ui/on
                   :mouse-down
                   (fn [_]
                     [[:update $properties dissoc k]])
                   (ui/label "X" (assoc (ui/font "Menlo" 10)
                                        :weight :bold)))
                  (ui/label k)
                  (code-editor/text-editor {:buf v}))))))))))))

(defui user-defined-editor [{:keys [src result]}]
  (let [component-name (get src :component-name (->buf ""))
        initial-state-form (get src :initial-state-form (->buf nil))
        ephemeral (get src ::ephemeral)
        state (get ephemeral ::user-defined-state)]
    (ui/vertical-layout
     (basic/button {:text "reset"
                    :on-click (fn []
                                [[:delete $state]])})
     (ui/horizontal-layout
      (code-editor/text-editor {:buf component-name})
      (code-editor/text-editor {:buf initial-state-form}))
     (when-let [result (and result
                            (-success result))]
       (try
         (ui/try-draw
          (let [component (:component result)
                initial-state (:initial-state result)]
            (when (and component initial-state)
              (let [state (or state initial-state)
                    user-defined-extra (get state ::extra)
                    state (into state
                                (map (fn [k]
                                       [(keyword (str "$" (name k)))
                                        [$state k]]))
                                (keys initial-state))
                    state (assoc state
                                 :extra user-defined-extra
                                 :$extra $user-defined-extra
                                 :context context
                                 :$context $context)
                    ]
                (component state))))
          (constantly nil))
         (catch Exception e
           nil))))))

(defui side-effect-editor [{:keys [src side-effect-ts]}]
  (ui/horizontal-layout
   (basic/button {:text "go"
                  :on-click (fn []
                              [[:set $side-effect-ts (-> (Instant/now)
                                                         (.toEpochMilli))]])})
   (code-editor/text-editor {:buf (:buf src)})))


(defeffect ::inspect-result [result]
  (tap> result))

(defui row-menu [{:keys []}]
  (let [hover? (get extra :hover?)]
    (basic/on-hover
     {:hover? hover?
      :$body nil
      :body
      (if hover?
        (ui/vertical-layout
         (basic/button {:text "save"
                        :on-click (constantly [[:save]])})
         (basic/button {:text "cleanup"
                        :on-click (constantly [[:cleanup]])}))
        (ui/padding 2
         (ui/filled-rectangle [0 0 0] 8 8)))})))

;; adds name
(defn wrap-drag-start-name [nm row]
  (ui/on
   ::dnd/drag-start
   (fn [m]
     [[::dnd/drag-start (assoc-in m [::dnd/obj :name] nm)]])
   row))

(defui spreadsheet-row [{:keys [row result]}]

  (horizontal-layout
   (basic/button {:text "X"
                  :on-click (fn []
                              [[:delete $row]])})
   (basic/checkbox {:checked? (:def row)})
   (basic/textarea {:text (:name row)})
   (case (:editor row)

     :user-defined
     (user-defined-editor {:src (:src row)
                           :result result})

     :side-effect
     (side-effect-editor {:src (:src row)
                          :side-effect-ts (:side-effect-ts row)})

     :code-editor
     (code-editor/text-editor {:buf (:src row)})

     :value
     nil

     :canvas
     (canvas-editor {:src (:src row)
                     :result result})

     :number-slider
     (basic/number-slider {:num (:src row)
                           :max-width 100
                           :min 0
                           :max 100
                           :integer? true}))
   (let [inspector-extra (get extra [:inspector (:id row)])
         view-toggle (get extra :view-toggle false)
         pv
         (if view-toggle
           (when result
             (ui/no-events
              (ui/try-draw
               (when-let [result (-success result)]
                 result)
               (constantly nil))))
           (wrap-drag-start-name
            (:name row)
            (iv/inspector {:obj (or result
                                    (iv/wrap nil))
                           :width (get inspector-extra :width 40)
                           :height (get row :height 1)
                           :show-context? (get inspector-extra :show-context?)
                           :extra inspector-extra})))
         [w h] (ui/bounds pv)
         toggle-view (ui/on
                      :mouse-down
                      (fn [_]
                        [[:update $view-toggle not]])
                      (ui/filled-rectangle [1 0 1 0.5]
                                           8 8))]
     [
      pv
      (ui/translate (max 0 (- w 8)) 0
                    toggle-view)
      ])))

(defeffect ::insert-spreadsheet-row [$spreadsheet row-id editor-type & {:as row}]
  (dispatch! :update $spreadsheet
             (fn [spreadsheet]
               (into []
                     cat
                     [(take-while #(not= row-id (:id %)) spreadsheet)
                      [(merge
                        (init-editor {:name (name (gensym))
                                      :id (genid)
                                      :editor editor-type} )
                        row)]
                      (drop-while #(not= row-id (:id %)) spreadsheet)]))))




(defeffect ::add-spreadsheet-row [$spreadsheet editor-type & {:as row}]
  (dispatch! :update $spreadsheet conj
             (merge (init-editor
                     {:name (name (gensym))
                      :id (genid)
                      :editor editor-type})
                    row)))

(defn simplify-path [path]
  (loop [last-expr nil
         simplified-path []
         path (seq path)]
    (if path
      (let [part (first path)
            op (first part)]
        (if (and (= 'find (first last-expr))
                 ('#{val} op))
          (let [prev-arg (second last-expr)]
            (recur nil
                   (conj (pop simplified-path)
                         `(~'get ~prev-arg))
                   (next path)))
          (recur part
                 (conj simplified-path part)
                 (next path))))
      simplified-path)
    ))

(defn- literal? [o]
  (or (number? o)
      (string? o)
      (keyword? o)
      (nil? o)
      (boolean? o)
      (char? o)))
(defn- add-quotes-if-necessary [path]
  (into []
        (map (fn [expr]
               (if (and (seq expr)
                        ('#{get find} (first expr))
                        (not (literal? (second expr))))
                 (list (first expr) (list 'quote (second expr)))
                 expr)))
        path))
(defn- path->expression [nm path]
  `(~'-> ~(read-string nm)
    ~@(-> path
          simplify-path
          add-quotes-if-necessary)))
(defeffect ::drop-in-spreadsheet-row [{:keys [row-id obj]
                                       $spreadsheet :$ss
                                       spreadsheet-id ::id}]
  (let [{:keys [row editor-type] :as m}
        (if (and (= spreadsheet-id
                    (::id obj))
                 (seq (:path obj)))
          (let [expr (path->expression (:name obj) (:path obj))]
            {:editor-type :code-editor
             :row {:src (buffer/buffer (with-out-str
                                         (clojure.pprint/pprint expr))
                                       {:mode :insert})}})
          {:editor-type :value
           :row {:value @(:x obj)}})]
    (if row-id
      (dispatch! ::insert-spreadsheet-row
                 $spreadsheet
                 row-id
                 editor-type
                 row)
      (dispatch! ::add-spreadsheet-row
                 $spreadsheet
                 editor-type
                 row))))


(defui button-bar [{:keys [ss row-id]}]
  (apply
   ui/horizontal-layout
   (for [[text editor-type] [["+" :code-editor]
                             ["+42" :number-slider]
                             ["+[]" :canvas]
                             ["+:)" :user-defined]
                             ["+!!" :side-effect]]]
     (basic/button {:text text
                    :hover? (get extra [:hover? row-id editor-type])
                    :on-click (fn []
                                (if row-id
                                  [[::insert-spreadsheet-row $ss row-id editor-type]]
                                  [[::add-spreadsheet-row $ss editor-type]]))}))))


(defn wrap-drag-start-spreadsheet-id [id body]
  (ui/on
   ::drop-in-spreadsheet-row
   (fn [m]
     [[::drop-in-spreadsheet-row (assoc m ::id id)]])
   ::dnd/drag-start
   (fn [m]
     [[::dnd/drag-start (assoc-in m [::dnd/obj ::id] id)]])
   body))

(defn ^:private stretch-bottom [[cw ch] top bottom]
  (ui/vertical-layout
   top
   (let [scroll-button-size 7]
     (assoc bottom
            :scroll-bounds
            [(max (- cw scroll-button-size)
                  0)

             (max 0
                  (- ch (ui/height top)
                     scroll-button-size))]))))

(defui spreadsheet-editor [{:keys [ss results
                                   ;; xtdb
                                   load-options
                                   ns-info
                                   ^:membrane.component/contextual
                                   drop-object]
                            :as args}]
  (let [container-size (:membrane.stretch/container-size context)]
    (wrap-drag-and-drop
     {:$body nil
      :body
      (wrap-drag-start-spreadsheet-id
       (::id args)
       (ui/wrap-on
        :key-event
        (fn [handler key scancode actions mods]
          (let [intents (handler key scancode actions mods)
                shift-down? (get context :shift-down?)
                shift? (= 1 (bit-and mods 1))]
            (if (not= shift? shift-down?)
              (conj intents [:set $shift-down? shift?])
              intents)))
        (stretch-bottom
         container-size
         (ui/vertical-layout
          (button-bar {:ss ss})
          #_(basic/button {:text "print-forms"
                           :on-click
                           (fn []
                             [[::print-ss-forms ss]])}
                          )
          (let [edit-ns? (get extra :edit-ns?)
                temp-ns-name (get extra :temp-ns-name)]
            (cond

              edit-ns?
              (ui/vertical-layout
               (basic/button {:text "save"
                              :on-click
                              (fn []
                                [[:set $edit-ns? false]
                                 [:update $ns-info
                                  assoc :name (symbol temp-ns-name)]])})
               (basic/textarea {:text temp-ns-name}))

              #_#_load-options
              (apply
               ui/vertical-layout
               (basic/button {:text "cancel"
                              :on-click
                              (fn []
                                [[:set $load-options nil]])})
               (for [option load-options]
                 (basic/button {:text option
                                :on-click
                                (fn []
                                  [[::load-from-db xtdb $ns-info $ss option]
                                   [:set $load-options nil]])})))
              
              :else
              (apply
               ui/horizontal-layout
               (interpose
                (ui/spacer 8)
                [(basic/button {:text "edit"
                                :on-click
                                (fn []
                                  [[:set $edit-ns? true]
                                   [:set $temp-ns-name (name (:name ns-info))]])})
                 #_(basic/button {:text "save"
                                :on-click
                                (fn []
                                  [[::save-to-db xtdb ns-info ss]])})
                 #_(basic/button {:text "load"
                                :on-click
                                (fn []
                                  [[::show-load-options $load-options xtdb]])})
                 (ui/label (:name ns-info))])))))
         (basic/scrollview
          {:scroll-bounds [1200 800]
           :$body nil
           :body
           (vertical-layout
            (vertical-layout
             (apply vertical-layout
                    (for [row ss]
                      (let [srow
                            (ui/on
                             :save (fn []
                                     [[::save-to-db ss (:id row)]])
                             :cleanup (fn []
                                        [[::cleanup $ss (:id row)]])
                             (spreadsheet-row {:row row
                                               :result (get results (:id row))}))
                            srow-width 23]
                        (vertical-layout
                         (if drop-object
                           (let [hover? (get extra [$row :drag-hover])]
                             (dnd/on-drop
                              (fn [_ m]
                                [[::drop-in-spreadsheet-row
                                  {:$ss $ss
                                   :row-id (:id row)
                                   :obj m}]])
                              (dnd/on-drag-hover
                               {:hover? hover?
                                :$body nil
                                :body
                                (if hover?
                                  (ui/with-color [1 0 0]
                                    (ui/rectangle srow-width 5))
                                  (ui/rectangle srow-width 5))})))
                           (let [hover? (get extra [$row :hover])]
                             (basic/on-hover
                              {:hover? hover?
                               :$body nil
                               :body (if hover?
                                       (button-bar {:ss ss
                                                    :row-id (:id row)})
                                       ;;(ui/spacer srow-width 5)
                                       (ui/rectangle srow-width 5)
                                       )})))
                         srow))))
             (when drop-object
               (dnd/on-drop
                (fn [_ m]
                  [[::drop-in-spreadsheet-row
                    {:$ss $ss
                     :obj m}]])
                (ui/filled-rectangle [0.7 0.5 0.5]
                                     32 32)))
             (ui/spacer 0 300)))}))))})))

(defonce spreadsheet-state (atom {}))

(defn sync-ns! [{:keys [name require import]}]
  (binding [*ns* (the-ns 'user)]
    (eval `(ns ~name
             ~@(when (seq require)
                 [`(:require ~@require)])
             ~@(when (seq import)
                 [`(:import ~@import)])))))

(declare calc-spreadsheet)
(defn run-results
  ([]
   (run-results spreadsheet-state))
  ([atm]
   (let [[old _] (swap-vals! atm dissoc :ss-chan :result-thread)]
     (when-let [ss-chan (:ss-chan old)]
       (async/close! ss-chan)))

   (let [ss-chan (async/chan (async/sliding-buffer 1))
         result-thread (let [binds (clojure.lang.Var/getThreadBindingFrame)]
                         (doto (Thread. (fn []
                                          (clojure.lang.Var/resetThreadBindingFrame binds)
                                          (loop []
                                            (when-let [ss (async/<!! ss-chan)]
                                              (try
                                                (let [state @atm
                                                      cache (get state :cache {})
                                                      side-effects (get state :side-effects {})

                                                      ns-info (get state :ns-info)
                                                      _ (assert ns-info)
                                                      cache (if (not= (get state :last-ns-info)
                                                                      ns-info)
                                                              (do (sync-ns! ns-info)
                                                                  ;; empty cache
                                                                  {})
                                                              ;; else
                                                              cache)
                                                      eval-ns (the-ns (:name ns-info))

                                                      {results :results
                                                       next-cache :cache
                                                       side-effects :side-effects} (calc-spreadsheet eval-ns cache side-effects ss)]
                                                  (swap! atm assoc
                                                         :results results
                                                         :side-effects side-effects
                                                         :cache next-cache
                                                         :last-ns-info ns-info)
                                                  (#'backend/glfw-post-empty-event))
                                                (catch Throwable e
                                                  (prn e)))
                                              (recur))))
                                        "Result-Thread")))]
     (swap! atm
            assoc
            :ss-chan ss-chan
            :result-thread result-thread)
     (.start result-thread)

     (add-watch atm ::update-results
                (fn check-update-results [key ref old new]
                  (when-not (= (:ss new)
                               (:ss old))
                    (async/put! ss-chan (:ss new)))))
     nil)))



#_(def cfg {:store {:backend :file :path (.getAbsolutePath
                                        (io/file "sstest"))}})


#_(defn mem-db
  ([]
   (xt/start-node {})))

#_(defn start-xtdb! []
  (let [home-dir (System/getProperty "user.home")]
    (assert (and home-dir
                 (not= home-dir "")))
    (letfn [(kv-store [dir]
              {:kv-store {:xtdb/module 'xtdb.rocksdb/->kv-store
                          :db-dir (io/file home-dir
                                           ".clojure"
                                           "devdb"
                                           dir)
                          :sync? true}})]
      (xt/start-node
       {:xtdb/tx-log (kv-store "data/dev/tx-log")
        :xtdb/document-store (kv-store "data/dev/doc-store")
        :xtdb/index-store (kv-store "data/dev/index-store")}))))

#_(defonce xnode (delay (start-xtdb!)))
#_(defn stop-xtdb! []
  (.close @xnode))

(defn init-spreadsheet []
  (swap! spreadsheet-state
         assoc
         :ss []
         ;; :xtdb @xnode
         ::id (genid)
         :ns-info {:name 'foo.baz
                   :require '([membrane.ui :as ui
                               :refer [vertical-layout
                                       horizontal-layout]]
                              [clojure.java.io :as io]
                              [com.phronemophobic.membrane.spreadsheet :as ss]
                              [clojure.edn :as edn]
                              ;; [xtdb.api :as xt]
                              clojure.set
                              [clojure.string :as str]
                              [clojure.data.json :as json])
                   :import '(java.io.PushbackReader
                             java.time.Instant)})
  nil)
(init-spreadsheet)

(defn current-spreadsheet []
  (:ss @spreadsheet-state))
(defn run []
  (swap! spreadsheet-state
         assoc
         :ns-info {:name (ns-name *ns*)})
  (run-results)
  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state)
    {:include-container-info true}))

(defmulti parse-src :editor)
(defmethod parse-src :code-editor [row]
  (let [src (:src row)
        [err form] (try
                     [nil (read-string-memo (->str (:src row)))]
                     (catch Exception e
                       [e nil]))]
    (if err
      (assoc row :err err)
      (assoc row :form form))))
(defmethod init-editor :code-editor [row]
  (merge row
         {:src (buffer/buffer "42" {:mode :insert})}))


(defmethod parse-src :number-slider [row]
  (let [src (:src row)]
    (assoc row :form (or src 42))))
(defmethod init-editor :number-slider [row]
  (merge row
         {:src 12}))


(defmethod parse-src :value [row]
  (assoc row :value (:value row)))
(defmethod init-editor :value [row]
  row)

(defmethod parse-src :canvas [row]
  (let [
        src (:src row)
        mouse-down (:mouse-down src)
        mouse-temp (:mouse-temp src)
        selection (:selection src)
        wrap-move (if (and selection
                           mouse-down
                           mouse-temp)
                    (let [ox (- (nth mouse-temp 0)
                                (nth mouse-down 0))
                          oy (- (nth mouse-temp 1)
                                (nth mouse-down 1))]

                      (map (fn [elem]
                             (if (selection (:element/id elem))
                               (-> elem
                                   (update :element/x
                                           (fn [x]
                                             (when x
                                               `(+ ~ox ~x))))
                                   (update :element/y
                                           (fn [y]
                                             (when y
                                               `(+ ~oy ~y)))))
                               elem))))
                    identity)
        elements (:elements (:src row))
        elements (spec/transform [spec/ALL
                                  ELEMENT-TREE-WALKER]
                                 wrap-properties
                                 elements)
        form (into []
                   (comp wrap-move
                         (map #(try
                                 (s2/compile-memo %)
                                 (catch Exception e
                                   (prn e))
                                 (catch AssertionError e))))
                   elements)]
    (assoc row :form form)))

(defmethod init-editor :canvas [row]
  (merge row
         {:src {:elements []}}))

(defmethod parse-src :user-defined [row]
  (let [src (:src row)
        user-defined-state-sym (gensym)
        [err form] (try
                     [nil
                      `(let [initial-state# ~(read-string-memo (->str (:initial-state-form src)))]
                         {:component ~(read-string-memo (->str (:component-name src)))
                          :initial-state initial-state#
                          :data (or ~user-defined-state-sym
                                    initial-state#)})]
                     (catch Exception e
                       [e nil]))]
    (if err
      (assoc row :err err)
      (assoc row
             :form form
             :bindings
             {user-defined-state-sym (iv/wrap (-> src
                                                  ::ephemeral
                                                  ::user-defined-state))}))))

(defmethod init-editor :user-defined [row]
  (merge row
         {}))


(defmethod parse-src :side-effect [row]
  (merge row
         (parse-src {:editor :code-editor
                     :src (-> row :src :buf)})
         {:side-effect? true
          :editor :side-effect}))

(defmethod init-editor :side-effect [row]
  (merge row
         {:src {:buf (buffer/buffer "42" {:mode :insert})}
          :side-effect? true}))


(defn parse-row [row]
  (let [ ;; src (:src row)
        ;; row (assoc row :src (->str src))
        ;; [err form] (try
        ;;              [nil (read-string-memo (:src row))]
        ;;              (catch Exception e
        ;;                [e nil]))

        row (parse-src row)]
    (if (:err row)
      row
      (let [[err sym] (try
                        (let [sym (read-string (:name row))]
                          (when (not (and (symbol? sym)
                                          (not (qualified-symbol? sym))))
                            (throw (Exception.  (str "Names must be unqualified symbols: " sym)) ))
                          [nil sym])
                        (catch Exception e
                          [e nil]))]
        (if err
          (assoc row :err err)
          (assoc row :sym sym))))))




;; https://stackoverflow.com/a/48541659


(def nil-ast
  {:op :const
   :env {}
   :type :unknown
   :literal? true
   ;; :val nil
   ;; :form nil
   :top-level true
   :tag clojure.lang.AFunction
   })
(def var-ast
  {:op :var,
   :assignable? false,
   ;; :var #'com.phronemophobic.membrane.spreadsheet/a,
   :o-tag java.lang.Object})
(defn free-variables [form]
  (let [free (volatile! #{})]
    (ana.jvm/analyze
     form
     (ana.jvm/empty-env)
     {:passes-opts
      (assoc ana.jvm/default-passes-opts
             :validate/unresolvable-symbol-handler
             (fn [_ sym _]
               (vswap! free conj sym)
               (assoc var-ast :form sym)))})
    @free))



(defn add-deps [env row]
  (if (:err row)
    [env row]
    (let [;;_ (prn (:form row))
          free (free-variables (:form row))
          deps (into #{}
                     (comp (map (fn [sym]
                                  (get-in env [:bindings sym])))
                           ;; dep can be missing if referring to var outside
                           ;; of spreadsheet
                           (remove nil?))
                     free)
          env (assoc-in env [:bindings (:sym row)] (:id row))]
      [env (assoc row :deps deps)])))

(defn find-row [env id]
  (->> (:rows env)
       (some (fn [row]
               (when (= id (:id row))
                 row)))))

(defn process-make-fn [env row]
  (let [form (:form row)]
    (if-not (and (seq? form)
                 (= (first form) 'make-fn))
      [env row]
      (let [[_ args ret] form
            ret-id (get-in env [:bindings ret])
            arg-ids (into #{}
                          (map (fn [sym]
                                 (get-in env [:bindings sym])))
                          args)
            bindings (loop [deps #{ret-id}
                            rows (seq (reverse (:rows env)))
                            bindings []]
                       (if rows
                         (let [row (first rows)]
                           (if (contains? deps (:id row))
                             (recur (-> deps
                                        (disj (:id row))
                                        (into (:deps row))
                                        (clojure.set/difference arg-ids))
                                    (next rows)
                                    (conj bindings [(:sym row) (:form row)]))
                             (recur deps (next rows) bindings)))
                         (into [] cat (reverse bindings))))
            
            form `(fn ~args
                    (let ~bindings
                      ~ret))]
        [env (assoc row :form form)])))
  
  )


(defn process-make-component [env row]
  (let [form (:form row)]
    (if-not (and (seq? form)
                 (= (first form) 'make-component))
      [env row]
      (let [[_ args ret] form
            ret-id (get-in env [:bindings ret])
            arg-ids (into #{}
                          (map (fn [sym]
                                 (get-in env [:bindings sym])))
                          (-> args first :keys))
            bindings (loop [deps #{ret-id}
                            rows (seq (reverse (:rows env)))
                            bindings []]
                       (if rows
                         (let [row (first rows)]
                           (if (contains? deps (:id row))
                             (recur (-> deps
                                        (disj (:id row))
                                        (into (:deps row))
                                        (clojure.set/difference arg-ids))
                                    (next rows)
                                    (conj bindings [(:sym row) (:form row)]))
                             (recur deps (next rows) bindings)))
                         (into [] cat (reverse bindings))))

            form `(defui ~(symbol (:name row)) ~args
                    (let ~bindings
                      ~ret))]
        [env (assoc row :form form)]))))


(defn complete-env [proc]
  (fn [env row]
    [env (proc row)]))

(def default-process-passes
  [(complete-env parse-row)
   process-make-fn
   process-make-component
   add-deps])

(defn process-spreadsheet
  ([spreadsheet]
   (process-spreadsheet spreadsheet default-process-passes))
  ([spreadsheet procs]
   (reduce
    (fn [[env rows] row]
      (let [[env row]
            (reduce (fn [[env ret] f]
                      (f env ret))
                    [env row]
                    procs)
            rows (conj rows row)]
        [(assoc env :rows rows)
         rows]))
    [{} []]
    spreadsheet)))


(comment
  (process-spreadsheet (current-spreadsheet)
                       [(complete-env parse-row)
                        process-make-fn
                        add-deps]))

(def ^:dynamic *spreadsheet-bindings* nil)

(defn calc-result [eval-ns row bindings]
  (let [{:keys [sym form err]} row]
    (if err
      (wrap-result-constant err nil)
      ;; else
      (let [binding-code (into []
                               (mapcat
                                (fn [[sym _]]
                                  [sym `(iv/-unwrap
                                         (get *spreadsheet-bindings* (quote ~sym)))]))
                               bindings)
            form (if (:def row)
                   `(let [result# ~form]
                      (def ~sym result#)
                      result#)
                   form)
            re-eval (fn eval-fn [form]
                      (let [eval-code `(let ~binding-code
                                         ~form)]
                        (binding [*spreadsheet-bindings* bindings]
                          (try
                            (wrap-result
                             eval-fn
                             nil
                             (let [result (binding [*ns* eval-ns]
                                            (eval eval-code))]
                               (if (seqable? result)
                                 (do
                                   ;; pre-realize parts of lazy seqs
                                   (bounded-count (max 10
                                                       (+ (get row :height 1)
                                                          ;; common chunk size
                                                          32))
                                                  result)
                                   result)
                                 result)))
                            (catch Throwable e
                              (wrap-result eval-fn e nil))))))

            result (re-eval form)]
        ;; (prn "running " sym)
        result)))
  )

(defn calc-spreadsheet [eval-ns cache side-effects ss]
  (binding [*ns* eval-ns]
    (let [[env ss] (process-spreadsheet ss
                                        [(complete-env parse-row)
                                         process-make-fn
                                         process-make-component
                                         add-deps])]
      (loop [ss (seq ss)
             vals {}
             bindings {}
             next-cache {}
             side-effects side-effects]
        (if-not ss
          {:cache next-cache
           :results vals
           :side-effects side-effects}
          (let [row (first ss)
                {:keys [sym form value err]} row
                cache-key [sym form value err bindings]
                [side-effect-result
                 result]

                (cond

                  (:side-effect? row)
                  (if-let [ts (:side-effect-ts row)]
                    (let [{result :result
                           last-run-ts :side-effect-ts
                           :as side-effect-result} (get side-effects (:id row))]
                      (if (or (not last-run-ts)
                              (not= last-run-ts ts))
                        (let [result (calc-result eval-ns row bindings)]
                          (prn "calculating side effect" (:name row))
                          [{:result result
                            :side-effect-ts ts}
                           result])
                        [side-effect-result
                         result]))

                    ;; no ts. return exception
                    [nil
                     (wrap-result-constant
                      (Exception. "Side Effect not marked to run yet.")
                      nil)])

                  (:value row)
                  [nil
                   ;; AResult has identity semantics
                   ;; reuse when possible
                   (if-let [result (get cache cache-key)]
                     result
                     (wrap-result-constant nil (:value row)))]

                  :else
                  [nil
                   (if-let [result (get cache cache-key)]
                     result
                     (let [row-bindings (into bindings (:bindings row))]
                       (calc-result eval-ns row row-bindings)))])]
            (recur (next ss)
                   (assoc vals (:id row) result)
                   (assoc bindings sym result)
                   (assoc next-cache cache-key result)
                   (if (:side-effect? row)
                     (assoc side-effects (:id row) side-effect-result)
                     side-effects))))))))




(defn print-forms
  ([]
   (print-forms (:ss @spreadsheet-state)))
  ([ss]
   (clojure.pprint/pprint
    (-> ss
        process-spreadsheet
        second
        (->> (map (juxt :name :form)))))))


(defeffect ::print-ss-forms [ss]
  (print-forms ss))


(defn get-ss []
  (-> @spreadsheet-state
      :ss))

(defn remove-ephemeral [ss]
  (spec/setval [spec/ALL :src map? ::ephemeral]
               spec/NONE
               ss))

(defn save-ss
  ([]
   (save-ss "ss.edn"))
  ([fname]
   (with-open [w (io/writer fname)]
     (write-edn w (-> (get-ss)
                      remove-ephemeral)))))

(defn fix-ids [ss]
  (let [ss (spec/transform [spec/ALL (spec/must :id) #(not (uuid? %))]
                           (fn [_] (genid))
                           ss)
        ss (spec/transform [spec/ALL
                            #(= :canvas (:editor %))
                            :src
                            :elements
                            spec/ALL
                            ELEMENT-TREE-WALKER
                            (spec/must :element/id)
                            #(not (uuid? %))]
                           (fn [_] (genid))
                           ss)]
    ss))

(defn load-ss
  ([]
   (load-ss "ss.edn"))
  ([fname]
   (let [ss (with-open [rdr (io/reader fname)
                        pbr (PushbackReader. rdr)]
              (clojure.edn/read pbr))]
     (swap! spreadsheet-state
            (fn [m]
              (-> m
                  (assoc :ss (fix-ids ss))
                  (dissoc :cache)))))
   nil))

(defn get-result-thread []
  (->>
   (Thread/getAllStackTraces)
   keys
   (filter
    (fn [thread]
      (= "Result-Thread" (.getName thread))))
   first))

(defn stop-result-thread!! []
  (.stop (get-result-thread)))

(defn dump-all-threads [fname]
  (with-open [w (io/writer fname)]
    (write-edn (->> (Thread/getAllStackTraces)
                    (map (fn [[thread trace]]
                           [(bean thread)
                            (map bean trace)]))))))


(def editors-schema
  [{:db/ident ::code-editor}
   {:db/ident ::number-slider}
   {:db/ident ::canvas}
   {:db/ident ::user-defined}
   {:db/ident ::side-effect}])

(def schema [
             ;; cells
             {:db/ident :cell/id
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique :db.unique/identity}
             {:db/ident :cell/name
              :db/doc "The cell's name"
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             #_{:db/ident :cell/ns
              :db/doc "The cell's ns"
              :db/valueType :db.type/symbol
              :db/cardinality :db.cardinality/one}
             {:db/ident :cell/editor
              :db/doc "The cell's editor type"
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}
             {:db/ident :cell.source/edn
              :db/doc "Editor state stored as edn string"
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :cell/def
              :db/valueType :db.type/boolean
              :db/cardinality :db.cardinality/one}

             ;; array
             {:db/ident :array/array
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many}
             {:db/ident :array/object
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}
             {:db/ident :array/idx
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}
             

             ;; namespaces
             {:db/ident :ns/id
              :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one
              :db/unique :db.unique/identity}
             {:db/ident :ns/name
              :db/valueType :db.type/symbol
              :db/cardinality :db.cardinality/one}
             {:db/ident :ns/cells
              :db/valueType :db.type/ref
              :db/doc "Reference to an array of cells"
              :db/cardinality :db.cardinality/one}


             ]
  )

(def full-schema (->> (concat editors-schema
                          schema)
                      (map #(vector (:db/ident %) %))
                      (into {})))
(comment
  (d/transact @conn
               full-schema
              )
  ,)





;; (defn ->transaction [{:keys [ss ns-info]}]
;;   (let [db-cells
;;         (->> ss
;;              (spec/transform [spec/ALL]
;;                              (fn [cell]
;;                                {:cell/id (:id cell)
;;                                 :xt/id {:cell/id (:id cell)}
;;                                 ;; :db/
;;                                 :cell/name (:name cell)
;;                                 ;; :cell/ns (ns-name *ns*)
;;                                 :cell/def (boolean (:def cell))
;;                                 :cell/side-effect? (boolean (:side-effect? cell))
;;                                 :cell/editor (:editor cell)
;;                                 :cell.source/edn (->edn (:src cell))})))
;;         cell-list {:xt/id (into {}
;;                                 (map-indexed (fn [idx cell]
;;                                                [idx (:xt/id cell)]))
;;                                 db-cells)
;;                    :cell/list (mapv :xt/id db-cells)}
;;         db-ns {:xt/id {:ns/name (:name ns-info)}
;;                :ns/name (:name ns-info)
;;                :ns/require (->edn (:require ns-info))
;;                :ns/import (->edn (:import ns-info))
;;                :ns/cells (:xt/id cell-list)}]
;;     (into []

;;           (comp cat
;;                 (map #(vector ::xt/put %)))
;;           [db-cells
;;            [cell-list
;;             db-ns]])))

(defn db-cell->cell
  [db-cell]
  (let [normalized-cell
        {:id (:cell/id db-cell),
         :name (:cell/name db-cell),
         :src (clojure.edn/read-string (:cell.source/edn db-cell)),
         :editor (:cell/editor db-cell)
         :side-effect? (:cell/side-effect? db-cell)
         :def (:cell/def db-cell)}]
      normalized-cell))

(defn db-ns->ns-info [ns]
  {:name (:ns/name ns)
   :require (edn/read-string (:ns/require ns))
   :import (edn/read-string (:ns/import ns))})

(defn ^:private db-result->sstate [result]
  {:ns-info (db-ns->ns-info result)
   :ss (mapv db-cell->cell
             (-> result
                 :ns/cells
                 :cell/list))})

#_(defn load-ns [db ns-name]
  (let [result
        (ffirst
         (xt/q db
               '{:find [(pull ?e [* {:ns/cells [{:cell/list [*]}]}])]
                 :in [ns-name]
                 :where
                 [[?e :ns/name ns-name]]}
               ns-name))]
    (db-result->sstate result)))

#_(defeffect ::save-to-db [node ns-info ss]
  (let [tx (xt/submit-tx node (->transaction
                             {:ns-info ns-info
                              :ss ss}))]
    (xt/await-tx node tx)))

#_(defeffect ::load-from-db [node $ns-info $ss ns-name]
  (let [{:keys [ns-info ss]} (load-ns (xt/db node) ns-name)]
    (when (and ns-info ss)
      (dispatch! :set $ns-info ns-info)
      (dispatch! :set $ss ss))))

#_(defeffect ::show-load-options [$load-options node]
  (let [ns-names (->> (xt/q (xt/db node)
                            '{:find [?name]
                              :where [[_ :ns/name ?name]]})
                      (map first))]
    (dispatch! :set $load-options ns-names)))

(comment

  ;; (def db (mem-db))
  ;; (xt/submit-tx db (->transaction (select-keys @spreadsheet-state [:ss :ns-info])))

  ,
  )




(defn -main [& args]
  (swap! spreadsheet-state
         assoc
         :ns-info {:name (ns-name *ns*)})
  (run-results)
  (backend/run-sync (com/make-app #'spreadsheet-editor spreadsheet-state)
                    {:include-container-info true})
  (async/close! (:ss-chan @spreadsheet-state)))
