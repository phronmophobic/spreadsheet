(ns com.phronemophobic.membrane.spreadsheet
  (:require [membrane.ui :as ui
             :refer [vertical-layout
                     horizontal-layout]]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            clojure.edn
            clojure.set
            [membrane.basic-components :as basic]
            [clojure.data.json :as json]
            [membrane.component :as com
             :refer [defui
                     defeffect]]
            [membrane.skia :as backend]
            [com.phronemophobic.viscous :as iv
             :refer [inspect]]
            [com.phronemophobic.membrane.schematic2 :as s2]
            [com.rpl.specter :as spec]
            ;; [membrane.java2d :as backend]
            ;; [membrane.skija :as backend]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [membrane.components.code-editor.code-editor :as code-editor]
            [liq.buffer :as buffer])
  (:import java.io.PushbackReader
           java.time.Instant)
  (:gen-class))

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
    ))

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

(defonce last-id (atom 0))
(defn genid []
  (swap! last-id inc))

(def read-string-memo (memoize read-string))
(defmulti init-editor :editor)

(defn ->str [buf-or-s]
  (if (map? buf-or-s)
    (buffer/text buf-or-s)
    buf-or-s))

(defn ->buf [x]
  (buffer/buffer (pr-str x) {:mode :insert}))

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
              :name (str "label_" id)
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
              :name (str "rect_" id)
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
              :name (str "code_" id)
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


(defui element-selector [{:keys [elements selection
                                 ^:membrane.component/contextual
                                 shift-down?
                                 ]}])
(defui element-selector [{:keys [elements selection
                                 ^:membrane.component/contextual
                                 shift-down?
                                 ]}]
  (apply
   ui/vertical-layout
   (for [element elements]
     (let [lbl (ui/on
                :mouse-down
                (fn [_]
                  (if shift-down?
                    [[:update $selection conj (:element/id element)]]
                    [[:set $selection #{(:element/id element)}]]))
                (ui/label
                 (or (:element/name element)
                     (:element/id element))))]
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
             (ui/translate 5 0
                           (element-selector {:elements children
                                              :selection selection})))))))))
  )

(defn matches-id? [id]
  (fn [m]
    (= id (:element/id m))))
(def matches-id?-memo (memoize matches-id?))

(defui canvas-editor [{:keys [src result  tool

                              ]}]
  
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
      
      (element-selector {:elements elements
                         :selection selection})
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
               (for-kv [[k v] properties]
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
        state (get src ::user-defined-state)]
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
           (iv/inspector {:obj (or result
                                   (iv/wrap nil))
                          :width (get inspector-extra :width 40)
                          :height (get row :height 1)
                          :show-context? (get inspector-extra :show-context?)
                          :extra inspector-extra
                          }))
         [w h] (ui/bounds pv)
         toggle-view (ui/on
                      :mouse-down
                      (fn [_]
                        [[:update $view-toggle not]])
                      (ui/filled-rectangle [1 0 1 0.5]
                                           8 8))]
     [
      pv
      (ui/translate (- w 8) 0
                    toggle-view)
      ])))

(defeffect ::insert-spreadsheet-row [$spreadsheet row-id editor-type]
  (dispatch! :update $spreadsheet
             (fn [spreadsheet]
               (into []
                     cat
                     [(take-while #(not= row-id (:id %)) spreadsheet)
                      [(init-editor {:name (name (gensym))
                                     :id (gensym)
                                     :editor editor-type} )]
                      (drop-while #(not= row-id (:id %)) spreadsheet)]))))




(defeffect ::add-spreadsheet-row [$spreadsheet editor-type]
  (dispatch! :update $spreadsheet conj
             (init-editor {:name (name (gensym))
                           :id (gensym)
                           :editor editor-type} )
             ))


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


(defui spreadsheet-editor [{:keys [ss results

                                   ]}]
  (ui/wrap-on
   :key-event
   (fn [handler key scancode actions mods]
     (let [intents (handler key scancode actions mods)
           shift-down? (get context :shift-down?)
           shift? (= 1 (bit-and mods 1))]
       (if (not= shift? shift-down?)
         (conj intents [:set $shift-down? shift?])
         intents)))
   (vertical-layout
    (button-bar {:ss ss})
    (basic/scrollview
     {:scroll-bounds [1200 800]
      :body
      (vertical-layout
       (apply vertical-layout
              (for [row ss]
                (let [srow (spreadsheet-row {:row row
                                             :result (get results (:id row))})
                      srow-width 23]
                  (vertical-layout
                   (let [hover? (get extra [$row :hover])]
                     (basic/on-hover
                      {:hover? hover?
                       :body (if hover?
                               (button-bar {:ss ss
                                            :row-id (:id row)})
                             
                               ;;(ui/spacer srow-width 5)
                               (ui/rectangle srow-width 5)
                               )}))
                   srow
                   ))))
       (ui/spacer 0 300))
      }))))

(defonce spreadsheet-state (atom {}))

(declare calc-spreadsheet)
(defn run-results
  ([]
   (run-results *ns*))
  ([eval-ns]
   (let [[old _] (swap-vals! spreadsheet-state dissoc :ss-chan :result-thread)]
     (when-let [ss-chan (:ss-chan old)]
       (async/close! ss-chan)))

   (let [ss-chan (async/chan (async/sliding-buffer 1))
         result-thread (let [binds (clojure.lang.Var/getThreadBindingFrame)]
                         (doto (Thread. (fn []
                                          (clojure.lang.Var/resetThreadBindingFrame binds)
                                          (loop []
                                            (when-let [ss (async/<!! ss-chan)]
                                              (try
                                                (let [state @spreadsheet-state
                                                      cache (get state :cache {})
                                                      side-effects (get state :side-effects {})

                                                      {results :results
                                                       next-cache :cache
                                                       side-effects :side-effects} (calc-spreadsheet eval-ns cache side-effects ss)]
                                                  (swap! spreadsheet-state assoc
                                                         :results results
                                                         :side-effects side-effects
                                                         :cache next-cache)
                                                  (#'backend/glfw-post-empty-event))
                                                (catch Exception e
                                                  (prn e)))
                                              (recur))))
                                        "Result-Thread")))]
     (swap! spreadsheet-state
            assoc
            :ss-chan ss-chan
            :result-thread result-thread)
     (.start result-thread)

     (add-watch spreadsheet-state ::update-results
                (fn check-update-results [key ref old new]
                  (let [ss (:ss new)]
                    (when-not (= (:ss new)
                                 (:ss old))
                      (async/put! ss-chan (:ss new))))))
     nil)))

(def my-spreadsheet
  [{:name "s"
    :id 1
    :editor :code-editor
    :src (buffer/buffer "(+ 1 2)" {:mode :insert})}
   {:name "s"
    :id 2
    :editor :code-editor
    :src (buffer/buffer "(+ s 2)" {:mode :insert})}
   {:name "t"
    :id 3
    :editor :code-editor
    :src (buffer/buffer "1234" {:mode :insert})}
   {:name "u"
    :id 4
    :editor :code-editor
    :src (buffer/buffer "(+ s t)" {:mode :insert})}
   ])
(defn init-spreadsheet []
  (swap! spreadsheet-state
         assoc :ss my-spreadsheet)
  nil)
(init-spreadsheet)

(defn current-spreadsheet []
  (:ss @spreadsheet-state))
(defn run []
  (run-results *ns*)
  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state)))









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
   (assoc row :form
          form)))
(defmethod init-editor :canvas [row]
  (merge row
         {:src {:elements [#:element{:type :element/shape
                                         :id (genid)


                                         ::properties
                                         #:element{
                                                   :x (->buf 5)
                                                   :y (->buf 10)
                                                   :width (->buf 15)
                                                   :height (->buf 20)
                                                   :fills (->buf [{}]) 
                                                   :corner-radius (->buf 3)}}]}}))

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
             {user-defined-state-sym (iv/wrap (::user-defined-state src))}))))

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
                     (map (fn [sym]
                            (get-in env [:bindings sym])))
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

(defn process-spreadsheet
  ([spreadsheet]
   (process-spreadsheet spreadsheet
                        [(complete-env parse-row)
                         process-make-fn
                         process-make-component
                         add-deps]))
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
                            (catch Exception e
                              (wrap-result eval-fn e nil))))))

            result (re-eval form)]
        ;; (prn "running " sym)
        result)))
  )

(defn calc-spreadsheet [eval-ns cache side-effects ss]
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
              {:keys [sym form err]} row
              cache-key [sym form err bindings]
              [side-effect-result
               result]
              (if (:side-effect? row)
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

                ;; else not side effect
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
                   side-effects)))))))

(defn -main [& args]

  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state))
  )


(defn print-forms []
  (clojure.pprint/pprint
   (-> @spreadsheet-state
       :ss
       process-spreadsheet
       second
       (->> (map (juxt :name :form))))))

(defn get-ss []
  (-> @spreadsheet-state
      :ss))

(defn save-ss
  ([]
    (save-ss "ss.edn"))
  ([fname]
   (with-open [w (io/writer fname)]
     (binding [*print-length* false
               *out* w]
       (pr (get-ss))))))

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
                  (assoc :ss ss)
                  (dissoc :cache)))))
   nil))

