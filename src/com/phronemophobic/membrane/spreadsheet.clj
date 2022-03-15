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
            [com.phronemophobic.viscous :as iv]
            [com.phronemophobic.membrane.schematic2 :as s2]
            [com.rpl.specter :as spec]
            ;; [membrane.java2d :as backend]
            ;; [membrane.skija :as backend]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [membrane.components.code-editor.code-editor :as code-editor]
            [liq.buffer :as buffer])
  (:import java.io.PushbackReader)
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
    (ui/horizontal-layout
     (basic/button {:text "+"
                    :on-click (fn []
                                [[::add-spreadsheet-row $ss :code-editor]])})

     (basic/button {:text "+42"
                    :on-click (fn []
                                [[::add-spreadsheet-row $ss :number-slider]])})
     (basic/button {:text "+[]"
                    :on-click (fn []
                                [[::add-spreadsheet-row $ss :canvas]])})
     (basic/button {:text "+!"
                    :on-click (fn []
                                [[::add-spreadsheet-row $ss :user-defined]])}))
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
                               (horizontal-layout
                                (basic/button {:text "+"
                                               :on-click (fn []
                                                           [[::insert-spreadsheet-row  $ss (:id row) :code-editor]])})

                                (basic/button {:text "+42"
                                               :on-click (fn []
                                                           [[::insert-spreadsheet-row $ss (:id row) :number-slider]])})
                                (basic/button {:text "+[]"
                                               :on-click (fn []
                                                           [[::insert-spreadsheet-row $ss (:id row) :canvas]])}))
                             
                               ;;(ui/spacer srow-width 5)
                               (ui/rectangle srow-width 5)
                               )}))
                   srow
                   ))))
       (ui/spacer 0 300))
      }))))

(defonce spreadsheet-state (atom {}))


(def my-text
  "1 	New York City[d] 	 New York 	8,336,817 	8,175,133 	+1.98% 	301.5 sq mi 	780.9 km2 	28,317/sq mi 	10,933/km2 	40.6635°N 73.9387°W
2 	Los Angeles 	 California 	3,979,576 	3,792,621 	+4.93% 	468.7 sq mi 	1,213.9 km2 	8,484/sq mi 	3,276/km2 	34.0194°N 118.4108°W
3 	Chicago 	 Illinois 	2,693,976 	2,695,598 	−0.06% 	227.3 sq mi 	588.7 km2 	11,900/sq mi 	4,600/km2 	41.8376°N 87.6818°W
4 	Houston[3] 	 Texas 	2,320,268 	2,100,263 	+10.48% 	637.5 sq mi 	1,651.1 km2 	3,613/sq mi 	1,395/km2 	29.7866°N 95.3909°W
5 	Phoenix 	 Arizona 	1,680,992 	1,445,632 	+16.28% 	517.6 sq mi 	1,340.6 km2 	3,120/sq mi 	1,200/km2 	33.5722°N 112.0901°W
6 	Philadelphia[e] 	 Pennsylvania 	1,584,064 	1,526,006 	+3.80% 	134.2 sq mi 	347.6 km2 	11,683/sq mi 	4,511/km2 	40.0094°N 75.1333°W
7 	San Antonio 	 Texas 	1,547,253 	1,327,407 	+16.56% 	461.0 sq mi 	1,194.0 km2 	3,238/sq mi 	1,250/km2 	29.4724°N 98.5251°W
8 	San Diego 	 California 	1,423,851 	1,307,402 	+8.91% 	325.2 sq mi 	842.3 km2 	4,325/sq mi 	1,670/km2 	32.8153°N 117.1350°W
9 	Dallas 	 Texas 	1,343,573 	1,197,816 	+12.17% 	340.9 sq mi 	882.9 km2 	3,866/sq mi 	1,493/km2 	32.7933°N 96.7665°W
10 	San Jose 	 California 	1,021,795 	945,942 	+8.02% 	177.5 sq mi 	459.7 km2 	5,777/sq mi 	2,231/km2 	37.2967°N 121.8189°W")

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
                                                (let [cache (get @spreadsheet-state :cache {})
                                                      [next-cache results] (calc-spreadsheet eval-ns cache ss)]
                                                  (swap! spreadsheet-state assoc
                                                         :results results
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
                          (assert (and (symbol? sym)
                                       (not (qualified-symbol? sym)))
                                  (str "Names must be unqualified symbols: " sym))
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


(defn complete-env [proc]
  (fn [env row]
    [env (proc row)]))

(defn process-spreadsheet [spreadsheet procs]
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
   spreadsheet))


(comment
  (process-spreadsheet (current-spreadsheet)
                       [(complete-env parse-row)
                        process-make-fn
                        add-deps]))

(def ^:dynamic *spreadsheet-bindings* nil)

(defn calc-result [eval-ns row bindings]
  (let [{:keys [sym form err]} row]
    (if err
      (let [v (atom nil)
            result (wrap-result (fn [& args]
                                  @v)
                                err
                                nil)]
        (reset! v result)
        result)
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
        result)))
  )

(defn calc-spreadsheet [eval-ns cache ss]
  (let [[env ss] (process-spreadsheet ss
                                      [(complete-env parse-row)
                                       process-make-fn
                                       add-deps])]
    (loop [ss (seq ss)
           vals {}
           bindings {}
           next-cache {}]
      (if-not ss
        [next-cache vals]
        (let [row (first ss)
              {:keys [sym form err]} row
              cache-key [sym form err bindings]
              result (if-let [result (get cache cache-key)]
                       result
                       (calc-result eval-ns row bindings))]
          (recur (next ss)
                   (assoc vals (:id row) result)
                   (assoc bindings sym result)
                   (assoc next-cache cache-key result)))))))

(def pop-text
  "1 	New York[d] 	New York 	8,804,190 	8,175,133 	+7.69% 	300.5 sq mi 	778.3 km2 	29,298/sq mi 	11,312/km2 	40.66°N 73.93°W
2 	Los Angeles 	California 	3,898,747 	3,792,621 	+2.80% 	469.5 sq mi 	1,216.0 km2 	8,304/sq mi 	3,206/km2 	34.01°N 118.41°W
3 	Chicago 	Illinois 	2,746,388 	2,695,598 	+1.88% 	227.7 sq mi 	589.7 km2 	12,061/sq mi 	4,657/km2 	41.83°N 87.68°W
4 	Houston 	Texas 	2,304,580 	2,099,451 	+9.77% 	640.4 sq mi 	1,658.6 km2 	3,599/sq mi 	1,390/km2 	29.78°N 95.39°W
5 	Phoenix 	Arizona 	1,608,139 	1,445,632 	+11.24% 	518.0 sq mi 	1,341.6 km2 	3,105/sq mi 	1,199/km2 	33.57°N 112.09°W
6 	Philadelphia[e] 	Pennsylvania 	1,603,797 	1,526,006 	+5.10% 	134.4 sq mi 	348.1 km2 	11,933/sq mi 	4,607/km2 	40.00°N 75.13°W
7 	San Antonio 	Texas 	1,434,625 	1,327,407 	+8.08% 	498.8 sq mi 	1,291.9 km2 	2,876/sq mi 	1,110/km2 	29.47°N 98.52°W
8 	San Diego 	California 	1,386,932 	1,307,402 	+6.08% 	325.9 sq mi 	844.1 km2 	4,256/sq mi 	1,643/km2 	32.81°N 117.13°W
9 	Dallas 	Texas 	1,304,379 	1,197,816 	+8.90% 	339.6 sq mi 	879.6 km2 	3,841/sq mi 	1,483/km2 	32.79°N 96.76°W
10 	San Jose 	California 	1,013,240 	945,942 	+7.11% 	178.3 sq mi 	461.8 km2 	5,683/sq mi 	2,194/km2 	37.29°N 121.81°W
11 	Austin 	Texas 	961,855 	790,390 	+21.69% 	319.9 sq mi 	828.5 km2 	3,007/sq mi 	1,161/km2 	30.30°N 97.75°W
12 	Jacksonville[f] 	Florida 	949,611 	821,784 	+15.55% 	747.3 sq mi 	1,935.5 km2 	1,271/sq mi 	491/km2 	30.33°N 81.66°W
13 	Fort Worth 	Texas 	918,915 	741,206 	+23.98% 	342.9 sq mi 	888.1 km2 	2,646/sq mi 	1,022/km2 	32.78°N 97.34°W
14 	Columbus 	Ohio 	905,748 	787,033 	+15.08% 	220.0 sq mi 	569.8 km2 	4,117/sq mi 	1,590/km2 	39.98°N 82.98°W
15 	Indianapolis[g] 	Indiana 	887,642 	820,445 	+8.19% 	361.6 sq mi 	936.5 km2 	2,455/sq mi 	948/km2 	39.77°N 86.14°W
16 	Charlotte 	North Carolina 	874,579 	731,424 	+19.57% 	308.3 sq mi 	798.5 km2 	2,837/sq mi 	1,095/km2 	35.20°N 80.83°W
17 	San Francisco[h] 	California 	873,965 	805,235 	+8.54% 	46.9 sq mi 	121.5 km2 	18,635/sq mi 	7,195/km2 	37.72°N 123.03°W
18 	Seattle 	Washington 	737,015 	608,660 	+21.09% 	83.8 sq mi 	217.0 km2 	8,795/sq mi 	3,396/km2 	47.62°N 122.35°W
19 	Denver[i] 	Colorado 	715,522 	600,158 	+19.22% 	153.1 sq mi 	396.5 km2 	4,674/sq mi 	1,805/km2 	39.76°N 104.88°W
20 	Washington[j] 	District of Columbia 	689,545 	601,723 	+14.60% 	61.1 sq mi 	158.2 km2 	11,286/sq mi 	4,358/km2 	38.90°N 77.01°W
21 	Nashville[k] 	Tennessee 	689,447 	601,222 	+14.67% 	475.8 sq mi 	1,232.3 km2 	1,449/sq mi 	559/km2 	36.17°N 86.78°W
22 	Oklahoma City 	Oklahoma 	681,054 	579,999 	+17.42% 	606.2 sq mi 	1,570.1 km2 	1,123/sq mi 	434/km2 	35.46°N 97.51°W
23 	El Paso 	Texas 	678,815 	649,121 	+4.57% 	258.4 sq mi 	669.3 km2 	2,627/sq mi 	1,014/km2 	31.84°N 106.42°W
24 	Boston 	Massachusetts 	675,647 	617,594 	+9.40% 	48.3 sq mi 	125.1 km2 	13,989/sq mi 	5,401/km2 	42.33°N 71.02°W
25 	Portland 	Oregon 	652,503 	583,776 	+11.77% 	133.5 sq mi 	345.8 km2 	4,888/sq mi 	1,887/km2 	45.53°N 122.65°W
26 	Las Vegas 	Nevada 	641,903 	583,756 	+9.96% 	141.8 sq mi 	367.3 km2 	4,527/sq mi 	1,748/km2 	36.22°N 115.26°W
27 	Detroit 	Michigan 	639,111 	713,777 	−10.46% 	138.7 sq mi 	359.2 km2 	4,608/sq mi 	1,779/km2 	42.38°N 83.10°W
28 	Memphis 	Tennessee 	633,104 	646,889 	−2.13% 	297.0 sq mi 	769.2 km2 	2,132/sq mi 	823/km2 	35.10°N 89.97°W
29 	Louisville[l] 	Kentucky 	633,045 	597,337 	+5.98% 	263.5 sq mi 	682.5 km2 	2,402/sq mi 	927/km2 	38.16°N 85.64°W
30 	Baltimore[m] 	Maryland 	585,708 	620,961 	−5.68% 	80.9 sq mi 	209.5 km2 	7,240/sq mi 	2,800/km2 	39.30°N 76.61°W
31 	Milwaukee 	Wisconsin 	577,222 	594,833 	−2.96% 	96.2 sq mi 	249.2 km2 	6,000/sq mi 	2,300/km2 	43.06°N 87.96°W
32 	Albuquerque 	New Mexico 	564,559 	545,852 	+3.43% 	187.3 sq mi 	485.1 km2 	3,014/sq mi 	1,164/km2 	35.10°N 106.64°W
33 	Tucson 	Arizona 	542,629 	520,116 	+4.33% 	241.0 sq mi 	624.2 km2 	2,252/sq mi 	870/km2 	32.15°N 110.87°W
34 	Fresno 	California 	542,107 	494,665 	+9.59% 	115.2 sq mi 	298.4 km2 	4,706/sq mi 	1,817/km2 	36.78°N 119.79°W
35 	Sacramento 	California 	524,943 	466,488 	+12.53% 	98.6 sq mi 	255.4 km2 	5,324/sq mi 	2,056/km2 	38.56°N 121.46°W
36 	Kansas City 	Missouri 	508,090 	459,787 	+10.51% 	314.7 sq mi 	815.1 km2 	1,615/sq mi 	624/km2 	39.12°N 94.55°W
37 	Mesa 	Arizona 	504,258 	439,041 	+14.85% 	138.7 sq mi 	359.2 km2 	3,636/sq mi 	1,404/km2 	33.40°N 111.71°W
38 	Atlanta 	Georgia 	498,715 	420,003 	+18.74% 	135.3 sq mi 	350.4 km2 	3,686/sq mi 	1,423/km2 	33.76°N 84.42°W
39 	Omaha 	Nebraska 	486,051 	408,958 	+18.85% 	133.2 sq mi 	345.0 km2 	3,433/sq mi 	1,325/km2 	41.26°N 96.04°W
40 	Colorado Springs 	Colorado 	478,961 	416,427 	+15.02% 	195.4 sq mi 	506.1 km2 	2,451/sq mi 	946/km2 	38.86°N 104.76°W
41 	Raleigh 	North Carolina 	467,665 	403,892 	+15.79% 	145.1 sq mi 	375.8 km2 	3,179/sq mi 	1,227/km2 	35.83°N 78.64°W")

(defn -main [& args]

  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state))
  )


(defn get-ss []
  (-> @spreadsheet-state
      :ss))

(defn save-ss []
  (with-open [w (io/writer "ss.edn")]
    (binding [*print-length* false
              *out* w]
      (pr (get-ss)))))

(defn load-ss  []
  (let [ss (with-open [rdr (io/reader "ss.edn")
                       pbr (PushbackReader. rdr)]
             (clojure.edn/read pbr))]
    (swap! spreadsheet-state
           (fn [m]
             (-> m
                 (assoc :ss ss)
                 (dissoc :cache)))))
  nil)
