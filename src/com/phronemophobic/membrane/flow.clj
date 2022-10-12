(ns com.phronemophobic.membrane.flow
  (:require [membrane.ui :as ui]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [membrane.basic-components :as basic]
            [membrane.component :as com
             :refer [defui
                     defeffect]]
            [membrane.skia :as backend]
            [com.phronemophobic.viscous :as iv
             :refer [inspect]]
            [com.rpl.specter :as spec]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.zip :as z]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            

            [com.phronemophobic.membrane.spreadsheet :as ss])
  (:import java.util.UUID
           java.time.Instant))

(defonce log (atom []))
(defn append-log [x]
  (swap! log conj x))
(defn clear-log []
  (reset! log []))
;; (add-tap append-log)



(defn rand-name []
  (apply str
         (repeatedly 8 #(rand-nth "abcdefghijklmnopqrstuvwxyz"))))


(s/def ::x number?)
(s/def ::y number?)
(s/def ::name string?)
(s/def ::id uuid?)

(s/def ::ui fn?)

{:textbox {:ui {:component  #'basic/textarea
                :init {:text "hello"}}
           :ports [:text-in :text]}}

(defui port-view [{:keys [port]}]
  (let [pid (::id port)]
    (ui/on
     :mouse-down
     (fn [_]
       [[::select-port pid]])
     (ui/horizontal-layout
      (ui/label (::id port))
      (let [value (::value port)]
        (ui/with-style
          (if value
            ::ui/style-fill
            ::ui/style-stroke)
          (with-meta
            (ui/rectangle 10 10)
            {::port port})))))))




(def flow-eval-ns *ns*)
(defeffect ::init-ss [$state $machine machine-id]
  (let [state (dispatch! :get $state)]
    (when-let [ss-chan (:ss-chan state)]
      (async/close! (:ss-chan state))
      (dispatch! :update $state
                 dissoc :ss-chan :result-thread)))

  (let [eval-ns flow-eval-ns
        ss-chan (async/chan (async/sliding-buffer 1))
        result-thread (let [binds (clojure.lang.Var/getThreadBindingFrame)]
                        (doto (Thread. (fn []
                                         (clojure.lang.Var/resetThreadBindingFrame binds)
                                         (loop []
                                           (when-let [ss (async/<!! ss-chan)]
                                             (try
                                               (let [state (dispatch! :get $state)
                                                     cache (get state :cache {})
                                                     side-effects (get state :side-effects {})

                                                     ns-info (get state :ns-info)
                                                     _ (assert ns-info)
                                                     cache (if (not= (get state :last-ns-info)
                                                                     ns-info)
                                                             (do (ss/sync-ns! ns-info)
                                                                 ;; empty cache
                                                                 {})
                                                             ;; else
                                                             cache)
                                                     eval-ns (the-ns (:name ns-info))
                                                     
                                                     {results :results
                                                      next-cache :cache
                                                      side-effects :side-effects} (ss/calc-spreadsheet eval-ns cache side-effects ss)]
                                                 (dispatch! :update $state
                                                            assoc
                                                            :results results
                                                            :side-effects side-effects
                                                            :last-ns-info ns-info
                                                            :cache next-cache)

                                                 (let [port-vals (into {}
                                                                       (comp
                                                                        (map (fn [m]
                                                                               (let [name (-> m :name keyword)
                                                                                     val (get results (:id m))]
                                                                                 [[:out name] val]))))
                                                                       ss)]
                                                   (dispatch! :update $machine
                                                              update ::ports
                                                              (fn [m]
                                                                (reduce (fn [m k]
                                                                          (assoc-in m [k ::id] k))
                                                                        m
                                                                        (keys port-vals))))

                                                   

                                                   (dispatch! :update $machine
                                                              update ::ports
                                                              (fn [m]
                                                                (reduce dissoc
                                                                        m
                                                                        (->> (keys m)
                                                                             (remove #(contains? port-vals %))))))

                                                   ;; add in ports for side effect rows
                                                   (dispatch! :update $machine
                                                              update ::ports
                                                              (fn [m]
                                                                (reduce (fn [m row]
                                                                          (if (= :side-effect (:editor row))
                                                                            (let [k [:in (-> row :name keyword)]]
                                                                              (assoc-in m [k ::id] k))
                                                                            m))
                                                                        m
                                                                        ss)))

                                                   (doseq [[k v] port-vals]
                                                     (dispatch! :put-port machine-id
                                                                k (iv/-unwrap v))))
                                                 
                                                 
                                                 (#'backend/glfw-post-empty-event))
                                               (catch Throwable e
                                                 (prn e)))
                                             (recur))))
                                       "Result-Thread")
                          ;; Not sure what the best way to
                          ;; determine the desired priority is.
                          ;; The basic idea is to use a lower
                          ;; priority than normal threads in
                          ;; case the thread gets into an infinite loop
                          (.setPriority (max Thread/MIN_PRIORITY
                                             (- Thread/NORM_PRIORITY 2)))))]
    (dispatch! :update $state
               assoc
               :ss-chan ss-chan
               :result-thread result-thread
               )

    (.start result-thread)

    (dispatch! :add-watch
               [::update-results $state]
               (fn check-update-results [key ref old new]
                 (let [old-ss (spec/select-one [(com/path->spec $state) (spec/keypath :ss)]
                                               old)
                       new-ss (spec/select-one [(com/path->spec $state) (spec/keypath :ss)]
                                               new)]
                   (when-not (= old-ss new-ss)
                     (async/put! ss-chan new-ss)))))


    nil)
  )



(defeffect ::load-spreadsheet [$state machine]
  (let [ss (:ss (dispatch! :get $state))
        k->id (into {}
                    (map (juxt #(-> % :name keyword)
                               :id))
                    ss)]
    (doseq [pid (keys (::ports machine))
            :when (and (vector? pid)
                       (= :in (first pid)))
            :let [k (second pid)
                  v (dispatch! :take-port (::id machine) pid)
                  id (k->id k)
                  ts (-> (Instant/now)
                         (.toEpochMilli))]
            :when v]
      (dispatch! :update $state
                 assoc-in [:side-effects id]
                 {:result (iv/wrap v)
                  :side-effect-ts ts})
      (dispatch! :update $state
                 (fn [state]
                   (spec/setval [:ss
                                 spec/ALL
                                 (fn [m]
                                   (= id (:id m)))
                                 :side-effect-ts]
                                ts
                                state))))))

(defui spreadsheet-machine-view [{:keys [sstate machine init?]}]
  (let [xtdb (:xtdb context)]
    (ui/vertical-layout
     (when (not init?)
       (basic/button {:text "init"
                      :on-click (fn []
                                  [[::init-ss $sstate $machine (::id machine)]
                                   [:set $init? true]])}))
     (basic/button {:text "load"
                    :on-click
                    (fn []
                      [[::load-spreadsheet $sstate machine]])})
     (ss/spreadsheet-editor {:ss (:ss sstate)
                             :xtdb xtdb
                             :ns-info (:ns-info sstate)
                             :results (:results sstate)}))))

(defn new-spreadsheet-machine! []
  {::x 500
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:sstate {:ss []
                                   :ns-info
                                   {:name (symbol (str "foo" "." (rand-name)))
                                    :require '([membrane.ui :as ui
                                                :refer [vertical-layout
                                                        horizontal-layout]]
                                               [clojure.java.io :as io]
                                               clojure.edn
                                               clojure.set
                                               [clojure.string :as str]
                                               [clojure.data.json :as json])
                                    :import '(java.io.PushbackReader
                                              java.time.Instant)}}
                          :init? false}
         ::component spreadsheet-machine-view}
   ::ports {}
   ::name (rand-name)})



(defui text-machine-view [{:keys [text]}]
  (let [focus (:focus context)]
    (ui/wrap-on
     :key-press
     (fn [handler s]
       (when (= focus $text)
         (if (= s :enter)
           [[:set $text ""]
            [:put-port :text text]]
           (handler s))))
     (basic/textarea {:text text
                      :focus focus}))))

(defeffect ::load-ports [machine port-refs]
  (doseq [[$ref port] port-refs]
    (when-let [val (dispatch! :take-port (::id machine) port)]
      (dispatch! :set $ref val))))

(defeffect ::unload-ports [machine port-refs]
  (doseq [[port val] port-refs]
    (dispatch! :put-port (::id machine) port val)))

(defeffect ::print-ingest [$obj machine-id port-name]
  (let [val (dispatch! :take-port machine-id port-name)]
    (dispatch! :set $obj (iv/wrap val))
    ;;(prn val)
    ))

(defui print-machine-view [{:keys [obj machine]}]
  (let [width (get extra :width 20)
        height (get extra :height 1)
        inspector-extra (get extra [:inspector (::id machine)])]
   (ui/vertical-layout
    (ui/on
     :mouse-down
     (fn [_]
       [[::print-ingest $obj (::id machine) :obj]])
     (basic/button {:text "ingest"}))
    (iv/inspector {:obj obj
                   :width width
                   :height height}))))

(defn new-text-machine! []
  {::x 250
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:text "hello"}
         ::component text-machine-view}
   ::ports {:text {::id :text}}
   ::name (rand-name)})

(defn new-print-machine! []
  {::x 500
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:obj (iv/wrap
                                '{::x 40
                                  ::y 40
                                  ::id (java.util.UUID/randomUUID)
                                  ::ui {::initial-state {:obj (iv/wrap )}
                                        ::component print-machine-view}
                                  ::ports {:obj {::id :obj}}
                                  ::name (rand-name)})}
         ::component print-machine-view}
   ::ports {:obj {::id :obj}}
   ::name (rand-name)})


;; transfer tool
(comment

  (defn new-transfer-machine! []
    {::x 250
     ::y 250
     ::id (java.util.UUID/randomUUID)
     ::ui {::initial-state {:from-search ""
                            :to-search ""
                            :transfer-social? true
                            :status ""
                            :from-info nil
                            :to-info nil
                            :game :monsters}
           ::component #'com.phronemophobic.pillar.gmail/transfer-tool}
     ::ports {:from-search {::id :from-search}
              :to-search {::id :to-search}}
     ::name (rand-name)})

  )

(defeffect ::load-machine [$machine-ui-state machine]
  (let [mid (::id machine)
        m (into {}
                (comp
                 (map val)
                 (map ::id)
                 (map (fn [k]
                        [k (dispatch! :take-port mid k)])))
                (::ports machine))]
    (dispatch! :update $machine-ui-state merge m)))

(defeffect ::unload-machine [machine-ui-state machine]
  (let [mid (::id machine)

        pids (into []
                   (comp
                    (map val)
                    (map ::id))
                   (::ports machine))]
    (doseq [pid pids]
      (dispatch! :put-port mid pid (get machine-ui-state pid)))))


#_(defn new-selector-machine! []
  {::x 250
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:emails nil
                          :selected nil}
         ::component #'gmail/email-selector}
   ::ports {:emails {::id :emails}
            :selected {::id :selected}}
   ::name (rand-name)}
  )

#_(defn new-thread-machine! []
  {::x 250
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:thread nil}
         ::component #'gmail/thread-ui}
   ::ports {:thread {::id :thread}}
   ::name (rand-name)}
  )

(defui machine-container [{:keys [machine]}]
  (let [x (get machine ::x 0)
        y (get machine ::y 0)
        machine-ui (::ui machine)
        machine-ui-state (get extra ::machine-ui-state
                              (::initial-state machine-ui))
        machine-ui-component (::component machine-ui)]
    (with-meta
      (ui/on
       :put-port
       (fn [port-name val]
         [[:put-port (::id machine) port-name val]])
       ::select-port
       (fn [pid]
         [[::select-port (::id machine) pid]])
       (ui/translate x y
                     (ui/vertical-layout
                      (ui/horizontal-layout
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[::select (::id machine)]])
                        (ui/with-style :membrane.ui/style-stroke
                          [(ui/path [5 0]
                                    [5 10])
                           (ui/path [0 5]
                                    [10 5])]))
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[:delete $machine]])
                        (ui/with-style :membrane.ui/style-stroke
                          [(ui/path [0 0]
                                    [10 10])
                           (ui/path [0 10]
                                    [10 0])]))
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[::load-machine $machine-ui-state machine]])
                        (ui/with-style :membrane.ui/style-stroke
                          [(ui/path [0 5]
                                    [5 0]
                                    [10 5])
                           (ui/path [5 0]
                                    [5 10])]))
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[::unload-machine machine-ui-state machine]])
                        (ui/with-style :membrane.ui/style-stroke
                          [(ui/path [0 5]
                                    [5 10]
                                    [10 5])
                           (ui/path [5 0]
                                    [5 10])])))
                      (ui/label (::name machine))
                      (apply
                       ui/vertical-layout
                       (for-kv [[_k port] (::ports machine)]
                         (port-view {:port port})))
                      (when machine-ui-component
                        (let [machine-extra (get extra ::machine-extra)]
                          (machine-ui-component
                           (into (assoc machine-ui-state
                                        :machine machine
                                        :$machine $machine
                                        :extra machine-extra
                                        :$extra $machine-extra
                                        :context context
                                        :$context $context)
                                 (comp (map
                                        (fn [k]
                                          [(keyword (str "$" (name k)))
                                           [$machine-ui-state k]])))
                                 (keys machine-ui-state)))))
                      )))
      {::machine machine})))

(defn stoggle [s x]
  (if (contains? s x)
    (disj s x)
    (conj s x)))

(defeffect ::move-selection [$machines selection mx my]
  (dispatch! :update $machines
             (fn [machines]
               (spec/transform [(spec/submap selection) spec/MAP-VALS]
                               #(assoc %
                                       ::x mx
                                       ::y my)
                               machines))))


(defeffect ::activate-connection [conn]
  (let [[[from-mid from-pid]
         [to-mid to-pid]] conn
        val (dispatch! :take-port from-mid from-pid)]
    (when val
      (dispatch! :put-port to-mid to-pid val))))

(defeffect :put-port* [$machines machine-id port-name val]
  (dispatch! :update $machines
             (fn [machines]
               (spec/setval (spec/keypath machine-id ::ports port-name ::value)
                            (iv/wrap val)
                            machines))))

(defeffect :take-port* [$machines machine-id port-name]
  (let [val (spec/select-one (spec/keypath machine-id ::ports port-name ::value)
                             (dispatch! :get $machines))]
    (dispatch! :update $machines
               (fn [machines]
                 (spec/setval (spec/keypath machine-id ::ports port-name ::value)
                              spec/NONE
                              machines)))
    (when val
     (iv/-unwrap val))))

(declare with-port-lines add-machine! init-state!)


(defeffect ::add-machine [$machines machine [ox oy]]
  (dispatch! :update $machines
             assoc (::id machine) (assoc machine
                                         ::x (+ ox 300)
                                         ::y (+ oy 300))))

(defui gview [{:keys [machines machine-registry selection temp-port connections]}]
  (let [offset (get extra :offset [0 0])]
    [
     (basic/workspace
      {:scroll-bounds [1800 1200]
       :offset offset
       :$body nil
       :body
       (with-port-lines
         connections
         (ui/wrap-on
          
          :mouse-up
          (fn [handler pos]
            (let [intents (handler pos)]
              (cons [:set $selection #{}]
                    intents)))

          :mouse-move-global
          (fn [handler [mx my :as pos]]
            (if (seq selection)
              [[::move-selection $machines selection mx my]]
              (handler pos)))

          (ui/on

           ::select-port
           (fn [mid pid]
             (if temp-port
               [[:set $temp-port nil]
                (when (not= temp-port [mid pid])
                  [:update $connections
                   stoggle [temp-port [mid pid]]])]
               [[:set $temp-port [mid pid]]]))

           ::select
           (fn [mid]
             [[:update $selection conj mid]])
           (vec
            (for-kv [[_id machine] machines]
              (machine-container {:machine machine}))))))})
     (apply
      ui/vertical-layout
      (ui/label (pr-str selection))
      (ui/label (pr-str temp-port))

      (for [machine-meta (vals machine-registry)]
        (basic/button {:text (:name machine-meta)
                       :on-click
                       (fn []
                         [[::add-machine $machines ((:new machine-meta)) offset]])})))]))

(defonce app-state (atom {}))

(defn init-state!
  ([]
   (init-state! app-state))
  ([atm]
   (reset! atm
           {:machines {}
            ::com/context {:xtdb @ss/xnode}
            :machine-registry {::text {:new new-text-machine!
                                       :name "text"}
                               ::print {:new new-print-machine!
                                        :name "print"}
                               ::spreadsheet {:new new-spreadsheet-machine!
                                              :name "spreadsheet"}
                               }
            :connections #{}
            :selection #{}}))) 

(defonce init-state!' (init-state! app-state))



(defn add-machine!
  ([] (add-machine! (new-text-machine!) ))
  ([machine]
   (add-machine! machine app-state ))
  ([machine atm]
   (swap! atm update-in [:machines]
          assoc (::id machine) machine)))

(defn register-machine!
  ([machine-meta]
   (register-machine! machine-meta app-state))
  ([machine-meta atm]
   (swap! atm update-in [:machine-registry]
          assoc (::id machine-meta) machine-meta)))


(defn show! []
  (let [
        $machines (list 'get :machines)
        handler
        (fn dispatch!
          ([& args]
           (case (first args)
             :take-port
             (let [[machine-id port-name] (next args)]
               (dispatch! :take-port*
                          $machines
                          machine-id
                          port-name))

             :add-watch
             (let [[key fn] (next args)]
               (add-watch app-state key fn))
             
             :put-port
             (let [[machine-id port-name val] (next args)]
               (dispatch! :put-port*
                          $machines
                          machine-id
                          port-name
                          val))

             ;; else
             (apply com/dispatch!* app-state dispatch! args))))]
    (backend/run (com/make-app #'gview app-state handler)))
  )

(comment
  
  (show!)
  )

(def g
  (-> (uber/digraph [[:email-data :out] [:search-bar :data]]
                    [[:search-bar :data] :search-bar]
	            [:search-bar [:search-bar :selected]]
                    [[:search-bar :selected] [:transfer :in]])
      (uber/add-attr :search-bar :foo :bar)
      (uber/add-attr :search-bar :foo :baz)
      ))


(defui filtered-list [{:keys [search elems]}])
(defui view-item [{:keys [item]}])
{:search-bar {:component basic/textarea
              :initial-state {:text "adsf"}}

 :filtered-list {:component filtered-list
                 :initial-state {:search ""
                                 :elems ["asdf"
                                         "aewf"
                                         "zxcvzxvc"]}}
 :detail-view {:component view-item
               :initial-state {:item nil}}
 }







(uber/edges g)

(defn show-graph [g]
  (uber/viz-graph g {:save {:filename "graph.png" :format :png}})
  (backend/run (constantly (ui/image "graph.png")))
  )

(comment
  
  

  

  ,)


(defn is-diagram-box? [elem]
  (instance? membrane.ui.Rectangle elem))

(defn elem-zip [elem]
  (z/zipper (constantly true)
            ui/children
            (fn [elem _] elem)
            elem))

(defn origin->global-coords [zelem]
  (loop [zip zelem
         [x y] [0 0]]
    (if zip
      (recur (z/up zip)
             (let [[ox oy] (ui/origin (z/node zip))]
               [(+ x ox) (+ y oy)]))
      [x y])))

(defn top-left [elem]
  (let [zelem
        ;; find the box in the element tree
        (loop [zip (elem-zip elem)]
          (if (z/end? zip)
            nil
            (if (is-diagram-box? (z/node zip))
              zip
              (recur (z/next zip)))))
        ;; translate its origin to global coordinates
        pt (loop [zip zelem
                  [x y] [0 0]]
             (if zip
               (recur (z/up zip)
                      (let [[ox oy] (ui/origin (z/node zip))]
                        [(+ x ox) (+ y oy)]))
               [x y]))]
    pt))



(defn zip-seq [elem]
  (->> (elem-zip elem)
       (iterate z/next)
       (take-while #(not (z/end? %)))))


(defn port-coords [view]
  (->> (zip-seq view)
       (partition-by #(::machine
                       (meta
                        (z/node %))))
       (drop 1)
       (partition 2)
       (mapcat (fn [[[machine-view] zviews]]
                 (let [zport-views (->> zviews
                                        (filter #(::port (meta (z/node %)))))
                       mid (-> (z/node machine-view)
                               meta
                               ::machine
                               ::id)]
                   (for [zport-view zport-views]
                     [[mid
                       (-> zport-view
                           z/node
                           meta
                           ::port
                           ::id)]
                      (-> zport-view
                          origin->global-coords)]
                     ))))
       (into {})))

(defn with-port-lines [connections view]
  (let [coords (port-coords view)]
    [
     view
     (into []
           (map (fn [[c1 c2 :as conn]]
                  
                  (let [[x1 y1 :as p1] (coords c1)
                        [x2 y2 :as p2] (coords c2)]
                    (when (and p1 p2)
                      [
                       (ui/with-style ::ui/style-stroke
                         (ui/path p1
                                  p2))
                       (ui/translate (int (/ (+ x1 x2) 2))
                                     (int (/ (+ y1 y2) 2))
                                     (ui/button "move"
                                                (fn []
                                                  [[::activate-connection
                                                    conn]])))]))))
           connections)]))

(defn left-most-point [elem pred]
  (->> (zip-seq elem)
       (filter #(is-diagram-box? (z/node %)))
       (reduce (fn [x zelem]
                 (let [[ox oy] (origin->global-coords zelem)]
                   (if (< ox x)
                     ox
                     x)))
               Long/MAX_VALUE)))
;; TODO
;; - make a way to draw these
;; - run the graph?
;; - add clipboard
;; - ...
;; - profit



(def myg (uber/digraph [[:email-data :out] [:search-bar :data]]
                           [[:search-bar :data] :search-bar]
	                   [:search-bar [:search-bar :selected]]
                           [[:search-bar :selected] [:transfer :in]]))

(alg/topsort )

(defprotocol IFlowNode
  :extend-via-metadata true
  (run-node [node g]))

(defprotocol IFlowEdge
  :extend-via-metadata true
  (run-edge [edge g]))


(uber/out-edges myg [:email-data :out])

(defn run-graph [g]
  (let [nodes (alg/topsort g)]

    (reduce (fn [g node]
              (let [g (run-node node g)]
                (reduce (fn [g edge]
                          (run-edge edge g))
                        g
                        (uber/out-edges g node))))
            g
            nodes)))


#_(def myg (uber/digraph [:text [:text :out]]
                       [[:text :out] [:print :in]]
                       [[:print :in] :print]))

(defn transform-machine-run [ports]
  (when (ready? :out)
   (when-let [v (take-port :in)]
     (put-port :out))))

(defn attach-transform [chans]
  
  (go
    (loop []
      (let [vals (loop [chans (seq chans)
                        vals []]
                   (if chans
                     (recur (next chans)
                            (conj vals (<! (first chans))))
                     vals))]
        (>! out (apply f vals)))
      (recur)))
  )
