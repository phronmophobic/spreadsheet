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
           java.util.concurrent.Executors
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


(defn ->graph
  "Produces a graph from @app-state"
  [{:keys [machines connections]}]
  (let [machine-edges
        (into #{}
              (mapcat (fn [[[from-id from-port :as from]
                            [to-id to-port :as to]]]
                        [[from-id from]
                         [to to-id]]))
              connections)

        ]
    (apply uber/digraph
           (concat machine-edges
                   connections
                   (keys machines))))
  )

(defn run-flow [{:keys [machines connections dispatch!] :as state}]
  (let [g (->graph state)
        sorted (alg/topsort g)
        machine-ids (->> sorted
                         (filter uuid?))]
    (doseq [mid machine-ids]
      (let [$machine `[(~'get :machines) (~'get ~mid)]]
        (dispatch! ::run-machine $machine))
      (doseq [{port-node :dest} (uber/out-edges g mid)
              {:keys [src dest]} (uber/out-edges g port-node)
              :let [conn [src dest]]]
        (dispatch! ::activate-connection conn)))))


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
                      :$text $text
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

(defui print-machine-view [{:keys [machine]}]
  (let [state (:state machine)
        obj (get state :obj (iv/wrap nil))]
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
                      :height height})))))


(defui inc-machine [{:keys [machine]}]
  (ui/label (-> machine :state :num)))

(defn new-inc-machine! []
  {::x 250
   ::y 250
   ::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:num 42
                          :out nil}
         ::component inc-machine}
   ::ports {:num {::id :num}
            :out {::id :out}}
   

   ::name (rand-name)

   :in-ports #{:num}
   :out-ports #{:out}
   :state {}
   :f (fn [{{:keys [num]} :inbox

            :as state}]
        (println "going up" num)
        (assoc state
               :outbox {:out (inc num)}
               :inbox {}
               :num (inc num)))
   })


(defn new-buf-machine! []
  {::id (java.util.UUID/randomUUID)
   ::ui {::initial-state {:num 42
                          :out nil}
         ::component inc-machine}
   ::ports {:num {::id :num}
            :out {::id :out}}
   

   ::name (rand-name)

   :in-ports #{:num}
   :out-ports #{:out}
   :state {}
   :f (fn [{:keys [in] :as state}]
        {:out (inc num)})
   })

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
   ::ui {::initial-state {:state
                          {:obj (iv/wrap
                                 '{::x 40
                                   ::y 40
                                   ::id (java.util.UUID/randomUUID)
                                   ::ui {::initial-state {:obj (iv/wrap )}
                                         ::component print-machine-view}
                                   ::ports {:obj {::id :obj}}
                                   ::name (rand-name)})}}
         ::component print-machine-view}
   :in-ports #{:obj}
   :f (fn [{{:keys [obj]} :inbox
            :as state}]
        (assoc state
               :inbox {}
               :obj (iv/wrap obj)))
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
        machine-ui-state machine
        #_(get extra ::machine-ui-state
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
                                    [5 10])]))
                       (ui/on
                        :mouse-down
                        (fn [_]
                          [[::run-machine $machine]])
                        (ui/with-style :membrane.ui/style-stroke
                          [(ui/path [5 0]
                                    [10 5]
                                    [5 10])
                           (ui/path [0 5]
                                    [10 5])])))
                      (ui/label (::name machine))
                      (apply
                       ui/vertical-layout
                       (for [[_k port] (::ports machine)]
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


(defn port-path [machine-id port-name]
  (spec/keypath machine-id ::ports port-name ::value))

(defeffect ::activate-connection* [$machines conn]
  (let [[[from-mid from-pid]
         [to-mid to-pid]] conn
        from-path (port-path from-mid from-pid)
        to-path (port-path to-mid to-pid)]
    (dispatch! :update $machines
               (fn [machines]
                 (if (spec/select-one to-path machines)
                   ;; already full
                   machines
                   (if-let [val (spec/select-one from-path machines)]
                     (->> machines
                          (spec/setval from-path spec/NONE)
                          (spec/setval to-path val))
                     machines))))))


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
             assoc (::id machine)
             (merge
              (assoc machine
                     ::x (+ ox 300)
                     ::y (+ oy 300))
              (-> machine ::ui ::initial-state))))

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
            (for [id (keys machines)]
              (machine-container {:machine (get machines id)}))))))})
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

(defonce run-flow-chan nil)
(defonce run-flow-thread nil)
(defn stop-flow! []
  (when run-flow-chan
    (async/close! run-flow-chan)))
(defn start-flow! []
  (stop-flow!)
  (alter-var-root #'run-flow-chan
                  (fn [_]
                    (async/chan (async/dropping-buffer 1))))
  (alter-var-root #'run-flow-thread
                  (fn [_]
                    (prn "hi")
                    (async/thread
                      (try
                        (prn "Start")
                        (loop [to nil]
                          (let [[val port]
                                (async/alts!! (if to
                                                [to run-flow-chan]
                                                [run-flow-chan]))]
                            (cond

                              (or (= port to)
                                  (and (= port run-flow-chan)
                                       val))
                              (do
                                (prn "running flow")
                                (run-flow @app-state)
                                (recur (async/timeout 500)))

                              ;; channel closed
                              (and (= port run-flow-chan)
                                   (nil? val))
                              nil

                              :else
                              (do
                                (prn "prn stopping flow")
                                (recur nil)))))
                        (catch Exception e
                          (prn e))
                        (finally
                          (prn "quitting flow runner")))))
                  )
  (async/put! run-flow-chan true))


(defn init-state!
  ([]
   (init-state! app-state))
  ([atm]
   (let [flow-executor (Executors/newSingleThreadExecutor)
         $flow-future (list 'get ::flow-future)

         $machines (list 'get :machines)
         dispatch!
         (fn dispatch!
           ([effects]
            (try
              (if (keyword? effects)
                ;; add backwords compatibility for 0 arg intent
                (com/dispatch!* app-state dispatch! effects)
                (run! #(apply dispatch! %) effects))
              (catch Exception e
                (prn "bad effects: " effects))))
           ([effect-type & args]
            (case effect-type
              :take-port
              (let [[machine-id port-name] args]
                (dispatch! :take-port*
                           $machines
                           machine-id
                           port-name))

              :add-watch
              (let [[key fn] args]
                (add-watch app-state key fn))

              :put-port
              (let [[machine-id port-name val] args]
                (dispatch! :put-port*
                           $machines
                           machine-id
                           port-name
                           val))

              ::activate-connection
              (let [conn (first args)]
                (dispatch! ::activate-connection*
                           $machines
                           conn))

              ::start-flow
              (fn []
                (dispatch! :update
                           $flow-future
                           (fn [future]
                             (if future
                               future
                               )))
                )

              ;; else
              (apply com/dispatch!* app-state dispatch! effect-type args))))

         ]
     (reset! atm
             {:machines {}
              :dispatch! dispatch!
              ::flow-executor flow-executor
              ::com/context {:xtdb @ss/xnode}
              :machine-registry {::text {:new new-text-machine!
                                         :name "text"}
                                 ::print {:new new-print-machine!
                                          :name "print"}
                                 ::inc {:new new-inc-machine!
                                        :name "inc"}
                                 ::spreadsheet {:new new-spreadsheet-machine!
                                                :name "spreadsheet"}
                                 }
              :connections #{}
              :selection #{}})))) 

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
  (backend/run (com/make-app #'gview app-state (:dispatch! @app-state)))
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
  (uber/viz-graph g {:save {:filename "graph3.png" :format :png}})
  (backend/run (constantly (ui/image "graph3.png")))
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



(defeffect ::machine-pre [$machine]
  (let [machine (dispatch! :get $machine)
        id (::id machine)
        in-ports (:in-ports machine)
        inbox (get machine :inbox)
        inbox (reduce (fn [inbox port]
                      (if (get inbox port)
                        inbox
                        (if-let [v (dispatch! :take-port id port)]
                          (assoc inbox port v)
                          inbox)))
                    inbox
                    in-ports)]
    (dispatch! :update $machine #(assoc % :inbox inbox))))

(defeffect ::machine-visit [$machine]
  (let [machine (dispatch! :get $machine)
        {:keys [f in-ports out-ports]} machine
        inbox (get machine :inbox)
        outbox (get machine :outbox)]
    (when (and (every? #(get inbox %) in-ports)
               (every? #(not (get outbox %)) out-ports))
      (dispatch! :update $machine f))))

(defeffect ::machine-post [$machine]
  (let [machine (dispatch! :get $machine)
        {:keys [out-ports]} machine
        id (::id machine)
        outbox (:outbox machine)
        outbox (reduce
                (fn [outbox [k v]]
                  (prn id k v)
                  (if (dispatch! :put-port id k v)
                    (dissoc outbox k)
                    outbox))
                outbox
                outbox)]
    (dispatch! :set [$machine :outbox] outbox)))

(defeffect ::run-machine [$machine]
  (when (:f (dispatch! :get $machine))
    (dispatch! ::machine-pre $machine)
    (dispatch! ::machine-visit $machine)
    (dispatch! ::machine-post $machine)))

(defmulti test-dispatch!
  (fn [type & args]
    type))


(def test-state (atom {:ports {:foo 42}
                       :machines {:my-machine {:in-ports [:in]
                                               :out-ports #{:out}
                                               ::id :my-machine
                                               :state {}
                                               :f (fn [{:keys [in]}]
                                                    (println "going up" in)
                                                    {:out (inc in)})}}}))

(defmethod test-dispatch! :take-port [_ machine-id port]
  (let [[old new] (swap-vals! test-state update-in [:ports machine-id] dissoc port)]
    (get-in old [:ports machine-id port])))

(defmethod test-dispatch! :put-port [_ machine-id port val]
  (let [[old new] (swap-vals! test-state update-in [:ports machine-id port]
                             (fn [old-val]
                               (if old-val
                                 old-val
                                 val)))]
    (nil? (get-in old [:ports machine-id port])) ))

(defmethod test-dispatch! :set [_ & args]
  (apply com/default-set test-state args))

(defmethod test-dispatch! :update [_ & args]
  (apply com/default-update test-state args))

(defmethod test-dispatch! :get [_ & args]
  (apply com/default-get test-state args))

(defmethod test-dispatch! :delete [_ & args]
  (apply com/default-delete test-state args))

(defmethod test-dispatch! ::run-machine [_ $machine]
  (effect-run-machine test-dispatch! $machine))

(defmethod test-dispatch! ::machine-pre [_ $machine]
  (effect-machine-pre test-dispatch! $machine))

(defmethod test-dispatch! ::machine-visit [_ $machine]
  (effect-machine-visit test-dispatch! $machine))

(defmethod test-dispatch! ::machine-post [_ $machine]
  (effect-machine-post test-dispatch! $machine))



(comment
  (test-dispatch! :put-port :my-machine :in 42)
  (test-dispatch! ::run-machine [:machines :my-machine])
  (test-dispatch! :take-port :my-machine :out )

  (test-dispatch! :take-port :my-machine :in)
  ,)





