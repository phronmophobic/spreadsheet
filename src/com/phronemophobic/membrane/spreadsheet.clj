(ns com.phronemophobic.membrane.spreadsheet
  (:require [membrane.ui :as ui
             :refer [vertical-layout
                     horizontal-layout]]
            [clojure.core.async :as async]
            clojure.set
            [membrane.basic-components :as basic]
            [membrane.component :as com
             :refer [defui
                     defeffect]]
            [membrane.skia :as backend]
            [com.phronemophobic.membrane.pretty-view :refer [pretty]]
            ;; [membrane.java2d :as backend]
            ;; [membrane.skija :as backend]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [membrane.components.code-editor.code-editor :as code-editor]
            [liq.buffer :as buffer])
  (:gen-class))


(def pretty-memo (memoize pretty))

(defeffect ::inspect-result [result]
  (tap> result))

(defui spreadsheet-row [{:keys [row result]}]
  (horizontal-layout
   (basic/textarea {:text (:name row)})
   (code-editor/text-editor {:buf (:src row)})
   (let [pv (ui/no-events
             (pretty-memo result))
         [w h] (ui/bounds pv)]
     (ui/on
      :mouse-down
      (fn [_]
        [[::inspect-result result]])
      (ui/scissor-view [0 0]
                       [(min w 400)
                        (min h 200)]
                       pv)))))

(defeffect ::add-spreadsheet-row [$spreadsheet]
  (dispatch! :update $spreadsheet conj
             {:name (name (gensym))
              :id (gensym)
              :src (buffer/buffer "42" {:mode :insert})}))

(defui spreadsheet-editor [{:keys [ss results]}]
  (vertical-layout
   (basic/button {:text "+"
                  :on-click (fn []
                              [[::add-spreadsheet-row $ss]])})
   (basic/scrollview
    {:scroll-bounds [1200 800]
     :body
     (apply vertical-layout
            (for [row ss]
              (spreadsheet-row {:row row
                                :result (get results (:id row))})))
     })))

(def spreadsheet-state (atom {}))


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
(defn run-results []
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
                                               (swap! spreadsheet-state assoc :results (calc-spreadsheet ss))
                                               (catch Exception e))
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
    nil))

(def my-spreadsheet
  [{:name "s"
    :id 1
    :src (buffer/buffer "(+ 1 2)" {:mode :insert})}
   {:name "s"
    :id 2
    :src (buffer/buffer "(+ s 2)" {:mode :insert})}
   {:name "t"
    :id 3
    :src (buffer/buffer "1234" {:mode :insert})}
   {:name "u"
    :id 4
    :src (buffer/buffer "(+ s t)" {:mode :insert})}
   ])
(swap! spreadsheet-state
       assoc :ss my-spreadsheet)

(defn current-spreadsheet []
  (:ss @spreadsheet-state))
(defn run []
  ;; (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state))
  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state)))


(defn ->str [buf-or-s]
  (if (map? buf-or-s)
    (buffer/text buf-or-s)
    buf-or-s))

(defn parse-row [row]
  (let [src (:src row)
        row (assoc row :src (->str src))
        [err form] (try
                     [nil (read-string (:src row))]
                     (catch Exception e
                       [e nil]))]
    (if err
      (assoc row :err err)
      (let [row (assoc row :form form)
            [err sym] (try
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
(defn calc-spreadsheet [ss]
  (let [[env ss] (process-spreadsheet ss
                                      [(complete-env parse-row)
                                       process-make-fn
                                       add-deps])]
    (loop [ss (seq ss)
           vals {}
           bindings {}]
      (if-not ss
        vals
        (let [row (first ss)
              {:keys [sym form err]} row]
          (if err
            (assoc vals (:id row) err)
            (let [binding-code (into []
                                     (mapcat
                                      (fn [[sym _]]
                                        [sym `(get *spreadsheet-bindings* (quote ~sym))]))
                                     bindings)
                  eval-code `(let ~binding-code
                               ~form)
                  ;; _ (prn eval-code)
                  [err result]
                  (binding [*spreadsheet-bindings* bindings]
                    (try
                      [nil (eval eval-code)]
                      (catch Exception e
                        [e nil])))]
              (if err
                (assoc vals (:id row) err)
                (recur (next ss)
                       (assoc vals (:id row) result)
                       (assoc bindings sym result))))
            ))))))

(defn -main [& args]
  (run-results)
  (backend/run (com/make-app #'spreadsheet-editor spreadsheet-state))
  )

