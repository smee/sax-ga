(ns sax-ga
  (:require [clojure.zip :as zip]))

(defn matches [sax-str motif]
  (boolean (re-matches (re-pattern (str ".*" motif ".*")) sax-str)))

(defn matches-at [sax-str motif pos leeway]
  (let [motif-len (count motif)
        input-len (count sax-str)
        start-pos (max 0 (- pos leeway))
        end-pos (min input-len (+ start-pos motif-len leeway))]
    (matches (subs sax-str start-pos end-pos) motif)))

(def operators
  {:and {:type :logical
         :op :and}
   :or {:type :logical
        :op :or}
   :not {:type :logical
         :op :not}
   :matches {:type :logical
             :op :matches}
   :matches-at {:type :logical
                :op :matches-at}
   :motif {:type :string
           :op :motif}
   :position {:type :integer
              :op :position}
   :leeway {:type :integer
              :op :leeway}})

(def non-terminal [:and :or :not :matches :matches-at])
(def terminal [:motif :position])

(defn random-motif-character [alphabet-size]
  (char (+ (int \a) (rand-int alphabet-size))))

(defn generate
  ([] (generate {:alphabet-size 5 
                 :motif-min-length 3 
                 :motif-max-length 6
                 :max-position 10}))
  ([opts] (generate (operators (rand-nth non-terminal)) opts))
  ([{:keys [op]} {:keys [alphabet-size motif-min-length motif-max-length max-position current-depth max-depth]
                  :as opts
                  :or {alphabet-size 5 
                       motif-min-length 3 
                       motif-max-length 6
                       max-position 10
                       current-depth 0
                       max-depth 8}}]
    (when (<= current-depth max-depth)
      (let [opts (update-in opts [:current-depth] (fnil inc 0))] 
        (condp = op
          :and (when-let [op1 (some #(generate (operators %) opts) (shuffle non-terminal))]
                 (when-let [op2 (some #(generate (operators %) opts) (shuffle non-terminal))]
                   [:and op1 op2]))
          :or (when-let [op1 (some #(generate (operators %) opts) (shuffle non-terminal))]
                 (when-let [op2 (some #(generate (operators %) opts) (shuffle non-terminal))] 
                   [:or op1 op2]))
          :not (when-let [op1 (some #(generate (operators %) opts) (shuffle non-terminal))]
                 [:not op1]) 
          :matches (when-let [motif (generate (operators :motif) opts)] [:matches motif])
          :matches-at (when-let [motif (generate (operators :motif) opts)] 
                        [:matches-at motif 
                         (generate (operators :position) opts)
                         (generate (operators :leeway) opts)])
          :motif (apply str (repeatedly (+ motif-min-length (rand-int (- motif-max-length motif-min-length))) 
                                        #(if (> (rand) 0.75) 
                                           (random-motif-character alphabet-size)
                                           ".")))
          :position (inc (rand-int max-position))
          :leeway (inc (rand-int (int (/ max-position 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;; creation of individuals as clojure functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defmulti emit (fn [arg-sym op] (first op)))
;
;(defmethod emit :matches [arg-sym [_ s]]
;  `(matches ~arg-sym ~s))
;(defmethod emit :matches-at [arg-sym [_ s pos lw]]
;  `(matches-at ~arg-sym ~s ~pos ~lw))
;(defmethod emit :and [arg-sym [_ a b]]
;  `(and ~(emit arg-sym a) ~(emit arg-sym b)))
;(defmethod emit :or [arg-sym [_ a b]]
;  `(or ~(emit arg-sym a) ~(emit arg-sym b)))
;(defmethod emit :not [arg-sym [_ a b]]
;  `(not ~(emit arg-sym a)))
;
;(defmacro generate-random-function []
;  (let [param (gensym)] 
;    `(fn [~param] ~(emit param (generate)))))

;;;;;;;;;;;;;;;;;;;; interpreter for individuals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti interprete (fn [[op] s] op))
(defmethod interprete :matches [[_ motif] s]
  (matches s motif))
(defmethod interprete :matches-at [[_ motif pos lw] s]
  (matches-at s motif pos lw))
(defmethod interprete :and [[_ left right] s]
  (and (interprete left s) (interprete right s)))
(defmethod interprete :or [[_ left right] s]
  (or (interprete left s) (interprete right s)))
(defmethod interprete :not [[_ expr] s]
  (not (interprete expr s)))

;;;;;;;;;;;; mutation ;;;;;;;;;;;;;;;;;;;;
(defn mutate-motif [motif {:keys [alphabet-size motif-min-length motif-max-length]}]
  (let [n (rand-int (count motif))
        a (int \a)
        mutate-char-fn (fn [c] 
                         (if (= c (int \.))
                           (random-motif-character alphabet-size)
                           (if (>=  (rand) 0.5) 
                             (inc c) 
                             (dec c))))
        c (-> (nth motif n) int (- a) mutate-char-fn (mod alphabet-size) (+ a) char)
        n (inc n)
        new-motif-seq (concat (take (dec n) motif)
                              [c]
                              (drop n motif))]
    (apply str new-motif-seq)))

(defn increase-motif [motif {:keys [alphabet-size motif-min-length motif-max-length]}]
  (if (< (count motif) motif-max-length)
    (let [[pre post] (split-at (rand-int (count motif)) motif)
          c (-> (rand-int alphabet-size) (+ (int \a)) char)]
      (apply str (concat pre [c] post)))
    motif))

(defn decrease-motif [motif {:keys [alphabet-size motif-min-length motif-max-length]}]
  (if (> (count motif) motif-min-length)
    (let [idx (rand-int (count motif))]
      (apply str (concat (take idx motif) (drop (inc idx) motif))))
    motif))

(defn mutate-position [pos opts]
  (->> pos
    ((if (>= (rand) 0.5) inc dec))
    (min (:max-position opts))
    (max 0)))

(defn mutate-leeway [lw opts]
  (->> lw
    ((if (>= (rand) 0.5) inc dec))
    (min (:max-position opts))
    (max 0)))

(defmacro by-proportion [& prob-expr-pairs]
  (let [probs (take-nth 2 prob-expr-pairs)
        exprs (take-nth 2 (rest prob-expr-pairs))
        sum (apply + probs)
        probs (map #(/ % (double sum)) probs)
        r (gensym)
        lefts (map (fn [[a b]] (list '<= a r b)) (partition 2 1 (cons 0 (reductions + probs))))]
    `(let [~r (rand)]
       (cond
         ~@(interleave lefts exprs)))))

(defmulti mutate (fn [[op] opts] op))
(defmethod mutate :default [v _] v)

(defmethod mutate :matches [[_ motif :as form] opts]
  (by-proportion 
    4 [:matches (mutate-motif motif opts)]
    4 [:matches (increase-motif motif opts)]
    4 [:matches (decrease-motif motif opts)]
    1 [:matches-at motif (generate (operators :position) opts) (generate (operators :leeway) opts)]))

(defmethod mutate :matches-at [[_ motif pos lw :as form] opts]
  (by-proportion 
    1 [:matches-at (mutate-motif motif opts) pos lw]
    1 [:matches-at (increase-motif motif opts) pos lw]
    1 [:matches-at (decrease-motif motif opts) pos lw]
    1 [:matches-at motif pos (mutate-leeway lw opts)]
    1 [:matches-at motif (mutate-position pos opts) lw]
    2 [:matches motif]))

(defmethod mutate :and [[_ left right :as form] opts]
  (by-proportion 
    1 [:or left right]
    1 [:not left]
    1 [:not right]
    1 left
    1 right))

(defmethod mutate :or [[_ left right :as form] opts]
  (by-proportion
    1 [:and left right]
    1 [:not left]
    1 [:not right]
    1 left
    1 right))

(defmethod mutate :not [[_ expr :as form] opts]
  (by-proportion
    1 expr))

(defn mutator [opts]
  (fn [ind]
    (clojure.walk/postwalk #(if (and (vector? %) 
                                     (<= (rand) (:mutation-probability opts)))
                              (mutate % opts)
                              %) 
                           ind)))
;;;;;;;;;;;; crossing over ;;;;;;;;;;;;;;;;;;;;
(defn depth [ind] 
  (if (or (not (vector? ind)) (not-any? vector? ind))
    1
    (inc (apply max (map depth ind)))))

(defn locs [individual]
  (let [zipper (zip/vector-zip individual)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    (filter #(let [n (zip/node %)]
              (and (vector? n)
                   (#{:and :or :not} (first n)))) all-locs)))

(defn replace-loc [l r]
  (zip/root (zip/replace l (zip/node r))))

(defn cross-over [ind1 ind2] 
  (let [locs-l (locs ind1)
        locs-r (locs ind2)] 
    (if (and (not-empty locs-l)
             (not-empty locs-r)) 
      (let [l (rand-nth locs-l)
            r (rand-nth locs-r)]
        [(replace-loc l r) 
         (replace-loc r l)])
      [ind1 ind2])))


  (def opts {:alphabet-size 5 
             :motif-min-length 3
             :motif-max-length 7 
             :max-position 10
             :max-depth 3
             :mutation-probability 0.1
             :sliding-window-length 10})
  
  (def sax-str "abcdeabcdeabbbbbaaabcdeabcaadeabcdeabcdebbbbbaaababcdeaaacde")
  (def expected #_[false false false false false false 
                   false false false true false false false false
                   false false false false false false 
                   false false false true false false false false]
    (for [to-be (partition 4 1 (drop (:sliding-window-length opts) sax-str)) 
          :let [to-be (apply str to-be)]]
      (boolean (re-matches #"aaa." to-be))))
      
(defn fitness [opts sax-str expected] 
  (fn [ind]
    (let [f #(interprete ind %) ;(eval ind)
        results (map (comp f (partial apply str)) (partition (:sliding-window-length opts) 1 sax-str))
        same? (map = expected results)
        tp (count (filter true? (map #(and % %2) results expected)))
        fp (count (filter true? (map #(and % (not %2)) results expected)))
        tn (count (filter false? (map #(or % %2) results expected)))
        fn (count (filter true? (map #(and (not %) %2) results expected)))
        div #(if (zero? %2) 0 (/ %1 %2))
        precision (div tp (+ tp fp))
        recall (div tp (+ tp fn))
        size (depth ind)
        size-punishment-factor (condp >= size 4 1, (:max-depth opts) 0.5, 0.25)]
    (when (:debug? opts) (println {:tp tp :fp fp :tn tn :fn fn}))
    (* 2 (div (* precision recall) (+ precision recall)) size-punishment-factor))))

(defn generate-population [n opts]
  (for [_ (range n)]
    (generate opts)))

(defn run-generation [population opts]
  (let [n (count population)
        fitness (fitness opts sax-str expected)
        fitnesses (pmap fitness population)
        sm (into (sorted-map) (map vector fitnesses population))
        best-fitness (first (last sm))
        good-third (map second (take (int (/ n 3)) (reverse (seq sm))))
        best-individual (first good-third)
        children (apply concat (pmap cross-over (shuffle good-third) (shuffle good-third)))]
    (with-meta (concat good-third (map (mutator opts) children)) 
      {:best-fitness best-fitness
       :best-individual best-individual})))

(comment  
  

  
  
(use '[incanter core charts])
(def results (map meta (iterate (fn [gen] (run-generation gen opts)) (generate-population 500 opts))))
(time (view (xy-plot (range) (map :best-fitness (take 500 results)))))
(-> (nth results 500) :best-individual clojure.pprint/pprint)
(-> (nth results 500) :best-individual ((fitness (assoc opts :debug? true) sax-str expected)))


(loop [idx 0]
  (let [gen (generate opts)
        _ (println (pr-str idx ": " gen))
        f-src (let [param (gensym)] 
                `(fn [~param] ~(emit param gen)))
        
        ]
    (if (or (= idx 100) (= 16 (fitness f-src)))
      f-src
      (recur (inc idx))))))