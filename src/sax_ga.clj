(ns sax-ga
  (:require [clojure.zip :as zip]))

(defn matches [sax-str motif]
  (boolean (re-matches (re-pattern (str ".*" motif ".*")) sax-str)))

(defn matches-at [sax-str motif reverse-pos leeway]
  (let [start-pos (max 0 (- (count sax-str) reverse-pos leeway))
        end-pos (min (count sax-str) 
                     (+ start-pos (count motif) leeway))]
    (matches (subs sax-str start-pos end-pos) motif)))

(def operators
  {:and {:type :logical
         :op :and
         :input-types [:logical :logical]}
   :or {:type :logical
        :op :or
        :input-types [:logical :logical]}
   :not {:type :logical
         :op :not
         :input-types [:logical]}
   :matches {:type :logical
             :op :matches
             :input-types [:motif]}
   :matches-at {:type :logical
                :op :matches-at
                :input-types [:motif :position :leeway]}
   :motif {:type :string
           :op :motif}
   :position {:type :integer
              :op :position}
   :leeway {:type :integer
              :op :leeway}})

(def non-terminal [:and :or :not :matches :matches-at])
(def terminal [:motif :position])

(defn- random-motif-character [alphabet-size]
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

(defmulti emit (fn [arg-sym op] (first op)))

(defmethod emit :matches [arg-sym [_ s]]
  `(matches ~arg-sym ~s))
(defmethod emit :matches-at [arg-sym [_ s pos lw]]
  `(matches-at ~arg-sym ~s ~pos ~lw))
(defmethod emit :and [arg-sym [_ a b]]
  `(and ~(emit arg-sym a) ~(emit arg-sym b)))
(defmethod emit :or [arg-sym [_ a b]]
  `(or ~(emit arg-sym a) ~(emit arg-sym b)))
(defmethod emit :not [arg-sym [_ a b]]
  `(not ~(emit arg-sym a)))

(defmacro generate-random-function []
  (let [param (gensym)] 
    `(fn [~param] ~(emit param (generate)))))

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

(defmacro by-probability [& prob-expr-pairs]
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
  (if (<= (rand) (:mutation-probability opts)) 
    (by-probability 
      1 [:matches (mutate-motif motif opts)]
      1 [:matches (increase-motif motif opts)]
      1 [:matches (decrease-motif motif opts)])
    form))

(defmethod mutate :matches-at [[_ motif pos lw :as form] opts]
  (if (<= (rand) (:mutation-probability opts))
    (by-probability 
      1 [:matches-at (mutate-motif motif opts) pos lw]
      1 [:matches-at (increase-motif motif opts) pos lw]
      1 [:matches-at (decrease-motif motif opts) pos lw]
      1 [:matches-at motif (mutate-position pos opts) (mutate-leeway lw opts)])
    form))

(defmethod mutate :and [[_ left right :as form] opts]
  (if (<= (rand) (:mutation-probability opts))
    (by-probability 
      1 [:and (mutate left opts) right]
      1 [:and left (mutate right opts)]
      1 [:or left right]
      1 [:not left]
      1 [:not right])
    form))

(defmethod mutate :or [[_ left right :as form] opts]
  (if (<= (rand) (:mutation-probability opts))
    (by-probability
      1 [:or (mutate left opts) right]
      1 [:or left (mutate right opts)]
      1 [:and left right]
      1 [:not left]
      1 [:not right])
    form))


;;;;;;;;;;;; crossing over ;;;;;;;;;;;;;;;;;;;;
(defn locs [individual]
  (let [zipper (zip/vector-zip individual)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    (filter #(let [n (zip/node %)]
              (and (vector? n)
                   (#{:and :or :not} (first n)))) all-locs)))

(defn replace-loc [l r]
  (zip/root (zip/replace l (zip/node r))))

(defn cross-over [ind1 ind2]; (println "crossing" ind1 "and" ind2) 
  (let [locs-l (locs ind1)
        locs-r (locs ind2)] 
    (if (and (not-empty locs-l)
             (not-empty locs-r)) 
      (let [l (rand-nth locs-l)
            r (rand-nth locs-r)]
        [(replace-loc l r) 
         (replace-loc r l)])
      [ind1 ind2])))



(def opts {:alphabet-size 4 :motif-max-length 5 :motif-min-length 1
           :max-position 10
           :max-depth 3
           :mutation-probability 0.1})
(def sax-str "aaaabbbbccccdddd")
(def expected (concat (repeat 11 false) 
                        [true]
                        (repeat 4 false)))
(defn fitness [ind]
  (let [f (eval ind)
        results (for [end (range (inc (count sax-str))) 
                      :let [s (subs sax-str 0 end)]]
                  (f s))
        same? (map = expected results)
        tp (count (filter true? (map #(and % %2) results expected)))
        fp (count (filter true? (map #(and % (not %2)) results expected)))
        tn (count (filter false? (map #(or % %2) results expected)))
        fn (count (filter true? (map #(and (not %) %2) results expected)))
;        div (fn [a b] (if (zero? b) 0 (/ a b)))
        precision (div tp (+ tp fp))
        recall (div tp (+ tp fn))
        ]
    ;(println {:tp tp :fp fp :tn tn :fn fn})
    (* 2 (div (* precision recall) (+ precision recall)))))

(defn generate-population [n opts]
  (for [_ (range n)]
    (generate opts)))

(defn run-generation [population opts]
  (let [n (count population)
        fitnesses (pmap #(fitness (let [param (gensym)] 
                                   `(fn [~param] ~(emit param %)))) population)
        sm (into (sorted-map) (map vector fitnesses population))
        best-fitness (first (last sm))
        good-third (map second (take (int (/ n 3)) (reverse (seq sm))))
        best-individual (first good-third)
        children (apply concat (pmap cross-over (shuffle good-third) (shuffle good-third)))]
    (with-meta (map #(mutate % opts) (concat good-third children)) 
      {:best-fitness best-fitness
       :best-individual best-individual})))

(comment  
  

(loop [idx 0]
  (let [gen (generate opts)
        _ (println (pr-str idx ": " gen))
        f-src (let [param (gensym)] 
                `(fn [~param] ~(emit param gen)))
        
        ]
    (if (or (= idx 100) (= 16 (fitness f-src)))
      f-src
      (recur (inc idx))))))