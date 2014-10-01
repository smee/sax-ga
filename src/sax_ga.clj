(ns sax-ga)

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
  ([{:keys [op]} {:keys [alphabet-size motif-min-length motif-max-length max-position]
                  :as opts
                  :or {alphabet-size 5 
                       motif-min-length 3 
                       motif-max-length 6
                       max-position 10}}] 
    (condp = op
      :and [:and (generate (operators (rand-nth non-terminal)) opts)
            (generate (operators (rand-nth non-terminal)) opts)]
      :or [:or (generate (operators (rand-nth non-terminal)) opts)
           (generate (operators (rand-nth non-terminal)) opts)]
      :not [:not (generate (operators (rand-nth non-terminal)) opts)] 
      :matches [:matches (generate (operators :motif) opts)]
      :matches-at [:matches-at (generate (operators :motif) opts) 
                   (generate (operators :position) opts)
                   (generate (operators :leeway) opts)]
      :motif (apply str (repeatedly (+ motif-min-length (rand-int (- motif-max-length motif-min-length))) 
                                    #(if (> (rand) 0.75) 
                                       (random-motif-character alphabet-size)
                                       ".")))
      :position (inc (rand-int max-position))
      :leeway (inc (rand-int (int (/ max-position 2)))))))

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
        mutate-char (if (>=  (rand) 0.5) inc dec)
        c (-> (nth motif n) int (- a) mutate-char (mod alphabet-size) (+ a) char)
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

(defmulti mutate (fn [[op] opts] op))
(defmethod mutate :default [v _] v)

(defmethod mutate :matches [[_ motif] opts]
  (condp > (rand)
    0.3333 [:matches (mutate-motif motif opts)]
    0.6666 [:matches (increase-motif motif opts)]
    [:matches (decrease-motif motif opts)]))

(defmethod mutate :matches-at [[_ motif pos lw] opts]
  (condp > (rand)
    0.25 [:matches-at (mutate-motif motif opts) pos lw]
    0.5 [:matches-at (increase-motif motif opts) pos lw]
    0.75 [:matches-at (decrease-motif motif opts) pos lw]
    [:matches-at motif (mutate-position pos opts) (mutate-leeway lw opts)]))



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
        fn (count (filter true? (map #(and (not %) %2) results expected)))]
    (println tp fp tn fn)
    (count (filter true? same?))))

(comment
  (def opts {:alphabet-size 5 :motif-max-length 10 :motif-min-length 3
             :max-position 10})
  
  

(loop [idx 0]
  (let [f-src (let [param (gensym)] 
                `(fn [~param] ~(emit param (generate (operators :matches-at)))))
        ]
    (if (or (= idx 100) (= 16 (fitness f-src)))
      f-src
      (do (println (pr-str idx ": " f-src)) 
        (recur (inc idx)))))))