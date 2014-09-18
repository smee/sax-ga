(ns sax-ga)

(defn matches [^String sax-str ^String motif]
  (.contains sax-str motif))

(defn matches-at [^String sax-str motif pos]
  (let [min-len (+ pos (count motif))] 
    (if (<= min-len (count sax-str)) 
      (.startsWith (subs sax-str pos) motif)
      false)))

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
                :input-types [:motif :position]}
   :motif {:type :string
           :op :motif}
   :position {:type :integer
              :op :position}})

(def non-terminal [:and :or :not :matches :matches-at])

(defn generate 
  ([] (generate (operators (rand-nth non-terminal))))
  ([{:keys [op]}] 
    (condp = op
      :and [:and (generate (operators (rand-nth non-terminal)))
            (generate (operators (rand-nth non-terminal)))]
      :or [:or (generate (operators (rand-nth non-terminal)))
           (generate (operators (rand-nth non-terminal)))]
      :not [:not (generate (operators (rand-nth non-terminal)))] 
      :matches [:matches (generate (operators :motif))]
      :matches-at [:matches-at (generate (operators :motif)) (generate (operators :position))]
      :motif (apply str (repeatedly (+ 3 (rand-int 6)) #(char (+ (int \a) (rand-int 5)))))
      :position (inc (rand-int 30)))))

(defmulti emit (fn [arg-sym op] (first op)))
(defmethod emit :matches [arg-sym [_ s]]
  `(matches ~arg-sym ~s))
(defmethod emit :matches-at [arg-sym [_ s pos]]
  `(matches-at ~arg-sym ~s ~pos))
(defmethod emit :and [arg-sym [_ a b]]
  `(and ~(emit arg-sym a) ~(emit arg-sym b)))
(defmethod emit :or [arg-sym [_ a b]]
  `(or ~(emit arg-sym a) ~(emit arg-sym b)))
(defmethod emit :not [arg-sym [_ a b]]
  `(not ~(emit arg-sym a)))

(defmacro generate-random-function []
  (let [param (gensym)] 
    `(fn [~param] ~(emit param (generate)))))

(def sax-str "aaaabbbbccccdddd")
(def expected (concat (repeat 11 false) 
                        [true]
                        (repeat 4 false)))
(defn fitness [ind]
  (let [f (eval ind)
        results (for [end (range (count sax-str)) 
                      :let [s (subs sax-str 0 end)]]
                  (f s))
        same? (map = expected results)]
    (count (filter true? same?))))

(comment
  
  
  

(loop [idx 0]
  (let [f-src (let [param (gensym)] 
                `(fn [~param] ~(emit param (generate (operators :matches)))))
        ]
    (if (= 16 (fitness f-src))
      f-src
      (do (println (pr-str idx ": " f-src)) 
        (recur (inc idx)))))))