(ns sicp-exercises.pairs)

;; Complementary functions to construct pairs

(definterface IPair
  (getCdr [])
  (setCdr [v])
  (getCar [])
  (setCar [v])
  (getCaar []))

(declare is-pair?)

(deftype Pair [^{:volatile-mutable true} car ^{:volatile-mutable true} cdr]
  IPair
  (getCar [this] car)
  (setCar [this v] (set! (.car this) v))
  (getCdr [this] cdr)
  (setCdr [this v] (set! (.cdr this) v))
  (getCaar [this]
    (if (is-pair? (.car this))
      (.getCar (.car this))
      (throw (Exception. "Tried to extract Caar from non-pair")))))

(defn print-pair
  ([to-print printed result]
    (if (= (count to-print) 0)
      result
      (let [ element (first to-print) ]
        (cond
          (printed element) (print-pair
                              (disj to-print element)
                              printed
                              result)

          (is-pair? element) (print-pair
                               (disj to-print element)
                               (conj printed element)
                               (str "{ " (print-pair (.getCar element)) " " (print-pair (.getCdr element)) " }"))

          :else (print-pair (disj to-print element)
                            printed
                            (str result element))))))

   ([v] (print-pair #{v} #{} "")))

(defmethod print-method Pair [v ^java.io.Writer w]
  (.write w (print-pair v)))

(defn is-pair?[n]
  (instance? sicp_exercises.pairs.Pair n))

(defn list-from-pair [n]
  (loop [current n
         result '()]
    (if (nil? current)
      (reverse result)
      (recur (.getCdr current) (cons (.getCar current) result)))))

(defn pair-from-list [l]
  (loop [ current l
          result nil ]
    (if (empty? current)
      result
      (recur (butlast current) (Pair. (last current) result)))))

(defn print-pair-list[l]
  (cond
    (nil? l) "()"
    (is-pair? l) (str "(" (print-pair (.getCar l)) " " (print-pair (.getCdr l)) ")")
    :else  (str l)))

(defn show-pairs[]
  (let [ test-pair (pair-from-list '(a b c d e))
         test-pair2 test-pair
         test-pair3 (pair-from-list '(a b c d e))]
    (println (= test-pair test-pair2))
    (println (= test-pair test-pair3))
    (println (list-from-pair test-pair))
    (println (print-pair-list test-pair))
  ))
