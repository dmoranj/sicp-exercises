(ns sicp-exercises.lesson3
  (:require [sicp-exercises.lesson1]
            [sicp-exercises.graphics :as g]
            [sicp-exercises.pairs]
            )
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]))

;; Withdraw examples
(defn new-withdraw []
  (let [balance (atom 100)]
    (fn withdraw [amount]
      (if (> @balance amount)
        (do
          (swap! balance #(- % amount))
          balance)
        "Insuficient funds"))))

;; Exercise 3.1
(defn make-accumulator [initial-value]
  (let [accumulator (atom initial-value)]
    (fn [amount]
      (swap! accumulator #(+ % amount))
      @accumulator)))

(defn show-accumulators[]
  (let [A (make-accumulator 5)]
    (println (A 10))
    (println (A 10))))

;; Exercise 3.2
(defn make-monitored[f]
  (let [internal-counter (atom 0)]
    (fn dispatch[arg]
      (cond
        (= arg 'how-many-calls) @internal-counter
        (= arg 'reset-count) (reset! internal-counter 0)
        :else
          (do
            (swap! internal-counter inc)
            (f arg))))))

;; Exercise 3.3 and 3.4
(defn make-account [balance password]
  (let [ inner-balance (atom balance)
         wrong-pass-counter (atom 0)
         withdraw (fn [amount]
                    (if (>= balance amount)
                      (do
                        (swap! inner-balance #(- % amount))
                        @inner-balance)))

         deposit (fn [amount]
                   (swap! inner-balance #(+ % amount))
                   @inner-balance)

         call-the-cops (fn [args]
                         (reset! wrong-pass-counter 0)
                         (println "SOMEONE CALL THE COPS!!!! STOLEN PASSWORD!!!"))

         dispatch (fn [pass m]
                    (if (= pass password)
                      (do
                        (reset! wrong-pass-counter 0)
                        (cond
                          (= m 'withdraw) withdraw
                          (= m 'deposit) deposit
                          :else (throw (Exception. "Unkwnown request -- MAKE-ACCOUNT"))))
                      (do
                        (swap! wrong-pass-counter inc)
                        (if (> @wrong-pass-counter 7)
                          call-the-cops
                          (throw (Exception. "Incorrect password"))))
                      ))]
    dispatch))

;; Exercise 3.5
(defn monte-carlo [trials experiment]
  (loop [trials-remaining trials
         trials-passed 0]
    (cond
      (== trials-remaining 0) (/ trials-passed trials)
      (experiment) (recur (dec trials-remaining) (inc trials-passed))
      :else        (recur (dec trials-remaining) trials-passed))))

(defn random [ini end]
  (let [span (- end ini)]
    (+ ini (* span (rand)))))

(defn cesaro-test[]
  (== (sicp-exercises.lesson1/gcd (int (random 1 10000)) (int (random 1 10000))) 1))

(defn estimate-pi [trials]
  (Math/sqrt (/ 6 (monte-carlo trials cesaro-test))))

(defn estimate-integral [P x1 x2 y1 y2 trials]
  (let [predicate (fn []
                    (let [x (random x1 x2)
                          y (random y1 y2)]
                      (P x y)))
        length (- x2 x1)
        width (- y2 y1)]
    (* length width (monte-carlo trials predicate))))

(defn show-estimate-integral[]
  (let [circle-3 (fn [x y]
                   (<= (+ (Math/pow (- x 5) 2) (Math/pow (- y 7) 2)) 9))
        circle-1 (fn [x y]
                   (<= (+ (Math/pow x 2) (Math/pow y 2)) 1))
        ]
    (println "Area of the circle with radious 3= " (estimate-integral circle-3 2.0 8.0 4.0 10.0 1000000))
    (println "Pi estimation using unit circle= " (estimate-integral circle-1 -1.0 1.0 -1.0 1.0 10000000))
    ))

;; Exercise 3.6
(defn make-random-generator []
  (let [ current (atom 1)
         a 1664525
         b 1013904223
         m (Math/pow 2 32)
         generate (fn []
                    (do
                      (swap! current #(rem (+ (* a %) b) m))
                      @current))

         set-new (fn [value]
                   (reset! current value))]

    (fn [arg]
      (cond
        (= 'generate arg) (generate)
        (= 'reset) set-new))))

;; Exercise 3.7
(defn make-joint [account old-password new-password]
  (fn [pass m]
    (if (= pass new-password)
      (account old-password m)
      (throw (Exception. "Incorrect password")))))

;; Exercise 3.8
(defn make-f []
  (let [ counter (atom 1) ]
    (fn [n]
      (do
        (swap! counter dec)
        (- (* @counter n))))))

(defn show-f[]
  (let [ f1 (make-f)
         f2 (make-f)]
    (println "F1: " (+ (f1 0) (f1 1)))
    (println "F2: " (+ (f2 1) (f2 0)))))

;; Exercise 3.17
(defn count-pairs-wrong [x]
  (if (not (is-pair? x))
    0
    (+ (count-pairs-wrong (.getCar x))
       (count-pairs-wrong (.getCdr x))
       1)))

(defn count-pairs[x]
  (loop [ to-test #{x}
          tested #{}]
    (if (= (count to-test) 0)
      (count tested)
      (let [extracted (first to-test)]
        (if (and (is-pair? extracted) (not (tested extracted)))
          (recur (conj (conj (disj to-test extracted) (.getCar extracted)) (.getCdr extracted)) (conj tested extracted))
          (recur (disj to-test extracted) tested))))))

(defn show-count-pairs[]
  (let [ test-pair (pair-from-list '(a b c d))
         pair1 (Pair. 'a (Pair. 'b nil))
         pair2 (Pair. pair1 pair1)
         ]
    (println "Wrong count (right result): " (count-pairs-wrong test-pair))
    (println "Wrong count (wrong result): " (count-pairs-wrong pair2))
    (println "Right count: " (count-pairs test-pair))
    (println "Right count: " (count-pairs pair2))
    ))

;; Exercise 3.18
(defn loops? [p]
  (loop [ to-test #{p}
          tested-cdrs #{}]
    (if (= (count to-test) 0)
      false
      (let [extracted (first to-test)]
        (cond
          (and (is-pair? extracted) (tested-cdrs extracted)) true
          (and (is-pair? extracted) (not (tested-cdrs extracted))) (recur (conj (conj (disj to-test extracted) (.getCar extracted)) (.getCdr extracted)) (conj tested-cdrs extracted))
          :else (recur (disj to-test extracted) tested-cdrs))))))

(defn show-loops[]
  (let [ test-pair (pair-from-list '(a b c d))
         pair1 (Pair. 'a (Pair. 'b nil))]

    (.setCdr (.getCdr pair1) pair1)
    (println "Loops in test-pair: " (loops? test-pair))
    (println "Loops in pair1: " (loops? pair1))))

;; Exercise 3.19
(defn loops-cte? [p]
  (loop [ to-test #{p} ]
    (if (= (count to-test) 0)
      false
      (let [extracted (first to-test)]
        (cond
          (and (is-pair? extracted) (= :loop_indicator (.getCar extracted)))
            true
          (and (is-pair? extracted) (not (= :loop_indicator (.getCar extracted))))
            (do
              (.setCar extracted :loop_indicator)
              (recur (conj (disj to-test extracted) (.getCdr extracted))))
          :else
            (recur (disj to-test extracted)))))))

(defn show-loops-cte[]
  (let [ test-pair (pair-from-list '(a b c d))
         pair1 (Pair. 'a (Pair. 'b nil))]

    (.setCdr (.getCdr pair1) pair1)
    (println "Loops in test-pair: " (loops-cte? test-pair))
    (println "Loops in pair1: " (loops-cte? pair1))))
