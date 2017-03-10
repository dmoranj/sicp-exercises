(ns sicp-exercises.lesson3
  (:require [sicp-exercises.lesson1]
            [sicp-exercises.graphics :as g]
            [sicp-exercises.pairs]
            [sicp-exercises.queue]
            [sicp-exercises.constraints]
            [sicp-exercises.circuits]
            )
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]
        [sicp-exercises.queue]
        [sicp-exercises.constraints]
        [sicp-exercises.circuits]))

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

;; Exercise 3.21
(defn print-queue[q]
  (let [ print-list (fn print-list[l]
                      (loop [ current l
                              result "" ]
                        (if (nil? current)
                          result
                          (recur (.getCdr current) (str result (.getCar current) " "))
                          ))) ]

    (if (nil? q)
      "( )"
      (str "( " (print-list (front-ptr q)) ")"))))


(defn show-print-queue[]
  (let [q1 (make-queue)]
    (println (print-queue q1))
    (println (print-queue (insert-queue! q1 'a)))
    (println (print-queue (insert-queue! q1 'b)))
    (println (print-queue (delete-queue! q1)))
    (println (print-queue (delete-queue! q1)))
    ))

;; Exercise 3.22
(defn make-proc-queue[]
  (let [ front-ptr (atom nil)
         rear-ptr (atom nil)
         is-empty? (fn []
                     (nil? front-ptr))

         pushq! (fn [v]
                  (let [ new-pair (Pair. v nil) ]
                    (if (nil? @rear-ptr)
                      (do
                        (reset! rear-ptr new-pair)
                        (reset! front-ptr new-pair))
                      (do
                        (.setCdr @rear-ptr new-pair)
                        (reset! rear-ptr new-pair)))))

         popq! (fn []
                 (let [ result @front-ptr ]
                   (reset! front-ptr (.getCdr result))
                   (.getCar result)
                   ))

         peekq (fn []
                 (.getCar @front-ptr))

         deleteq (fn []
                   (if (nil? @front-ptr)
                     (throw (Exception. "Tried to remove an item from an empty queue"))
                     (reset! front-ptr (.getCdr @front-ptr))))

         printq (fn []
                  (str "("
                       (loop [current @front-ptr
                              result ""]
                         (if (nil? current)
                           result
                           (recur (.getCdr current) (str result (.getCar current) " " ))))
                       ")" ))

         dispatch (fn [m]
                    (cond
                      (= m 'is-empty?) is-empty?
                      (= m 'pushq!) pushq!
                      (= m 'popq!) popq!
                      (= m 'peekq) peekq
                      (= m 'deleteq) deleteq
                      (= m 'printq) printq
                      :else (throw (Exception. "Unknown operation for procedural queue"))))]
    dispatch))

(defn show-proc-queue []
  (let [q1 (make-proc-queue)]
    (println "Q1: " ((q1 'printq)))
    ((q1 'pushq!) :a)
    (println "Q1: " ((q1 'printq)))
    ((q1 'pushq!) :b)
    (println "Q1: " ((q1 'printq)))
    (println "Pop Q1: " ((q1 'popq!)))
    (println "Q1: " ((q1 'printq)))
    (println "Pop Q1: " ((q1 'popq!)))
    (println "Q1: " ((q1 'printq)))
  ))

;; Exercise 3.23
(defn make-dequeue []
  (Pair. nil nil))

(defn front-dequeue [q]
  (.getCar q))

(defn rear-dequeue [q]
  (.getCdr q))

(defn empty-dequeue? [q]
  (nil? (front-dequeue q)))

(defn set-front-dequeue! [q v]
  (.setCar q v))

(defn set-rear-dequeue! [q v]
  (.setCdr q v))

(defn print-dequeue [queue]
  (str "( "
       (loop [current (front-dequeue queue)
              result ""]
         (if (nil? current)
           result
           (recur (.getCdr current) (str result (.getCar (.getCar current)) " ")))) ")"))

(defn front-insert-dequeue! [queue item]
  (let [ new-pair (Pair. item nil)
         indirection (Pair. new-pair nil)]
    (if (empty-queue? queue)
      (do
        (set-front-dequeue! queue indirection)
        (set-rear-dequeue! queue indirection)
        queue)
      (do
        (println "Setting indirection")
        (.setCdr indirection (front-dequeue queue))
        (println "Setting new car. FD: " (front-dequeue queue) )
        (.setCdr (.getCar (front-dequeue queue)) indirection)
        (set-front-dequeue! queue indirection)
        queue
        ))))

(defn rear-insert-dequeue! [queue item]
  (let [ new-pair (Pair. item nil)
         indirection (Pair. new-pair nil)]
    (if (empty-queue? queue)
      (do
        (set-front-dequeue! queue indirection)
        (set-rear-dequeue! queue indirection)
        queue)
      (do
        (.setCdr (rear-dequeue queue) indirection)
        (.setCdr new-pair (rear-dequeue queue))
        (set-rear-dequeue! queue indirection)
        queue))))

(defn front-delete-dequeue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. "DELETE! called with an empty queue"))
    (do
      (set-front-dequeue! queue (.getCdr (front-dequeue queue)))
      (.setCdr (front-dequeue queue) nil)
      queue
      )))

(defn rear-delete-dequeue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. "DELETE! called with an empty queue"))
    (let [ but-last-item (.getCdr (.getCar (rear-dequeue queue))) ]
      (set-rear-dequeue! queue but-last-item)
      (.setCdr but-last-item nil)
      queue
      )))

(defn show-double-linked[]
  (let [ q1 (make-dequeue) ]
    (println "Q1: " (print-dequeue q1))
    (front-insert-dequeue! q1 :m)
    (front-insert-dequeue! q1 :f)
    (rear-insert-dequeue! q1 :r)
    (println "Q1: " (print-dequeue q1))
    (rear-delete-dequeue! q1)
    (println "Q1: " (print-dequeue q1))
    (front-delete-dequeue! q1)
    (println "Q1: " (print-dequeue q1))
    ))

;; Exercise 3.26
(defn make-tree-table []
  (let [ local-table (Pair. '*table* nil)

         lookup-node (fn lookup-node [key1 current]
                           (let [ current-key (.getCar (.getCar current))
                                  left (.getCar (.getCdr current))
                                  right (.getCdr (.getCdr current)) ]
                             (cond
                               (= current-key key1) (.getCdr (.getCar current))
                               (< (compare key1 current-key) 0) (if (nil? left)
                                                      false
                                                      (lookup-node key1 left))
                               (> (compare key1 current-key) 0) (if (nil? right)
                                                      false
                                                      (lookup-node key1 right)))))

         lookup (fn lookup [key1]
                  (if (nil? (.getCdr local-table))
                    false
                    (lookup-node key1 (.getCdr local-table))))

         make-empty-node (fn make-empty-node[key1 value]
                           (Pair. (Pair. key1
                                         value)
                                  (Pair. nil
                                         nil)))

         insert-in-node! (fn insert-in-node![key1 value current]
                           (let [ current-key (.getCar (.getCar current))
                                  left (.getCar (.getCdr current))
                                  right (.getCdr (.getCdr current)) ]
                             (cond
                               (= current-key key1) (.setCdr (.getCar current) value)
                               (< (compare key1 current-key) 0) (if (nil? left)
                                                      (.setCar (.getCdr current) (make-empty-node key1 value))
                                                      (insert-in-node! key1 value left))

                               (> (compare key1 current-key) 0) (if (nil? right)
                                                      (.setCdr (.getCdr current) (make-empty-node key1 value))
                                                      (insert-in-node! key1 value right)))))

         insert! (fn insert! [key1 value]
                   (let [ root (.getCdr local-table) ]
                     (if (nil? root)
                       (.setCdr local-table (make-empty-node key1 value))
                       (insert-in-node! key1 value root))))

         dispatch (fn dispatch [m]
                    (cond
                      (= m 'lookup-proc) lookup
                      (= m 'insert-proc) insert!
                      :else (throw (Exception. "Unknown operation for tree table"))))]
    dispatch))

(defn show-tree-table[]
  (let [ tree-table (make-tree-table) ]
    ((tree-table 'insert-proc) :math :+)
    ((tree-table 'insert-proc) :letters :a)
    ((tree-table 'insert-proc) :aircraft :plane)
    ((tree-table 'insert-proc) :zealots :fenix)
    (println "Value for key :math = " ((tree-table 'lookup-proc) :math))
    (println "Value for key :aircraft = " ((tree-table 'lookup-proc) :aircraft))))

;; Exercise 3.29 (Composed OR gate)
(defn composed-or [i1 i2 o]
  (let [ a (make-wire)
         b (make-wire)
         c (make-wire) ]
    (inverter i1 a)
    (inverter i2 b)
    (probe 'and-i1 a)
    (probe 'and-i2 b)
    (probe 'and-output c)
    (and-gate a b c)
    (inverter c o)))

(defn show-composed-or[]
  (let [ input-1 (make-wire)
         input-2 (make-wire)
         output-1 (make-wire)]

    (probe 'input1 input-1)
    (probe 'input2 input-2)
    (probe 'composed output-1)
    (composed-or input-1 input-2 output-1)
    (set-signal! input-1 0)
    (set-signal! input-2 1)
    (println "Propagating signal.....")
    (propagate)
    (println "Output signal (0 OR 1): " (get-signal output-1))
    (set-signal! input-1 0)
    (set-signal! input-2 0)
    (println "Propagating signal.....")
    (propagate)
    (println "Output signal (0 OR 0): " (get-signal output-1))
    ))

;; Exercise 3.33
(defn averager [a b c]
  (let [ s (make-connector)
         d (make-connector) ]
    (adder a b s)
    (multiplier d c s)
    (constant 2 d)
    'ok))

(defn show-averager[]
  (let [ v1 (make-connector)
         v2 (make-connector)
         result1 (make-connector)
         v3 (make-connector)
         v4 (make-connector)
         result2 (make-connector) ]
    (const-probe "Input1: " v1)
    (const-probe "Input2: " v2)
    (const-probe "Result: " result1)
    (averager v1 v2 result1)
    (set-value! v1 12 'user)
    (set-value! v2 8 'user)
    (const-probe "Input3: " v3)
    (const-probe "Input3: " v4)
    (const-probe "Result2: " result2)
    (averager v4 v3 result2)
    (set-value! v4 10 'user)
    (set-value! result2 6 'user)
    ))
