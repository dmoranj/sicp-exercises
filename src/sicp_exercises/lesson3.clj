(ns sicp-exercises.lesson3
  (:require [sicp-exercises.graphics :as g]))

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
        (= arg 'reset-count) (swap! internal-counter #(* 0 %))
        :else
          (do
            (swap! internal-counter inc)
            (f arg))))))

