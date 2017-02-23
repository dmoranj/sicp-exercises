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

