(ns sicp-exercises.lesson3
  (:require [sicp-exercises.graphics :as g]))

(defn new-withdraw []
  (let [balance (atom 100)]
    (fn withdraw [amount]
      (if (> @balance amount)
        (do
          (swap! balance #(- % amount))
          balance)
        "Insuficient funds"))))

(def wd (new-withdraw))

