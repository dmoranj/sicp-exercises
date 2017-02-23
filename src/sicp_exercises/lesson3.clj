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

