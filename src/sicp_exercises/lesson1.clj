(ns sicp-exercises.lesson1)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; Exercise 1.11
(defn f [n]
  (if (< n 3)
    n
    (+ (f (dec n))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(defn fiter[f1 f2 f3 count]
  (if (= count 0)
    f1
    (fiter (+ f1 (* 2 f2) (* 3 f3)) f1 f2 (dec count))))


(defn f-i [n]
  (if (< n 3)
    n
    (fiter 2 1 0 (- n 2))))

;; Exercise 1.12
(defn pairwise-sum-iter[v acc counter]
  (if (== 0 counter)
    acc
    (pairwise-sum-iter (rest v) (conj acc (+ (first v) (second v))) (dec counter))))

(defn pairwise-sum [v]
  (pairwise-sum-iter v [] (dec (count v))))

(pairwise-sum [1 3 3 1])


(defn pascal [n]
  (cond
    (== n 0) [1]
    (== n 1) [1 1]
    :else    (concat [1]
                     (pairwise-sum (pascal (dec n)))
                     [1])
  ))

;; Exercise 1.16
(defn fast-exp [b n m]
  (cond (== n 1) (* b m)
        (== n 0) 1
        (even? n) (fast-exp (Math/pow b 2) (/ n 2) m)
        :else (fast-exp b (- n 1) (* b m))))

(fast-exp 3 3 1)
