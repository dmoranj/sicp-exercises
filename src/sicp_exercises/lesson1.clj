(ns sicp-exercises.lesson1)

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
(defn fast-exp-iter [b n m]
  (cond (== n 1) (* b m)
        (== n 0) 1
        (even? n) (fast-exp-iter (Math/pow b 2) (/ n 2) m)
        :else (fast-exp-iter b (- n 1) (* b m))))

(defn fast-exp [b n]
  (fast-exp-iter b n 1))


;; Exercise 1.17
(defn doubfn [n] (* 2 n))

(defn halvfn [n] (/ n 2))

(defn fast-mul-rec [b n]
  (cond (== n 1) b
        (== n 0) 0
        (even? n) (fast-mul-rec (doubfn b) (halvfn n))
        :else (+ b (fast-mul-rec b (- n 1)))))

;; Exercise 1.18
(defn fast-mul-iter [b n m]
  (cond (== n 1) (+ b m)
        (== n 0) 0
        (even? n) (fast-mul-iter (doubfn b) (halvfn n) m)
        :else (fast-mul-iter b (- n 1) (+ b m))))

(defn fast-mul [b n]
  (fast-mul-iter b n 0))

;; Exercise 1.19
(defn pprime [p q]
  (+ (* q q) (* p p)))

(defn qprime [p q]
  (+ (* q q) (* 2 q p)))

(defn fib-iter [a b p q coun]
  (cond (== coun 0) b
        (even? coun) (fib-iter a b (pprime p q) (qprime p q) (/ coun 2))
        :else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- coun 1))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

(defn showfib[n]
  (map fib (range n)))

;; Exercise 1.21
(defn divides? [a b]
  (== (rem b a) 0))

(defn create-find-divisor [ite]
  (fn find-divisor [n-init ts-init]
    (loop [n n-init
           test-divisor ts-init]
      (cond (> (* test-divisor test-divisor) n) n
            (divides? test-divisor n) test-divisor
            :else (recur n (ite test-divisor))))))

(def find-divisor (create-find-divisor inc))

(defn smallest-divisor [n]
  (find-divisor n 2))

;; Exercise 1.22
(defn prime?[n]
  (== (smallest-divisor n) n))

(defn create-search-for-primes-iter[prime-fn]
  (fn search-for-primes-iter [a b i n total]
    (let [test-n (+ a i)]
      (cond (== test-n b) n
            (== (count n) total) n
            (prime-fn test-n) (search-for-primes-iter a b (inc i) (conj n test-n) total)
            :else (search-for-primes-iter a b (inc i) n total)))))

(defn search-for-primes[a b total prime-fn]
  {:pre  [(and (< a b) (> total 0))]}
  (let [searching-fn (create-search-for-primes-iter prime-fn)]
    (searching-fn a b 0 [] total)))

(defn prime-time[]
  (time (search-for-primes 1000 1100 3 prime?))
  (time (search-for-primes 10000 11000 3 prime?))
  (time (search-for-primes 100000 110000 3 prime?))
  (time (search-for-primes 1000000 1100000 3 prime?)))

;; Exercise 1.23
(defn next-div [n]
  (if (== n 2)
    3
    (+ n 2)))

(def find-divisor-next (create-find-divisor next-div))

(defn smallest-divisor-next [n]
  (find-divisor-next n 2))

(defn prime-next?[n]
  (== (smallest-divisor-next n) n))

(defn prime-time-next[]
  (time (search-for-primes 1000 1100 3 prime-next?))
  (time (search-for-primes 10000 11000 3 prime-next?))
  (time (search-for-primes 100000 110000 3 prime-next?))
  (time (search-for-primes 1000000 1100000 3 prime-next?)))

;; Exercise 1.24
(defn expmod [base exp m]
  (cond
    (== exp 0) 1
    (even? exp) (rem (Math/pow (expmod base (/ exp 2) m) 2) m)
    :else      (rem (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test [n]
  (let [try-it (fn [a]
                 (== (expmod a n n) a))]
    (try-it (+ 1 (int (* (Math/random) (dec n)))))))

(defn fast-prime? [n times]
  (cond (== times 0) true
        (fermat-test n) (fast-prime? n (dec times))
        :else false))

(defn fprime? [n]
  (fast-prime? n 3))


(defn prime-time-fast[]
  (time (search-for-primes 1000 1100 3 fprime?))
  (time (search-for-primes 10000 11000 3 fprime?))
  (time (search-for-primes 100000 110000 3 fprime?))
  (time (search-for-primes 1000000 1100000 3 fprime?)))

;; Exercise 1.27
(defn carmichael-test[n]
  (loop [a 2
         fermat true]
    (if (or (== a n) (not fermat))
      fermat
      (recur (inc a) (and fermat (== (expmod a n n) a))))))

(defn carmichael []
  (let [test-numbers [561, 1105, 1729, 2465, 2821, 6601]]
    (map carmichael-test test-numbers)))

(carmichael)


;; Exercise 1.28 (solution adapted to Clojure from http://www.billthelizard.com/2010/03/sicp-exercise-128-miller-rabin-test.html).
;; I tried after failing miserably once and again with my Clojure implementation and this one still fails miserably with
;; carmichael's numbers.
(defn square-check [x m]
  (if (and (not (or (== x 1) (== x (- m 1))))
           (== (rem (* x x) m) 1))
      0
      (rem (* x x) m)))

(defn expmod-mr [base exp m]
  (cond (== exp 0) 1
        (even? exp)
          (square-check (expmod-mr base (/ exp 2) m) m)
        :else
          (rem (* base (expmod-mr base (- exp 1) m)) m)))

(defn miller-rabin-test [n]
  (let [try-it (fn [a] (== (expmod a (- n 1) n) 1))]
    (try-it (+ 2 (rand-int (- n 2))))))

(defn carmichael-mr []
  (let [test-numbers (range 2 50)]
    (map #(vector % (miller-rabin-test %)) test-numbers)))

;; Exercise 1.29
(defn cube [n]
  (* n n n))

(defn sum [term a nex b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nex a) nex b))))

(defn integral [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)))

(defn ssum [term a nex b i coef]
  (if (> a b)
    0
    (+ (coef i (term a))
       (ssum term (nex a) nex b (inc i) coef))))

(defn simpson [f a b n]
  (let [h (/ (- b a) n)
        nex #(+ % h)
        coef (fn [i value]
               (* (cond (== i 0) 1.0
                     (== i n) 1.0
                     (even? i) 2.0
                     :else 4) value))]

    (* (/ h 3) (ssum f a nex b 0 coef))))

(simpson cube 0 1 4)

;; Exercise 1.30
(defn sumi [term a nex b]
  (let [iter (fn iter[a result]
               (if (> a b)
                 result
                 (iter (nex a) (+ result (term a)))))]
    (iter a 0)
    ))

(defn integral-iter [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sumi f (+ a (/ dx 2.0)) add-dx b) dx)))


;; Exercise 1.31
(defn product [term a nex b]
  (if (> a b)
    1
    (* (term a)
       (product term (nex a) nex b))))

(defn factorial[n]
  (product identity 1 inc n))

(defn alternate[n]
  (if (even? n)
    (/ n (inc n))
    (/ (inc n) n)))

(defn pi[n]
  (* 4.0 (product alternate 2 inc n)))

(defn product-iter [term ainit nex b]
  (loop [a ainit
         result 1]
    (if (> a b)
      result
      (recur (nex a) (* result (term a))))))

(defn pi-iter[n]
  (* 4.0 (product-iter alternate 2 inc n)))

;; Exercise 1.32
(defn accumulate[combiner null-value term ainit nex b]
  (loop [a ainit
         result null-value]
    (if (> a b)
      result
      (recur (nex a) (combiner result (term a))))))

(defn show-accumulations[]
  (accumulate + 0 identity 1 inc 5) ;; 5 + 4 + 3 + 2 + 1
  (accumulate * 1 identity 1 inc 5) ;; 5 * 4 * 3 * 2 * 1 = 5!
)

