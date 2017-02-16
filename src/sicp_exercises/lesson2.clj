(ns sicp-exercises.lesson2
  (:require [sicp-exercises.lesson1]
            [sicp-exercises.graphics :as g]))

;; Common code
(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(defn make-rat [n d]
  (let [g (gcd n d)
        new-n (/ n g)
        new-d (/ d g)]
    (if (and (< new-d 0) (> new-n 0))
      (list (- new-n) (- new-d))
      (list new-n new-d))))

(defn numer [x]
  (first x))

(defn denom [x]
  (second x))

(defn print-rat [x]
  (println)
  (print (numer x))
  (print "/")
  (print (denom x)))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn basic-example[]
  (let [one-half   (make-rat 1 2)
        one-third (make-rat 1 -3)]
  (print-rat (add-rat one-half one-third))
  (print-rat (mul-rat one-half one-third))
  (print-rat (add-rat one-third one-third))))

;; Exercise 2.2
(defn make-segment [i e]
  (list i e))

(defn start-segment [s]
  (first s))

(defn end-segment [s]
  (second s))

(defn make-point [x y]
  (list x y))

(defn x-point [p]
  (first p))

(defn y-point [p]
  (second p))

(defn print-point [p]
  (println)
  (print "(")
  (print (x-point p))
  (print ", ")
  (print (y-point p))
  (print ")"))

(defn midpoint-segment[s]
  (let [x0 (x-point (start-segment s))
        y0 (y-point (start-segment s))
        x1 (x-point (end-segment s))
        y1 (y-point (end-segment s))]
    (make-point (/ (+ x0 x1) 2)
                (/ (+ y0 y1) 2))))

(defn show-points[]
  (let [p0 (make-point 2 0)
        p1 (make-point 6 0)
        p2 (make-point 2 1)
        p3 (make-point 6 5)
        s0 (make-segment p0 p1)
        s1 (make-segment p2 p3)]
    (print-point (midpoint-segment s0))
    (print-point (midpoint-segment s1))))

;; Exercise 2.3
(defn make-rectangle1 [x0 y0 w h]
  (list (make-point x0 y0) (make-point (+ x0 w) (+ y0 h))))

(defn height1 [r]
  (- (second (second r))
     (second (first r))))

(defn width1 [r]
  (- (first (second r))
     (first (first r))))

(defn make-rectangle2 [x0 y0 w h]
  (list (make-segment (make-point x0 y0) (make-point (+ x0 w) y0))
        (make-segment (make-point x0 y0) (make-point x0 (+ y0 h)))))

(defn modulus[r]
  (let [x0 (x-point (start-segment r))
        y0 (y-point (start-segment r))
        x1 (x-point (end-segment r))
        y1 (y-point (end-segment r))]
    (Math/sqrt (+ (Math/pow (- x1 x0) 2) (Math/pow (- y1 y0) 2)))))

(defn height2 [r]
  (modulus (second r)))

(defn width2 [r]
  (modulus (first r)))

;; The implementation to use is passed as function parameters to the fns
;; to avoid redefining the procedures in the same file
(defn perimeter [r wfn hfn]
  (+ (* 2 (wfn r)) (* 2 (hfn r))))

(defn area [r wfn hfn]
  (* (wfn r) (hfn r)))

(defn show-rectangles[]
  (let [r1 (make-rectangle1 0 0 8 4)
        r2 (make-rectangle2 0 0 8 4)]
    (print "R1 -> Area: " (area r1 width1 height1) " Perimeter: " (perimeter r1 width1 height1) "\n")
    (print "R2 -> Area: " (area r2 width2 height2) " Perimeter: " (perimeter r2 width2 height2) "\n")
    ))

(show-rectangles)

;; Exercise 2.4
(defn cons-1 [x y]
  (fn [m]
    (m x y)))

(defn car-1 [z]
  (z (fn [p q] p)))

(defn cdr-1 [z]
  (z (fn [p q] q)))

(defn show-cdr[]
  (print "Car: " (car-1 (cons-1 3 4)) "\n")
  (print "Cdr: " (cdr-1 (cons-1 3 4)) "\n"))

;; Exercise 2.5
(defn factorpower[n p]
  {:pre [(> n 0) (or (== p 2) (== p 3))] }
  (if (== (rem n p) 0)
    (inc (factorpower (/ n p) p))
    0))

(defn cons-2 [x y]
  {:pre [(> x 0) (> y 0)] }
  (* (Math/pow 2 x) (Math/pow 3 y)))

(defn car-2 [z]
  (factorpower z 2))

(defn cdr-2 [z]
  (factorpower z 3))

(defn show-cdr-2[]
  (print "Car: " (car-2 (cons-2 3 4)) "\n")
  (print "Cdr: " (cdr-2 (cons-2 3 4)) "\n"))

;; Exercise 2.6
(def zero
  (fn [f]
    (fn [x]
      x)))

(def one
  (fn [f]
    (fn [x]
      (f x))))

(defn add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x))
      )))

(defn show-church-numerals[]
  (println "Zero applied to a number: " ((zero inc) 2))
  (println "One applied o a number: " ((one inc) 2))
  (println "One calculated as succ(0) and applied: " (((add-1 zero) inc) 2)))

;; Exercise 2.7, 2.8, 2.10
(defn make-interval [a b]
  (list a b))

(defn upper-bound [i]
  (second i))

(defn lower-bound [i]
  (first i))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  {:pre [(not (and (> (upper-bound y) 0) (< (lower-bound y) 0)))] }
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(defn print-interval[i]
  (println "(" (lower-bound i) ", " (upper-bound i) ")"))

(defn show-intervals[]
  (let [i1 (make-interval 12.8 13.2)
        i2 (make-interval 5.4 5.6)]
    (print-interval (add-interval i1 i2))
    (print-interval (sub-interval i1 i2))
    (print-interval (mul-interval i1 i2))
    (print-interval (div-interval i1 i2))
    ))

;; Exercise 2.9
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn show-interval-widths[]
  (let [i1 (make-interval 12.8 13.2)
        i2 (make-interval 5.4 5.6)
        sum (add-interval i1 i2)
        substraction (sub-interval i1 i2)
        product (mul-interval i1 i2)
        division (div-interval i1 i2)]
    (println "Width of i1: " (width i1) " Width of i2: " (width i2))
    (println "Width of the sum: " (width sum))
    (println "Width of the substraction: " (width substraction))
    (println "Width of the product: " (width product))
    (println "Width of the division: " (width division))
    ))


;; Exercise 2.11
(defn mul-interval-op [x y]
  (cond
    (and (pos? (lower-bound x)) (pos? (lower-bound y)))
      (make-interval (* (lower-bound x) (lower-bound y))
                (* (upper-bound x) (upper-bound y)))

    (and (neg? (upper-bound x)) (neg? (upper-bound y)))
      (make-interval (* (upper-bound x) (upper-bound y))
                (* (lower-bound x) (lower-bound y)))

    (and (pos? (lower-bound x)) (neg? (upper-bound y)))
      (make-interval (* (upper-bound x) (lower-bound y))
                (* (lower-bound x) (upper-bound y)))

    (and (neg? (upper-bound x)) (pos? (lower-bound y)))
      (make-interval (* (lower-bound x) (upper-bound y))
                (* (upper-bound x) (lower-bound y)))

    (and (neg? (lower-bound x)) (pos? (upper-bound y)) (neg? (upper-bound y)))
      (make-interval (* (upper-bound x) (lower-bound y))
                (* (lower-bound x) (lower-bound y)))

    (and (neg? (lower-bound x)) (pos? (upper-bound y)) (pos? (lower-bound y)))
      (make-interval (* (lower-bound x) (upper-bound y))
                (* (upper-bound x) (upper-bound y)))

    (and (neg? (upper-bound x)) (pos? (lower-bound y)) (neg? (upper-bound x)))
      (make-interval (* (lower-bound x) (upper-bound y))
                (* (lower-bound x) (lower-bound y)))

    (and (neg? (upper-bound x)) (pos? (lower-bound y)) (pos? (lower-bound x)))
      (make-interval (* (upper-bound x) (lower-bound y))
                (* (upper-bound x) (upper-bound y)))

    :else
      (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))


(defn show-improved-mul[]
  (let [i1 (make-interval 12.8 13.2)
        i2 (make-interval 8.7 9.1)
        i3 (make-interval -9.6 2.0)
        i4 (make-interval -5.3 11.2)
        i5 (make-interval -12.8 -3.6)
        i6 (make-interval -7.2 2.7)
        i7 (make-interval -9.4 1.5)
        tests [[i1 i2],[i1 i3],[i1 i4],[i1 i5],[i3 i5],[i5 i5],[i6 i7]]
        display (fn [[x1 x2]]
                  (print-interval (mul-interval x1 x2))
                  (print-interval (mul-interval-op x1 x2))
                  ) ]

    (map display tests)))

;; Exercise 2.12
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [center percent]
  (let [t (Math/abs (* center (/ percent 100.0)))]
  (make-interval (- center t) (+ center t))))

(defn percent[i]
  (let [c (center i)
        t (width i)]
    (Math/abs (* (/ t c) 100))))

(defn show-center-percent[]
  (print-interval (make-center-percent 8 10))
  (print-interval (make-center-percent 6 5))
  (println (percent (make-interval 7.2 8.8)))
  )

;; Exercise 2.13
(defn show-small-tolerance[]
  (let [i1 (make-center-percent 8 0.01)
        i2 (make-center-percent 6 0.5)
        i3 (make-center-percent 7 0.02)]
    (println "%1: " (percent i1) " %2: " (percent i2) " %Product: " (percent (mul-interval i1 i2)))
    (println "%1: " (percent i2) " %2: " (percent i3) " %Product: " (percent (mul-interval i2 i3)))
    (println "%1: " (percent i3) " %2: " (percent i1) " %Product: " (percent (mul-interval i3 i1)))
  ))

;; Exercise 2.14
(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(defn show-differences[]
  (let [i1 (make-center-percent 8 0.01)
        i2 (make-center-percent 6 0.5)
        i3 (make-center-percent 7 0.02)
        diff (fn [[x1 x2]]
              (print-interval (par1 x1 x2))
              (print-interval (par2 x1 x2))
              (println "C1: " (center x1) " C2: " (center x2) " CPar1: " (center (par1 x1 x2)) " CPar2: " (center (par2 x1 x2)))
              (println "%1: " (percent x1) " %2: " (percent x2) " %Par1: " (percent (par1 x1 x2)) " %Par2: " (percent (par2 x1 x2))))
        pairs [[i1 i2], [i2 i3], [i3 i1]]
               ]
    (dorun (map diff pairs))))

;; Exercise 2.17
(defn last-pair[l]
  (if (empty? (rest l))
    (first l)
    (last-pair (rest l))))

;; Exercise 2.18
(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn revers [l]
  (loop [result '()
         current l]
    (if (empty? current)
      result
      (recur (cons (first current) result) (rest current)))))

(revers '(1 2 3 4 5))

;; Exercise 2.19
(def us-coins (list 50 25 10 5 1))

(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [coin-values]
  (empty? coin-values))

(defn except-first-denomination [coin-values]
  (rest coin-values))

(defn first-denomination [coin-values]
  (first coin-values))

(defn cc [amount coin-values]
  (cond (== amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values))))

;; Exercise 2.20
(defn equal-parity? [x y]
  (or (and (even? x) (even? y)) (and (odd? x) (odd? y))))

(defn same-parity[& l]
  (let [initial (first l)]
    (revers (loop [result '()
           current l]
      (cond
        (empty? current) result
        (equal-parity? initial (first current)) (recur (cons (first current) result) (rest current))
        :else (recur result (rest current)))))))

;; Exercise 2.21
(defn square-list1 [items]
  (if (empty? items)
    '()
    (cons (* (first items) (first items)) (square-list1 (rest items)))))

(defn square-list2 [items]
  (map #(* % %) items))

(defn show-square-list[]
  (println (square-list1 '(1 2 3 4)))
  (println (square-list2 '(1 2 3 4))))

;; Exercise 2.23
(defn foreach [f items]
  (if (empty? items)
    true
    (do
      (f (first items))
      (foreach f (rest items)))))

(defn show-foreach[]
  (foreach #(println "The item is: " %) '(1 2 3 4)))

;; Exercise 2.25
(def list1 '(1 3 (5 7) 9))
(def list2 '((7)))
(def list3 '(1 (2 (3 (4 (5 (6 7)))))))

(defn show-carcdrs[]
  (println (-> list1 rest rest first rest first))
  (println (-> list2 first first))
  (println (-> list3 rest first rest first rest first rest first rest first rest first)))

;; Exercise 2.27
(defn deep-reverse[l]
  (let [car (first l)
        cdr (rest l)]
    (cond
      (empty? l) '()
      (list? car) (append (deep-reverse cdr) (list (deep-reverse car)))
      :else       (append (deep-reverse cdr) (list car)))))

(defn show-deep-reverse[]
  (let [x (list (list 1 2) (list 3 4))]
    (println x)
    (println (reverse x))
    (println (deep-reverse x))))

;; Exercise 2.28
(defn fringe[l]
  (let [head (first l)
        tail (rest l)]
    (cond
      (empty? l) l
      (not (list? head)) (cons head (fringe tail))
      :else              (append (fringe head) (fringe tail)))))

(defn show-fringe[]
  (let [x (list (list 1 2) (list 3 4))]
    (println x)
    (println (fringe x))
    (println (fringe (list x x)))))

;; Exercise 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch[m]
  (first m))

(defn right-branch[m]
  (second m))

(defn branch-length[b]
  (first b))

(defn branch-structure[b]
  (second b))

(declare total-weight)

(defn branch-weight[b]
  (if (list? (branch-structure b))
    (total-weight (branch-structure b))
    (branch-structure b)))

(defn total-weight[m]
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(defn torque[b]
  (* (branch-length b) (branch-weight b)))

(defn balanced[m]
  (if (list? m)
    (let [l (left-branch m)
          r (right-branch m)]
      (and (== (torque l) (torque r)) (balanced (branch-structure l)) (balanced (branch-structure r))))

    true))

(defn show-mobiles[]
  (let [b1 (make-branch 12 8)
        b2 (make-branch 9 6)
        m1 (make-mobile b1 b2)
        b3 (make-branch 7 m1)
        m2 (make-mobile b3 b1)
        m3 (make-mobile b1 b1)
        b4 (make-branch 10 m3)
        b5 (make-branch 1 160)
        m4 (make-mobile b4 b5)]

    (println "M2 weight: " (total-weight m2))
    (println "M2 balanced: " (balanced m2))
    (println "M3 balanced: " (balanced m3))
    (println "M4 balanced: " (balanced m4))
    ))

;; Exercise 2.30
(defn square-tree-r[t]
  (cond
    (not (list? t)) (* t t)
    (empty? t) t
    :else
      (cons   (square-tree-r (first t))
              (square-tree-r (rest t)))))

(defn square-tree-m[t]
  (if (list? t)
    (map
        (fn [subt]
           (cond
             (not (list? subt)) (* subt subt)
             (empty? subt) subt
             :else
              (append (square-tree-m (first subt))
                      (square-tree-m (rest subt)))))
         t)
    (list (* t t))))


(defn show-square-trees[]
  (let [t1 (list 1 (list 2 (list 3 4) 5) (list 6 7))]
    (println "T1 recur: " (square-tree-r t1))
    (println "T1 map: " (square-tree-m t1))))


;; Exercise 2.31
(defn tree-map[f t]
  (cond
    (not (list? t)) (f t)
    (empty? t) t
    :else
    (cons   (square-tree-r (first t))
            (square-tree-r (rest t)))))

(defn show-treemap[]
  (let [t1 (list 1 (list 2 (list 3 4) 5) (list 6 7))]
    (println "T1 recur: " (tree-map #(* % %) t1))))

;; Exercise 2.32
(defn subsets [s]
  (if (empty? s)
    (list '())
    (let [cdr (subsets (rest s))]
      (append cdr (map #(cons (first s) %) cdr)))))

;; Basic list operation implementations
(defn accumulate[op initial sequenc]
  (if (empty? sequenc)
    initial
    (op (first sequenc)
        (accumulate op initial (rest sequenc)))))

;; Exercise 2.33
(defn alt-map [p sequenc]
  (accumulate (fn [x y] (cons (p x) y)) nil sequenc))

(defn alt-append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn length [sequenc]
  (accumulate (fn [current total] (+ total 1)) 0 sequenc))

(defn show-accumulators[]
  (println (length '(1 2 3 4)))
  (println (alt-append '(1 2 3) '(4 5 6)))
  (println (alt-map #(* % %) '(1 2 3 4))))

;; Exercise 2.34
(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms]
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(defn show-horner-eval[]
  (let [coeffs (list 1 3 0 5 0 1)]
    (println "Horner evaluation at x=2 -> " (horner-eval 2 coeffs))))

;; Exercise 2.35
(defn get-leaves[t]
  (if (list? t)
    (fringe t)
    (list t)))

(defn count-leaves[t]
  (accumulate (fn [current total] (+ (length current) total)) 0 (map get-leaves t)))

(defn show-count-leaves []
  (let [t1 (list 1 (list 2 (list 3 4) 5) (list 6 7))]
    (println "T1 leaves: " (count-leaves t1))))

;; Exercise 2.36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    '()
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))


(defn show-accumulate-n[]
  (let [s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))]
    (println "Sequence accumulator: " (accumulate-n + 0 s))))

;; Exercise 2.37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product v %) m))

(defn transpose [mat]
  (accumulate-n cons
                '()
                mat))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row]
            (map #(dot-product % row) cols))
         m)))

(defn show-matrix-operations[]
  (let [v '(1 2 3)
        w '(4 5 6)
        m '((1 2 3)(4 5 6)(7 8 9))]
    (println "v·w = " (dot-product v w))
    (println "m·v = " (matrix-*-vector m v))
    (println "m' = " (transpose m))
    (println "m·m = " (matrix-*-matrix m m))
  ))

;; Exercise 2.38
(def fold-right accumulate)

(defn fold-left [op initial sequenc]
  (loop [result initial
         res sequenc]
    (if (empty? res)
      result
      (recur (op result (first res))
             (rest res)))))

(defn show-fold-right[]
  (println (fold-right / 1 (list 1 2 3)))
  (println (fold-left / 1 (list 1 2 3)))
  (println (fold-left + 1 (list 1 2 3)))
  (println (fold-right + 1 (list 1 2 3)))
  (println (fold-right list '() (list 1 2 3)))
  (println (fold-left list '() (list 1 2 3)))
  )

;; Exercise 2.39
(defn reverse1 [s]
  (fold-right (fn [x y]
                  (append y (list x)))
              nil s))

(defn reverse2 [s]
  (fold-left (fn [x y] (cons y x)) nil s))

(defn show-reverse-n []
  (println (reverse1 '(1 2 3 4)))
  (println (reverse2 '(1 2 3 4))))


;; Exercise 2.40
(defn enumerate-interval [low high]
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(defn flatmap [proc sequ]
  (accumulate append nil (map proc sequ)))

(defn prime-sum [pair]
  (sicp-exercises.lesson1/prime? (+ (first pair) (second pair))))

(defn make-pairs [s]
  (let [rs (reverse s)
        h (first rs)
        cdr (rest rs)]
    (map #(list % h) cdr)))

(defn unique-pairs [n]
  (flatmap
    make-pairs
    (map #(enumerate-interval 1 %) (enumerate-interval 1 n))))

(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum? [pair]
  (sicp-exercises.lesson1/prime? (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;; Exercise 2.41
(defn make-triples[s]
  (let [prefix (first s)
        rs (second s)]
    (map #(append prefix (list %)) rs)))

(defn unique-triples[n]
  (flatmap make-triples (map #(list % (enumerate-interval (inc (second %)) n)) (unique-pairs n))))

(defn is-sum? [triple s]
  (== s (reduce + triple)))

(defn sum-triples [n s]
  (filter #(is-sum? % s) (unique-triples n)))

;; Exercise 2.42
(def empty-board '())

(defn adjoin-position[new-row k rest-of-queens]
  (append rest-of-queens (list new-row)))

(defn safe?[k positions]
  (let [queen (last positions)
        rest-of-queens (butlast positions)]

    (loop [current rest-of-queens
           col (dec k)]
      (cond
        (empty? current) true
        (== (first current) queen) false
        (or (== (first current) (+ queen col)) (== (first current) (- queen col))) false
        :else (recur (rest current) (dec col))))))

(defn queens[board-size]
  (let [queen-cols (fn queen-cols [k]
                    (if (== k 0)
                      (list empty-board)
                      (filter
                        #(safe? k %)
                        (flatmap
                          (fn [rest-of-queens]
                            (map #(adjoin-position % k rest-of-queens) (enumerate-interval 1 board-size)))
                          (queen-cols (- k 1))))))]

  (queen-cols board-size)))

;; Exercise 2.46
(defn make-vect [x y]
  (list x y))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(defn show-vectors[]
  (let [v1 (make-vect 2 8)
        v2 (make-vect 1 9)
        v3 (make-vect -4 3)]
    (println "v1= " v1 "v2= " v2 "v3= " v3)
    (println "v1 + v2= " (add-vect v1 v2))
    (println "v2 - v1= " (sub-vect v2 v1))
    (println "5 · v3= " (scale-vect 5 v3))
  ))

;; Exercise 2.47
(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame [f]
  (first f))

(defn edge1-frame [f]
  (second f))

(defn edge2-frame [f]
  (second (rest f)))

;; Exercise 2.48
(defn make-segment[v1 v2]
  (list v1 v2))

(defn start-segment[v]
  (first v))

(defn end-segment[v]
  (second v))

;; Exercise 2.49
(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

(defn segments-painter [segment-list]
  (fn painter[frame]
      (dorun (map (fn liner[segment]
             (g/draw-line
               ((frame-coord-map frame) (start-segment segment))
               ((frame-coord-map frame) (end-segment segment))))
           segment-list))))

(defn draw-painter[painter frame]
  (fn drawer[]
    (painter frame)))

(def wave (segments-painter (list
                                       (make-segment (make-vect 0 0.7)
                                                     (make-vect 0.2 0.5))
                                       (make-segment (make-vect 0.2 0.5)
                                                     (make-vect 0.35 0.65))
                                       (make-segment (make-vect 0.35 0.65)
                                                     (make-vect 0.4 0.6))
                                       (make-segment (make-vect 0.4 0.6)
                                                     (make-vect 0.3 0))
                                       (make-segment (make-vect 0.3 0)
                                                     (make-vect 0.4 0))
                                       (make-segment (make-vect 0.4 0)
                                                     (make-vect 0.5 0.3))
                                       (make-segment (make-vect 0.5 0.3)
                                                     (make-vect 0.6 0))
                                       (make-segment (make-vect 0.6 0)
                                                     (make-vect 0.7 0))
                                       (make-segment (make-vect 0.7 0)
                                                     (make-vect 0.6 0.45))
                                       (make-segment (make-vect 0.6 0.45)
                                                     (make-vect 1 0.2))
                                       (make-segment (make-vect 1 0.2)
                                                     (make-vect 1 0.3))
                                       (make-segment (make-vect 1 0.3)
                                                     (make-vect 0.72 0.7))
                                       (make-segment (make-vect 0.72 0.7)
                                                     (make-vect 0.6 0.7))
                                       (make-segment (make-vect 0.6 0.7)
                                                     (make-vect 0.65 0.85))
                                       (make-segment (make-vect 0.65 0.85)
                                                     (make-vect 0.6 1))
                                       (make-segment (make-vect 0.6 1)
                                                     (make-vect 0.45 1))
                                       (make-segment (make-vect 0.45 1)
                                                     (make-vect 0.4 0.85))
                                       (make-segment (make-vect 0.4 0.85)
                                                     (make-vect 0.45 0.7))
                                       (make-segment (make-vect 0.45 0.7)
                                                     (make-vect 0.35 0.7))
                                       (make-segment (make-vect 0.35 0.7)
                                                     (make-vect 0.2 0.55))
                                       (make-segment (make-vect 0.2 0.55)
                                                     (make-vect 0 0.8))
                                       (make-segment (make-vect 0 0.8)
                                                     (make-vect 0 0.7))
                                        )))

(def frame-paint (segments-painter (list
                                       (make-segment (make-vect 0 0)
                                                     (make-vect 1 0))
                                       (make-segment (make-vect 1 0)
                                                     (make-vect 1 1))
                                       (make-segment (make-vect 1 1)
                                                     (make-vect 0 1))
                                       (make-segment (make-vect 0 1)
                                                     (make-vect 0 0)))))

(def diamond-paint (segments-painter (list
                                       (make-segment (make-vect 0.5 0)
                                                     (make-vect 1 0.5))
                                       (make-segment (make-vect 1 0.5)
                                                     (make-vect 0.5 1))
                                       (make-segment (make-vect 0.5 1)
                                                     (make-vect 0 0.5))
                                       (make-segment (make-vect 0 0.5)
                                                     (make-vect 0.5 0)))))
(defn show-frames[]
  (let [ rects (map #(make-frame (make-vect (+ 20 (* 200 %))  20)
                                 (make-vect 100 0)
                                 (make-vect 0 100)) (range 0 4))

         obliqs (map #(make-frame (make-vect (+ 20 (* 200 %))  300)
                                  (make-vect 100 20)
                                  (make-vect 20 -100)) (range 0 4))

         x-paint (segments-painter (list
                                       (make-segment (make-vect 0 0)
                                                     (make-vect 1 1))
                                       (make-segment (make-vect 0 1)
                                                     (make-vect 1 0))))
         ]

         (g/draw (fn []
                   (frame-paint (nth rects 0))
                   (frame-paint (nth obliqs 0))
                   (x-paint (nth rects 1))
                   (x-paint (nth obliqs 1))
                   (diamond-paint (nth rects 2))
                   (diamond-paint (nth obliqs 2))
                   (wave (nth rects 3))
                   (wave (nth obliqs 3))
                   ))))


;; Exercise 2.50
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
        (make-frame new-origin
                    (sub-vect (m corner1) new-origin)
                    (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn shrink-to-upper-right [painter]
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defn rotate180 [painter]
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(defn rotate270 [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter painter1
                                      (make-vect 0.0 0.0)
                                      split-point
                                      (make-vect 0.0 1.0))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))


(defn show-transformations[]
  (let [ ups (map #(make-frame (make-vect (+ 20 (* 200 %))  220)
                                 (make-vect 200 0)
                                 (make-vect 0 -200)) (range 0 4))

         downs (map #(make-frame (make-vect (+ 20 (* 200 %))  500)
                                  (make-vect 200 0)
                                  (make-vect 0 -200)) (range 0 4))]
    (g/draw (fn []
                   (dorun
                     (map #(do
                             (wave (nth ups %))
                             (frame-paint (nth ups %))
                             (frame-paint (nth downs %))) (range 0 4)))
                   ((rotate90 wave) (nth downs 0))
                   ((rotate270 wave) (nth downs 1))
                   ((flip-horiz wave) (nth downs 2))
                   ((rotate180 wave) (nth downs 3))
                   ))))

;; Exercise 2.51
(defn below[painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)
        paint-up (transform-painter painter1
                                    split-point
                                    (make-vect 1.0 0.5)
                                    (make-vect 0.0 1.0))
        paint-down (transform-painter painter2
                                      (make-vect 0.0 0.0)
                                      (make-vect 1.0 0.0)
                                      split-point)]
    (fn [frame]
      (paint-up frame)
      (paint-down frame))))

(defn show-below[]
  (let [ ups (map #(make-frame (make-vect (+ 20 (* 200 %))  220)
                                 (make-vect 200 0)
                                 (make-vect 0 -200)) (range 0 4))

         downs (map #(make-frame (make-vect (+ 20 (* 200 %))  500)
                                  (make-vect 200 0)
                                  (make-vect 0 -200)) (range 0 4))]
    (g/draw (fn []
                   (dorun
                     (map #(do
                             (wave (nth ups %))
                             (frame-paint (nth ups %))
                             (frame-paint (nth downs %))) (range 0 4)))
                   ((beside wave wave) (nth downs 0))
                   ((below wave wave) (nth downs 1))
                   (let [r9 (rotate90 wave)]
                     ((rotate270 (beside r9 r9)) (nth downs 2)))))))

;; Exercise 2.45
(defn painter-split [transf1 transf2]
  (fn inner-split[painter n]
    (if (== 0 n)
      painter
      (let [smaller (inner-split painter (- n 1))]
        (transf1 painter (transf2 smaller smaller))))))

(def right-split (painter-split beside below))
(def up-split (painter-split below beside))

(defn show-split-transforms[]
  (let [ ups (map #(make-frame (make-vect (+ 20 (* 400 %))  520)
                               (make-vect 400 0)
                               (make-vect 0 -400)) (range 0 4))]

    (g/draw (fn []
              ((right-split wave 10) (nth ups 0))
              ((up-split wave 10) (nth ups 1))))))


;; Exercise 2.52
(defn corner-split [painter n]
  (if (== n 0)
    painter
    (let [up (up-split painter (- n 1))
          right (right-split painter (- n 1))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (- n 1))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

(defn alt-square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (rotate90 quarter) (rotate180 quarter))]
    (below (rotate180 half) half)))

(defn show-square-limit[]
  (let [ ups (map #(make-frame (make-vect (+ 20 (* 300 %))  320)
                                 (make-vect 300 0)
                                 (make-vect 0 -300)) (range 0 4))

         downs (map #(make-frame (make-vect (+ 20 (* 300 %))  630)
                                  (make-vect 300 0)
                                  (make-vect 0 -300)) (range 0 4))

         wave-fn (square-limit wave 5)
         diamond-fn (square-limit diamond-paint 5)
         alt-square-fn (alt-square-limit wave 5)
         ]

    (g/draw (fn []
              (wave-fn (nth ups 0))
              (diamond-fn (nth ups 1))
              (alt-square-fn (nth downs 0))
              ))))

;; Exercise 2.53
(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

;; Exercise 2.54
(defn equal?[o1 o2]
  (cond
    (and (not (list? o1)) (not (list? o2))) (= o1 o2)
    (and (list? o1) (list? o2) (empty? o1) (empty? o2)) true
    (and (list? o1) (list? o2)) (and (equal? (first o1) (first o2)) (equal? (rest o1) (rest o2)))
    :else false))

;; Exercise 2.56/2.57
(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp numb]
  (and (number? exp) (= exp numb)))

(defn make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn sum? [x]
  (and (list? x) (= (first x) '+)))

(defn addend [s]
  (second s))

(defn augend [s]
  (let [res (rest (rest s))]
    (cond
      (and (list? res) (== (count res) 1)) (first res)
      (list? res) (append '(+) res)
      :else res
      )))

(defn product? [x]
  (and (list? x) (= (first x) '*)))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
 (let [res (rest (rest p))]
    (cond
      (and (list? res) (== (count res) 1)) (first res)
      (list? res) (append '(*) res)
      :else res
      )))

(defn make-exponentiation [m1 m2]
  (cond
    (=number? m1 0) 0
    (=number? m2 0) 1
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (Math/pow m1 m2)
    :else (list '** m1 m2)))

(defn base [x]
  (second x))

(defn exponent [x]
  (second (rest x)))

(defn exponentiation? [x]
  (and (list? x) (= (first x) '**)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
          (if (same-variable? exp var) 1 0)
        (sum? exp)
          (make-sum
                    (deriv (addend exp) var)
                    (deriv (augend exp) var))
        (product? exp)
          (make-sum
                    (make-product (multiplier exp)
                                  (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                  (multiplicand exp)))
        (exponentiation? exp)
          (make-product
                    (exponent exp)
                    (make-exponentiation (base exp)
                                         (make-sum (exponent exp)
                                                   -1)))
        :else
          (println "Derivation error: unknown expressions")))

(defn show-deriv[]
  (println (deriv '(+ x 3) 'x))
  (println (deriv '(* x y) 'x))
  (println (deriv '(* (* x y) (+ x 3)) 'x))
  (println (deriv '(** x y) 'x)))

;; Exercise 2.58
(defn alt-make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list a1 '+ a2)))

(defn alt-make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list m1 '* m2)))

(defn alt-sum? [x]
  (and (list? x) (> (count (filter #(= '+ %) x)) 0)))

(defn alt-addend [s]
  (let [result (first (split-with #(not (= '+ %)) s))]
    (if (and (seq? result) (== 1 (count result)))
      (first result)
      result)))

(defn alt-augend [s]
  (let [result (rest (first (rest (split-with #(not (= '+ %)) s))))]
    (if (and (seq? result) (== 1 (count result)))
      (first result)
      result)))

(defn alt-product? [x]
  (and (list? x) (= (second x) '*)))

(defn alt-multiplier [p]
  (first p))

(defn alt-multiplicand [p]
  (second (rest p)))

(defn alt-deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
          (if (same-variable? exp var) 1 0)
        (alt-sum? exp)
          (alt-make-sum
                    (alt-deriv (alt-addend exp) var)
                    (alt-deriv (alt-augend exp) var))
        (alt-product? exp)
          (alt-make-sum
                    (alt-make-product (alt-multiplier exp)
                                      (alt-deriv (alt-multiplicand exp) var))
                    (alt-make-product (alt-deriv (alt-multiplier exp) var)
                                      (alt-multiplicand exp)))
        :else
          (println "Derivation error: unknown expressions")))

(defn show-alt-deriv[]
  (println (alt-deriv '(x + 3) 'x))
  (println (alt-deriv '(x * y) 'x))
  (println (alt-deriv '((x * y) * (x * 3)) 'x))
  (println (alt-deriv '((3 * x) + (5 * y) + (10 * (x * y))) 'x)))

;; Exercise 2.59
(defn element-of-set? [x uset]
  (cond
    (empty? uset) false
    (equal? x (first uset)) true
    :else (element-of-set? x (rest uset))))

(defn adjoin-set [x uset]
  (if (element-of-set? x set)
    x
    (cons x uset)))

(defn intersection-set [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) '()
    (element-of-set? (first set1) set2)
      (cons (first set1)
            (intersection-set (rest set1) set2))
    :else
      (intersection-set (rest set1) set2)))

(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (element-of-set? (first set1) set2)
      (union-set (rest set1) set2)
    :else
      (cons (first set1)
            (union-set (rest set1) set2))))

(defn show-sets[]
  (let [s1 '(1 5 2)
        s2 '(5 9 8 2)
        s3 '(7 3 9 1)]
    (println (element-of-set? 2 s1))
    (println (element-of-set? 3 s2))
    (println (intersection-set s1 s2))
    (println (union-set s1 s2))
    (println (union-set s1 s3))
    ))

;; Exercise 2.60
(defn dup-element-of-set? [x uset]
  (cond
    (empty? uset) false
    (equal? x (first uset)) true
    :else (element-of-set? x (rest uset))))

(defn dup-adjoin-set [x uset]
  (cons x uset))

(defn dup-intersection-set [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) '()
    (dup-element-of-set? (first set1) set2)
      (cons (first set1)
            (dup-intersection-set (rest set1) set2))
    :else
      (dup-intersection-set (rest set1) set2)))

(defn dup-union-set [set1 set2]
  (if (empty? set1)
    set2
    (cons (first set1) (dup-union-set (rest set1) set2))))

(defn show-dup-sets[]
  (let [s1 '(1 5 2)
        s2 '(5 9 8 2)
        s3 '(7 3 9 1)]
    (println (dup-element-of-set? 2 s1))
    (println (dup-element-of-set? 3 s2))
    (println (dup-intersection-set s1 s2))
    (println (dup-union-set s1 s2))
    (println (dup-union-set s1 s3))
    ))

