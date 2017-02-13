(ns sicp-exercises.lesson2)

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

