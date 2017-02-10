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

