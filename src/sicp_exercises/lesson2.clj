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

