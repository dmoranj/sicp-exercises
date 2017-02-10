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
