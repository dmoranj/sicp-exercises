(ns sicp-exercises.constraints
  (:require [sicp-exercises.pairs])
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]))

(defn has-value? [connector]
  (connector 'has-value?))

(defn get-value [connector]
  (connector 'value))

(defn set-value! [connector new-value informant]
  ((connector 'set-value!) new-value informant))

(defn forget-value! [connector retractor]
  ((connector 'forget) retractor))

(defn connect [connector new-constraint]
  ((connector 'connect) new-constraint))

(defn inform-about-value [constraint]
  (constraint 'I-have-a-value))

(defn inform-about-no-value [constraint]
  (constraint 'I-lost-my-value))

(defn adder [a1 a2 sum]
  (letfn [(process-new-value []
                             (cond
                               (and (has-value? a1) (has-value? a2)) (set-value! sum
                                                                                 (+ (get-value a1) (get-value a2))
                                                                                 me)
                               (and (has-value? a1) (has-value? sum)) (set-value! a2
                                                                                  (- (get-value sum) (get-value a1))
                                                                                  me)
                               (and (has-value? a2) (has-value? sum)) (set-value! a1
                                                                                  (- (get-value sum) (get-value a2))
                                                                                  me)))

          (process-forget-value []
                                (forget-value! sum me)
                                (forget-value! a1 me)
                                (forget-value! a2 me)
                                (process-new-value))

          (me[request]
              (cond
                (= request 'I-have-a-value) (process-new-value)
                (= request 'I-lost-my-value) (process-forget-value)
                :else (throw (Exception. "Unknown request --- ADDER")))) ]

      (connect a1 me)
      (connect a2 me)
      (connect sum me)))

(defn multiplier [m1 m2 product]
  (letfn [ (process-new-value []
                              (cond
                                (or (and (has-value? m1) (== (get-value m1) 0))
                                    (and (has-value? m2) (== (get-value m2) 0))) (set-value! product 0)
                                (and (has-value? m1) (has-value? m2)) (set-value! product
                                                                                  (* (get-value m1) (get-value m2))
                                                                                  me)
                                (and (has-value? product) (has-value? m1)) (set-value! m2
                                                                                       (/ (get-value product) (get-value m1))
                                                                                       m2)
                                (and (has-value? product) (has-value? m2)) (set-value! m1
                                                                                       (/ (get-value product) (get-value m2))
                                                                                       me)))

           (process-forget-value []
                                 (forget-value! product me)
                                 (forget-value! m1 me)
                                 (forget-value! m2 me)
                                 (process-new-value))

           (me [request]
               (cond
                 (= request 'I-have-a-value) (process-new-value)
                 (= request 'I-lost-my-value) (process-forget-value)
                 :else (throw (Exception. "Unknown request --- ADDER")))) ]
    (connect m1 me)
    (connect m2 me)
    (connect product me)))

(defn constant [value connector]
  (let [ me (fn [request]
              (throw (Exception. "Unknown request -- CONSTANT"))) ]
    (connect connector me)
    (set-value! connector value me)))

(defn const-probe [the-name connector]
  (letfn [ (print-probe [value]
                        (println "Probe: " the-name " = " value))

           (process-new-value []
                              (print-probe (get-value connector)))

           (process-forget-value []
                                 (print-probe "?"))

           (me [request]
               (cond
                 (= request 'I-have-a-value) (process-new-value)
                 (= request 'I-lost-my-value) (process-forget-value)
                 :else (throw (Exception. "Unkwnon request -- PROBE"))))]

    (connect connector me)))

(defn for-each-except [exception procedure the-list]
  (letfn [ (loopfn [items]
                  (cond
                    (empty? items) 'done
                    (= (first items) exception) (loopfn (rest items))
                    :else (do
                            (procedure (first items))
                            (loopfn (rest items)))))]
    (loopfn the-list)))

(defn make-connector []
  (let [ value (atom false)
         constraints (atom '())
         informant (atom false)]
    (letfn [ (set-my-value [newval setter]
                           (cond
                             (not (has-value? me)) (do
                                                     (reset! value newval)
                                                     (reset! informant setter)
                                                     (for-each-except setter
                                                                      inform-about-value
                                                                      @constraints))

                             (not (= @value newval)) (throw (Exception. (str "Contradiction: " (list @value newval))))

                             :else 'ignored))

             (forget-my-value [retractor]
                              (if (= retractor @informant)
                                (do
                                  (reset! informant false)
                                  (for-each-except retractor
                                                   inform-about-no-value
                                                   @constraints))
                                'ignored))

             (connect [new-constraint]
                      (if (not (some #(= % new-constraint) @constraints))
                        (reset! constraints (cons new-constraint @constraints)))
                      (if (has-value? me)
                        (inform-about-value new-constraint))
                      'done)

             (me [request]
                 (cond
                   (= request 'has-value?) (if @informant true false)
                   (= request 'value) @value
                   (= request 'set-value!) set-my-value
                   (= request 'forget) forget-my-value
                   (= request 'connect) connect
                   :else (throw (Exception. (str "Unknown operation -- CONECTOR" request)))))]
      me)))

(defn show-constraints[]
  (let [ a (make-connector)
         b (make-connector)
         c (make-connector) ]
    (adder a b c)
    (const-probe "Input 1: " a)
    (const-probe "Input 2: " b)
    (const-probe "Result: " c)
    (set-value! a 9 'user)
    (set-value! c 2 'user)
    ))

