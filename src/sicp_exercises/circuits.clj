(ns sicp-exercises.circuits
  (:require [sicp-exercises.pairs]
            [sicp-exercises.queue])
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]
        [sicp-exercises.queue]))

(defn make-time-segment [the-time queue]
  (Pair. the-time queue))

(defn segment-time [s]
  (.getCar s))

(defn segment-queue [s]
  (.getCdr s))

(defn make-agenda []
  (Pair. 0 '()))

(defn current-time [agenda]
  (.getCar agenda))

(defn set-current-time! [agenda the-time]
  (.setCar agenda the-time))

(defn segments [agenda]
  (.getCdr agenda))

(defn set-segments! [agenda segments]
  (.setCdr agenda segments))

(defn first-segment [agenda]
  (first (segments agenda)))

(defn rest-segments [agenda]
  (rest (segments agenda)))

(defn empty-agenda? [agenda]
  (empty? (segments agenda)))

(def the-agenda (make-agenda))
(def inverter-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 5)

(defn remove-first-agenda-item! [agenda]
  (let [q (segment-queue (first-segment agenda))]
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (throw (Exception. "Agenda is emtpy -- FIRST-AGENDA-ITEM"))
    (let [ first-seg (first-segment agenda) ]
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(defn add-to-agenda! [the-time action agenda]
  (let [ belongs-before? (fn [segments]
                           (or (empty? segments)
                               (< the-time (segment-time (first segments)))))

         make-new-time-segment (fn [the-time action]
                                 (let [q (make-queue)]
                                   (insert-queue! q action)
                                   (make-time-segment the-time q)))

         add-to-segments! (fn add-to-segments! [segments]
                            (if (== (segment-time (first segments)) the-time)
                              (do
                                (insert-queue! (segment-queue (first segments))
                                             action)
                                segments)
                              (let [the-rest (rest segments)]
                                (cond
                                  (empty? the-rest) (list  (first segments) (make-new-time-segment the-time action))
                                  (belongs-before? the-rest) (cons (first segments) (cons (make-new-time-segment the-time action) the-rest))
                                  :else (cons (first segments) (add-to-segments! the-rest))))))

         segments (segments agenda)]

    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment the-time action)
              segments))
      (set-segments! the-agenda (add-to-segments! segments)))))

(defn propagate[]
  (if (empty-agenda? the-agenda)
    'done
    (let [ first-item (first-agenda-item the-agenda) ]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(defn after-delay [new-delay action]
  (add-to-agenda! (+ new-delay (current-time the-agenda))
                  action
                  the-agenda))

(defn call-each [procedures]
  (if (empty? procedures)
    'done
    (do
      ((first procedures))
      (call-each (rest procedures)))))

(defn make-wire []
  (let [ signal-value (atom 0)
         action-procedures (atom '())
         set-my-signal! (fn [new-value]
                          (if (not (= signal-value new-value))
                            (do
                              (reset! signal-value new-value)
                              (call-each @action-procedures))
                            'done))

         accept-action-procedure! (fn [proc]
                                    (swap! action-procedures #(cons proc %))
                                    (proc))

         dispatch (fn [m]
                    (cond
                      (= m 'get-signal) @signal-value
                      (= m 'set-signal) set-my-signal!
                      (= m 'add-action!) accept-action-procedure!
                      :else (throw (Exception. "Unknown operation -- WIRE"))))]
    dispatch))

(defn get-signal [wire]
  (wire 'get-signal))

(defn set-signal! [wire new-value]
  ((wire 'set-signal) new-value))

(defn add-action! [wire action-procedure]
  ((wire 'add-action!) action-procedure))

(defn probe [wire-name wire]
  (add-action! wire
               (fn []
                 (println wire-name " " (current-time the-agenda) " New-value= " (get-signal wire)))))

(defn logical-not [s]
  (cond
    (== s 0) 1
    (== s 1) 0
    :else (throw (Exception. "Invalid signal"))))

(defn logical-and [s1 s2]
  (cond
    (or (not (or (== s1 1) (== s1 0))) (not (or (== s2 1) (== s2 0)))) (throw (Exception. "Invalid signal"))
    (and (== s1 1) (== s2 1)) 1
    :else 0))

(defn logical-or [s1 s2]
  (cond
    (or (not (or (== s1 1) (== s1 0))) (not (or (== s2 1) (== s2 0)))) (throw (Exception. "Invalid signal"))
    (or (== s1 1) (== s2 1)) 1
    :else 0))

(defn inverter [input output]
  (let [ invert-input (fn []
                        (let [new-value (logical-not (get-signal input)) ]
                          (after-delay inverter-delay
                                      (fn []
                                        (set-signal! output new-value)))))]
         (add-action! input invert-input)
         'ok))

(defn and-gate [a1 a2 output]
  (let [and-action-procedure (fn []
                               (let [new-value (logical-and (get-signal a1) (get-signal a2))]
                                 (after-delay and-gate-delay
                                              (fn []
                                                (set-signal! output new-value)))))]
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok))

(defn or-gate [a1 a2 output]
  (let [or-action-procedure (fn []
                              (let [new-value (logical-or (get-signal a1) (get-signal a2))]
                                (after-delay or-gate-delay
                                             (fn []
                                               (set-signal! output new-value)))))]
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

(defn show-circuits[]
  (let [ input-1 (make-wire)
         output-1 (make-wire)]

    (probe 'input input-1)
    (probe 'inverted output-1)
    (inverter input-1 output-1)
    (println "Setting signal to 1")
    (set-signal! input-1 1)
    (println "Propagating signal.....")
    (propagate)
    (println "Output signal: " (get-signal output-1))
    (set-signal! input-1 0)
    (println "Propagating signal.....")
    (propagate)
    (println "Output signal: " (get-signal output-1))
    ))

(defn show-logic-operator[operator]
  (let [ a (make-wire)
         b (make-wire)
         c (make-wire)
         test-case (fn test-case [[i1 i2]]
                     (println "Setting inputs [" i1 " " i2 "]")
                     (set-signal! a i1)
                     (set-signal! b i2)
                     (propagate)
                     (println "Output signal = " (get-signal c))
                     (get-signal c)
                     )]
    (probe 'i1 a)
    (probe 'i2 b)
    (probe 'and-result c)
    (operator a b c)
    (dorun
      (map test-case [[0 0] [0 1] [1 0] [1 1]]))
  ))

(defn show-and[]
  (show-logic-operator and-gate))

(defn show-or[]
  (show-logic-operator or-gate))

(defn half-adder [a b s c]
  (let [ d (make-wire)
         e (make-wire) ]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defn show-half-adder[]
  (let [ a (make-wire)
         b (make-wire)
         c (make-wire)
         s (make-wire)
         test-case (fn test-case [[i1 i2]]
                     (println "Setting inputs [" i1 " " i2 "]")
                     (set-signal! a i1)
                     (set-signal! b i2)
                     (println "Propagating...")
                     (propagate)
                     (println "Output signal = [" (get-signal s) " " (get-signal c) "]")
                     (get-signal c)
                     )]
    (probe 'sum-result s)
    (probe 'carry c)
    (half-adder a b s c)
    (propagate)
    (dorun
      (map test-case [[0 0] [1 0] [0 1] [1 1]]))
    ))

