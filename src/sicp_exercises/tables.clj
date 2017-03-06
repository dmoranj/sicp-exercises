(ns sicp-exercises.tables
  (:require [sicp-exercises.pairs])
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]))

(defn t-assoc [k records]
  (cond
    (nil? records) false
    (= k (.getCaar records)) (.getCar records)
    :else (t-assoc k (.getCdr records))))

(defn t-lookup [k table]
  (let [ record (t-assoc k (.getCdr table)) ]
    (if record
      (.getCdr record)
      false)))

(defn t-insert! [k value table]
  (let [record (t-assoc k (.getCdr table))]
    (if record
      (.setCdr record value)
      (.setCdr table
               (Pair. (Pair. k value) (.getCdr table))))))

(defn lookup [ key1 key2 table ]
  (let [ subtable (t-assoc key1 (.getCdr table)) ]
    (if subtable
      (let [ record (t-assoc key2 (.getCdr subtable)) ]
        (if record
          (.getCdr record)
          false))
      false)))

(defn t-insert2! [key1 key2 value table]
  (let [ subtable (t-assoc key1 (.getCdr table)) ]
    (if subtable
      (let [ record (t-assoc key2 (.getCdr subtable)) ]
        (if record
          (.setCdr record value)
          (.setCdr subtable
                   (Pair. (Pair. key2 value)
                          (.getCdr subtable)))))
      (.setCdr table
               (Pair. (Pair. key1
                             (Pair. (Pair. key2 value)
                                    nil))
                      (.getCdr table))))
    'ok))

(defn make-table []
  (Pair. '*table* nil))

(defn show-tables-2-keys []
  (let [ table  (make-table) ]
    (t-insert2! :math :+ 43 table)
    (t-insert2! :math :- 45 table)
     (t-insert2! :math :* 42 table)
     (t-insert2! :letters :b 97 table)
     (t-insert2! :letters :a 98 table)
     (println "Full table: " table)
     (println "Value for keys :math :* = " (lookup :math :* table))
     (println "Value for key :letters :b = " (lookup :letters :b table))
    ))

(defn show-tables[]
  (let [ table (make-table) ]
    (t-insert! :math :+ table)
    (t-insert! :letters :a table)
    (println "Full table: " table)
    (println "Value for key :math = " (t-lookup :math table))
    (println "Value for key :letters = " (t-lookup :letters table))
    ))
