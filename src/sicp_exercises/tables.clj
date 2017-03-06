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

(defn lookup [ key1 key2 table ]
  (let [ subtable (t-assoc key1 (.getCdr table)) ]
    (if subtable
      (let [ record (t-assoc key2 (.getCdr subtable)) ]
        (if record
          (.getCdr record)
          false))
      false)))

(defn rec-insert [k value table]
  (let [record (t-assoc k (.getCdr table))]
    (if record
      (.setCdr record value)
      (.setCdr table
               (Pair. (Pair. k value) (.getCdr table))))))

(defn t-insert! [key-vector value table]
  (let [ first-key (first key-vector)
         rest-keys (rest key-vector)
         subtable (t-assoc first-key (.getCdr table))]
    (if subtable
      (if (== (count rest-keys) 1)
        (rec-insert (first rest-keys) value subtable)
        (t-insert! rest-keys value subtable))

      (let [ new-subtable (Pair. first-key nil) ]
        (.setCdr table
                 (Pair. new-subtable
                        (.getCdr table)))
        (t-insert! key-vector value table)))))

(defn make-table []
  (Pair. '*table* nil))

(defn show-tables-2-keys []
  (let [ table  (make-table) ]
    (t-insert! [ :math :+ ] 43 table)
    (t-insert! [ :math :- ] 45 table)
     (t-insert! [ :math :* ] 42 table)
     (t-insert! [ :letters :b ] 97 table)
     (t-insert! [ :letters :a ] 98 table)
     (println "Full table: " table)
;;     (println "Value for keys :math :* = " (lookup :math :* table))
;;     (println "Value for key :letters :b = " (lookup :letters :b table))
    ))

(defn show-tables[]
  (let [ table (make-table) ]
    (t-insert! :math :+ table)
    (t-insert! :letters :a table)
    (println "Full table: " table)
    (println "Value for key :math = " (t-lookup :math table))
    (println "Value for key :letters = " (t-lookup :letters table))
    ))

(show-tables-2-keys)
