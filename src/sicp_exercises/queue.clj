(ns sicp-exercises.queue
  (:require [sicp-exercises.pairs])
  (:import (sicp_exercises.pairs Pair))
  (:use [sicp-exercises.pairs]))

(defn front-ptr [queue]
  (.getCar queue))

(defn rear-ptr [queue]
  (.getCdr queue))

(defn set-front-ptr! [queue item]
  (.setCar queue item))

(defn set-rear-ptr! [queue item]
  (.setCdr queue item))

(defn empty-queue? [queue]
  (nil? (front-ptr queue)))

(defn make-queue []
  (Pair. nil nil))

(defn front-queue [queue]
  (if (empty-queue? queue)
    (throw (Exception. "FRONT Called with an empty queue"))
    (.getCar (front-ptr queue))))

(defn insert-queue! [queue item]
  (let [ new-pair (Pair. item nil) ]
    (if (empty-queue? queue)
      (do
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue)
      (do
        (.setCdr (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))

(defn delete-queue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. "DELETE! called with an empty queue"))
    (do
      (set-front-ptr! queue (.getCdr (front-ptr queue)))
      queue
      )))

(defn show-queues[]
  (let [ q (make-queue) ]
    (insert-queue! q 2)
    (insert-queue! q 3)
    (insert-queue! q 4)
    (println "First " (front-queue q))
    (delete-queue! q)
    (println "Second " (front-queue q))
    (delete-queue! q)
    (println "Third " (front-queue q))
    (delete-queue! q)
    q))

(show-queues)

