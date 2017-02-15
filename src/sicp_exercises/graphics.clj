(ns sicp-exercises.graphics
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 5)
  (q/color-mode :rgb)
  {})

(defn draw-state [state]
  (q/background 240)

  (q/fill 0 255 255)
  (q/ellipse 100 100 100 100))

(defn draw[f]
  (q/sketch
    :size [1000 800]
    :setup setup
    :draw f
    :features [:keep-on-top]))

(defn draw-line[x1 x2]
  (q/stroke 0 0 0)
  (q/line x1 x2))

(defn test-draw[state]
  (draw-line '(0 0) '(199 199))
  )

