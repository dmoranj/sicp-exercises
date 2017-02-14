(ns drawing-tests.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  {})

(defn update-state [state]
  {})

(defn draw-state [state]
  (q/background 240)

  (q/fill 0 255 255)
  (q/ellipse 100 100 100 100))

(defn start-drawing[]
  (q/defsketch drawing-tests
  :title "You spin my circle right round"
  :size [1000 800]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode]))

(start-drawing)
