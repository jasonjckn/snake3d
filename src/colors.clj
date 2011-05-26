(ns colors)

(defn interp [factor src dst]
  (+ src (* factor (- dst src))))

(defn interp-3 [factor src dst]
  (map (partial interp factor) src dst))

(def max-promotion 0.8)
(def error-clr [0.9 0 0])
(def food-clr [0.9 0.9 0])
(def natural-clr [[0.1 0.5 0.7] [0 0.1 0.9] [0.2 0.3 0.8] [0 0.6 0.9]])
(def snake-clr [0 0.8 0.1])
(defn transition-clr [p] (map #(interp-3 p % snake-clr) natural-clr))
