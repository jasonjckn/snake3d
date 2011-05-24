(ns cube
  (:use [penumbra opengl]))

(defn interp [factor src dst]
  (+ src (* factor (- dst src))))

(defn interp-3 [factor src dst]
  (map (partial interp factor) src dst))

(def cube-vertex (read-string (slurp "cube.vertex")))

(defn solid-color-cube [color]
  (let [add-color (fn [v] (assoc v :color color))]
    (map add-color cube-vertex)))


(defn four-color-cube [colors]
  (let [colors (map #(hash-map :color %) colors)
        add-4-colors (fn [vertices] (map merge colors vertices))]
    (flatten (map add-4-colors (partition 4 cube-vertex)))))

(def error-clr [0.9 0 0])
(def food-clr [0.9 0.9 0])
(def natural-clr [[0.1 0.5 0.7] [0 0.1 0.9] [0.2 0.3 0.8] [0 0.6 0.9]])
(def snake-clr [0 0.8 0.1])
(defn transition-clr [p] (map #(interp-3 p % snake-clr) natural-clr))

(defn render [mesh]
  (draw-quads
   (doseq [m mesh]
     (color (:color m)) (vertex (:vertex m)))))

(defn render-transition-cube [promotion]
  (render (four-color-cube (transition-clr promotion))))


