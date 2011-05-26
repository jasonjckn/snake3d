(ns cube
  (:use
   [util]
   [penumbra opengl]))

(def cube-vertex (read-string (slurp "cube.vertex")))

(defn solid-color-cube [color]
  (let [add-color (fn [v] (assoc v :color color))]
    (map add-color cube-vertex)))


(defn four-color-cube [colors]
  (let [colors (map #(hash-map :color %) colors)
        add-4-colors (fn [vertices] (map merge colors vertices))]
    (flatten (map add-4-colors (partition 4 cube-vertex)))))


(defn render [mesh]
  (draw-quads
   (doseq [m mesh]
     (color (:color m)) (vertex (:vertex m)))))

(defnl register-mesh [st kw mesh]
  (assoc-in st [:meshes kw] gl-list)
  :where
  [gl-list (create-display-list (render mesh))])

(defnl render-mesh [st kw]
  (call-display-list (kw (:meshes st))))

