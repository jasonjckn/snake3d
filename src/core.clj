(ns core
  (:use
   [penumbra opengl]
   [util]
   [pallet thread-expr]
   [matchure])
  (:require
   [cube :as c]
   [colors :as l]
   [penumbra.app :as app]
   [penumbra.text :as text]))

(defn advance)
(defn add-food)
(def max-promotion 0.8)


(defnl simple-state []
  {:dimensions d
   :rot-x 39, :rot-y 155

   :snake-head [0 0] :snake-tail nil :snake-dir [0 1] 
   :snake-body [[0 0]] :snake-len 3

   :board (vec (repeat d (vec (repeat d {:promotion 0.0}))))}
  :where [d 10])

(defnl initial-state []
  (-> (simple-state)
      (register-all-meshes))
  :where
  [register-all-meshes (fn-st
                        (c/register-mesh :natural (c/four-color-cube l/natural-clr))
                        (c/register-mesh :food (c/solid-color-cube l/food-clr))
                        (c/register-mesh :error (c/solid-color-cube l/error-clr))
                        (c/register-mesh :promo (c/four-color-cube
                                                 (l/transition-clr max-promotion))))])

(defn init [st]
  (enable :depth-test)
  (depth-test :lequal)
  (app/periodic-update! 3 (wrap-exc #'advance))
  (app/periodic-update! 0.5 (wrap-exc #'add-food))
  (initial-state))


(defn reshape [[x y width height] st]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  st)

(defn mouse-drag [[dx dy] _ _ st]
  (-> st
      (update-in [:rot-x] #(+ dy %))
      (update-in [:rot-y] #(+ dx %))))

(defnl gen-board [{:keys [dimensions board game-over] :as st} f]
  (doseq [y rng x rng]
    (plet 
     (push-matrix
      (translate (+ offset y) 0 (+ offset x))
      (scale 0.45 (+ 0.45 (* height-f 0.30)) 0.45)
      (f st cell))

     :where [{:keys [promotion] :as cell} ((board y) x)
             height-f (/ promotion 2) ]))

  :where [offset (- (/ dimensions 2))
          rng (range dimensions)])

(defn game-over-board [st {:keys [promotion]}]
  (c/render-mesh st
   (if (< promotion 0.1) :natural :error)))

(defnl play-board [st {:keys [food promotion]}]
  (cond
   food (c/render-mesh st :food)
   
   (< promotion 0.1) (c/render-mesh st :natural)
   (> promotion (- max-promotion 0.1)) (c/render-mesh st :promo)
   (> promotion 0) (render-growing-cube promotion))

  :where
  [render-growing-cube (fn [promotion]
                         (c/render (c/four-color-cube (l/transition-clr promotion))))])

(defn draw-board [{:keys [game-over] :as st}]
  (if game-over
    (gen-board st game-over-board)
    (gen-board st play-board)))

(defnl hit-detection [st]
  (-> st (when-> (duplicates? (:snake-body st))
                 (assoc :game-over true)))
  :where [duplicates? #(not= (count %) (count (set %)))])


(defnl add-food [st]
  (assoc-in st `(:board ~@random-coords :food) true)
  :where
  [rnd (fn [] (rand-int (:dimensions st)))
   random-coords [(rnd) (rnd)]])


(defnl advance [st]
  (-> st
      (when-not-> (:game-over st)
                  (update-in [:snake-head] advance-head)
                  (update-in [:snake-tail] advance-tail)

                  (update-body)
                  (hit-detection)
                  (eat-food)))

  :where
  [apply-bounds (fn [addr] (let [ubound (- (:dimensions st) 1)]
                            (map max [0 0] (map min [ubound ubound] addr))))
   
   advance-head (fn [addr] (apply-bounds (map + addr (:snake-dir st))))

   advance-tail (fn [old] (let [t (:snake-body st)]
                           (if (>= (count t) (:snake-len st))
                             (first t)
                             old)))

   tail-subset (fn [t] (if (>= (count t) (:snake-len st))
                        (drop 1 t) t))

   eat-food (fnl [st] (-> st
                      (when-> (select-in st food-addr)
                              (update-in [:snake-len] #(+ 1 %)))
                      (assoc-in food-addr false))
             :where [food-addr `(:board ~@(:snake-head st) :food)])

   update-body (fn [st]
                 (update-in st [:snake-body]
                            #(concat (tail-subset %) [(:snake-head st)])))])


(defnl key-press [key st]
  (cond-match key
              :up (dir 0 1) :down (dir 0 -1)
              :left (dir 1 0) :right (dir -1 0)
              "r" (initial-state)
              _ st)
  :where [dir #(assoc st :snake-dir %&)])


(def cur-st (atom {})) ;; DEBUG
#_ (swap! cur-st (fn [_] st)) ;; DEBUG

(defnl update [[dt time] st]
  (-> st
      (update-promotion (:snake-tail st) #(- % (* dt 3.5)))
      (update-promotion (:snake-head st) #(+ % (* dt 3.5)))

      (when-> (:game-over st) (spin-camera)))

  :where
  [bound-promotion #(min (max % 0) max-promotion)

   update-promotion (fn [st addr f]
                      (-> st (when-> addr
                                     (update-in `(:board ~@addr :promotion)
                                                (comp bound-promotion f)))))
        
   spin-camera (fn-st (update-in [:rot-y] #(+ (* 15 dt) %)))])

(defnl display [[dt time] st]
  (birds-eye-view)
  (draw-board st)
  (draw-ui)
  (app/repaint!)

  :where
  [draw-ui (fn []
             (if (:game-over st)
               (text/write-to-screen "Game Over! (push 'r' to restart)" 350 300))
             (text/write-to-screen (format "%d FPS" (int (/ 1 dt))) 3 1))

   birds-eye-view (fn []
                    (translate 0 1 -12)
                    (rotate (:rot-x st) 1 0 0)
                    (rotate (:rot-y st) 0 1 0))])

(defn -main []
  (app/start 
   {:display (wrap-exc #'display), :reshape #'reshape, :init (wrap-exc init),
    :update (wrap-exc #'update) :key-press (wrap-exc #'key-press)
    :mouse-drag (wrap-exc #'mouse-drag)} {})
)