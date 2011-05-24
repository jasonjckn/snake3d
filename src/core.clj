(ns core
  (:use [penumbra opengl]
        [util]
        [pallet thread-expr]
        [matchure])
  (:require
   [cube :as c]
   [penumbra.app :as app]
   [penumbra.text :as text]))

(defn advance)
(defn add-food)

(defn init-state []
  (let [d 10]
    {:dimensions d
     :rot-x 39, :rot-y 155

     :snake-head [0 0] :snake-tail nil :snake-dir [0 1] 
     :snake-body [[0 0]]
     :snake-len 3

     :board (vec (repeat d (vec (repeat d {:promotion 0.0}))))}))


(def max-promotion 0.8)

(defn init [st]
  (app/vsync! true)
  #_ (app/display-mode! 200 200)
  (enable :depth-test)
  (depth-test :lequal)
  (app/periodic-update! 3 (wrap-exc #'advance))
  (app/periodic-update! 0.5 (wrap-exc #'add-food))
  (def the-cube (create-display-list
                 (c/render (c/four-color-cube c/natural-clr))))
  (def the-food-cube (create-display-list
                      (c/render (c/solid-color-cube c/food-clr))))
  (def the-red-cube (create-display-list
                      (c/render (c/solid-color-cube c/error-clr))))
  (def the-promotion-cube (create-display-list
                           (c/render-transition-cube max-promotion)))
  st)


(defn reshape [[x y width height] st]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  st)



(defn mouse-drag [[dx dy] _ _ st]
  (-> st
      (update-in [:rot-x] #(+ dy %))
      (update-in [:rot-y] #(+ dx %))))

(defn gen-board [{:keys [dimensions board game-over]} f]
  (let [offset (- (/ dimensions 2))
        rng (range dimensions)]
    (doseq [y rng x rng]
      (let [{:keys [promotion] :as cell} ((board y) x)
            height-f (/ promotion 2) ]
        (push-matrix
         (translate (+ offset y) 0 (+ offset x))
         (scale 0.45 (+ 0.45 (* height-f 0.30)) 0.45)
         (f cell))))))

(defn game-over-board [{:keys [promotion]}]
  (if (< promotion 0.1)
    (call-display-list the-cube)
    (call-display-list the-red-cube)))

(defn play-board [{:keys [food promotion]}]
  (cond
   food (call-display-list the-food-cube)
   
   (< promotion 0.1) (call-display-list the-cube)
   (> promotion (- max-promotion 0.1)) (call-display-list the-promotion-cube)
   (> promotion 0) (c/render-transition-cube promotion)))

(defn board [{:keys [game-over] :as st}]
  (if game-over
    (gen-board st game-over-board)
    (gen-board st play-board)))


(defn hit-detection [st]
  (let [dupes? #(not= (count %) (count (set %)))]
    (-> st
        (when-> (dupes? (:snake-body st))
                (assoc :game-over true)))))

(defn add-food [st]
  (let [rnd (fn [] (rand-int (:dimensions st)))
        food-loc (repeat 2 (rnd))]
    (assoc-in st (concat [:board] food-loc [:food]) true)))

(defn advance [st]
  (let [apply-bounds (fn [addr] (let [ubound (- (:dimensions st) 1)]
                                  (map max [0 0] (map min [ubound ubound] addr))))
        
        advance-head (fn [addr] (apply-bounds (map + addr (:snake-dir st))))

        advance-tail (fn [old]
                       (let [t (:snake-body st)]
                         (if (>= (count t) (:snake-len st))
                           (first t)
                           old)))

        tail-subset (fn [t] (if (>= (count t) (:snake-len st))
                              (drop 1 t) t))

        eat-food (fn-st
                  (let-with-arg-> st [food-addr (concat [:board] (:snake-head st) [:food])]
                    (when-> (select-in st food-addr)
                            (update-in [:snake-len] #(+ 1 %)))
                    (assoc-in food-addr false)))

        update-body (fn [st]
                      (update-in st [:snake-body]
                                 #(concat (tail-subset %) [(:snake-head st)])))]

    (-> st
        (when-not-> (:game-over st)
                    (update-in [:snake-head] advance-head)
                    (update-in [:snake-tail] advance-tail)
                    (update-body)
                    (hit-detection)
                    (eat-food)))))


(defn key-press [key st]
  (cond-match key
              :up (assoc st :snake-dir [0 1])
              :left (assoc st :snake-dir [1 0])
              :right (assoc st :snake-dir [-1 0])
              :down (assoc st :snake-dir [0 -1])
              "r" (init-state)
              _ st))


(def cur-st (atom {})) ;; DEBUG

(concat nil nil [:a])

(defn update [[dt time] st]
  (swap! cur-st (fn [_] st)) ;; DEBUG

  (let [bound-promotion (fn [v] (min (max v 0) max-promotion))

        update-promotion (fn [st addr f]
                           (-> st (when-> addr
                                          (update-in (concat [:board] addr [:promotion])
                                                     (comp bound-promotion f)))))
        
        spin-camera (fn-st (update-in [:rot-y] #(+ (* 15 dt) %)))]

    
    
    (-> st
        (update-promotion (:snake-tail st) #(- % (* dt 3.5)))
        (update-promotion (:snake-head st) #(+ % (* dt 3.5)))

        (when-> (:game-over st)
                (spin-camera)))))


(defn display [[dt time] st]
  (translate 0 1 -12)
  (rotate (:rot-x st) 1 0 0)
  (rotate (:rot-y st) 0 1 0)
  (board st)
  (if (:game-over st)
    (text/write-to-screen "Game Over! (push 'r' to restart)" 350 300))
  (text/write-to-screen (format "%d FPS" (int (/ 1 dt))) 3 1)
  (app/repaint!))


(defn -main []
  (app/start 
   {:display (wrap-exc #'display), :reshape #'reshape, :init (wrap-exc init),
    :update (wrap-exc #'update) :key-press (wrap-exc #'key-press)
    :mouse-drag (wrap-exc #'mouse-drag)} 
   (init-state))
  )