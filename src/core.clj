(ns core
  (:use [penumbra opengl]
        [util]
        [pallet thread-expr]
        [matchure])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]))

(defn cube)
(defn advance)
(defn hit-detection)
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
  (def the-cube (create-display-list (cube)))
  (def the-food-cube (create-display-list (cube :food true)))
  (def the-red-cube (create-display-list (cube :red true)))
  (def the-promotion-cube (create-display-list (cube :promotion max-promotion)))
  st)


(defn reshape [[x y width height] st]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  st)

(defn interp [factor src dst]
  (+ src (* factor (- dst src))))

(defn interp-3 [factor src dst]
  (map (partial interp factor) src dst))

(def promotion-clr [0 0.8 0.1])

(defn mouse-drag [[dx dy] _ _ st]
  (-> st
      (update-in [:rot-x] #(+ dy %))
      (update-in [:rot-y] #(+ dx %))))

(defn cube [& {:keys [promotion red food], :or {promotion 0 red false food false}}]
  (let [clrf #(apply color (interp-3 promotion %& promotion-clr))
        clrfr$ #(if red (clrf 0.9 0 0) (apply clrf %&))
        clrfr #(if food (clrf 0.9 0.9 0) (apply clrfr$ %&))
        sq (fn [m z]
             (clrfr 0.1 0.5 0.7) (vertex (m 1 1 z))
             (clrfr 0 0.1 0.9) (vertex (m -1 1 z))
             (clrfr 0.2 0.3 0.8) (vertex (m -1 -1 z))
             (clrfr 0 0.6 0.9) (vertex (m 1 -1 z)))
        twosq (fn [m]
              (doseq [z [-1 1]]
                (sq m z)))]
    (draw-quads
     (twosq #(vec [%1 %2 %3]))
     (twosq #(vec [%3 %2 %1]))
     (twosq #(vec [%2 %3 %1])))))

(defn board [{:keys [dimensions board game-over]}]
  (let [offset (- (/ dimensions 2))
        rng (range dimensions)]
    (doseq [y rng x rng]
      (let [{:keys [promotion food]} ((board y) x)
            promotion-f (+ (/ promotion 2) 1.0)]
        (push-matrix
         (translate (+ offset y) 0 (+ offset x))
         (scale 0.45 (* promotion-f 0.25) 0.45)
         (cond
          game-over (if (< promotion 0.1)
                      (call-display-list the-cube)
                      (cube :red true))

          food (call-display-list the-food-cube)

          (< promotion 0.1) (call-display-list the-cube)
          (> promotion (- max-promotion 0.1)) (call-display-list the-promotion-cube)
          (> promotion 0) (cube :promotion promotion))
         )))))


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

        tail-subset #(if (>= (count %) (:snake-len st))
                       (drop 1 %) %)

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

(defn update [[delta time] st]
  (swap! cur-st (fn [_] st)) ;; DEBUG

  (let [bound-promotion (fn [v] (min (max v 0) max-promotion))
        
        cell-promotion (fn [st op cell-addr]
                         (-> st
                             (when-> cell-addr
                                     (update-in (concat [:board] cell-addr [:promotion])
                                                #(bound-promotion (op % (* delta 3.5)))))))

        spin-camera (fn-st (update-in [:rot-y] #(+ (* 15 delta) %)))]
    (-> st
        (cell-promotion - (:snake-tail st))
        (cell-promotion + (:snake-head st))
        (when-> (:game-over st)
                (spin-camera)))))


(defn display [[delta time] st]
  (translate 0 1 -12)
  (rotate (:rot-x st) 1 0 0)
  (rotate (:rot-y st) 0 1 0)
  (board st)
  (if (:game-over st)
    (text/write-to-screen "Game Over! (push 'r' to restart)" 350 300))
  (text/write-to-screen (format "%d FPS" (int (/ 1 delta))) 3 1)
  (app/repaint!))


(defn -main []
  (app/start 
   {:display (wrap-exc #'display), :reshape #'reshape, :init (wrap-exc init),
    :update (wrap-exc #'update) :key-press (wrap-exc #'key-press)
    :mouse-drag (wrap-exc #'mouse-drag)} 
   (init-state))
  )