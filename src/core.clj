(ns core
  (:use [penumbra opengl])
  (:use [util])
  (:use [matchure])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]))

(defn cube)
(defn advance)
(defn hit-detection)
(defn add-food)

(defn def-state []
  (let [size 10]
    {:size size :focus [0 0] :dir [0 1] 
     :rot-x 39, :rot-y 155
     :tail '([0 0])
     :tail-size 3
     :board (vec (repeat size (vec (repeat size {:active 0.0}))))}))


(def max-active 0.8)

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
  (def the-active-cube (create-display-list (cube :active max-active)))
  st)


(defn reshape [[x y width height] st]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (def width width)
  (def height height)
  (load-identity)
  st)

(defn interp [factor src dst]
  (+ src (* factor (- dst src))))

(defn interp-3 [factor src dst]
  (map (partial interp factor) src dst))

(def active-clr [0 0.8 0.1])

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn cube [& {:keys [active red food], :or {active 0 red false food false}}]
  (let [clrf #(apply color (interp-3 active %& active-clr))
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

(defn board [{:keys [size board game-over]}]
  (let [offset (- (/ size 2))
        rng (range size)]
    (doseq [y rng x rng]
      (let [{:keys [active food]} ((board y) x)
            active-f (+ (/ active 2) 1.0)]
        (push-matrix
         (translate (+ offset y) 0 (+ offset x))
         (scale 0.45 (* active-f 0.25) 0.45)
         (cond
          
          game-over (if (< active 0.1)
                      (call-display-list the-cube)
                     #_ (call-display-list the-red-cube)
                     (cube :red true)
                      )
          food (call-display-list the-food-cube)
          (< active 0.1) (call-display-list the-cube)
          (> active (- max-active 0.1)) (call-display-list the-active-cube)
          (> active 0) (cube :active active))
         #_ (cond
          (< active 0.1) (cube)
          (> active (- max-active 0.1)) (cube :active max-active)
          (> active 0) (cube :active active))
         )))))

(def cur-st (atom {}))
#_ @cur-st

(defn hit-detection [st]
  (let [tail (:tail st)
        tail-dupes? (not= (count tail)
                           (count (set tail)))]
    (if tail-dupes?
      (assoc st :game-over true)
      st)))

(defn add-food [st]
  (let [tail (set (:tail st))
        rnd (fn [] (rand-int (:size st)))
        food-loc (repeat 2 (rnd))]
    (update-in st (concat [:board] food-loc [:food]) (fn [_] true))))

(defn advance [st]
  (let [move-focus (fn [old]
                     (let [ubound (- (:size st) 1)]
                       (map max [0 0]
                            (map min [ubound ubound]
                                 (map + old (:dir st))))))
        move-unfocus (fn [old]
                       (let [t (:tail st)]
                         (if (>= (count t) (:tail-size st))
                           (first t)
                           old)))
        ]

    (let [tail-size (:tail-size st)
          tail-subset #(if (>= (count %) tail-size)
                          (drop 1 %)
                          %)
          eat-food (fn [st]
                     (let [f (:focus st)
                           addr (concat [:board] f [:food])]
                       
                       (-> st
                           (update-in addr (fn [o] (def o o) false))
                           (update-in [:tail-size] #(if o (+ 1 %) %)))))

          update-tail (fn [st]
                         (update-in st [:tail]
                                    #(concat (tail-subset %) [(:focus st)])))]
      (if-not (:game-over st)
        (-> st
            (update-in [:focus] move-focus)
            (update-in [:unfocus] move-unfocus)
            (update-tail)
            (hit-detection)
            (eat-food)
            )
        st))))


(defn key-press [key st]
  (let [dir {:north [0 1]
             :east [-1 0]
             :west [1 0]}]
    (println key)
    (cond-match
     key
     :up (assoc st :dir [0 1])
     :left (assoc st :dir [1 0])
     :right (assoc st :dir [-1 0])
     :down (assoc st :dir [0 -1])
     "r" (def-state)
     _ st)))

(defn update [[delta time] st]
  (swap! cur-st (fn [_] st))
  (let [uf (:unfocus st)
        f (:focus st)

        maybe-unfocus (fn [st]
                        (if uf
                          (update-in st
                                     (concat [:board] uf [:active])
                                             #(max (- % (* delta 3.5)) 0))
                          st))
        gameover-rotate (fn [st]
                          (if (:game-over st)
                            (update-in st [:rot-y] #(+ (* 15 delta) %))
                            st))
        ]
  (-> st
      (maybe-unfocus)
    (update-in (concat [:board] f)
               (fn [m] (update-in m [:active]
                                  #(min (+ % (* delta 3.5))
                                        max-active))))
   (gameover-rotate))))


(defn display [[delta time] st]
  (translate 0 1 -12)
  (rotate (:rot-x st) 1 0 0)
  (rotate (:rot-y st) 0 1 0)
  (board st)
  (if (:game-over st)
    (text/write-to-screen "Game Over! (push 'r' to restart)" 350 300))
  (text/write-to-screen (format "%d FPS" (int (/ 1 delta))) 3 1)
  (app/repaint!))


(defn -main[]
  (app/start 
   {:display (wrap-exc #'display), :reshape #'reshape, :init (wrap-exc init),
    :update (wrap-exc #'update) :key-press (wrap-exc #'key-press)
    :mouse-drag (wrap-exc #'mouse-drag)} 
   (def-state))
  )