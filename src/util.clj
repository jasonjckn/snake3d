(ns util
  (:use pallet.thread-expr))

(defn wrap-exc [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        (.printStackTrace e *out*)
        (throw)))))

(defmacro fn-st [& body]
  `(fn [st#]
     (-> st#
      ~@body)))

(defn select-in [v ks]
  (if (seq ks)
    (recur (v (first ks)) (rest ks))
    v))

(defmacro plet [& all]
  (assert (>= (count all) 3))
  (let [body (drop-last 2 all)
        [wh v] (take-last 2 all)]
    (assert (= wh :where))
    `(let ~v
      ~@body)))

(defmacro defnl [name args & body]
  (let [[where v] (take-last 2 body)]
    (if (= :where where)
      (let [body (drop-last 2 body)]
        `(clojure.core/defn ~name ~args (let ~v ~@body)))
      `(clojure.core/defn ~name ~args ~@body))))

(defmacro fnl [args & body]
  (let [[where v] (take-last 2 body)]
    (if (= :where where)
      (let [body (drop-last 2 body)]
        `(clojure.core/fn ~args (let ~v ~@body)))
      `(clojure.core/fn ~args ~@body))))




