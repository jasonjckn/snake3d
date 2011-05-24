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


