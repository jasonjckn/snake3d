(ns util)

(defn wrap-exc [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        (.printStackTrace e *out*)
        (throw)))))