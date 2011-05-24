(ns gen)

(defn gen-cube []
  (let [sq (fn [m z]
             [{:vertex (m 1 1 z)} {:vertex (m -1 1 z)}
              {:vertex (m -1 -1 z)} {:vertex (m 1 -1 z)}])

        twosq (fn [m] (for [z [-1 1]] (sq m z)))]

    (flatten
     (for [f [#(vec [%1 %2 %3])
               #(vec [%3 %2 %1])
               #(vec [%2 %3 %1])]]
       (twosq f)))))

#_ (spit "cube.vertex" (vec x))

