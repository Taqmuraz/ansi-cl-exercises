(defn hash-test [n]
  (time
    (let [
        h (atom {})
      ]
      (loop [i 0]
        (when (< i n)
          (swap! h #(assoc % i (- i)))
          (recur (+ i 1))
        )
      )
    )
  )
)
