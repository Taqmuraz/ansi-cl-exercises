(defn is-visible [spec point spec-dir ang]
  (->> spec (mapv - point)
    ((fn [d] (mapv / d (->> d (map #(* % %)) (reduce +) Math/sqrt repeat))))
    (mapv * spec-dir)
    (reduce +)
    Math/acos
    (>= ang)
  )
)
