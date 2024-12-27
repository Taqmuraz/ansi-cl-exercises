(defn diff1 [l]
  (apply = 1 (map - l (rest l)))
)
