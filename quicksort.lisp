(defun quicksort (s)
  (if s
    (let (
        (m (first s))
        (o nil)
        (l nil)
        (r nil)
      )
      (loop for x in s do
        (cond
          ((< x m) (push x l))
          ((> x m) (push x r))
          (t (push x o))
        )
      )
      (append (quicksort l) o (quicksort r))
    )
    s
  )
)
