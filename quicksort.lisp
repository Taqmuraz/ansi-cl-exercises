(defun quicksort (s)
  (if s
    (let* (
        (m (first s))
        (l (remove-if (lambda (x) (>= x m)) s))
        (o (remove-if-not (lambda (x) (= x m)) s))
        (r (remove-if (lambda (x) (<= x m)) s))
      )
      (append (quicksort l) o (quicksort r))
    )
    s
  )
)
