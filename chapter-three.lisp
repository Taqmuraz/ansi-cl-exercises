(defun new-union (&rest ls)
  (reduce
    (lambda (r e)
      (if (member e r) r (append r (list e)))
    )
    (apply #'append ls)
    :initial-value '()
  )
)

(defun freq (l)
  (sort (reduce
    (lambda (m e)
      (let ((a (assoc e m)))
        (if a
          (progn (setf (rest a) (+ 1 (rest a))) m)
          (append m (list (cons e 1)))
        )
      )
    )
    l
    :initial-value '()
  ) (lambda (a b) (> (cdr a) (cdr b))))
)
