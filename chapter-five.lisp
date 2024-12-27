(defun precedes (e seq)
  (let (
      (r nil)
      (l (coerce seq 'list))
    )
    (dolist (c (maplist #'identity l)) (format t "~A~%" c)
      (if (and (< 1 (length c)) (eql e (second c)))
        (setf r (append r (list (first c))))
      )
    )
    r
  )
)
