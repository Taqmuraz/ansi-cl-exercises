(defun precedes (e seq)
  (let (
      (r nil)
      (l (coerce seq 'list))
    )
    (dolist (c (maplist #'identity l))
      (if (and (< 1 (length c)) (eql e (second c)))
        (setf r (append r (list (first c))))
      )
    )
    r
  )
)

(defun precedes-rec (e seq)
  (if (and (< 1 (length seq)))
    (append
      (if (eql e (elt seq 1)) (list (elt seq 0)))
      (precedes-rec e (subseq seq 1))
    )
  )
)

(defun intersperse (e l)
  (rest (apply #'append (loop for i in l collect (list e i))))
)

(defun intersperse-rec (e l)
  (if (< (length l) 2)
    l
    (append (list (first l) e) (intersperse-rec e (rest l)))
  )
)
