(defun quarter-turn (a)
  (let* (
      (dims (array-dimensions a))
      (w (first dims))
      (h (second dims))
      (cols (loop for x below w collect
        (loop for y below h collect
          (aref a x y)
        )
      ))
      (r (make-array (list h w)))
      (trans (apply #'mapcar #'list cols))
      (new-cols (mapcar #'reverse trans))
      (x 0)
      (y 0)
    )
    (loop for c in new-cols do
      (loop for i in c do
        (setf (aref r y x) i)
        (incf x)
      )
      (setf x 0)
      (incf y)
    )
    r
  )
)

(defun my-copy-list (l)
  (reduce (lambda (r e) (append r (list e))) l :initial-value nil)
)

(defun my-reverse (l) (reduce (lambda (r e) (cons e r)) l :initial-value nil))
