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

(defstruct my-tree a b c val)

(defun my-copy-my-tree (tr)
  (if tr
    (make-my-tree
      :a (my-copy-my-tree (my-tree-a tr))
      :b (my-copy-my-tree (my-tree-b tr))
      :c (my-copy-my-tree (my-tree-c tr))
      :val (my-tree-val tr)
    )
  )
)

(defun search-my-tree (tr val)
  (if tr
    (if (eql (my-tree-val tr) val) tr
      (or
        (search-my-tree (my-tree-a tr) val)
        (search-my-tree (my-tree-b tr) val)
        (search-my-tree (my-tree-c tr) val)
      )
    )
  )
)
