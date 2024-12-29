(defstruct node val l r)

(defun node-balance (n)
  (if n
    (let (
        (l (node-l n))
        (r (node-r n))
      )
      (cond
        ((and l r) (- (node-balance r) (node-balance l)))
        (l (+ 1 (node-balance l)))
        (r (+ 1 (node-balance r)))
        (t 0)
      )
    )
    0
  )
)

(defun print-node (n stream &optional (indent 0))
  (if n
    (progn
      (format stream "~&~v@T|~A ~A" (* 2 indent) (node-val n) (node-balance n))
      (print-node (node-l n) stream (+ 1 indent))
      (print-node (node-r n) stream (+ 1 indent))
    )
    (format stream "~&~v@T|~A" (* 2 indent) nil)
  )
)

(defmethod print-object ((n node) stream)
  (print-node n stream)
)

(defun insert-node (tree node < =)
  (if tree
    (let (
        (nv (node-val node))
        (tv (node-val tree))
        (tl (node-l tree))
        (tr (node-r tree))
      )
      (cond
        ((funcall = nv tv) tree)
        ((funcall < nv tv) (make-node :val tv :l (insert-node tl node < =) :r tr))
        (t (make-node :val tv :l tl :r (insert-node tr node < =)))
      )
    )
    node
  )
)

(defun tree-min (tree)
  (cond
    ((node-l tree) (tree-min (node-l tree)))
    (t tree)
  )
)

(defun tree-max (tree)
  (cond
    ((node-r tree) (tree-max (node-r tree)))
    (t tree)
  )
)
