(load "bst")

(defun get-ms ()
  (/ (get-internal-real-time) internal-time-units-per-second 1/1000)
)

(defun bst-insert-test (n)
  (let (
      (s (get-ms))
      (r (make-node :val 0))
    )
    (dotimes (i n) (setf r (insert-node r (make-node :val i) #'< #'=)))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun bst-pair-insert-test (n)
  (let (
      (s (get-ms))
      (r (make-node :val (make-pair :key 0 :val 0)))
    )
    (dotimes (i n) (setf r (insert-value r i (- i) (key< #'<) (key= #'=))))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun hash-insert-test (n)
  (let (
      (s (get-ms))
      (r (make-hash-table))
    )
    (dotimes (i n) (setf (gethash i r) i))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun hash-search-test (e n)
  (let (
      (r (funcall (hash-insert-test e)))
      (s (get-ms))
    )
    (dotimes (i n) (setf (gethash (mod i e) r) 0))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun bst-search-test (e n)
  (let (
      (r (funcall (bst-pair-insert-test e)))
      (s (get-ms))
    )
    (dotimes (i n) (setf
      (pair-val
        (node-val
          (search-tree r (mod i e) #'< #'= :key #'pair-key))) 0))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun assoc-insert-test (n)
  (let (
      (s (get-ms))
      (r nil)
    )
    (dotimes (i n) (setf r (acons i (- i) r)))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)

(defun assoc-search-test (e n)
  (let (
      (r (funcall (assoc-insert-test e)))
      (s (get-ms))
    )
    (dotimes (i n) (setf (cdr (assoc (mod i e) r)) 0))
    (values
      (lambda () r)
      (round (- (get-ms) s))
    )
  )
)
