(ql:quickload :trivial-tco)

(tco:with-tail-call-optimization ()
  (labels ((sum-aux (acc x)
             (if (zerop x)
                 acc
                 (sum-aux (+ acc x) (- x 1))))
           (sum (n)
             (sum-aux 0 n)))
    (sum 1000000)))
