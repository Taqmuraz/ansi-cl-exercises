(defparameter r (y-or-n-p "Do you really want to pay me 1000 dollars?"))
(do () (r (format t "Thank you!~%")) (setf r (y-or-n-p "Are you sure? Please, try another option")))
