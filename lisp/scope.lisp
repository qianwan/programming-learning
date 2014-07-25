;;; lexical scope

(defparameter regular 2)

(defun check-lexical-scope ()
    regular)

(let ((regular 10))
    (print regular)
    (print (check-lexical-scope)))
