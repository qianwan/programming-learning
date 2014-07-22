(format nil "WTF")

(defun this ()
  (if t
      (print 'A)
      (format nil "Yes")))

(as:start-event-loop
 (lambda ()
   (as:delay
    (format t "Timer fired. Exiting event loop.~%")
    :time 3)))
