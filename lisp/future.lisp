(ql:quickload :cl-async-future)
(ql:quickload :cl-async)
(use-package :cl-async-future)

(defun future-calc (x)
  "Return a future"
  (let ((future (make-future)))
    (as:delay (lambda () (finish future (+ x 1)))
              :time 1)
    future))

(as:start-event-loop
 (lambda ()
   (let ((future (attach (future-calc 0)
                         (lambda (x) ;; x is 1 here
                           (attach (future-calc x)
                                   (lambda (x) ;; x is 2 here
                                     (attach (future-calc x)
                                             (lambda (x) ;; x is 3 here
                                               (* x 5)))))))))
     (attach future
             (lambda (x)
               (format t "Final result: ~a" x))))))

(let ((future (make-future)))
  (finish future 'asdf)
  (attach future
          (lambda (x)
            (print x))))
