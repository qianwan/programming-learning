(ql:quickload :cl-async)
(ql:quickload :babel)

(defparameter *http-response*
  (babel:string-to-octets
   (with-output-to-string (s)
     (format s "HTTP/1.1 200 OK~c~c" #\return #\newline)
     (format s "Date: Wed, 03 Oct 2012 23:43:10 GMT~c~c" #\return #\newline)
     (format s "Content-Type: text/plain~c~c" #\return #\newline)
     (format s "Content-Length: 9~c~c" #\return #\newline)
     (format s "~c~c" #\return #\newline)
     (format s "omglolwtf"))))

(defun tcp-server-test (&key stats)
  (as:start-event-loop
    (lambda ()
      (format t "Starting TCP server.~%")
      (let ((listener nil)
            (quit nil)
            (finished-requests 0)
            (last-finished 0)
            (last-time 0))
        (setf listener
              (as:tcp-server nil 9009
                             (lambda (socket data)
                               (declare (ignore data))
                               (as:delay (lambda ()
                                           (unless (as:socket-closed-p socket)
                                             (as:write-socket-data
                                               socket *http-response*
                                               :write-cb (lambda (socket)
                                                           (as:close-socket socket)
                                                           (incf finished-requests)))))
                                         :time 5))
                             (lambda (err)
                               (format t "tcp server event: ~a~%" err))))
        (as:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (setf quit t)
                               (as:free-signal-handler 2)
                               (as:close-tcp-server listener)))
        (labels ((show-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "incoming: ~a~%outgoing: ~a~%finished: ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests rate))
                   (unless quit
                     (as:delay #'show-stats :time 1))))
          (when stats (show-stats)))))
    :catch-app-errors t)
  (format t "TCP server exited.~%"))

;; run it
(tcp-server-test :stats t)
