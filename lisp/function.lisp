(proclaim '(optimize speed))

;;; tail recursive
(defun my-length (lst)
  (labels ((rec (lst n)
             (if (null lst)
                 n
                 (rec (cdr lst) (1+ n)))))
    (rec lst 0)))

(print (my-length '(1 2 3 4 5)))

(defun triangle (n)
  (labels ((tri (n c)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (1- n))
                      (the fixnum (+ n c))))))
    (tri n 0)))

(print (triangle 100))

;;; check if a function is compiled
(compiled-function-p #'triangle)

;;; inline function
(proclaim '(inline 50th))

(defun 50th (lst) (nth 49 lst))

(defun see-args (&rest args)
  (prin1 args))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))

;;;(defvar our-length (lrec #'(lambda (x f) (1+ (funcall f))) 0))

(defvar our-every
      (lrec #'(lambda (x f)
                (and (oddp x) (funcall f)))))

(defun my-copy-list (lst)
  "my version of copy-list"
  (if (atom lst)
      lst
      (cons (car lst) (my-copy-list (cdr lst)))))

(defun my-copy-tree (tree)
  "my version of copy-tree"
  (if (atom tree)
      tree
      (cons (my-copy-tree (car tree))
            (my-copy-tree (cdr tree)))))

(let ((x '(1 2 (3 4)))
      (y nil))
  (setf y (my-copy-tree x))
  (setf (caaddr x) 3.3)
  (print x)
  (print y))

(defun count-leaves (tree)
  "Count the number of leave nodes in a tree"
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (count-leaves (cdr tree)))))

(print (count-leaves '((a b (c d)) (e) f)))

(defun flatten (tree)
  "Return all atoms on a tree in the form of a list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec tree nil)))

(print (flatten '((a b (c d)) (e) f)))

(defun rfind-if (fn tree)
  "Recursively find in a tree an element that applies on fn"
  (labels ((rec (x acc)
             (cond (acc acc)
                   ((and (not (null x)) (atom x) (funcall fn x)) x)
                   ((not (atom x)) (rec (car x) (rec (cdr x) acc)))
                   (t nil))))
    (rec tree nil)))

(rfind-if #'oddp '(2 4 (7 (8 3))))

(defun tree-traverse (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(let ((x '(1 2 (3 4)))
      (y nil)
      (copy-tree (tree-traverse #'cons)))
  (setf y (funcall copy-tree x))
  (setf (caaddr x) 3.5)
  (print x)
  (print y))

(let ((count-leaves (tree-traverse #'(lambda (left right)
                                       (+ left (or right 1))) 1)))
  (print (funcall count-leaves '((a b (c d)) (e) f))))

(let ((flatten (tree-traverse #'nconc
                              #'(lambda (x) (list x)))))
  (print (funcall flatten '((a b (c d)) (e) f))))
