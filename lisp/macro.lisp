(defmacro backwards (expr) (reverse expr))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro my-dolist (spec &rest body)
  (let ((var (car spec))
        (lst (cadr spec))
        (listsym (make-symbol "list")))
    `(let ((,var nil)
           (,listsym ,lst))
       (tagbody
          start
          (when ,listsym
            (setq ,var (car ,listsym))
            ,@body)
          (setq ,listsym (cdr ,listsym))
          (if ,listsym
              (go start)))
       nil)))

(defmacro my-dotime (spec &rest body)
  (let ((var (car spec))
        (rag (cadr spec))
        (endv (make-symbol "endv")))
    `(let ((,var 0)
           (,endv ,rag))
       (tagbody
          start
          (when (< ,var ,endv)
            ,@body)
          (incf ,var)
          (if (< ,var ,endv) (go start))
          end))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-init-list (variable-definitions)
    (let ((variable-list nil))
      (dolist (var-def variable-definitions)
        (let ((var (car var-def))
              (val (cadr var-def)))
          (setq variable-list (cons (list var val) variable-list))))
      variable-list))

  (defun get-step-list (variable-definitions)
    (let ((step-list nil))
      (dolist (step-def variable-definitions)
        (setq step-list (cons (caddr step-def) step-list)))
      step-list)))

(defmacro my-do (variable-definitions test-result-form &rest body)
  (let ((init-list (get-init-list variable-definitions))
        (step-list (get-step-list variable-definitions)))
    `(let* ,init-list
       (tagbody
          start
          (if ,(car test-result-form)
              (go end)
              (progn ,@body))
          ,@step-list
          (go start)
          end)
       ,(cadr test-result-form))))

(macroexpand-1
 '(my-do ((x 1 (incf x))
          (y 5 (decf y)))
   ((< (* x y) 0) (* x y))
   (print x)
   (prin1 y)))

(defmacro my-setf (x v)
  `(setq ,x ,v))

(defmacro real-macro (x y)
  (list 'progn (list 'print x) (list 'print y)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-something ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var ,start (1+ ,var))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                    (if (consp x) (car x) x))
                binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun mappend (fn &rest lsts)
    (apply #'append (apply #'mapcar fn lsts)))

  (defmacro condlet (clauses &body body)
    (let ((bodfn (gensym))
          (vars (mapcar #'(lambda (v) (cons v (gensym)))
                        (remove-duplicates
                         (mapcar #'car
                                 (mappend #'cdr clauses))))))
      `(labels ((,bodfn ,(mapcar #'car vars)
                  ,@body))
         (cond ,@(mapcar #'(lambda (cl)
                             (condlet-clause vars cl bodfn))
                         clauses)))))

  (defun condlet-clause (vars cl bodfn)
    `(,(car cl) (let ,(mapcar #'cdr vars)
                  (let ,(condlet-binds vars cl)
                    (,bodfn ,@(mapcar #'cdr vars))))))

  (defun condlet-binds (vars cl)
    (mapcar #'(lambda (bindform)
                (if (consp bindform)
                    (cons (cdr (assoc (car bindform) vars))
                          (cdr bindform))))
            (cdr cl))))



(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t       (x (princ 'e)) (z (princ 'f))))
         (list x y z))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(in '/ '+ '- '*)

(pprint (macroexpand '(inq '/ + - /)))

(eval-when (:execute)
  (print 'c))
