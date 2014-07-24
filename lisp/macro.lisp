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
    step-list))

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

(my-do ((x 1 (incf x))
        (y 5 (decf y)))
    ((< (* x y) 0) (* x y))
  (print x)
  (prin1 y))
