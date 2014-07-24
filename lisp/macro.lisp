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

(defmacro my-do (variable-definition test-result-form &rest body)
  (let ((variable-definition-syms (make-symbol "variable-definition-symbols"))
        (variable (make-symbol "variable")))
    `(let ((,variable-definition-syms ())
           (,variable ',variable-definition))
       (tagbody
          start
          (if ,variable
              )
          end)
       (tagbody
          start
          end)
       ,@body)))

(defmacro get-init-list (variable-definitions)
  (let ((init-list (make-symbol "init-list"))
        (list-iter (make-symbol "list-iter"))
        (list-var (make-symbol "list-var")))
    `(let ((,init-list nil)
           (,list-iter ',variable-definitions))
       (tagbody
          start
          (if ,list-iter
              (let ((,list-var (car ,list-iter)))
                (setq ,init-list (cons '((car ,list-var) (cadr ,list-var)) ,init-list)))
              (go end))
          (setq ,list-iter (cdr ,list-iter))
          (go start)
          end)
       ,init-list)))

(macroexpand-1
 '(get-init-list ((x 1 (incf x))
                  (y 2 (decf y)))))
