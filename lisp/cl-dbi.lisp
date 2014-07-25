(defvar *mysql-connection*
  (dbi:connect :mysql
               :database-name "bucket"
               :username "root"
               :password ""))
(let* ((query (dbi:prepare *mysql-connection*
                           "SELECT id, ccode FROM backend_endpoint"))
       (result (dbi:execute query 0 "2011-11-01")))
  (do ((record (dbi:fetch result) (dbi:fetch result)))
      (record t)
    (print record)))
