(defstruct document
  "Document"
  id
  features)

(defparameter *inverted-index* (make-hash-table))

(defun feed (doc)
  (let ((id (document-id doc)))
   (dolist (feature-score-pair (document-features doc) nil)
     (destructuring-bind (fea . score) feature-score-pair
       (setf (gethash fea *inverted-index*)
             (cons (cons id score)
                   (gethash fea *inverted-index*)))
       (if (null (get fea 'upper-bound))
           (setf (get fea 'upper-bound) score)
           (if (> score (get fea 'upper-bound))
               (setf (get fea 'upper-bound) score)))))))

(feed (make-document :id 1 :features '((t0 . 1) (t1 . 1) (t3 . 1))))
(feed (make-document :id 2 :features '((t1 . 1) (t2 . 1))))
(feed (make-document :id 3 :features '((t0 . 1) (t2 . 1))))
(feed (make-document :id 4 :features '((t1 . 1) (t3 . 1))))
(feed (make-document :id 5 :features '((t3 . 1) (t4 . 1))))
(feed (make-document :id 6 :features '((t2 . 1))))
(feed (make-document :id 10 :features '((t1 . 1))))
(feed (make-document :id 14 :features '((t4 . 1))))
(feed (make-document :id 23 :features '((t3 . 1))))
(feed (make-document :id 26 :features '((t0 . 1))))
(feed (make-document :id 34 :features '((t2 . 1))))
(feed (make-document :id 56 :features '((t2 . 1))))
(feed (make-document :id 70 :features '((t3 . 1))))
(feed (make-document :id 78 :features '((t4 . 1))))
(feed (make-document :id 100 :features '((t1 . 1))))
(feed (make-document :id 200 :features '((t3 . 1))))

(setf (get 't0 'upper-bound) 0)
(setf (get 't1 'upper-bound) 1)
(setf (get 't2 'upper-bound) 2)
(setf (get 't3 'upper-bound) 3)
(setf (get 't4 'upper-bound) 4)

(defun index-prepare ()
  (maphash (lambda (fea plist)
             (setf (gethash fea *inverted-index*)
                   (nreverse plist)))
           *inverted-index*))

(index-prepare)

(defstruct posting
  "Posting"
  term
  post
  upper-bound)

(defun wand (terms theta)
  (let ((cur-doc 0)
        (postings (loop for fea in terms
                     collect (make-posting :term fea
                                           :post (gethash fea *inverted-index*)
                                           :upper-bound (get fea 'upper-bound)))))
    (labels ((next ()
               (let* ((sorted-postings (sort (remove-if (lambda (p) (null (posting-post p))) postings)
                                             (lambda (tp1 tp2)
                                               (let ((p1 (posting-post tp1))
                                                     (p2 (posting-post tp2)))
                                                 (or (< (caar p1) (caar p2)))))))
                      (pivot-posting (loop named outer for p in sorted-postings
                                        summing (posting-upper-bound p) into sum do
                                          (if (>= sum theta)
                                              (return-from outer p)))))
                 (if (null pivot-posting)
                     nil
                     (let ((pivot-doc (caar (posting-post pivot-posting))))
                       (if (null pivot-doc)
                           nil
                           (if (<= pivot-doc cur-doc)
                               (progn (let ((pre-posting (car sorted-postings)))
                                        (do ()
                                            ((or (null (posting-post pre-posting))
                                                 (>= (caar (posting-post pre-posting)) (1+ cur-doc)))
                                             pre-posting)
                                          (setf (posting-post pre-posting)
                                                (cdr (posting-post pre-posting)))))
                                      (next))
                               (if (= (caar (posting-post (car sorted-postings)))
                                      pivot-doc)
                                   (setf cur-doc pivot-doc)
                                   (progn (setf (posting-post (car sorted-postings))
                                                (cdr (posting-post (car sorted-postings))))
                                          (next))))))))))
      (do ((doc (next) (next)))
          ((null doc))
        (print doc)))))

(wand '(t0 t1 t2 t3 t4) 4)
