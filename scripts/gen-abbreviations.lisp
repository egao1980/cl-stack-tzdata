;;;; Mine data/zoneinfo/** transitions into data/abbreviations.sexp.
;;;; Run after scripts/update-tzdata.lisp. Pure-CL; only needs cl-stack-tzdata
;;;; on CL_SOURCE_REGISTRY (the checkout itself).

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&error: ~a~%" c)
        (uiop:quit 1)))

(require :asdf)
(asdf:load-system "cl-stack-tzdata")

(in-package #:cl-stack-tzdata)

(defun %mine-abbreviations (repo)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (id (available-zones repo))
      (let* ((zone (find-zone id repo))
             (transitions (zone-transitions zone))
             (n (length transitions)))
        (loop for i from 0 below n
              for tr = (aref transitions i)
              for abbr = (transition-abbreviation tr)
              for start = (transition-at tr)
              for end = (if (< (1+ i) n)
                            (transition-at (aref transitions (1+ i)))
                            nil)
              do (when (and abbr
                            (plusp (length abbr))
                            (every (lambda (c)
                                     (and (graphic-char-p c) (char/= c #\Space)))
                                   abbr))
                   (push (list :zone id
                               :offset (transition-offset tr)
                               :start start
                               :end end
                               :dst (transition-dst-p tr))
                         (gethash abbr table))))))
    table))

(defun %write-abbreviations (table path version)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out ";; abbreviation -> candidates (generated from TZif transitions)~%")
    (format out ";; tzdb release: ~a~%(~%" version)
    (let ((keys (sort (loop for k being the hash-keys of table collect k) #'string<)))
      (dolist (abbr keys)
        (format out "(~s~%" abbr)
        (dolist (c (nreverse (gethash abbr table)))
          (format out " ~s~%" c))
        (format out ")~%")))
    (format out ")~%")))

(let* ((repo (or *tzdata-repository* (load-default-repository)))
       (table (%mine-abbreviations repo))
       (path (merge-pathnames "abbreviations.sexp" (tzdata-repository-root repo)))
       (n 0))
  (maphash (lambda (k v) (declare (ignore k)) (incf n (length v))) table)
  (%write-abbreviations table path (tzdata-repository-version repo))
  (format t "~&abbreviations: ~d tokens, ~d validity windows → ~a~%"
          (hash-table-count table) n path))

(uiop:quit 0)
