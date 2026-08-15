(in-package #:cl-stack-tzdata)

(defclass tzdata-repository ()
  ((root :initarg :root :reader tzdata-repository-root)
   (version :initarg :version :reader tzdata-repository-version)
   (aliases :initarg :aliases :reader tzdata-repository-aliases
            :documentation "Hash-table alias-id → canonical-id.")
   (zones :initarg :zones :initform (make-hash-table :test #'equal)
          :reader tzdata-repository-zones
          :documentation "Lazy cache: canonical-id → ZONE.")
   (abbreviations :initarg :abbreviations :initform nil
                  :reader tzdata-repository-abbreviations
                  :documentation "Hash-table abbr → list of candidate plists.")))

(defun tzdata-repository-p (x) (typep x 'tzdata-repository))

(defvar *tzdata-repository* nil)

(defun %system-data-root ()
  (merge-pathnames "data/" (asdf:system-source-directory "cl-stack-tzdata")))

(defun tzdata-root (&optional (repo (or *tzdata-repository* (load-default-repository))))
  (tzdata-repository-root repo))

(defun tzdata-version (&optional (repo (or *tzdata-repository* (load-default-repository))))
  (tzdata-repository-version repo))

(defun %read-version (root)
  (let ((path (merge-pathnames "VERSION" root)))
    (if (probe-file path)
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (uiop:read-file-string path))
        "unknown")))

(defun %load-aliases (root)
  (let ((path (merge-pathnames "links.sexp" root))
        (table (make-hash-table :test #'equal)))
    (when (probe-file path)
      (with-open-file (in path)
        (let ((form (read in)))
          (dolist (pair form)
            (setf (gethash (car pair) table) (cdr pair))))))
    table))

(defun %load-abbreviations (root)
  (let ((path (merge-pathnames "abbreviations.sexp" root))
        (table (make-hash-table :test #'equal)))
    (when (probe-file path)
      (with-open-file (in path)
        (let ((form (read in)))
          (dolist (entry form)
            (setf (gethash (car entry) table) (cdr entry))))))
    table))

(defun make-tzdata-repository (&key (root (%system-data-root)))
  (let ((root (uiop:ensure-directory-pathname root)))
    (make-instance 'tzdata-repository
                   :root root
                   :version (%read-version root)
                   :aliases (%load-aliases root)
                   :abbreviations (%load-abbreviations root))))

(defun load-default-repository ()
  (setf *tzdata-repository* (make-tzdata-repository)))

(defun canonical-zone-id (id &optional (repo (or *tzdata-repository* (load-default-repository))))
  (or (gethash id (tzdata-repository-aliases repo)) id))

(defun zone-aliases (&optional (repo (or *tzdata-repository* (load-default-repository))))
  "Return alist of (alias . canonical)."
  (let ((out '()))
    (maphash (lambda (k v) (push (cons k v) out))
             (tzdata-repository-aliases repo))
    (sort out #'string< :key #'car)))

(defun available-zones (&optional (repo (or *tzdata-repository* (load-default-repository))))
  "Canonical zone ids present under data/zoneinfo/."
  (let* ((root (merge-pathnames "zoneinfo/" (tzdata-repository-root repo)))
         (ids '()))
    (uiop:collect-sub*directories
     root (constantly t) (constantly t)
     (lambda (dir)
       (dolist (file (uiop:directory-files dir))
         (when (uiop:file-exists-p file)
           (push (enough-namestring file root) ids)))))
    (sort ids #'string<)))

(defun %zoneinfo-path (repo canonical-id)
  (merge-pathnames canonical-id
                   (merge-pathnames "zoneinfo/" (tzdata-repository-root repo))))

(defun find-zone (id &optional (repo (or *tzdata-repository* (load-default-repository))))
  "Return ZONE for ID (alias or canonical). Signals ZONE-NOT-FOUND."
  (let* ((canonical (canonical-zone-id id repo))
         (cache (tzdata-repository-zones repo))
         (cached (gethash canonical cache)))
    (or cached
        (let ((path (%zoneinfo-path repo canonical)))
          (unless (probe-file path)
            (error 'zone-not-found :id id
                   :message (format nil "no TZif for ~s (canonical ~s)" id canonical)))
          (let ((zone (parse-tzif-file path :id id :canonical-id canonical)))
            (setf (gethash canonical cache) zone)
            zone)))))

(defun zone-offset-at (zone-or-id unix-seconds
                       &optional (repo (or *tzdata-repository* (load-default-repository))))
  (let ((zone (if (zone-p zone-or-id) zone-or-id (find-zone zone-or-id repo))))
    (zone-type-at zone unix-seconds)))

(defun zone-abbreviation-at (zone-or-id unix-seconds
                             &optional (repo (or *tzdata-repository* (load-default-repository))))
  (nth-value 2 (zone-offset-at zone-or-id unix-seconds repo)))

(defun abbreviation-candidates (abbr
                                &optional (repo (or *tzdata-repository* (load-default-repository))))
  (gethash abbr (tzdata-repository-abbreviations repo)))

(defun resolve-abbreviation (abbr unix-seconds
                             &key zone-hints region
                               (repo (or *tzdata-repository* (load-default-repository))))
  "Return (values zone-id offset) for ABBR at UNIX-SECONDS.
   Filters abbreviation index by validity window; optional ZONE-HINTS / REGION."
  (let* ((cands (or (abbreviation-candidates abbr repo) '()))
         (cands
          (remove-if-not
           (lambda (c)
             (let ((start (getf c :start))
                   (end (getf c :end)))
               (and (or (null start) (>= unix-seconds start))
                    (or (null end) (< unix-seconds end)))))
           cands))
         (cands
          (if region
              (remove-if-not
               (lambda (c) (uiop:string-prefix-p (format nil "~a/" region)
                                                 (getf c :zone)))
               cands)
              cands))
         (cands
          (if zone-hints
              (or (remove-if-not
                   (lambda (c) (member (getf c :zone) zone-hints :test #'equal))
                   cands)
                  cands)
              cands)))
    (cond
      ((null cands) (values nil nil))
      ((= 1 (length cands))
       (values (getf (first cands) :zone) (getf (first cands) :offset)))
      (t
       ;; Prefer zone-hints order when multiple remain.
       (let ((pick (or (when zone-hints
                         (find-if (lambda (c)
                                    (member (getf c :zone) zone-hints :test #'equal))
                                  cands))
                       (first cands))))
         (values (getf pick :zone) (getf pick :offset) cands))))))

;; Eager default load on ASDF system load — data/ must be present.
(load-default-repository)
