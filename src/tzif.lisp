(in-package #:cl-stack-tzdata)

;;; Pure-CL TZif v2/v3 parser (RFC 8536). Reads the 64-bit section of fat files.

(defclass local-time-type ()
  ((offset :initarg :offset :reader local-time-type-offset
           :documentation "Seconds east of UT.")
   (dst-p :initarg :dst-p :reader local-time-type-dst-p)
   (abbreviation :initarg :abbreviation :reader local-time-type-abbreviation)))

(defun local-time-type-p (x) (typep x 'local-time-type))

(defclass transition ()
  ((at :initarg :at :reader transition-at
       :documentation "Unix instant (seconds) at which this type begins.")
   (offset :initarg :offset :reader transition-offset)
   (dst-p :initarg :dst-p :reader transition-dst-p)
   (abbreviation :initarg :abbreviation :reader transition-abbreviation)))

(defun transition-p (x) (typep x 'transition))

(defclass zone ()
  ((id :initarg :id :reader zone-id)
   (canonical-id :initarg :canonical-id :reader zone-canonical-id)
   (types :initarg :types :reader zone-types
          :documentation "Vector of LOCAL-TIME-TYPE.")
   (transitions :initarg :transitions :reader zone-transitions
                :documentation "Vector of TRANSITION, sorted by AT ascending.")
   (posix-tz :initarg :posix-tz :reader zone-posix-tz
             :documentation "POSIX TZ footer string, or NIL.")))

(defun zone-p (x) (typep x 'zone))

(defun %u8 (bytes i)
  (aref bytes i))

(defun %u32-be (bytes i)
  (logior (ash (%u8 bytes i) 24)
          (ash (%u8 bytes (+ i 1)) 16)
          (ash (%u8 bytes (+ i 2)) 8)
          (%u8 bytes (+ i 3))))

(defun %i32-be (bytes i)
  (let ((u (%u32-be bytes i)))
    (if (>= u #x80000000) (- u #x100000000) u)))

(defun %i64-be (bytes i)
  (let ((hi (%u32-be bytes i))
        (lo (%u32-be bytes (+ i 4))))
    (let ((u (logior (ash hi 32) lo)))
      (if (>= hi #x80000000)
          (- u (ash 1 64))
          u))))

(defun %read-header (bytes offset)
  "Return (values version isutcnt isstdcnt leapcnt timecnt typecnt charcnt next-offset)."
  (unless (and (>= (length bytes) (+ offset 44))
               (equalp (subseq bytes offset (+ offset 4)) #(84 90 105 102))) ; TZif
    (error 'tzif-parse-error :message (format nil "bad TZif magic at ~d" offset)))
  (let* ((version-byte (%u8 bytes (+ offset 4)))
         (version (cond ((= version-byte 0) 1)
                        ((= version-byte (char-code #\2)) 2)
                        ((= version-byte (char-code #\3)) 3)
                        ((= version-byte (char-code #\4)) 4)
                        (t version-byte)))
         (base (+ offset 20)))
    (values version
            (%u32-be bytes base)
            (%u32-be bytes (+ base 4))
            (%u32-be bytes (+ base 8))
            (%u32-be bytes (+ base 12))
            (%u32-be bytes (+ base 16))
            (%u32-be bytes (+ base 20))
            (+ offset 44))))

(defun %section-bytes (timecnt typecnt charcnt leapcnt isstdcnt isutcnt time-width)
  (+ (* timecnt time-width)
     timecnt
     (* typecnt 6)
     charcnt
     (* leapcnt (+ time-width 4))
     isstdcnt
     isutcnt))

(defun %decode-cstr (bytes start end)
  (let ((nul (or (position 0 bytes :start start :end end) end)))
    (map 'string #'code-char (subseq bytes start nul))))

(defun %parse-section (bytes offset timecnt typecnt charcnt leapcnt isstdcnt isutcnt
                       &key (time-width 8))
  (let* ((p offset)
         (times (make-array timecnt :element-type 'integer))
         (type-idxs (make-array timecnt :element-type '(unsigned-byte 8))))
    (dotimes (i timecnt)
      (setf (aref times i)
            (if (= time-width 8) (%i64-be bytes p) (%i32-be bytes p)))
      (incf p time-width))
    (dotimes (i timecnt)
      (setf (aref type-idxs i) (%u8 bytes p))
      (incf p))
    (let ((raw-types (make-array typecnt)))
      (dotimes (i typecnt)
        (let ((utoff (%i32-be bytes p))
              (isdst (plusp (%u8 bytes (+ p 4))))
              (desig (%u8 bytes (+ p 5))))
          (setf (aref raw-types i) (list utoff isdst desig))
          (incf p 6)))
      (let ((desig-start p))
        (incf p charcnt)
        (incf p (* leapcnt (+ time-width 4)))
        (incf p isstdcnt)
        (incf p isutcnt)
        (let ((types (make-array typecnt)))
          (dotimes (i typecnt)
            (destructuring-bind (utoff isdst desig) (aref raw-types i)
              (setf (aref types i)
                    (make-instance 'local-time-type
                                   :offset utoff
                                   :dst-p isdst
                                   :abbreviation
                                   (%decode-cstr bytes
                                                 (+ desig-start desig)
                                                 (+ desig-start charcnt))))))
          (let ((transitions (make-array timecnt)))
            (dotimes (i timecnt)
              (let ((tt (aref types (aref type-idxs i))))
                (setf (aref transitions i)
                      (make-instance 'transition
                                     :at (aref times i)
                                     :offset (local-time-type-offset tt)
                                     :dst-p (local-time-type-dst-p tt)
                                     :abbreviation (local-time-type-abbreviation tt)))))
            (values types transitions p)))))))

(defun %read-posix-footer (bytes offset)
  "POSIX TZ string is wrapped in NL … NL after the last section (RFC 8536 §3.3)."
  (when (>= offset (length bytes))
    (return-from %read-posix-footer (values nil offset)))
  (unless (= (%u8 bytes offset) (char-code #\Newline))
    (return-from %read-posix-footer (values nil offset)))
  (let ((end (position (char-code #\Newline) bytes :start (1+ offset))))
    (unless end
      (return-from %read-posix-footer (values nil offset)))
    (values (map 'string #'code-char (subseq bytes (1+ offset) end))
            (1+ end))))

(defun parse-tzif (bytes &key (id "unknown") (canonical-id id))
  "Parse TZif octets into a ZONE. Prefers the 64-bit v2+ section when present."
  (multiple-value-bind (version isutcnt isstdcnt leapcnt timecnt typecnt charcnt next)
      (%read-header bytes 0)
    (cond
      ((>= version 2)
       (let ((skip (%section-bytes timecnt typecnt charcnt leapcnt isstdcnt isutcnt 4)))
         (multiple-value-bind (v2 isut2 isstd2 leap2 t2 ty2 c2 next2)
             (%read-header bytes (+ next skip))
           (declare (ignore v2))
           (multiple-value-bind (types transitions after)
               (%parse-section bytes next2 t2 ty2 c2 leap2 isstd2 isut2 :time-width 8)
             (multiple-value-bind (posix ignore)
                 (%read-posix-footer bytes after)
               (declare (ignore ignore))
               (make-instance 'zone
                              :id id
                              :canonical-id canonical-id
                              :types types
                              :transitions transitions
                              :posix-tz posix))))))
      (t
       (multiple-value-bind (types transitions after)
           (%parse-section bytes next timecnt typecnt charcnt leapcnt isstdcnt isutcnt
                           :time-width 4)
         (declare (ignore after))
         (make-instance 'zone
                        :id id
                        :canonical-id canonical-id
                        :types types
                        :transitions transitions
                        :posix-tz nil))))))

(defun parse-tzif-file (path &key (id nil) (canonical-id nil))
  (let* ((path (pathname path))
         (id (or id (enough-namestring path)))
         (bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                  (let ((buf (make-array (file-length in)
                                         :element-type '(unsigned-byte 8))))
                    (read-sequence buf in)
                    buf))))
    (parse-tzif bytes :id id :canonical-id (or canonical-id id))))

(defun %transition-index-at (transitions unix-seconds)
  "Largest i such that (transition-at i) <= unix-seconds, or -1 if before first."
  (let ((lo 0)
        (hi (length transitions)))
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (<= (transition-at (aref transitions mid)) unix-seconds)
                   (setf lo (1+ mid))
                   (setf hi mid))))
    (1- lo)))

(defun zone-type-at (zone unix-seconds)
  "Return (values offset dst-p abbreviation) for ZONE at UNIX-SECONDS."
  (let* ((transitions (zone-transitions zone))
         (types (zone-types zone))
         (n (length transitions))
         (idx (%transition-index-at transitions unix-seconds)))
    (cond
      ((and (minusp idx) (plusp (length types)))
       (let ((tt (aref types 0)))
         (values (local-time-type-offset tt)
                 (local-time-type-dst-p tt)
                 (local-time-type-abbreviation tt))))
      ;; Past the last transition → POSIX footer when present.
      ((and (plusp n)
            (>= idx (1- n))
            (zone-posix-tz zone)
            (> unix-seconds (transition-at (aref transitions (1- n)))))
       (let ((ptz (parse-posix-tz (zone-posix-tz zone))))
         (if ptz
             (posix-tz-offset-at ptz unix-seconds)
             (let ((tr (aref transitions (1- n))))
               (values (transition-offset tr)
                       (transition-dst-p tr)
                       (transition-abbreviation tr))))))
      ((>= idx 0)
       (let ((tr (aref transitions idx)))
         (values (transition-offset tr)
                 (transition-dst-p tr)
                 (transition-abbreviation tr))))
      (t (values 0 nil "UTC")))))
