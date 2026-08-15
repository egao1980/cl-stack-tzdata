(in-package #:cl-stack-tzdata)

;;; Minimal POSIX TZ footer evaluator (RFC 8536 / tzcode).
;;; Enough to extrapolate past the last TZif transition.

(defstruct (posix-tz (:conc-name ptz-))
  std-name
  std-offset                          ; seconds east of UT (POSIX sign flipped)
  dst-name
  dst-offset
  ;; start/end: (:j n) Julian 1-365 no leap | (:n n) 0-365 with leap | (:m m w d)
  start
  start-time                          ; seconds after local midnight
  end
  end-time)

(defun %parse-offset-seconds (string start)
  "Parse `[+|-]hh[:mm[:ss]]` → (values seconds-east-of-UT next-index).
   POSIX: positive means *west* of Greenwich — we flip to east-of-UT."
  (let* ((len (length string))
         (i start)
         (sign 1))
    (when (>= i len) (return-from %parse-offset-seconds (values 0 i)))
    (cond ((char= (char string i) #\+) (incf i))
          ((char= (char string i) #\-) (setf sign -1) (incf i)))
    (unless (and (< i len) (digit-char-p (char string i)))
      (return-from %parse-offset-seconds (values 0 start)))
    (let ((hh 0) (mm 0) (ss 0))
      (loop while (and (< i len) (digit-char-p (char string i)))
            do (setf hh (+ (* hh 10) (digit-char-p (char string i))))
               (incf i))
      (when (and (< i len) (char= (char string i) #\:))
        (incf i)
        (loop while (and (< i len) (digit-char-p (char string i)))
              do (setf mm (+ (* mm 10) (digit-char-p (char string i))))
                 (incf i)))
      (when (and (< i len) (char= (char string i) #\:))
        (incf i)
        (loop while (and (< i len) (digit-char-p (char string i)))
              do (setf ss (+ (* ss 10) (digit-char-p (char string i))))
                 (incf i)))
      ;; POSIX west-positive → east-of-UT
      (values (- (* sign (+ (* hh 3600) (* mm 60) ss))) i))))

(defun %parse-name (string start)
  "STD/DST name: quoted `<…>` or alphabetic run."
  (let ((len (length string)) (i start))
    (cond
      ((and (< i len) (char= (char string i) #\<))
       (incf i)
       (let ((end (or (position #\> string :start i) len)))
         (values (subseq string i end) (if (< end len) (1+ end) end))))
      (t
       (let ((end i))
         (loop while (and (< end len) (alpha-char-p (char string end)))
               do (incf end))
         (values (subseq string i end) end))))))

(defun %parse-rule (string start)
  "Parse `Jn` | `n` | `Mm.w.d` → (values rule-form next)."
  (let ((len (length string)) (i start))
    (cond
      ((and (< i len) (char= (char string i) #\J))
       (incf i)
       (let ((n 0))
         (loop while (and (< i len) (digit-char-p (char string i)))
               do (setf n (+ (* n 10) (digit-char-p (char string i))))
                  (incf i))
         (values (list :j n) i)))
      ((and (< i len) (char= (char string i) #\M))
       (incf i)
       (let ((m 0) (w 0) (d 0))
         (loop while (and (< i len) (digit-char-p (char string i)))
               do (setf m (+ (* m 10) (digit-char-p (char string i))))
                  (incf i))
         (when (and (< i len) (char= (char string i) #\.)) (incf i))
         (loop while (and (< i len) (digit-char-p (char string i)))
               do (setf w (+ (* w 10) (digit-char-p (char string i))))
                  (incf i))
         (when (and (< i len) (char= (char string i) #\.)) (incf i))
         (loop while (and (< i len) (digit-char-p (char string i)))
               do (setf d (+ (* d 10) (digit-char-p (char string i))))
                  (incf i))
         (values (list :m m w d) i)))
      (t
       (let ((n 0))
         (loop while (and (< i len) (digit-char-p (char string i)))
               do (setf n (+ (* n 10) (digit-char-p (char string i))))
                  (incf i))
         (values (list :n n) i))))))

(defun parse-posix-tz (string)
  "Parse a POSIX TZ string into a POSIX-TZ structure, or NIL on failure."
  (when (or (null string) (zerop (length string)))
    (return-from parse-posix-tz nil))
  (handler-case
      (multiple-value-bind (std i) (%parse-name string 0)
        (multiple-value-bind (std-off i) (%parse-offset-seconds string i)
          (let ((ptz (make-posix-tz :std-name std :std-offset std-off
                                    :dst-offset (+ std-off 3600)
                                    :start-time 7200 :end-time 7200)))
            (when (>= i (length string))
              (return-from parse-posix-tz ptz))
            (multiple-value-bind (dst i) (%parse-name string i)
              (setf (ptz-dst-name ptz) dst)
              (when (and (< i (length string))
                         (or (digit-char-p (char string i))
                             (member (char string i) '(#\+ #\-))))
                (multiple-value-bind (dst-off i2) (%parse-offset-seconds string i)
                  (setf (ptz-dst-offset ptz) dst-off
                        i i2)))
              (when (and (< i (length string)) (char= (char string i) #\,))
                (incf i)
                (multiple-value-bind (start i) (%parse-rule string i)
                  (setf (ptz-start ptz) start)
                  (when (and (< i (length string)) (char= (char string i) #\/))
                    (incf i)
                    (multiple-value-bind (tm i2) (%parse-offset-seconds string i)
                      ;; time-of-day is not sign-flipped; re-parse absolute
                      (setf (ptz-start-time ptz) (abs tm)
                            i i2)))
                  (when (and (< i (length string)) (char= (char string i) #\,))
                    (incf i)
                    (multiple-value-bind (end i) (%parse-rule string i)
                      (setf (ptz-end ptz) end)
                      (when (and (< i (length string)) (char= (char string i) #\/))
                        (incf i)
                        (multiple-value-bind (tm _) (%parse-offset-seconds string i)
                          (declare (ignore _))
                          (setf (ptz-end-time ptz) (abs tm))))))))
              ptz))))
    (error () nil)))

(defun %gregorian-leap-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun %civil-from-days (z)
  "Days since Unix epoch → (values year month day). Algorithm from Howard Hinnant."
  (let* ((z (+ z 719468))
         (era (floor (if (>= z 0) z (- z 146096)) 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096))
                     365))
         (y (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (d (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (m (+ mp (if (< mp 10) 3 -9))))
    (values (if (<= m 2) (1+ y) y) m d)))

(defun %days-from-civil (y m d)
  (let* ((y (- y (if (<= m 2) 1 0)))
         (era (floor (if (>= y 0) y (- y 399)) 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (1- d)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun %weekday-from-days (z)
  "0=Sunday … 6=Saturday for days-since-Unix-epoch Z."
  (mod (+ z 4) 7))

(defun %rule-to-local-seconds (rule time year)
  "Local seconds from year-start for RULE in YEAR."
  (ecase (first rule)
    (:j
     (let* ((n (second rule))
            ;; Jn: day n of year ignoring Feb 29 (n=1..365)
            (doy (if (and (%gregorian-leap-p year) (>= n 60)) (1+ n) n)))
       (+ (* (1- doy) 86400) time)))
    (:n
     (+ (* (second rule) 86400) time))
    (:m
     (destructuring-bind (_ m w d) rule
       (declare (ignore _))
       (let* ((dim (aref #(0 31 28 31 30 31 30 31 31 30 31 30 31) m))
              (dim (if (and (= m 2) (%gregorian-leap-p year)) 29 dim))
              (day1 (%days-from-civil year m 1))
              (wd1 (%weekday-from-days day1))
              (day
               (if (= w 5)
                   ;; last weekday D of month
                   (let* ((last (+ day1 dim -1))
                          (wdl (%weekday-from-days last))
                          (delta (mod (- wdl d) 7)))
                     (- last delta))
                   (let* ((delta (mod (- d wd1) 7))
                          (first (+ day1 delta)))
                     (+ first (* (1- w) 7))))))
         (+ (* (- day (%days-from-civil year 1 1)) 86400) time))))))

(defun posix-tz-offset-at (ptz unix-seconds)
  "Return (values offset-seconds-east abbreviation dst-p) for UNIX-SECONDS."
  (unless ptz
    (return-from posix-tz-offset-at (values 0 "UTC" nil)))
  (unless (ptz-dst-name ptz)
    (return-from posix-tz-offset-at
      (values (ptz-std-offset ptz) (ptz-std-name ptz) nil)))
  (multiple-value-bind (year) (%civil-from-days (floor unix-seconds 86400))
    (let* ((std (ptz-std-offset ptz))
           (dst (ptz-dst-offset ptz))
           ;; Transition instants are local wall times → convert to UT via the
           ;; offset currently in force on the *other* side of the transition.
           (start-local (%rule-to-local-seconds (ptz-start ptz) (ptz-start-time ptz) year))
           (end-local (%rule-to-local-seconds (ptz-end ptz) (ptz-end-time ptz) year))
           (year-start (* (%days-from-civil year 1 1) 86400))
           (start-ut (- (+ year-start start-local) std))
           (end-ut (- (+ year-start end-local) dst))
           (in-dst (if (< start-ut end-ut)
                       (and (>= unix-seconds start-ut) (< unix-seconds end-ut))
                       (or (>= unix-seconds start-ut) (< unix-seconds end-ut)))))
      (if in-dst
          (values dst (ptz-dst-name ptz) t)
          (values std (ptz-std-name ptz) nil)))))
