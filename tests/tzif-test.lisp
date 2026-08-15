(in-package #:cl-stack-tzdata/tests)

(deftest parse-europe-london
  (let* ((zone (find-zone "Europe/London"))
         (tr (zone-transitions zone)))
    (ok (zone-p zone))
    (ok (equal "Europe/London" (zone-canonical-id zone)))
    (ok (plusp (length tr)))
    (ok (stringp (zone-posix-tz zone)))
    (ok (search "GMT" (zone-posix-tz zone)))))

(deftest offset-at-known-instants
  ;; 2020-01-15 12:00 UTC → GMT (+0)
  (multiple-value-bind (off dst abbr)
      (zone-offset-at "Europe/London" 1579093200)
    (ok (= 0 off))
    (ok (not dst))
    (ok (equal "GMT" abbr)))
  ;; 2020-07-15 12:00 UTC → BST (+3600)
  (multiple-value-bind (off dst abbr)
      (zone-offset-at "Europe/London" 1594818000)
    (ok (= 3600 off))
    (ok dst)
    (ok (equal "BST" abbr))))

(deftest lord-howe-half-hour-dst
  ;; Australia/Lord_Howe: +10:30 std, +11:00 DST (30-min DST)
  (let ((zone (find-zone "Australia/Lord_Howe")))
    (ok (zone-p zone))
    (let ((offsets (remove-duplicates
                    (map 'list #'transition-offset (zone-transitions zone)))))
      (ok (member 37800 offsets))   ; +10:30
      (ok (member 39600 offsets))))) ; +11:00

(deftest negative-dst-ireland
  ;; Europe/Dublin historically used negative DST (IST winter / GMT summer naming).
  (let ((zone (find-zone "Europe/Dublin")))
    (ok (zone-p zone))
    (ok (plusp (length (zone-transitions zone))))))
