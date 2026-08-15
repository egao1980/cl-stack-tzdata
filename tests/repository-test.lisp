(in-package #:cl-stack-tzdata/tests)

(deftest version-and-aliases
  (ok (equal "2026c" (tzdata-version)))
  (ok (equal "Europe/Kyiv" (canonical-zone-id "Europe/Kiev")))
  (ok (equal "Asia/Kolkata" (canonical-zone-id "Asia/Calcutta")))
  (ok (equal "America/New_York" (canonical-zone-id "US/Eastern")))
  (let ((zone (find-zone "Europe/Kiev")))
    (ok (equal "Europe/Kyiv" (zone-canonical-id zone)))))

(deftest available-zones-nonempty
  (let ((ids (available-zones)))
    (ok (> (length ids) 300))
    (ok (member "Europe/London" ids :test #'equal))
    (ok (member "America/New_York" ids :test #'equal))))

(deftest zone-not-found-signals
  (ok (signals (find-zone "Not/A/Zone") 'zone-not-found)))

(deftest abbreviation-index
  (ok (probe-file (merge-pathnames "abbreviations.sexp" (tzdata-root))))
  (let ((cands (abbreviation-candidates "BST")))
    (ok (consp cands))
    (ok (find-if (lambda (c) (equal "Europe/London" (getf c :zone))) cands)))
  (multiple-value-bind (zone off)
      (resolve-abbreviation "BST" 1594818000
                            :zone-hints '("Europe/London"))
    (ok (equal "Europe/London" zone))
    (ok (= 3600 off))))
