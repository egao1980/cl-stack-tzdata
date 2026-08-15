(in-package #:cl-stack-tzdata)

(define-condition tzdata-error (error)
  ((message :initarg :message :reader tzdata-error-message))
  (:report (lambda (c s)
             (format s "tzdata error: ~a" (tzdata-error-message c)))))

(define-condition zone-not-found (tzdata-error)
  ((id :initarg :id :reader zone-not-found-id))
  (:report (lambda (c s)
             (format s "tzdata zone not found: ~s" (zone-not-found-id c)))))

(define-condition tzif-parse-error (tzdata-error) ())
