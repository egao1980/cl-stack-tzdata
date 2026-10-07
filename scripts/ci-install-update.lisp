;;;; Phase 1: install runtime deps for update-tzdata.lisp via cl-repository-client.
;;;; Dogfoods http-backend-async × event-backend-libuv (+ cl-stack-ssl).
;;;; Overlay init must not run until after the workflow stages OpenSSL
;;;; (same pattern as http-backend-async).

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql
                  (lambda (c)
                    (declare (ignore c))
                    (let ((r (find-restart 'continue)))
                      (when r (invoke-restart r))))))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))

(defun ci-record-installed-version (system env-var)
  (let ((ver (cl-repo:installed-system-version system))
        (env (uiop:getenv "GITHUB_ENV")))
    (when (and ver env)
      (with-open-file (out env :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "~a=~a~%" env-var ver))
      (format t "~&; ci: ~a=~a~%" env-var ver))))

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(call-with-ci-muffles
 (lambda ()
   ;; http2 is an optional dep of http-backend-async (its :ci :with); without it
   ;; the default :http-version :auto errors "http2 system not loadable".
   (cl-repo:ensure-systems '("cl-stack-http" "http-backend-async")
     :with '("event-backend-libuv" "cl-stack-ssl" "http2"))
   (ci-record-installed-version "cl-stack-ssl" "CL_STACK_SSL_VERSION")))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
