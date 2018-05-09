(in-package :cl-user)
(defpackage with-time-test
  (:use :cl
        :with-time
        :prove))
(in-package :with-time-test)

;; NOTE: To run this test file, execute `(asdf:test-system :with-time)' in your Lisp.

(setf *enable-colors* nil)

(plan 1)

(subtest "with-time test"
  (is-expand (with-time form)
	     (let ((with-time::fn
		    (sb-ext:call-with-timing #'with-time::stream-time (lambda () form))))
	       (values with-time::fn (with-time::cons-time-output with-time:*timer-stream*)))))
 
(finalize)
