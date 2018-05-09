(in-package :cl-user)
(defpackage with-time 
  (:use :cl)
  (:export :with-time
           :*timer-stream*))
(in-package :with-time)

;; Example:

; (with-time
;   (drakma:http-request "https://api.simplyrets.com/properties"
;                        :basic-authorization '("simplyrets" "simplyrets")))

;; Code:

(defparameter *timer-stream* (make-string-output-stream)
  "Redirect `*trace-output*' to here. Temporarily holds the previous
time output.")

(defun clean-strings (str)
  "Strip spaces, carriage returns, and line breaks from a list of
string."
  (map 'list
       (lambda (x) (string-trim '(#\Space #\Return #\Newline) x)) str))

(defun cons-time-output (stream)
  "Regexp parser for the output of SBCL's `time' function."
  (clean-strings
   (cl-ppcre:split "([ ]{2,})" (get-output-stream-string stream))))


(defun stream-time (&key real-time-ms user-run-time-us system-run-time-us
                   gc-run-time-ms processor-cycles eval-calls
                   lambdas-converted page-faults bytes-consed
                   aborted)
  (let ((total-run-time-us (+ user-run-time-us system-run-time-us)))
    (format *timer-stream*
            "~&Evaluation took:~%~
                         ~@<  ~@;~/sb-impl::format-milliseconds/ of real time~%~
                                 ~/sb-impl::format-microseconds/ of total run time ~
                                  (~@/sb-impl::format-microseconds/ user, ~@/sb-impl::format-microseconds/ system)~%~
                                 ~[[ Run times consist of ~/sb-impl::format-milliseconds/ GC time, ~
                                                      and ~/sb-impl::format-milliseconds/ non-GC time. ]~%~;~2*~]~
                                 ~,2F% CPU~%~
                                 ~@[~:D form~:P interpreted~%~]~
                                 ~@[~:D lambda~:P converted~%~]~
                                 ~@[~:D processor cycles~%~]~
                                 ~@[~:D page fault~:P~%~]~
                                 ~:D bytes consed~%~
                                 ~@[~%before it was aborted by a non-local transfer of control.~%~]~:>~%"
            real-time-ms
            total-run-time-us
            user-run-time-us
            system-run-time-us
            (if (zerop gc-run-time-ms) 1 0)
            gc-run-time-ms ;; Round up so we don't mislead by saying 0.0 seconds of non-GC time...
            (- (ceiling total-run-time-us 1000) gc-run-time-ms)
            (if (zerop real-time-ms) 100.0
                (float (* 100 (/ (round total-run-time-us 1000) real-time-ms))))
            eval-calls
            lambdas-converted
            processor-cycles
            page-faults bytes-consed
            aborted)))


(defmacro with-time (form)
  `(let ((fn
          (sb-impl::call-with-timing #'stream-time (lambda () ,form))))
     (values fn (cons-time-output *timer-stream*))))
