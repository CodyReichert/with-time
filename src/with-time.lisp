(in-package :cl-user)
(defpackage with-time 
  (:use :cl)
  (:export :with-time))
(in-package :with-time)

;; Example:

; (with-time ()
;   (drakma:http-request "https://api.simplyrets.com/properties"
;                        :basic-authorization '("simplyrets" "simplyrets")))

;; Code:

(defvar *timer-stream* (make-string-output-stream)
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

(defmacro with-time (&body body)
  "Wrap a function in this macro. It returns a values list containing
first, the return value of the provided function, and second, the execution
time of the function provided. The execution time is consed into a list
to make it easier to programatically work with."
  (setf *trace-output* *timer-stream*)
  `(let ((fn-val (time ,@body))
          (times (cons-time-output `,*timer-stream*)))
     `(values ,fn-val ,times)))
