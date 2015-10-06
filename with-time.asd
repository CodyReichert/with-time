#|
  This file is a part of with-time project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

#|
  Author: Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage with-time-asd
  (:use :cl :asdf))
(in-package :with-time-asd)

(defsystem with-time 
  :version "0.1"
  :author "Cody Reichert"
  :license ""
  :depends-on (:cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "with-time"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op with-time-test))))
