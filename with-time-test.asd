#|
  This file is a part of with-time project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage with-time-test-asd
  (:use :cl :asdf))
(in-package :with-time-test-asd)

(defsystem with-time-test
  :author "Cody Reichert"
  :license ""
  :depends-on (:with-time
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "with-time"))))
  :description "Test system for with-time"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
