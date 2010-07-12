;;; -*- lisp -*-

(defsystem :vecmath
  :description "Simple 2d and 3d vector and matrix math library."
  :long-description "Simple 2d and 3d vector and matrix math library."
  :version "0.1"
  :author "Ole Arndt <ole@sugarshark.com"
  :maintainer "Ole Arndt <ole@sugarshark.com>"
  :licence "BSD"
  :depends-on (:alexandria)
  :in-order-to ((test-op (load-op #:vecmath-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:vecmath-tests) '#:vecmath-test)))
  :components ((:doc-file "README")
               (:static-file "vecmath.asd")
               (:module "src"
                        :components ((:file "packages")
                                     (:file "util"       :depends-on ("packages"))
                                     (:file "vecmath"    :depends-on ("util"))
                                     (:file "types"      :depends-on ("vecmath"))
                                     (:file "vector"     :depends-on ("types"))
                                     (:file "matrix"     :depends-on ("vector"))
                                     (:file "quat"       :depends-on ("matrix"))))))

(defsystem :vecmath-test
  :components ((:module "test"
                        :components ((:file "suite")
                                     (:file "vector-tests" :depends-on ("suite"))
                                     (:file "matrix-tests" :depends-on ("suite"))
                                     (:file "quat-tests"   :depends-on ("suite")))))
  :depends-on (:vecmath :stefil))

;;;; * Introduction
;;;;
;;;; This library provides some vector mathematics operations targeted for 3D graphics.
;;;;

;;;;@include "src/packages.lisp"

;;;;@include "test/suite.lisp"
