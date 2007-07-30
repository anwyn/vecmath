;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "VECMATH.SYSTEM")
    (defpackage #:vecmath.system
      (:use :common-lisp :asdf))))

(in-package #:vecmath.system)

(defsystem :vecmath 
  :description "Simple 2d and 3d vector and matrix math library." 
  :long-description "Simple 2d and 3d vector and matrix math library." 
  :version "0.1"
  :author "Ole Arndt <ole@sugarshark.com"
  :maintainer "Ole Arndt <ole@sugarshark.com>"
  :licence "BSD"
  :depends-on ()
  :in-order-to ((test-op (load-op #:vecmath-test)))
  :perform (test-op :after (op c)
                    (funcall (intern (string '#:run!) '#:it.bese.FiveAM) :vecmath))
  :components ((:doc-file "README")
               (:static-file "vecmath.asd")
               (:module "src"
                        :components ((:file "packages")
                                     (:file "vecmath" :depends-on ("packages"))
                                     (:file "vector"  :depends-on ("vecmath"))
                                     (:file "matrix"  :depends-on ("vector"))
                                     (:file "quat"    :depends-on ("matrix"))))))

(defsystem :vecmath-test
  :components ((:module "test"
                        :components ()))
  :depends-on (:vecmath :fiveam))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :vecmath))))
  (values nil))
