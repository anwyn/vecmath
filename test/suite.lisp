;;; suite.lisp --- Main testsuite for the vecmath system.
;;;            _ _
;;;  ___ _   _(_) |_ ___
;;; / __| | | | | __/ _ \
;;; \__ \ |_| | | ||  __/
;;; |___/\__,_|_|\__\___|
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

;;;; * The Unit-Tests of the Vecmath Package

(in-package :vecmath)

(defpackage :vecmath-test
    (:nicknames :vm-test)
  (:use :vecmath :common-lisp :stefil))

(in-package :vecmath-test)

(in-root-suite)

(defsuite* (vecmath-tests
            :documentation "Vector Mathematics Tests"))

;;;; ** Tests for the scalar helper functions

(deftest scalar-functions ()
  (is (= (vm::square 42.0) (* 42.0 42.0))))

;;;;@include "vector-tests.lisp"

;;;;@include "matrix-tests.lisp"

;;; suite.lisp ends here
