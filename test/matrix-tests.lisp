;;; matrix-tests.lisp --- testing the matrices
;;;                  _        _           _            _
;;;  _ __ ___   __ _| |_ _ __(_)_  __    | |_ ___  ___| |_ ___
;;; | '_ ` _ \ / _` | __| '__| \ \/ /____| __/ _ \/ __| __/ __|
;;; | | | | | | (_| | |_| |  | |>  <_____| ||  __/\__ \ |_\__ \
;;; |_| |_| |_|\__,_|\__|_|  |_/_/\_\     \__\___||___/\__|___/
;;;
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath-test)

(in-suite vecmath-tests)

(defsuite* matrix-tests)

(deftest matrix-identity-test ()
  (is (equalp (mat3) (mat3 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)))
  (is (equal? (mat3) (mat3 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0))))

(deftest matrix-multiplication-test ()
  (is (equal? (mat3) (mat-mul (mat3) (mat3))))
  (is (equal? (mat3) (mat3-mul (mat3) (mat3)))))

(deftest matrix-transpose-test ()
  (is (equal? (mat3) (mat-transpose (mat3))))
  (is (equal? (mat3) (mat3-transpose (mat3))))
  (is (equal? (mat3) (mat3-tmul (mat3) (mat3)))))

(deftest matrix-invert-test ()
  (is (equal? (mat3) (mat3-invert (mat3)))))

(deftest matrix-negate-test ()
  (is (equal? (mat-scale (mat3) -1.0) (mat3-negate (mat3))))
  (is (equal? (mat-scale (mat2) -1.0) (mat2-negate (mat2))))
  (is (equal? (mat-scale (mat3) -1.0) (mat3-negate (mat3))))
  (is (equal? (mat-scale (mat4) -1.0) (mat4-negate (mat4)))))

(deftest matrix-determinant-test ()
  (is (= 1.0 (mat2-determinant (mat2))))
  (is (= 1.0 (mat3-determinant (mat3))))
  (is (= 1.0 (mat4-determinant (mat4)))))

;;; matrix-tests.lisp ends here
