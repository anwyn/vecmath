;;; vecmath.lisp --- Simple vector and matrix math for 3d graphics
;;;
;;; Copyright (C) 2007 Ole Arndt
;;; Author: Ole Arndt <ole@sugarshark.com>
;;; Licence: BSD
;;;
;;; Commentary:
;;;
;;;

(in-package :vecmath)

;; (declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimization*
    '(optimize (speed 3) (space 2) (debug 0) (safety 0))))

(deftype scalar () 'single-float)
(defconstant +most-positive-scalar+ most-positive-single-float)
(defconstant +scalar-epsilon+ single-float-epsilon)
(defconstant +scalar-zero+ 0.0)
(defconstant +scalar-one+ 1.0)
(defconstant +scalar-half+ 0.5)
(defconstant +scalar-minus-one+ -1.0)

(defconstant +infinity+ +most-positive-scalar+)
(defconstant +delta+ (the scalar (sqrt +scalar-epsilon+)))

;;;; Simple Functions On Scalars
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (number) scalar) ensure-scalar))
  (declaim (inline ensure-scalar scalarp))

  (setf (get 'scalar 'element-type) 'scalar)

  (defun scalar (s)
    (coerce s 'scalar))

  (defun ensure-scalar (s)
    (coerce s 'scalar))

  (defun scalarp (s)
    (typep s 'scalar)))

(declaim (ftype (function (scalar) scalar) invert half inverse-sqrt square))
(declaim (inline invert half inverse-sqrt lerp square))

(defun invert (s)
  "Return one over scalar."
  (the scalar (/ +scalar-one+ s)))

(define-compiler-macro invert (&whole form s)
  (cond ((and (constantp s) (numberp s) (not (typep s 'scalar)))
         `(invert ,(ensure-scalar s)))
        (t
         `(,@form))))


(defun half (s)
  "Return half the value."
  (the scalar (* +scalar-half+ s)))

(defun inverse-sqrt (s)
  "Take the inverse square root of a scalar."
  (invert (the scalar (sqrt (abs s)))))

(define-compiler-macro inverse-sqrt (&whole form s)
  (cond ((and (constantp s) (numberp s) (not (typep s 'scalar)))
         `(inverse-sqrt ,(ensure-scalar s)))
        (t
         `(,@form))))

(defun lerp (delta low high)
  "Linearly interpolate between low and high."
  (declare (type scalar delta low high)
           (optimize (speed 3)))
  (the scalar (+ low (* delta (- high low)))))

(defun square (s)
  (declare (type scalar s)
           (optimize (speed 3)))
  (the scalar (* s s)))

 ;;; vecmath.lisp ends here
