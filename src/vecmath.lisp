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

;; (declaim (optimize (speed 3) (space 2) (debug 0) (safety 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimization*
    '(optimize (speed 3) (space 2) (debug 0) (safety 0))))

(deftype scalar () 'single-float)
(deftype angle () '(single-float 0.0 6.3))

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

;;; We are going to define a lot of functions inline
;;;
(defmacro definline (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (number) scalar) ensure-scalar))
  (declaim (inline ensure-scalar))
  (defun ensure-scalar (s)
    (coerce s 'scalar)))

(declaim (ftype (function (number) scalar) invert negate half inverse-sqrt square))
(declaim (inline invert negate half inverse-sqrt lerp square))

(defun invert (s)
  "Return one over scalar."
  (the scalar (/ +scalar-one+ s)))

(define-compiler-macro invert (&whole form s)
  (cond ((and (constantp s) (not (typep s 'scalar)))
         `(invert ,(ensure-scalar s)))
        (t
         `(,@form))))

(defun negate (s)
  "Return s * -1."
  (the scalar (* s -1)))

(defun half (s)
  "Return half the value."
  (the scalar (* +scalar-half+ s)))

(defun inverse-sqrt (s)
  "Take the inverse square root of a scalar."
  (invert (the scalar (sqrt (abs s)))))

(defun lerp (delta low high)
  "Linearly interpolate between low and high."
  (declare (type scalar delta low high))
  (the scalar (+ low (* delta (- high low)))))

(defun square (s)
  (declare (type scalar s))
  (* s s))

 ;;; vecmath.lisp ends here
