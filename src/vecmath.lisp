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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype scalar () 'single-float)

  (declaim (ftype (function (number) scalar) scalar))
  (declaim (inline scalar scalarp ensure-saclar))

  (setf (get 'scalar 'element-type) 'scalar)

  (defun scalarp (s)
    (typep s 'scalar))

  (defun scalar (s)
    (if (scalarp s)
        s
        (coerce s 'scalar)))

  (defun ensure-scalar (s)
    (scalar s))

  (define-compiler-macro ensure-scalar (&whole form s)
    (cond ((scalarp s) s)
          ((numberp s) (scalar s))
          (t form))))

(defconstant +most-positive-scalar+ most-positive-single-float)
(defconstant +scalar-epsilon+ single-float-epsilon)

(defconstant +scalar-zero+ (ensure-scalar 0.0))
(defconstant +scalar-one+ (ensure-scalar 1.0))
(defconstant +scalar-two+ (ensure-scalar 2.0))
(defconstant +scalar-half+ (ensure-scalar 0.5))
(defconstant +scalar-minus-one+ (ensure-scalar -1.0))

(defconstant +scalar-pi+ (ensure-scalar pi))
(defconstant +scalar-pi-half+ (ensure-scalar (/ pi 2)))
(defconstant +scalar-deg2rad+ (ensure-scalar (/ pi 180.0)))
(defconstant +scalar-rad2deg+ (ensure-scalar (/ 180.0 pi)))

(defconstant +infinity+ +most-positive-scalar+)
(defconstant +delta+ (ensure-scalar (sqrt +scalar-epsilon+)))

;;;; Simple Functions On Scalars
;;;

(declaim (ftype (function (scalar) scalar)
                invert half inverse-sqrt square))
(declaim (ftype (function (number) scalar)
                deg2rad rad2deg))


(declaim (inline invert half twice inverse-sqrt lerp square deg2rad rad2deg))

(defun deg2rad (angle)
  "Convert ANGLE from degrees to radians"
  (* (ensure-scalar angle) +scalar-deg2rad+))

(defun rad2deg (angle)
  "Convert ANGLE from radians to degrees"
  (* (ensure-scalar angle) +scalar-rad2deg+))


(defun invert (s)
  "Return one over scalar."
  (the scalar (/ +scalar-one+ s)))

(defun half (s)
  "Return half the value."
  (the scalar (* +scalar-half+ s)))

(defun twice (s)
  "Return twice the value."
  (the scalar (* +scalar-half+ s)))

(defun inverse-sqrt (s)
  "Take the inverse square root of a scalar."
  (invert (the scalar (sqrt (abs s)))))

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
