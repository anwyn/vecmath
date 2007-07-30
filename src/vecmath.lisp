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
(defconstant +scalar-one+ 1.0)
(defconstant +scalar-zero+ 0.0)

(defconstant +infinity+ +most-positive-scalar+)
(defconstant +delta+ (the scalar (sqrt +scalar-epsilon+)))

;;; (@* "Simple Functions On Scalars")
;;;

;;; We are going to define a lot of functions inline
;;; 
(defmacro definline (name params &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,params
       ,@body)))

(declaim (inline invert))
(defun invert (s)
  "Return one over scalar."
  (declare (type scalar s)
           (optimize (speed 3)))
  (the scalar (/ +scalar-one+ s)))

(declaim (inline inverse-sqrt))
(defun inverse-sqrt (s)
  "Take the inverse square root of a scalar."
  (declare (type scalar s)
           (optimize (speed 3)))
  (invert (the scalar (sqrt (abs s)))))

(declaim (inline lerp))
(defun lerp (delta low high)
  "Linearly interpolate between low and high."
  (declare (type scalar delta low high)
           (optimize (speed 3)))
  (the scalar (+ low (* delta (- high low)))))

(declaim (inline square))
(defun square (s)
  (declare (type scalar s)
           (optimize (speed 3)))
  (the scalar (* s s)))

 ;;; vecmath.lisp ends here
