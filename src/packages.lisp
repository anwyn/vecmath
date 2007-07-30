;;; packages.lisp --- Package declarations for the vecmath library.
;;;
;;; Copyright (C) 2007 Ole Arndt
;;; Author: Ole Arndt <oliver.arndt@cegedim.fr>
;;; Licence: BSD
;;;
;;; Commentary: 
;;;
;;;

(in-package :common-lisp-user)

(defpackage :vecmath
  (:nicknames :vm)
  (:use :common-lisp)
  (:export #:vec2
           #:vec3
           #:vec4

           #:mat2
           #:mat3
           #:mat4

           #:with-vector
           #:with-vectors

           #:vec3-add
           #:vec3-add!
           #:vec3-sub
           #:vec3-sub!
           #:vec3-mul
           #:vec3-mul!
           #:vec3-div
           #:vec3-div!

           #:vec3-dot
           #:vec3-normalize
           #:vec3-normalize!
           ))

;;; packages.lisp ends here
