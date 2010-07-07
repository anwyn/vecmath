;;; packages.lisp --- Package declarations for the vecmath library.
;;;                   _
;;;  _ __   __ _  ___| | ____ _  __ _  ___  ___
;;; | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;;; | |_) | (_| | (__|   < (_| | (_| |  __/\__ \
;;; | .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
;;; |_|                         |___/
;;;
;;; Copyright (C) 2007 Ole Arndt <oliver.arndt@cegedim.fr>
;;;

;;;; * The Main Vecmath Package
;;;;
;;;; First comes the package definition and the list af exported symbols.

(in-package :common-lisp-user)

(defpackage :vecmath
    (:nicknames :vm)
  (:use :common-lisp)
  (:import-from :alexandria
                #:symbolicate)
  (:export #:scalar
           #:deg2rad
           #:rad2deg

           #:vec
           #:vec-equal
           #:vec-copy
           #:vec-mul
           #:vec-div
           #:vec-invert
           #:vec-add
           #:vec-sub
           #:vec-dot
           #:vec-magnitude^2
           #:vec-magnitude
           #:vec-distance^2
           #:vec-distance
           #:vec-scale
           #:vec-normalize
           #:vec-angle
           #:vec-interpolate

           #:with-elements
           #:with-vector
           #:with-vectors

           #:vec2
           #:make-vec2
           #:vec2-clone
           #:vec2-copy
           #:vec2-scale           #:vec2-scale*           #:vec2-scale%
           #:vec2-mul             #:vec2-mul*             #:vec2-mul5
           #:vec2-div             #:vec2-div*             #:vec2-div%
           #:vec2-invert          #:vec2-invert*          #:vec2-invert%
           #:vec2-add             #:vec2-add*             #:vec2-add%
           #:vec2-sub             #:vec2-sub*             #:vec2-sub%
           #:vec2-dot             #:vec2-dot*             #:vec2-dot%
           #:vec2-magnitude^2     #:vec2-magnitude^2*     #:vec2-magnitude^2%
           #:vec2-magnitude       #:vec2-magnitude*       #:vec2-magnitude%
           #:vec2-distance^2      #:vec2-distance^2*      #:vec2-distance^2%
           #:vec2-distance        #:vec2-distance*        #:vec2-distance%
           #:vec2-normalize       #:vec2-normalize*       #:vec2-normalize%
           #:vec2-angle           #:vec2-angle*           #:vec2-angle%
           #:vec2-interpolate     #:vec2-interpolate*     #:vec2-interpolate%
           #:vec2-transform       #:vec2-transform*       #:vec2-transform%

           #:vec3
           #:make-vec3
           #:vec3-clone
           #:vec3-copy
           #:vec3-mul             #:vec3-mul*             #:vec3-mul%
           #:vec3-div             #:vec3-div*             #:vec3-div%
           #:vec3-invert          #:vec3-invert*          #:vec3-invert%
           #:vec3-add             #:vec3-add*             #:vec3-add%
           #:vec3-sub             #:vec3-sub*             #:vec3-sub%
           #:vec3-dot             #:vec3-dot*             #:vec3-dot%
           #:vec3-magnitude^2     #:vec3-magnitude^2*     #:vec3-magnitude^2%
           #:vec3-magnitude       #:vec3-magnitude*       #:vec3-magnitude%
           #:vec3-distance^2      #:vec3-distance^2*      #:vec3-distance^2%
           #:vec3-distance        #:vec3-distance*        #:vec3-distance%
           #:vec3-scale           #:vec3-scale*           #:vec3-scale%
           #:vec3-normalize       #:vec3-normalize*       #:vec3-normalize%
           #:vec3-angle           #:vec3-angle*           #:vec3-angle%
           #:vec3-interpolate     #:vec3-interpolate*     #:vec3-interpolate%
           #:vec3-cross           #:vec3-cross*           #:vec3-cross%
           #:vec3-transform       #:vec3-transform*       #:vec3-transform%

           #:vec4
           #:make-vec4
           #:vec4-clone
           #:vec4-copy
           #:vec4-mul             #:vec4-mul*             #:vec4-mul%
           #:vec4-div             #:vec4-div*             #:vec4-div%
           #:vec4-invert          #:vec4-invert*          #:vec4-invert%
           #:vec4-add             #:vec4-add*             #:vec4-add%
           #:vec4-sub             #:vec4-sub*             #:vec4-sub%
           #:vec4-dot             #:vec4-dot*             #:vec4-dot%
           #:vec4-magnitude^2     #:vec4-magnitude^2*     #:vec4-magnitude^2%
           #:vec4-magnitude       #:vec4-magnitude*       #:vec4-magnitude%
           #:vec4-distance^2      #:vec4-distance^2*      #:vec4-distance^2%
           #:vec4-distance        #:vec4-distance*        #:vec4-distance%
           #:vec4-scale           #:vec4-scale*           #:vec4-scale%
           #:vec4-normalize       #:vec4-normalize*       #:vec4-normalize%
           #:vec4-angle           #:vec4-angle*           #:vec4-angle%
           #:vec4-interpolate     #:vec4-interpolate%     #:vec4-interpolate*
           #:vec4-transform       #:vec4-transform%       #:vec4-transform*

           #:mat
           #:mat-scale
           #:mat-mul
           #:mat-transpose
           #:mat-transform

           #:mat2
           #:make-mat2
           #:mat2-clone
           #:mat2-copy
           #:mat2-scale           #:mat2-scale*           #:mat2-scale%
           #:mat2-mul             #:mat2-mul*             #:mat2-mul%
           #:mat2-transpose       #:mat2-transpose*       #:mat2-transpose%
           #:mat2-determinant     #:mat2-determinant*     #:mat2-determinant%
           #:mat2-negate          #:mat2-negate*          #:mat2-negate%

           #:mat3
           #:make-mat3
           #:mat3-clone
           #:mat3-copy
           #:mat3<-euler          #:mat3<-euler*
           #:mat3-scale           #:mat3-scale*           #:mat3-scale%
           #:mat3-mul             #:mat3-mul*
           #:mat3-tmul            #:mat3-tmul*            #:mat3-tmul%
           #:mat3-transpose       #:mat3-transpose*       #:mat3-transpose%
           #:mat3-determinant     #:mat3-determinant*     #:mat3-determinant%
           #:mat3-invert          #:mat3-invert*          #:mat3-invert%
           #:mat3-negate          #:mat3-negate*          #:mat3-negate%

           #:mat4
           #:make-mat4
           #:mat4-clone
           #:mat4-copy
           #:mat4-scale           #:mat4-scale*           #:mat4-scale%
           #:mat4-mul             #:mat4-mul*             #:mat4-mul%
           #:mat4-transpose       #:mat4-transpose*       #:mat4-transpose%
           #:mat4-determinant     #:mat4-determinant*     #:mat4-determinant%
           #:mat4-negate          #:mat4-negate*          #:mat4-negate%

           #:quat
           #:make-quat
           #:quat-clone
           #:quat-copy
           #:quat-identity!
           #:quat-invert          #:quat-invert*          #:quat-invert%
           #:quat-scale           #:quat-scale*           #:quat-scale%
           #:quat-mul             #:quat-mul*             #:quat-mul%
           #:quat-magnitude^2     #:quat-magnitude^2*     #:quat-magnitude^2%
           #:quat-magnitude       #:quat-magnitude*       #:quat-magnitude%
           #:quat-normalize       #:quat-normalize*       #:quat-normalize%
           #:quat-angle           #:quat-angle*           #:quat-angle%
           #:quat-axis            #:quat-axis*            #:quat-axis%

           #:axis/angle
           #:make-axis/angle
           #:axis/angle-clone
           #:axis/angle-copy
           #:axis/angle<-mat3     #:axis/angle<-mat3*
           #:axis/angle<-mat4     #:axis/angle<-mat4*
           #:mat3<-axis/angle     #:mat3<-axis/angle*
           #:mat3<-axis-angle     #:mat3<-axis-angle*
           #:mat4<-axis/angle     #:mat4<-axis/angle*
           #:mat4<-axis-angle     #:mat4<-axis-angle*
           #:axis/angle<-quat     #:axis/angle<-quat*
           #:quat<-axis/angle     #:quat<-axis/angle*
           #:quat<-axis-angle     #:quat<-axis-angle*
           ))


;;;;@include "util.lisp"

;;;;@include "vecmath.lisp"

;;;;@include "types.lisp"

;;;;@include "vector.lisp"

;;;;@include "matrix.lisp"

;;;;@include "quat.lisp"

;;;;@include "axis-angle.lisp"

;;; packages.lisp ends here
