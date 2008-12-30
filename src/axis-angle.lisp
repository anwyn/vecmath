;;; axis-angle.lisp --- Simple vector math for 3D graphics
;;;
;;;             _                             _      
;;;   __ ___  _(_)___        __ _ _ __   __ _| | ___ 
;;;  / _` \ \/ / / __|_____ / _` | '_ \ / _` | |/ _ \
;;; | (_| |>  <| \__ \_____| (_| | | | | (_| | |  __/
;;;  \__,_/_/\_\_|___/      \__,_|_| |_|\__, |_|\___|
;;;                                     |___/        
;;; 
;;; Copyright (C) 2007 Ole Arndt <ole@sugarshark.com>
;;; 

(in-package :vecmath)


(defvector axis-angle (vec3)
    (angle))

(defvecfun axis-angle<-mat3 (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:type mat3)
     (:return-type axis-angle)
     (:documentation "Extract an axis and an angle from a rotation matrix."))
  (let* ((x (- m21 m12))
         (y (- m02 m20))
         (z (- m10 m01))
         (cos (half (- 1.0 (+ m00 m11 m22))))
         (sin (half (sqrt (+ (square x)
                             (square y)
                             (square z))))))
    (values x y z (atan sin cos))))

(defvecfun axis-angle<-quat (((x y z w) q))
    ((:type quat)
     (:return-type axis-angle)
     (:documentation "Extract an axis and an angle from a quaternion."))
  (multiple-value-bind (ax ay az)
      (quat-axis* x y z w)
    (values ax ay az (quat-angle* x y z w))))



;;; axis-angle.lisp ends here
