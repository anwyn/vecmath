;;; matrix.lisp --- Simple vector and matrix math for 3d graphics
;;;
;;; Copyright (C) 2007 Ole Arndt
;;; Author: Ole Arndt <ole@sugarshark.com>
;;; Licence: BSD
;;;
;;; Commentary: 
;;;
;;;

(in-package :vecmath)


(defmacro with-matrix-rows (slots vector &body body)
  (let ((vec (gensym))
        (index-counter 0))
    `(let ((,vec ,vector))
       (declare (ignorable ,vec))
       ,@(let ((vector (if (and (consp vector) (eq (car vector) 'the))
                             (third vector)
                             vector)))
           (and (symbolp vector)
                `((declare (%variable-rebinding ,vec ,vector)))))
       ,vec
       (macrolet ,(mapcar (lambda (slot-entry)
                                   (let ((var-name
                                          (if (symbolp slot-entry)
                                              slot-entry
                                              (car slot-entry)))
                                         (index
                                          (if (symbolp slot-entry)
                                              index-counter
                                              (cadr slot-entry))))
                                     (setf index-counter (1+ index))
                                     `(,var-name (n)
                                       (list 'aref ',vec ,index n))))
                                 slots)
                        ,@body))))

(defvector mat2 ()
    ((m00 1.0) m01
     m10 (m11 1.0)))

(defvector mat3 ()
    ((m00 1.0) m01 m02
     m10 (m11 1.0) m12
     m20 m21 (m22 1.0)))

(defvector mat4 ()
    ((m00 1.0) m01 m02 m03
     m10 (m11 1.0) m12 m13
     m20 m21 (m22 1.0) m23
     m30 m31 m32 (m33 1.0)))


;; (defun mat3<-rows (a b c)
;;   (with-vectors ((ax ay az) a
;;                  (bx by bz) b
;;                  (cx cy cz) c)
;;     ))

(defun mat3-column-0* (m)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (values ax ay az)))

(defun mat3-column-0 (m)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (vec3 ax ay ay)))

(defun mat3-column-0! (m v)
  (with-vector-elements ((ax 0) (ay 3) (az 6)) m
    (multiple-value-setq (ax ay az)
      (vec3->values v))))


(defvecfun mat3-transpose (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Transpose the matrix"))
  (values m00 m10 m20 m01 m11 m21 m02 m12 m22))

(defvecfun mat3-determinant (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:returning-scalar t)
     (:documentation "Calculate the determinant of the matrix"))
  (+ m00 m10 m20 m01 m11 m21 m02 m12 m22))

(defvecfun mat3-invert (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Invert the matrix"))
  (values m00 m10 m20 m01 m11 m21 m02 m12 m22))

(defvecfun mat3-negate (((m00 m01 m02 m10 m11 m12 m20 m21 m22) m))
    ((:documentation "Negate the matrix"))
  (values (- m00) (- m01) (- m02)
          (- m10) (- m11) (- m12)
          (- m20) (- m21) (- m22)))

;;; matrix.lisp ends here
