;;; types.lisp --- defining vector types
;;;
;;;  _
;;; | |_ _   _ _ __   ___  ___
;;; | __| | | | '_ \ / _ \/ __|
;;; | |_| |_| | |_) |  __/\__ \
;;;  \__|\__, | .__/ \___||___/
;;;      |___/|_|
;;;
;;; Copyright (C) 2010 Ole Arndt
;;; Author: Ole Arndt <ole@sugarshark.com>
;;;

(in-package :vecmath)

;;;; ----------------------------------------------------------------------------
;;;; * Type definitions

;;;; ----------------------------------------------------------------------------
;;;; ** A vector type
;;;;
;;;; A vector of arbitrary length with element type scalar

(deftype vec (&optional len (element-type 'scalar))
  `(simple-array ,element-type (,len)))

(setf (get 'vec 'element-type) 'scalar)

(declaim (inline vec))
(defun vec (&rest args)
  "Make a vector from all arguments. Any arguments that are a sequence
will be flattened into the resulting vector."
  (if (every #'scalarp args)
      (coerce args 'vec)
      (coerce (mapcan (lambda (a)
                        (etypecase a
                          (number (list (ensure-scalar a)))
                          (sequence (map 'list #'ensure-scalar a))))
                      args) 'vec)))

(define-compiler-macro vec (&whole form &rest args)
  (cond ((every #'numberp args)
         (coerce (mapcar #'ensure-scalar args) 'vec))
        (t form)))

(defun make-vec (&key size (initial-element +scalar-zero+))
  (make-array size :element-type 'scalar :initial-element initial-element))

(defmacro vec<-values (form)
  "Make a vec from multiple values."
  `(coerce (multiple-value-list ,form) 'vec))

(defun add-offset (offset slots)
  (cons (list (first slots) offset) (rest slots)))

(defmacro vec->values (v &key (offset 0) (size 1))
  "Convert the SIZE elements starting at index OFFSET of the vector VEC
to multiple values."
  (let ((unique-slots (loop for i below size collect (gensym))))
    `(with-elements ,(add-offset offset unique-slots) ,v
       (values ,@unique-slots))))

(defmacro vec<-values! (v form &key (offset 0) (size 1))
  "Set the SIZE elements starting at index OFFSET of the vector VEC
to the multiple values returned by the FORM."
  (let ((unique-slots (loop for i below size collect (gensym "S")))
        (vec (gensym "V")))
    `(let ((,vec ,v))
       (with-elements ,(add-offset offset unique-slots) ,vec
         (setf (values ,@unique-slots) ,form))
       ,vec)))


;;;; ----------------------------------------------------------------------------
;;;; ** A square matrix type

(defun square-matrix-p(a)
  (let ((len (length a)))
    (= (isqrt len) (sqrt len))))

;;; A square matrix with element type scalar
(deftype mat (&optional dimension (element-type 'scalar))
  `(and (simple-array ,element-type (,(if (eq '* dimension)
                                          dimension
                                          (* dimension dimension))))
        (satisfies square-matrix-p)))

(setf (get 'mat 'element-type) 'scalar)

(declaim (inline mat))
(defun mat (&rest args)
  (apply #'vec args))

;;;; ----------------------------------------------------------------------------
;;;; * Macro definitions

(defun expand-vector-arg (vec &optional slot-list)
  (let ((sym (ensure-car vec))
        (type (ensure-cadr vec nil)))
    (cond ((and type (constantp type))
           (list sym type))
          ((or (null type)
               (eq 'scalar type)
               (not (symbolp type)))
           (list sym))
          (t
           (let ((*package* (or (symbol-package sym) *package*)))
             (mapcar (lambda (slot)
                       (symbolicate sym '#:. slot))
                     (or slot-list
                         (get type 'slot-list)
                         (error 'type-error
                                :format-control "~a is not a vecmath vector type ~a"
                                :datum type
                                :expected-type "defined with DEFVECTOR"))))))))

(defun expand-vector-lambda-list (vecs)
  (mapcan #'expand-vector-arg vecs))

(defmacro with-vector ((form type) &body body)
  (let ((name (ensure-car form))
        (init (ensure-cadr form))
        (slot-list (get type 'slot-list)))
    (if slot-list
        `(let ((,name ,init))
           (declare (type (or null vec) ,name) (ignorable ,name))
           (with-elements ,(expand-vector-arg (list name type) slot-list)
             ,name
             ,@body))
        `(let ((,name ,init))
           (declare (type (or null ,type) ,name) (ignorable ,name))
           ,@body))))

(defmacro with-vectors ((&rest forms) &body body)
  "Convenience macro for nesting WITH-VECTOR forms. "
  (cond ((null forms)
         `(progn ,@body))
        ((consp (car forms))
         `(with-vector ,(car forms)
            (with-vectors ,(cdr forms) ,@body)))
        (t
         `(with-vectors ,(cdr forms) ,@body))))

;;;; -------------------------------------------------------------------------
;;;; * Macro helper functions

(defun emit-type-declaration (type element-type slots)
  "Emit the vector type declaration."
  (let ((default-args (mapcar (lambda (s)
                                (list (ensure-car s)
                                      (coerce (ensure-cadr s 0) element-type)))
                              slots))
        (simple-args (mapcar #'ensure-car slots)))
    `((deftype ,type (&optional (element-type ',element-type))
        ,(list 'list ''vec (length slots) 'element-type))

      (declaim (inline ,type)
               (ftype (function ,(cons '&optional
                                       (mapcar (constantly element-type)
                                               slots))
                                ,type)))

      (defun ,type (&optional ,@default-args)
        (vec ,@simple-args))

      (define-compiler-macro ,type (&whole form &optional ,@default-args)
        (let ((args (list ,@simple-args)))
          (cond ((every #'numberp args)
                 (coerce (mapcar #'ensure-scalar args) ',type))
                (t form)))))))


(defun emit-meta-data (type element-type slots)
  `((eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (get ',type 'slot-list) ',(mapcar #'ensure-car slots))
      (setf (get ',type 'element-type) ',element-type))))

(defun emit-converters (type slots)
  "Emit converting functions."
  (let ((vec (gensym "V"))
        (unique-slots (mapcar #'(lambda (slot)
                                  (symbolicate (ensure-car slot)))
                              slots))
        (len (length slots))
        (vec->values (symbolicate type '#:->values))
        (vec<-values (symbolicate type '#:<-values))
        (vec<-values! (symbolicate type '#:<-values!))
        (copy (symbolicate type '#:-copy))
        (map (symbolicate type '#:-map)))
    `((defmacro ,vec->values (v &key (offset 0))
        "Convert a vec to multiple values starting from an offset into the vector."
        (list 'with-elements (add-offset offset ',unique-slots) v
              '(values ,@unique-slots)))

      (defmacro ,vec<-values (form)
        "Make a vec from multiple values."
        (list 'multiple-value-bind ',unique-slots form
              '(,type ,@unique-slots)))

      (defmacro ,vec<-values! (v form &key (offset 0))
        "Set a vec from multiple values starting from an offset into the vector."
        (list 'let (list (list ',vec (list 'or v (list ',type))))
              (list 'with-elements (add-offset offset ',unique-slots) ',vec
                    (list 'setf (cons 'values ',unique-slots) form) ',vec)))

      (declaim (inline ,copy))

      (defun ,copy (v &key (offset 0))
        "Clone a vector from the source vector V with offset."
        (declare (type vec v))
        (with-elements ,(add-offset 'offset unique-slots) v
          (,type ,@unique-slots)))

      (defun ,map (target fn source &key (target-offset 0) (source-offset 0) length)
        "Map vector SOURCE into vector TARGET. Returns TARGET."
        (declare (type vec source)
                 (type (or null vec) target))
        (let* ((l (or length (min (length source) (length target))))
               (tgt (or target (make-vec :size l))))
          (loop for len = l then (- len ,len)
                for i = (* source-offset ,len) then (+ i ,len)
                for j = (* target-offset 3) then (+ i ,len)
                while (>= len ,len)
                do (with-elements ,(add-offset 'j unique-slots) tgt
                     (setf (values ,@unique-slots)
                           (with-elements ,(add-offset 'i unique-slots) source
                             (funcall fn ,@unique-slots))))
                finally (return tgt)))))))


;;;; -------------------------------------------------------------------------
;;;; * Vector type defining macro

(defmacro defvector (type slots &body options)
  (let* ((element-type (or (cadr (assoc :element-type options)) 'scalar)))
    `(progn ,@(emit-type-declaration type element-type slots)
            ,@(emit-meta-data type element-type slots)
            ,@(emit-converters type slots))))

(defun slot-list-to-values (element-type slot-list)
  (cons 'values (mapcar (lambda (slot)
                          (declare (ignore slot))
                          element-type)
                        slot-list)))

(defun emit-declarations (return-type scalar-type name args &key (inline t))
  `((declaim ,@(when inline (list (list 'inline name)))
             (ftype (function
                     ,(let ((&-seen nil))
                        (mapcar (lambda (arg)
                                  (if (atom arg)
                                      (if (or (eq arg '&optional)
                                              (eq arg '&key))
                                          (prog1 arg
                                            (setf &-seen t))
                                          (if &-seen
                                              `(or null ,scalar-type)
                                              scalar-type))
                                      (if &-seen
                                          `(or null ,(ensure-cadr arg))
                                          (ensure-cadr arg)))) args))
                     ,return-type) ,name))))

(defun emit-vector-function (name args doc body)
  `((defun ,name ,(mapcar #'ensure-car args)
      ,doc
      (with-vectors ,(remove '&optional (remove '&key args))
        ,@body))))

(defun emit-plain-function (name args doc body)
  `((defun ,name ,(mapcar #'ensure-car args)
      ,doc
      ,@body)))

;;;; ----------------------------------------------------------------------------
;;;; * Vector function defining macro

(defmacro defvfun (name arglist type &body body)
  "Define a vector operation. This macro will define several functions."
  (let ((element-type (or (get type 'element-type)
                          (error 'type-error
                                 :format-control "~a is not a vecmath vector type ~a"
                                 :datum type
                                 :expected-type "defined with DEFVECTOR")))
        (doc (when (stringp (first body))
               (prog1 (first body) (pop body))))
        (options (when (keywordp (first (first body)))
                   (prog1 (first body) (pop body))))
        (slot-list (get type 'slot-list))
        (destructive-name (symbolicate name "!"))
        (scalar-args-name (symbolicate name "*"))
        (mv-name (symbolicate name "%")))
    (flet ((option? (option default)
             (getf options option default)))
      (let* ((car-args (mapcar #'ensure-car arglist))
             (no&-args (remove '&key (remove '&optional car-args)))
             (scalar-args (and (or (eq type 'scalar) slot-list)
                               (option? :scalar-args-version t)
                               (expand-vector-lambda-list arglist)))
             (scalar-no&-args (remove '&key (remove '&optional scalar-args))))

        (cond ((eq type 'scalar)
               (nconc (list 'progn)
                      (emit-declarations type element-type name arglist :inline (option? :inline t))
                      (emit-vector-function name arglist doc body)
                      (when (option? :scalar-args-version t)
                        (nconc (emit-declarations type element-type scalar-args-name scalar-args)
                               (emit-plain-function scalar-args-name scalar-args
                                                    (concatenate 'string doc "
Multiple values version. Takes the individual vector element as arguments.")
                                                    body)))))

              ;; Emit functions for a vector operation returning type not defined
              ;; with defvector. This may be the types vec and mat.
              ((null slot-list)
               (nconc (list 'progn)
                      (emit-declarations type element-type name arglist :inline (option? :inline nil))
                      (emit-vector-function name arglist doc body)))

              ;; Emit functions for a vector operation returning a vector type
              (t
               (nconc (list 'progn)
                      ;; function with flat arglist
                      (when (option? :scalar-args-version t)
                        (nconc (emit-declarations (slot-list-to-values element-type slot-list)
                                                  element-type scalar-args-name scalar-args)
                               (emit-plain-function scalar-args-name scalar-args
                                                    (concatenate 'string doc "
Multiple values version. Takes individual vector components as arguments
and returns the result as multiple values.")
                                                    body)))

                      ;; function returning multiple values
                      (emit-declarations (slot-list-to-values element-type slot-list)
                                         element-type mv-name arglist)
                      (emit-vector-function mv-name arglist doc
                                            (if (option? :scalar-args-version t)
                                                `((,scalar-args-name ,@scalar-no&-args))
                                                body))

                      ;; main function
                      (emit-declarations type element-type name arglist)
                      (emit-plain-function name arglist doc
                                           `((multiple-value-call #',type
                                               (,mv-name ,@no&-args))))

                      ;; destructive function
                      (when (and (option? :destructive-version t)
                                 (eq type (ensure-cadr (first arglist))))
                        (nconc (emit-declarations type element-type destructive-name arglist)
                               (emit-vector-function destructive-name arglist
                                                     (concatenate 'string doc "
Will destructivly modify the the first parameter to contain the result
of the operation.")
                                                     `((setf (values ,@(expand-vector-arg (first arglist)))
                                                             (,mv-name ,@no&-args))
                                                       ,(ensure-car (first arglist)))))))))))))


;;; types.lisp ends here
