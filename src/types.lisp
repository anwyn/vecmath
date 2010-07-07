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

;;;; ---------------------------------------------------------------------------
;;;; * Type definitions

;;;; ---------------------------------------------------------------------------
;;;; ** A vector type
;;;;
;;;; A vector of arbitrary length with element type scalar

(deftype vec (&optional len (element-type 'scalar))
  `(simple-array ,element-type (,len)))

(setf (get 'vec 'element-type) 'scalar)

(declaim (inline vec))
(defun vec (&rest args)
  (make-array (length args) :element-type 'scalar
              :initial-contents (mapcar #'ensure-scalar args)))

(define-compiler-macro vec (&whole form &rest args)
  (cond ((every #'numberp args)
         (apply #'vec args))
        (t form)))

;;;; ---------------------------------------------------------------------------
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
  (if (square-matrix-p args)
      (apply #'vec args)
      (error 'simple-error
             :format-control "arglist must fit a square matrix
and be of length (* dimension dimension)")))

;;;; ---------------------------------------------------------------------------
;;;; * Macro definitions

(defun expand-vector-arg (vec &optional slot-list)
  (let ((sym (ensure-car vec))
        (type (ensure-cadr vec nil)))
    (if (or (null type) (not (symbolp type)))
        (list sym)
        (let ((*package* (or (symbol-package sym) *package*)))
          (mapcar (lambda (slot)
                    (symbolicate sym '#:. slot))
                  (or slot-list
                      (get type 'slot-list)
                      (error 'type-error
                             :format-control "~a is not a vecmath vector type ~a"
                             :datum type
                             :expected-type "defined with DEFVECTOR")))))))

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
  (let ((len (length slots))
        (args (mapcar #'ensure-car slots)))
    `((deftype ,type (&optional (element-type 'scalar))
        ,(list 'list ''vec len 'element-type))

      (declaim (inline ,type))

      (defstruct (,type
                   (:type (vector ,element-type))
                   (:copier nil)
                   (:constructor)
                   (:constructor ,type ,(cons '&optional args)))
        ,@(mapcar (lambda (s)
                    (list (ensure-car s)
                          (coerce (ensure-cadr s 0) element-type)
                          ':type element-type))
           slots)))))


(defun emit-meta-data (type element-type slots)
  `((eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (get ',type 'slot-list) ',(mapcar #'ensure-car slots))
      (setf (get ',type 'element-type) ',element-type))))

(defun add-offset (offset slots)
  (cons (list (first slots) offset) (rest slots)))

(defun emit-converters (type slots)
  "Emit converting functions."
  (let ((vec (gensym "V"))
        (unique-slots (mapcar #'(lambda (slot)
                                  (symbolicate (ensure-car slot)))
                              slots))
        (vec->values (symbolicate type '#:->values))
        (vec<-values (symbolicate type '#:<-values))
        (vec<-values! (symbolicate type '#:<-values!))
        (cloner (symbolicate type '#:-copy))
        (copier (symbolicate type '#:-copy!)))
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

      (declaim (inline ,cloner ,copier))

      (defun ,cloner (v &key (offset 0))
        "Clone a vector from the source vector V with offset."
        (declare (type vec v))
        (with-elements ,(add-offset 'offset unique-slots) v
          (,type ,@unique-slots)))

      (defun ,copier (source target &key (source-offset 0) (target-offset 0))
        "Copy vector SOURCE into vector TARGET. Returns TARGET."
        (declare (type vec source target))
        (,vec<-values! target (,vec->values source :offset source-offset)
                       :offset target-offset)))))

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
                     ,(mapcar (lambda (arg)
                                (if (atom arg)
                                    (if (eq arg '&optional)
                                        arg
                                        scalar-type)
                                    (ensure-cadr arg))) args)
                     ,return-type) ,name))))

(defun emit-vector-function (name args doc body)
  `((defun ,name ,(mapcar #'ensure-car args)
      ,doc
      (with-vectors ,args
        ,@body))))

(defun emit-plain-function (name args doc body)
  `((defun ,name ,(mapcar #'ensure-car args)
      ,doc
      ,@body)))

;;;; ---------------------------------------------------------------------------
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
      (let ((scalar-args (and (or (eq type 'scalar) slot-list)
                              (option? :scalar-args-version t)
                              (expand-vector-lambda-list arglist))))
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
                                                `((,scalar-args-name ,@scalar-args))
                                                body))

                      ;; main function
                      (emit-declarations type element-type name arglist)
                      (emit-plain-function name arglist doc
                                           `((multiple-value-call #',type
                                               (,mv-name ,@(mapcar #'ensure-car arglist)))))

                      ;; destructive function
                      (when (and (option? :destructive-version t)
                                 (eq type (ensure-cadr (first arglist))))
                        (nconc (emit-declarations type element-type destructive-name arglist)
                               (emit-vector-function destructive-name arglist
                                                     (concatenate 'string doc "
Will destructivly modify the the first parameter to contain the result
of the operation.")
                                                     `((setf (values ,@(expand-vector-arg (first arglist)))
                                                             (,mv-name ,@(mapcar #'ensure-car arglist)))
                                                       ,(ensure-car (first arglist)))))))))))))


;;; types.lisp ends here
