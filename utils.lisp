;;;; utils.lisp

(in-package #:list-accumulator)

#| Utility functions for the list-accumulator functions.|#

;;; Typical named-lambda convention.
;;;   Local def to avoid dependencies.
(defmacro named-lambda (name lambda-list &body body)
  "Return a recursive lambda callable from fn-namespace."
  `(labels ((,name ,lambda-list ,@body)) #',name))

;;; Returns a lambda that can recur on self using the
;;;   local macro REC which always refers to the gensym.
;;;   See NAMED-LAMBDA for details on the local macro.
(defmacro genlambda (lambda-list &body body)
  "Bind a NAMED-LAMBDA to a GENSYM. Call REC to recur."
  (let ((name (gensym)))
    `(named-lambda ,name ,lambda-list
       (macrolet ((rec (&rest args)
                    `(,',name ,@args)))
         ,@body))))

#| Define Macros to be local to anaphoric-list-accumulator |#

(defvar *macrolets* nil
  "Hold macrolets local to ANAPHORIC-LIST-ACCUMULATOR.")

(defvar *symbol-macrolets* nil
  "Hold symbol-macrolets local to ANAPHORIC-LIST-ACCUMULATOR.")

(defun pushnews (obj &rest args)
  "PUSHNEW all items in ARGS to OBJ."
  (dolist (arg args) (pushnew arg obj)))

(defmacro alacc-macrolets (&rest args)
  `(lacc::pushnews lacc::*macrolets* ,@args))

(defmacro alacc-symbols (&rest args)
  `(lacc::pushnews lacc::*symbol-macrolets* ,@args))

(alacc-macrolets
    (operate (fn) `(setf acc (funcall ,fn acc it))))

(alacc-symbols
    (op          operate)
    (increment   (incf acc))
    (inc         increment)
    (decrement   (decf acc))
    (dec         decrement)
    (collect     (push it acc))
    (collect-new (pushnew it acc))
    (sum         (op #'+))
    (add         sum)
    (subtract    (op #'-)) 
    (sub         subtract)
    (multiply    (op #'*))
    (mult        multiply)
    (mul         multiply)
    (divide      (op #'/))
    (div         divide))
