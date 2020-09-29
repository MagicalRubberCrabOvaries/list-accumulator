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
