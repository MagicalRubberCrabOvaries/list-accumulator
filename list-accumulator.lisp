;;;; list-accumulator.lisp

(in-package #:list-accumulator)
(proclaim '(optimize))

;;; Typical named-lambda/alambda convention.
;;;   Can self reference by calling NAME
(defmacro named-lambda (name lambda-list &body body)
  "Return a recursive lambda callable from fn-namespace."
  `(labels ((,name ,lambda-list ,@body)) #',name))

;;; Bind NAMED-LAMBDA to a GENSYM. Allow self-reference
;;;   with REC symbol-macrolet. Can CALL
(defmacro genlambda (lambda-list &body body)
  "Bind a NAMED-LAMBDA to a GENSYM. Call REC to recur."
  (let ((name (gensym)))
    `(named-lambda ,name ,lambda-list
       (macrolet ((rec (&rest args) `(,',name ,@args)))
         ,@body))))

;;; Return a tail-recursive list-eater function.
;;;   ACC-INIT specifies initial value for ACC.
;;;   BODY is inside an implicit PROGN which modifies
;;;   ACC to be passed to next recursion.
;;;   Local macros defined in *MACROLACCS* and
;;;   *SYMBOL-MACROLACCS* are defined in file
;;;   macrolets.lisp and are user-extendable.
(defmacro list-accumulator (acc-init &rest body)
  "Return a recursive lambda for recurring on cdrs."
  `(genlambda (lst &optional (acc ,acc-init))
      (macrolet (,@*common-macrolets*
                 ,@*macrolaccs*)
        (symbol-macrolet (,@*common-symbol-macrolets*
                          ,@*symbol-macrolaccs*)
            (if lst
                (rec (cdr lst) (progn ,@body acc))
                acc))))))

;;; Wrapper for LIST-ACCUMULATOR.
(defmacro accumulate-from-list (lst acc-init &body body)
  "Recur on CDRs of LST using LACC DSL."
  `(funcall (list-accumulator ,acc-init ,@body) ,lst))

#| Return a tail-recursive tree-eater function.
   ACC-INIT specifies initial value for ACC
   BODY is inside a BLOCK (NIL) which can be
      returned from with the return macro. |#
(defmacro tree-accumulator (acc-init &body body)
  `(genlambda (tree &optional (acc acc-init))
      (macrolet (,@*common-macrolets*
                 ,@*macrotraccs*)
        (symbol-macrolet (,@*common-macrolets*
                          ,@*symbol-macrotraccs*)
          (if (atom tree)
              acc
              (block nil
                ,@body
                (setf acc (rec (car tree) acc))
                (rec (cdr tree) acc)))))))

;; Wrapper for TREE-ACCUMULATOR.
(defmacro accumulate-from-tree (tree acc-init &body body)
  "Walk TREE using TRACC DSL."
  `(funcall (tree-accumulator ,acc-init ,@body) ,tree))
