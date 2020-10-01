;;;; macrolets.lisp

(in-package #:list-accumulator)
(proclaim '(optimize))

;;; Allow user-definition of LACC DSL.

(defvar *common-macrolets* nil
  "List of macrolets common to all accumulator macros.")

(defvar *common-symbols* nil
  "List of symbol-macrolets common to all accumulator macros.")

(defvar *macrolaccs* *common-macrolets*
  "List of macrolets for LIST-ACCUMULATOR.")

(defvar *symbol-macrolaccs* *common-symbols*
  "List of symbol-marolets for LIST-ACCUMULATOR.")
     
(defvar *macrotraccs* *common-macrolets*
  "Define macrolets for TREE-ACCUMULATOR.")

(defvar *symbol-macrotraccs* *common-macrolets*
  "Define symbol-macrolets for TREE-ACCUMULATOR.")

;;; Macro instead of Fn to implicitly quote args.
(defmacro add-macro-to (lst &rest args)
  "Convenience macro for user-defined macrolets."
  `(dolist (f ',args) (pushnew f ,lst)))

(defmacro common-macrolet (&rest args)
  "Define common-macrolets for all accumulator macros."
  `(lacc::add-macro-to *common-macrolets* ,@args))

(defmacro common-symbol-macrolet (&rest args)
  "Define common symbol-macrolets for all accumulator macros."
  `(lacc::add-macro-to *common-symbol-macrolets* ,@args))

(defmacro macrolacc (&rest args)
  "Define macrolets for LIST-ACCUMULATOR."
  `(lacc::add-macro-to *macrolaccs* ,@args))
   
(defmacro symbol-macrolacc (&rest args)
  "Define symbol-macrolets for LIST-ACCUMULATOR."
  `(lacc::add-macro-to *symbol-macrolaccs* ,@args))

(defmacro macrotracc (&rest args)
  "Define macrolets for TREE-ACCUMULATOR."
  `(lacc::add-macro-to *macrotraccs* ,@args))

(defmacro symbol-macrotracc (&rest args)
  "Define symbol-macrolets for TREE-ACCUMULATOR."
  `(lacc::add-macro-to *symbol-macrotraccs* ,@args))

(common-symbol-macrolet
     (nullify     (setf acc nil))
     (zeroize     (setf acc 0))

     (increment   (incf acc))
     (inc         increment)
     (decrement   (decf acc))
     (dec         decrement)

     (collect     (push it acc))
     (collect-new (pushnew it acc)))

(macrolacc
     (operate (fn) `(setf acc (funcall ,fn acc it))))

(symbol-macrolacc
     ;; LST is list passed to ALACC
     (it          (car lst))       
     (next        (second lst)) 


     (op          operate)
     (sum         (op #'+))
     (add         sum)
     (subtract    (op #'-)) 
     (sub         subtract)
     (multiply    (op #'*))
     (mult        multiply)
     (mul         multiply)
     (divide      (op #'/))
     (div         divide))

(symbol-macrotracc
     ;; TREE is the tree passed to TRACC.
 )
