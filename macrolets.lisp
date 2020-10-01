;;;; macrolets.lisp

(in-package #:list-accumulator)
(proclaim '(optimize))

;;; Allow user-definition of LACC DSL.
(defvar *macrolaccs* nil
  "List of macrolets for LIST-ACCUMULATOR.")

(defvar *symbol-macrolaccs* nil
  "List of symbol-marolets for LIST-ACCUMULATOR.")

(defmacro macrolacc (&rest args)
  "Define macrolets for LIST-ACCUMULATOR."
  `(dolist (f ',args) (pushnew f *macrolaccs*)))
   
(defmacro symbol-macrolacc (&rest args)
  "Define symbol-macrolets for LIST-ACCUMULATOR."
  `(dolist (f ',args) (pushnew f *symbol-macrolaccs*)))

(macrolacc
     (operate (fn) `(setf acc (funcall ,fn acc it))))

(symbol-macrolacc
     ;; LST is list passed to ALACC
     (it          (car lst))       
     (next        (second lst))     

     (nullify     (setf acc nil))
     (zeroize     (setf acc 0))

     (increment   (incf acc))
     (inc         increment)
     (decrement   (decf acc))
     (dec         decrement)

     (collect     (push it acc))
     (collect-new (pushnew it acc))

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
     
