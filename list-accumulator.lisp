;;;; list-accumulator.lisp

(in-package #:list-accumulator)
(proclaim '(optimize))

;;; TAIL-RECURSIVE LIST-EATERS



;;; Return a tail-recursive list-eater function.
;;;   FN should modify the accumulator ACC.
;;;   ACC-INIT specifies initial value for ACC.
(defun list-accumulator (fn &optional acc-init)
  "Return a recursive lambda for recurring on cdrs."
  (genlambda (lst &optional (acc acc-init))
       (if lst
           ;; If LST is not nil, recur on CDR.
           (rec (cdr lst)
                (funcall fn
                         (car lst)
                         acc))
           ;; Return ACC at end of list.
           acc)))

;;; Create a LACC with anaphoric reference to
;;;   (CAR LST) as IT and the accumulator, ACC.
;;;   The BODY will comprise the contents of the
;;;   FN in LACC.
(defmacro anaphoric-list-accumulator (acc-init &body body)
  "Anaphoric wrapper for list-accumulator with smybol macrolet for ."
  `(lacc #'(lambda (it acc)
             (macrolet (,@lacc::*macrolets*)
               (symbol-macrolet (,@lacc::*symbols*)
                 ,@body)))
         ,acc-init))

;;; Wrapper for ALACC to embed within DEFUN or DEFMACRO.
(defmacro accumulate-from-list (lst acc-init &body body)
  "Call a function to recur on CDRs of LST."
  `(funcall (anaphoric-list-accumulator ,acc-init ,@body) ,lst))
