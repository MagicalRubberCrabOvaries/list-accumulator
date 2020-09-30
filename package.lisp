;;;; package.lisp

(defpackage #:list-accumulator
  (:nicknames :lacc :accl)

  (:use #:cl)

  (:export

   ;; SYM                ALIAS
   :named-lambda         :alambda
   :genlamda             :ulambda
   :*macrolaccs*
   :*symbol-macrolaccs*
   :macrolacc
   :symbol-macrolacc
   :list-accumulator     :lacc
   :accumulate-from-list :accl))
