;;;; package.lisp

(defpackage #:list-accumulator
  (:nicknames :lacc)

  (:use #:cl)

  (:export

   ;; utils
   :named-lambda :genlamda

   ;; list-accumulator and alias from aliases
   :list-accumulator :lacc
   :anaphoric-list-accumulator :alacc
   :accumulate-from-list :accl))
