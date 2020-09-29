;;;; aliases.lisp

(in-package #:list-accumulator)
(proclaim '(optimize))

;;; Typical alias convention.
;;;     Defined locally to avoid dependencies.
(defmacro alias (new old)
  `(defmacro ,new (&rest args)
     `(,',old ,@args)))

(alias lacc list-accumulator)
(alias alacc anaphoric-list-accumulator)
(alias accl accumulate-from-list)
(alias ulambda genlambda)
