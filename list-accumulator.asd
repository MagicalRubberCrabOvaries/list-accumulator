;;;; list-accumulator.asd

(asdf:defsystem #:list-accumulator
  :description "Provides utilities for producing tail-recursive list eaters fast."
  :author "MRCO <angryspacefungus@gmail.com"
  :license  "GPL 3.0"
  :version "0.0.2"
  :serial t
  :components ((:file "package")
               (:file "macrolets"        :depends-on ("package"))
               (:file "list-accumulator" :depends-on ("macrolets"))
               (:file "aliases"          :depends-on ("list-accumulator"))))
