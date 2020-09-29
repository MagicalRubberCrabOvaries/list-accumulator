;;;; list-accumulator.asd

(asdf:defsystem #:list-accumulator
  :description "Provides utilities for producing tail-recursive list eaters fast."
  :author "MRCO <angryspacefungus@gmail.com"
  :license  "GPL 3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "list-accumulator"
                :depends-on ("package" "utils"))
               (:file "aliases"
                :depends-on ("list-accumulator"))))
