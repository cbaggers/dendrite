;;;; dendrite.asd

(asdf:defsystem #:dendrite.micro-l-system
  :description "Tiniest L-System"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :components ((:file "dendrite.micro-l-system/package")
               (:file "dendrite.micro-l-system/micro-l")))
