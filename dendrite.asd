;;;; dendrite.asd

(asdf:defsystem #:dendrite
  :description "Master package for all dendrite packages"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:dendrite.micro-l-system
               #:dendrite.primitives)
  :components ((:file "dendrite/package")))
