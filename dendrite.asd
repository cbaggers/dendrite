;;;; dendrite.asd

(asdf:defsystem #:dendrite
  :description "Master package for all dendrite packages"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:dendrite.micro-l-system)
  :components ((:file "dendrite/package")))
