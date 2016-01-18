;;;; dendrite.asd

(asdf:defsystem #:dendrite.micro-l-system
  :description "Master package for all dendrite packages"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "dendrite.micro-l-system/package")
               (:file "dendrite.micro-l-system/micro-l")))
