;;;; jungl.asd

(asdf:defsystem #:jungl.micro-l-system
  :description "Master package for all jungl packages"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "jungl.micro-l-system/package")
               (:file "jungl.micro-l-system/micro-l")))
