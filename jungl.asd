;;;; jungl.asd

(asdf:defsystem #:jungl
  :description "Master package for all jungl packages"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:jungl.micro-l-system)
  :components ((:file "jungl/package")))
