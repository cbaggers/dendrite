;;;; dendrite.asd

(asdf:defsystem #:dendrite.primitives
  :description "Generates mesh data for primitives"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "2 Clause BSD"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:rtg-math #:cffi)
  :components ((:file "dendrite.primitives/package")
               (:file "dendrite.primitives/primitives-cffi")
               (:file "dendrite.primitives/primitives")))
