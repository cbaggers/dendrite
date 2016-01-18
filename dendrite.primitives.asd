;;;; dendrite.asd

(asdf:defsystem #:dendrite.primitives
  :description "Generates mesh data for primitives"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "2 Clause BSD"
  :serial t
  :depends-on (#:cl-game-math)
  :components ((:file "dendrite.primitives/package")
               (:file "dendrite.primitives/primitives")))
