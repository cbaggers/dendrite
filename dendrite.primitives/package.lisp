;; dendrite.primitives

(uiop:define-package :dendrite.primitives
  (:use :cl :rtg-math :rtg-math.base-maths :cffi)
  (:export :lattice-data
           :primitive-data
           :cap-data
           :plain-data
           :box-data
           :cube-data
           :sphere-data
           :cone-data
           :cylinder-data
           :prim-array
           :swap-winding-order

           ;; Versions returning cffi pointers
           :lattice-foreign
           :plain-foreign
           :box-foreign
           :cube-foreign
           :cone-foreign
           :cylinder-foreign
           :sphere-foreign))
