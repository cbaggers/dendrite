
(defpackage :dendrite.primitives
  (:use :cl :rtg-math :rtg-math.base-maths :cffi)
  (:export :latice-data
           :primitive-data
           :cap-data
           :plain-data
           :box-data
           :cube-data
           :equilateral-triangle-data
           :sphere-data
           :cone-data
           :cylinder-data
           :prim-array
           :swap-winding-order

           ;; Versions returning cffi pointers
           :latice-foreign
           ))
