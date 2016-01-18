
(defpackage :dendrite.primitives
  (:use :cl
        :cl-game-math.base-vectors
        :cl-game-math.base-matrices
        :cl-game-math.base-maths)
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
           :swap-winding-order))
