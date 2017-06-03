(in-package :dendrite.primitives)

(defmacro write-elem (ptr
                      (p-x p-y p-z)
                      (normal-pred n-x n-y n-z)
                      (tex-pred t-x t-y))
  `(progn
     (setf (mem-aref ,ptr :float 0) ,p-x)
     (setf (mem-aref ,ptr :float 1) ,p-y)
     (setf (mem-aref ,ptr :float 2) ,p-z)
     (incf-pointer ,ptr (* 3 4))
     (when ,normal-pred
       (setf (mem-aref ,ptr :float 0) ,n-x)
       (setf (mem-aref ,ptr :float 1) ,n-y)
       (setf (mem-aref ,ptr :float 2) ,n-z)
       (incf-pointer ,ptr (* 3 4)))
     (when ,tex-pred
       (setf (mem-aref ,ptr :float 0) ,t-x)
       (setf (mem-aref ,ptr :float 1) ,t-y)
       (incf-pointer ,ptr (* 2 4)))))

(defun plain-foreign (&key (width 1.0) (height 1.0) (normals t) (tex-coords t))
  (latice-foreign :width width :height height
                  :x-segments 1 :y-segments 1
                  :normals normals :tex-coords tex-coords))

(defun latice-foreign (&key (width 1.0) (height 1.0) (x-segments 30)
                         (y-segments 30) (normals t) (tex-coords t))
  (let* ((x-step (/ width x-segments))
         (y-step (/ height y-segments))
         (vert-width (1+ x-segments))
         (vert-height (1+ y-segments))
         (origin (v! (- (/ width 2)) (- (/ height 2)) 0))
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts-len (* vert-width vert-height elem-size))
         (verts (foreign-alloc :float :count verts-len))
         (verts-final (make-pointer (pointer-address verts)))
         (index-len (* x-segments y-segments 6))
         (indices (foreign-alloc :uint :count index-len))
         (indices-final (make-pointer (pointer-address indices))))
    ;;
    (loop :for y :below vert-height :do
       (loop :for x :below vert-width :do
          (setf (mem-aref verts :float 0) (+ (* x x-step) (x origin)))
          (setf (mem-aref verts :float 1) 0f0)
          (setf (mem-aref verts :float 2) (+ (* y y-step) (y origin)))
          (incf-pointer verts (* 3 4))
          (when normals
            (setf (mem-aref verts :float 0) 0f0)
            (setf (mem-aref verts :float 1) 1f0)
            (setf (mem-aref verts :float 2) 0f0)
            (incf-pointer verts (* 3 4)))
          (when tex-coords
            (setf (mem-aref verts :float 0) (/ (float x) x-segments))
            (setf (mem-aref verts :float 1) (/ (float y) y-segments))
            (incf-pointer verts (* 2 4)))))
    ;;
    (let ((index 0))
      (loop :for y :below y-segments :do
         (loop :for x :below x-segments :do
            (setf (mem-aref indices :uint 0) index)
            (setf (mem-aref indices :uint 1) (+ index vert-width))
            (setf (mem-aref indices :uint 2) (+ index vert-width 1))
            (setf (mem-aref indices :uint 3) index)
            (setf (mem-aref indices :uint 4) (+ index vert-width 1))
            (setf (mem-aref indices :uint 5) (+ index 1))
            (incf-pointer indices (* 4 6))
            (incf index))
         (incf index)))
    ;;
    (list verts-final verts-len
          indices-final index-len)))

(defun cube-foreign (&key (size 1.0) (normals t) (tex-coords t))
  (box-foreign :width size :height size :depth size :normals normals
               :tex-coords tex-coords))

(defun box-foreign (&key (width 1.0) (height 1.0) (depth 1.0)
                      (normals t) (tex-coords t))
  (let* ((width (/ width 2.0))
         (height (/ height 2.0))
         (depth (/ depth 2.0))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts-len (* 24 elem-size))
         (verts (foreign-alloc :float :count verts-len))
         (verts-final (make-pointer (pointer-address verts)))
         (index-len 36)
         (indices (foreign-alloc :ushort :count index-len)))
    ;;
    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth)
    (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) -1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) -1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) -1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) -1.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) -1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) -1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) -1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) -1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) height)
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) -1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) (- depth)) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) -1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 1.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) width)
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) -1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 1.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    (setf (mem-aref verts :float 0) (- width))
    (setf (mem-aref verts :float 1) (- height))
    (setf (mem-aref verts :float 2) depth) (incf-pointer verts (* 3 4))
    (when normals
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) -1.0)
      (setf (mem-aref verts :float 2) 0.0)
      (incf-pointer verts (* 3 4)))
    (when tex-coords
      (setf (mem-aref verts :float 0) 0.0)
      (setf (mem-aref verts :float 1) 0.0)
      (incf-pointer verts (* 2 4)))

    ;;
    (loop :for i :across #(0 1 2 0 2 3 4 5 6 4 6 7 8 9 10
                           8 10 11 12 13 14 12 14 15 16 17
                           18 16 18 19 20 21 22 20 22 23)
       :for c :from 0
       :do (setf (mem-aref indices :ushort c) i))
    ;;
    (list verts-final verts-len
          indices index-len)))

(defun %cap-index-float-len (segments)
  (* 3 (+ 1 segments)))

(defun %write-cap-indices (ptr segments norm-y index-offset)
  (let ((up-norm (> norm-y 0f0)))
    (if up-norm
        (loop :for s :from index-offset
           :upto (+ index-offset segments)
           :do
           (setf (mem-aref ptr :ushort 0) index-offset)
           (setf (mem-aref ptr :ushort 1) (1+ s))
           (setf (mem-aref ptr :ushort 2) s)
           (incf-pointer ptr (* 2 3)))
        (loop :for s :from index-offset
           :upto (+ index-offset segments)
           :do
           (setf (mem-aref ptr :ushort 0) index-offset)
           (setf (mem-aref ptr :ushort 1) s)
           (setf (mem-aref ptr :ushort 2) (1+ s))
           (incf-pointer ptr (* 2 3)))))
  ptr)

(defun %cap-vert-float-len (segments normals tex-coords)
  (let ((elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0))))
    (* elem-size (%cap-vert-len segments))))

(defun %cap-vert-len (segments)
  (+ 2 segments))

(defun %write-cap-verts (ptr segments y-pos norm-y radius normals tex-coords)
  (let ((angle (/ (* +pi+ 2f0) segments)))

    (write-elem
     ptr
     (0f0 y-pos 0f0)
     (normals 0f0 norm-y 0f0)
     (tex-coords 0.5f0 0.5f0))

    (loop :for s :upto segments :for ang = (* s angle) :do
       (write-elem
        ptr
        ((* radius (cos ang)) y-pos (* radius (sin ang)))
        (normals 0f0 norm-y 0f0)
        (tex-coords (sin ang) (cos ang)))))
  ptr)

(defun cone-foreign (&key (segments 10) (height 1) (radius 0.5f0)
                    (normals t) (tex-coords t) (cap t))
  (let* ((height (float height 0f0))
         (radius (float radius 0f0))
         (angle (/ (* +pi+ 2) segments))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         ;;
         (verts-size (+ (* (* 2 (1+ segments)) elem-size)
                        (if cap
                            (%cap-vert-float-len segments normals tex-coords)
                            0)))
         (index-size (+ (* segments 3)
                        (if cap (%cap-index-float-len segments) 0)))
         ;;
         (verts (foreign-alloc :float :count verts-size))
         (verts-final (make-pointer (pointer-address verts)))
         ;;
         (indices (foreign-alloc :ushort :count index-size))
         (indices-final (make-pointer (pointer-address indices))))

    (when cap
      (setf verts (%write-cap-verts verts segments 0f0 -1f0 radius normals
                                    tex-coords))
      (setf indices (%write-cap-indices indices segments -1f0 0)))

    (loop :for s :upto segments
       :for ang = (* (- s) angle)
       :for normal = (v:normalize
                      (v! (* height (cos ang))
                          radius
                          (* height (sin ang))))

       :do
       (write-elem
        verts
        (0f0 height 0f0)
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 1f0))

       (write-elem
        verts
        ((* radius (cos ang)) 0f0 (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 0f0)))

    (let ((cap-len (%cap-vert-len segments)))
      (loop :for s :below segments :for index = (+ cap-len (* 2 s)) :do
         (setf (mem-aref indices :ushort 0) index)
         (setf (mem-aref indices :ushort 1) (+ 1 index))
         (setf (mem-aref indices :ushort 2) (+ 3 index))
         (incf-pointer indices (* 2 3))))

    (list verts-final verts-size
          indices-final index-size)))

(defun cylinder-foreign (&key (segments 10) (height 1) (radius 0.5)
                           (normals t) (tex-coords t) (cap t))
  (let* ((height (float height 0f0))
         (radius (float radius 0f0))
         (angle (/ (* +pi+ 2) segments))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         ;;
         (verts-size (+ (* (1+ segments) elem-size)
                        (if cap
                            (* 2 (%cap-vert-float-len segments normals tex-coords))
                            0)))
         (index-size (+ (* segments 6)
                        (if cap (* 2 (%cap-index-float-len segments)) 0)))
         ;;
         (verts (foreign-alloc :float :count verts-size))
         (verts-final (make-pointer (pointer-address verts)))
         ;;
         (indices (foreign-alloc :ushort :count index-size))
         (indices-final (make-pointer (pointer-address indices))))

    (when cap
      (setf verts (%write-cap-verts verts segments 0f0 -1f0 radius normals tex-coords))
      (setf verts (%write-cap-verts verts segments height 1f0 radius normals tex-coords))
      (%write-cap-indices indices segments 1f0 0)
      (%write-cap-indices indices segments 1f0
                          (* (%cap-vert-float-len segments normals tex-coords)
                             elem-size)))

    (loop :for s :upto segments
       :for ang = (* s angle)
       :for normal = (v:normalize (v! (cos ang) 0 (sin ang)))
       :do

       (write-elem
        verts
        ((* radius (cos ang)) 0f0 (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 0f0))

       (write-elem
        verts
        ((* radius (cos ang)) height (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 1f0)))

    (loop :for s :below segments :for index = (* 2 s) :do
       (setf (mem-aref indices :ushort 0) index)
       (setf (mem-aref indices :ushort 1) (+ 1 index))
       (setf (mem-aref indices :ushort 2) (+ 3 index))

       (setf (mem-aref indices :ushort 3) index)
       (setf (mem-aref indices :ushort 4) (+ 3 index))
       (setf (mem-aref indices :ushort 5) (+ 2 index))

       (incf-pointer indices (* 2 6)))

    (list verts-final verts-size
          indices-final index-size)))

;;------------------------------------------------------------

(defun sphere-foreign (&key (radius 0.5) (lines-of-latitude 30)
                         (lines-of-longitude 30) (normals t) (tex-coords t))
  (declare (type (unsigned-byte 8) lines-of-longitude lines-of-latitude))
  ;; latitude  -  horizontal
  ;; longitude -  vertical
  (let* ((elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts-len (* elem-size
                       (1+ lines-of-latitude)
                       (1+ lines-of-longitude)))
         (index-len (* 6 lines-of-latitude (1+ lines-of-longitude)))
         ;;
         (verts (foreign-alloc :float :count verts-len))
         (verts-final (make-pointer (pointer-address verts)))
         ;;
         (indices (foreign-alloc :ushort :count index-len))
         (indices-final (make-pointer (pointer-address indices)))
         ;;
         (lat-angle (/ +pi+ lines-of-latitude)) ;; ring
         (lon-angle (/ (* 2.0 +pi+) lines-of-longitude)) ;; seg
         (f-index 0)
         (v-index 0)
         (lines-of-long (float lines-of-longitude 0f0)) ;; seg
         (lines-of-lat (float lines-of-latitude 0f0))) ;; ring
    (loop :for lat :upto lines-of-latitude :do ;; ring
       (let* ((part (* lat lat-angle))
              (carry (* radius (sin part)))
              (y (* radius (cos part))))
         (loop :for lon :upto lines-of-longitude :do ;; seg
            (let* ((part (* lon lon-angle))
                   (x (* carry (sin part)))
                   (z (* carry (cos part)))
                   (pos (v! x y z))
                   (normal (v3:normalize pos)))
              (when (/= (floor lat) lines-of-latitude) ;; ring
                (setf (mem-aref indices :ushort f-index) (+ v-index lines-of-longitude 1)
                      (mem-aref indices :ushort (+ f-index 1))  v-index
                      (mem-aref indices :ushort (+ f-index 2)) (+ v-index lines-of-longitude)
                      (mem-aref indices :ushort (+ f-index 3)) (+ v-index lines-of-longitude 1)
                      (mem-aref indices :ushort (+ f-index 4)) (+ v-index 1)
                      (mem-aref indices :ushort (+ f-index 5)) v-index
                      f-index (+ 6 f-index)
                      v-index (1+ v-index)))
              (write-elem
               verts
               (x y z)
               (normals (x normal) (y normal) (z normal))
               (tex-coords (/ lon lines-of-long)
                           (/ lat lines-of-lat)))))))
    (list verts-final verts-len
          indices-final index-len)))
