(in-package :dendrite.primitives)

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
         (verts (foreign-alloc
                 :float :count (* vert-width vert-height elem-size)))
         (verts-final (make-pointer (pointer-address verts)))
         (indices (foreign-alloc :uint :count (* x-segments y-segments 6)))
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
    (list verts-final indices-final)))

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
         (verts (foreign-alloc :float :count (* 24 elem-size)))
         (verts-final (make-pointer (pointer-address verts)))
         (indices (foreign-alloc :ushort :count 36)))
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
    (list verts-final indices)))

(defun %cap-index-len (segments)
  (* 3 (+ 1 segments)))

(defun %write-cap-indices (ptr segments norm-y index-offset)
  (let ((up-norm (> norm-y 0f0)))
    (if up-norm
        (loop :for s :from (+ 1 index-offset)
           :below (+ 1 segments index-offset)
           :do
           (setf (mem-aref ptr :ushort 0) index-offset)
           (setf (mem-aref ptr :ushort 1) s)
           (setf (mem-aref ptr :ushort 2) (1+ s)))
        (loop :for s :from (+ 1 index-offset)
           :below (+ 1 segments index-offset)
           :do
           (setf (mem-aref ptr :ushort 0) index-offset)
           (setf (mem-aref ptr :ushort 1) (1+ s))
           (setf (mem-aref ptr :ushort 2) s)))))

(defun %cap-vert-len (segments)
  (+ 1 segments))

(defun %write-cap-verts (ptr segments y-pos norm-y radius normals tex-coords)
  (let ((angle (/ (* +pi+ 2f0) segments)))

    (setf (mem-aref ptr :float 0) 0f0)
    (setf (mem-aref ptr :float 1) y-pos)
    (setf (mem-aref ptr :float 2) 0f0)
    (incf-pointer ptr (* 3 4))

    (when normals
      (setf (mem-aref ptr :float 0) 0f0)
      (setf (mem-aref ptr :float 1) norm-y)
      (setf (mem-aref ptr :float 2) 0f0)
      (incf-pointer ptr (* 3 4)))

    (when tex-coords
      (setf (mem-aref ptr :float 0) 0.5)
      (setf (mem-aref ptr :float 1) 0.5)
      (incf-pointer ptr (* 2 4)))

    (loop :for s :upto segments :for ang = (* s angle) :do
       (setf (mem-aref ptr :float 0) (* radius (cos ang)))
       (setf (mem-aref ptr :float 1) y-pos)
       (setf (mem-aref ptr :float 2) (* radius (sin ang)))
       (incf-pointer ptr (* 3 4))

       (when normals
         (setf (mem-aref ptr :float 0) 0f0)
         (setf (mem-aref ptr :float 1) norm-y)
         (setf (mem-aref ptr :float 2) 0f0)
         (incf-pointer ptr (* 3 4)))

       (when tex-coords
         (setf (mem-aref ptr :float 0) (sin ang))
         (setf (mem-aref ptr :float 1) (cos ang))
         (incf-pointer ptr (* 2 4))))
    (values)))

(defun cone-data (&key (segments 10) (height 1) (radius 0.5)
                    (normals t) (tex-coords t) (cap t))
  (let* ((angle (/ (* pi 2) segments))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts (foreign-alloc
                 :float :count (+ (* (1+ segments) elem-size)
                                  (if cap
                                      (* elem-size (%cap-vert-len segments))
                                      0))))
         (verts-final (make-pointer (pointer-address verts)))
         (indices (foreign-alloc
                   :ushort :count (+ (* segments 3)
                                     (if cap
                                         (* (%cap-index-len segments) 6)
                                         0))))
         (indices-final (make-pointer (pointer-address indices))))

    (when cap
      (%write-cap-verts verts segments 0 1f0 radius normals tex-coords)
      (%write-cap-indices indices segments 1f0 (1+ (* 2 segments))))

    (loop :for s :upto segments
       :for ang = (* (- s) angle)
       :for normal = (v:normalize
                      (v! (* height (cos ang))
                          radius
                          (* height (sin ang))))

       :do
       (setf (mem-aref verts :float 0) 0f0)
       (setf (mem-aref verts :float 1) height)
       (setf (mem-aref verts :float 2) 0f0)
       (incf-pointer verts (* 3 4))
       (when normals
         (setf (mem-aref verts :float 0) (x normal))
         (setf (mem-aref verts :float 1) (y normal))
         (setf (mem-aref verts :float 2) (z normal))
         (incf-pointer verts (* 3 4)))
       (when tex-coords
         (setf (mem-aref verts :float 0) 0.5)
         (setf (mem-aref verts :float 1) 0.5)
         (incf-pointer verts (* 2 4)))

       ;;
       (setf (mem-aref verts :float 0) (* radius (cos ang)))
       (setf (mem-aref verts :float 1) 0f0)
       (setf (mem-aref verts :float 2) (* radius (sin ang)))
       (incf-pointer verts (* 3 4))
       (when normals
         (setf (mem-aref verts :float 0) (x normal))
         (setf (mem-aref verts :float 1) (y normal))
         (setf (mem-aref verts :float 2) (z normal))
         (incf-pointer verts (* 3 4)))
       (when tex-coords
         (setf (mem-aref verts :float 0) (sin ang))
         (setf (mem-aref verts :float 1) (cos ang))
         (incf-pointer verts (* 2 4))))

    (loop :for s :below segments :for index = (* 2 s) :do
       (setf (mem-aref indices :ushort 0) index)
       (setf (mem-aref indices :ushort 1) (+ 1 index))
       (setf (mem-aref indices :ushort 2) (+ 3 index))
       (incf-pointer indices (* 2 3)))

    (list verts-final indices-final)))
