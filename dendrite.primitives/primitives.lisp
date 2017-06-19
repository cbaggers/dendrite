(in-package :dendrite.primitives)

(defmacro set-elem (arr index
                    (p-x p-y p-z)
                    (normal-pred n-x n-y n-z)
                    (tex-pred t-x t-y))
  `(progn
     (setf (aref ,arr (+ ,index 0)) ,p-x)
     (setf (aref ,arr (+ ,index 1)) ,p-y)
     (setf (aref ,arr (+ ,index 2)) ,p-z)
     (incf ,index 3)
     (when ,normal-pred
       (setf (aref ,arr (+ ,index 0)) ,n-x)
       (setf (aref ,arr (+ ,index 1)) ,n-y)
       (setf (aref ,arr (+ ,index 2)) ,n-z)
       (incf ,index 3))
     (when ,tex-pred
       (setf (aref ,arr (+ ,index 0)) ,t-x)
       (setf (aref ,arr (+ ,index 1)) ,t-y)
       (incf ,index 2))))

(defun plain-data (&key (width 1.0) (height 1.0) (normals t) (tex-coords t))
  (latice-data :width width :height height
               :x-segments 1 :y-segments 1
               :normals normals :tex-coords tex-coords))

;; {TODO} If only position, dont put in list, just return list of :vec3

(defun latice-data (&key (width 1.0) (height 1.0) (x-segments 30)
                         (y-segments 30) (normals t) (tex-coords t))
  (let* ((x-step (/ width x-segments))
         (y-step (/ height y-segments))
         (vert-width (1+ x-segments))
         (vert-height (1+ y-segments))
         (origin (v! (- (/ width 2)) (- (/ height 2)) 0))
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts-len (* vert-width vert-height elem-size))
         (verts (make-array verts-len :element-type 'single-float))
         (index-len (* x-segments y-segments 6))
         (indices (make-array index-len :element-type '(unsigned-byte 32)))
         (arr-idx 0))
    ;;
    (loop :for y :below vert-height :do
       (loop :for x :below vert-width :do
          (setf (aref verts (+ arr-idx 0)) (+ (* x x-step) (x origin)))
          (setf (aref verts (+ arr-idx 1)) 0f0)
          (setf (aref verts (+ arr-idx 2)) (+ (* y y-step) (y origin)))
          (incf arr-idx 3)
          (when normals
            (setf (aref verts (+ arr-idx 0)) 0f0)
            (setf (aref verts (+ arr-idx 1)) 1f0)
            (setf (aref verts (+ arr-idx 2)) 0f0)
            (incf arr-idx 3))
          (when tex-coords
            (setf (aref verts (+ arr-idx 0)) (/ (float x) x-segments))
            (setf (aref verts (+ arr-idx 1)) (/ (float y) y-segments))
            (incf arr-idx 2))))
    ;;
    (let ((arr-idx 0)
          (index 0))
      (loop :for y :below y-segments :do
         (loop :for x :below x-segments :do
            (setf (aref indices (+ arr-idx 0)) index)
            (setf (aref indices (+ arr-idx 1)) (+ index vert-width))
            (setf (aref indices (+ arr-idx 2)) (+ index vert-width 1))
            (setf (aref indices (+ arr-idx 3)) index)
            (setf (aref indices (+ arr-idx 4)) (+ index vert-width 1))
            (setf (aref indices (+ arr-idx 5)) (+ index 1))
            (incf arr-idx 6)
            (incf index))
         (incf index)))
    ;;
    (list verts indices)))

(defun cube-data (&key (size 1.0) (normals t) (tex-coords t))
  (box-data :width size :height size :depth size :normals normals
            :tex-coords tex-coords))

(defun box-data (&key (width 1.0) (height 1.0) (depth 1.0)
                      (normals t) (tex-coords t))
  (let* ((width (/ width 2.0))
         (height (/ height 2.0))
         (depth (/ depth 2.0))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         (verts-len (* 24 elem-size))
         (verts (make-array verts-len :element-type 'single-float))
         (indices (make-array
                   36 :element-type '(unsigned-byte 16)
                   :initial-contents '(0 1 2 0 2 3 4 5 6 4 6 7 8 9 10
                                       8 10 11 12 13 14 12 14 15 16 17
                                       18 16 18 19 20 21 22 20 22 23)))
         (vert-idx 0))
    ;;
    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth)
    (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) -1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) -1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) -1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) -1.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) -1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) -1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) -1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) -1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) height)
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) -1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) (- depth)) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) -1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 1.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) width)
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) -1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 1.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))

    (setf (aref verts (+ vert-idx 0)) (- width))
    (setf (aref verts (+ vert-idx 1)) (- height))
    (setf (aref verts (+ vert-idx 2)) depth) (incf vert-idx 3)
    (when normals
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) -1.0)
      (setf (aref verts (+ vert-idx 2)) 0.0)
      (incf vert-idx 3))
    (when tex-coords
      (setf (aref verts (+ vert-idx 0)) 0.0)
      (setf (aref verts (+ vert-idx 1)) 0.0)
      (incf vert-idx 2))
    ;;
    (list verts indices)))

(defun %set-cap-verts (arr idx segments y-pos norm-y radius normals tex-coords)
  (let ((angle (/ (* +pi+ 2f0) segments)))

    (set-elem
     arr idx
     (0f0 y-pos 0f0)
     (normals 0f0 norm-y 0f0)
     (tex-coords 0.5f0 0.5f0))

    (loop :for s :upto segments :for ang = (* s angle) :do
       (set-elem
        arr idx
        ((* radius (cos ang)) y-pos (* radius (sin ang)))
        (normals 0f0 norm-y 0f0)
        (tex-coords (sin ang) (cos ang)))))
  idx)

(defun %set-cap-indices (arr idx segments norm-y index-offset)
  (let ((up-norm (> norm-y 0f0)))
    (if up-norm
        (loop :for s :from index-offset
           :upto (+ index-offset segments)
           :do
           (setf (aref arr (+ idx 0)) index-offset)
           (setf (aref arr (+ idx 1)) (1+ s))
           (setf (aref arr (+ idx 2)) s)
           (incf idx 3))
        (loop :for s :from index-offset
           :upto (+ index-offset segments)
           :do
           (setf (aref arr (+ idx 0)) index-offset)
           (setf (aref arr (+ idx 1)) s)
           (setf (aref arr (+ idx 2)) (1+ s))
           (incf idx 3))))
  idx)

(defun cone-data (&key (segments 10) (height 1) (radius 0.5f0)
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
         (verts (make-array verts-size :element-type 'single-float))
         (indices (make-array index-size :element-type '(unsigned-byte 16)))
         (vert-idx 0)
         (index-idx 0))

    (when cap
      (setf vert-idx (%set-cap-verts verts vert-idx segments 0f0 -1f0 radius
                                     normals tex-coords))
      (setf index-idx (%set-cap-indices indices index-idx segments -1f0 0)))

    (loop :for s :upto segments
       :for ang = (* (- s) angle)
       :for normal = (v:normalize
                      (v! (* height (cos ang))
                          radius
                          (* height (sin ang))))

       :do
       (set-elem
        verts vert-idx
        (0f0 height 0f0)
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 1f0))

       (set-elem
        verts vert-idx
        ((* radius (cos ang)) 0f0 (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 0f0)))

    (let ((cap-len (%cap-vert-len segments)))
      (loop :for s :below segments :for index = (+ cap-len (* 2 s)) :do
         (setf (aref indices (+ index-idx 0)) index)
         (setf (aref indices (+ index-idx 1)) (+ 1 index))
         (setf (aref indices (+ index-idx 2)) (+ 3 index))
         (incf index-idx 3)))

    (list verts indices)))

(defun cylinder-data (&key (segments 30) (height 1) (radius 0.5)
                           (normals t) (tex-coords t) (cap t))
  (let* ((height (float height 0f0))
         (radius (float radius 0f0))
         (angle (/ (* +pi+ 2) segments))
         ;;
         (elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
         ;;
         (verts-size (+ (* (* 2 (1+ segments)) elem-size)
                        (if cap
                            (* 2 (%cap-vert-float-len segments normals tex-coords))
                            0)))
         (index-size (+ (* segments 6)
                        (if cap (* 2 (%cap-index-float-len segments)) 0)))
         ;;
         (verts (make-array verts-size :element-type 'single-float))
         ;;
         (indices (make-array index-size :element-type '(unsigned-byte 16)))
         (vert-idx 0)
         (index-idx 0))

    (when cap
      (setf vert-idx (%set-cap-verts verts vert-idx segments 0f0 -1f0 radius
                                     normals tex-coords))
      (setf vert-idx (%set-cap-verts verts vert-idx segments height 1f0 radius
                                     normals tex-coords))
      (setf index-idx (%set-cap-indices indices index-idx segments -1f0 0))
      (setf index-idx (%set-cap-indices indices index-idx segments 1f0
                                        (%cap-vert-len segments))))

    (loop :for s :upto segments
       :for ang = (* s angle)
       :for normal = (v:normalize (v! (cos ang) 0 (sin ang)))
       :do

       (set-elem
        verts vert-idx
        ((* radius (cos ang)) 0f0 (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 0f0))

       (set-elem
        verts vert-idx
        ((* radius (cos ang)) height (* radius (sin ang)))
        (normals (x normal) (y normal) (z normal))
        (tex-coords (/ ang pi-f) 1f0)))

    (let ((cap-len (* 2 (%cap-vert-len segments))))
      (loop :for s :below segments :for index = (+ cap-len (* 2 s)) :do
         (setf (aref indices (+ index-idx 0)) index)
         (setf (aref indices (+ index-idx 1)) (+ 1 index))
         (setf (aref indices (+ index-idx 2)) (+ 3 index))

         (setf (aref indices (+ index-idx 3)) index)
         (setf (aref indices (+ index-idx 4)) (+ 3 index))
         (setf (aref indices (+ index-idx 5)) (+ 2 index))

         (incf index-idx 6)))

    (list verts indices)))

(defun sphere-data (&key (radius 0.5) (lines-of-latitude 30)
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
         (verts (make-array verts-len :element-type 'single-float))
         (vert-idx 0)
         ;;
         (indices (make-array index-len :element-type '(unsigned-byte 16)))
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
                (setf (aref indices f-index) (+ v-index lines-of-longitude 1)
                      (aref indices (+ f-index 1))  v-index
                      (aref indices (+ f-index 2)) (+ v-index lines-of-longitude)
                      (aref indices (+ f-index 3)) (+ v-index lines-of-longitude 1)
                      (aref indices (+ f-index 4)) (+ v-index 1)
                      (aref indices (+ f-index 5)) v-index
                      f-index (+ 6 f-index)
                      v-index (1+ v-index)))
              (set-elem
               verts vert-idx
               (x y z)
               (normals (x normal) (y normal) (z normal))
               (tex-coords (/ lon lines-of-long)
                           (/ lat lines-of-lat)))))))
    (list verts indices)))

;; (defun swap-winding-order (data)
;;   (%swap-winding-order data nil nil))

;; (defun %swap-winding-order (data accum sub)
;;   (if data
;;       (let* ((sub (cons (first data) sub))
;;              (sub-len (length sub))
;;              (add (= sub-len 3)))
;;         (%swap-winding-order
;;          (rest data)
;;          (if add
;;              (append (list (first sub) (third sub) (second sub)) accum)
;;              accum)
;;          (unless add sub)))
;;       (reverse accum)))
