(in-package :dendrite.primitives)

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
