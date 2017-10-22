(in-package :dendrite.primitives)

(defun ptr-to-lists (vptr iptr normals tex-coords
                     vert-len idx-len uintp)
  (let ((idx 0))
    (labels ((get-v3 ()
               (prog1 (v! (mem-aref vptr :float (+ idx 0))
                          (mem-aref vptr :float (+ idx 1))
                          (mem-aref vptr :float (+ idx 2)))
                 (incf idx 3)))
             (get-v2 ()
               (prog1 (v! (mem-aref vptr :float (+ idx 0))
                          (mem-aref vptr :float (+ idx 1)))
                 (incf idx 2))))
      (list (cond
              ((and normals tex-coords)
               (loop :for y :below vert-len :collect
                  (list (get-v3) (get-v3) (get-v2))))
              (normals
               (loop :for y :below vert-len :collect
                  (list (get-v3) (get-v3))))
              (tex-coords
               (loop :for y :below vert-len :collect
                  (list (get-v3) (get-v2))))
              (t
               (loop :for y :below vert-len :collect
                  (get-v3))))
            (if uintp
                (loop :for i :below idx-len :collect
                   (mem-aref iptr :uint i))
                (loop :for i :below idx-len :collect
                   (mem-aref iptr :ushort i)))))))

(defun lattice-data (&key (width 1.0) (height 1.0) (x-segments 30)
                      (y-segments 30) (normals t) (tex-coords t))
  ;;
  (destructuring-bind (vptr vlen iptr ilen)
      (lattice-foreign :width width :height height
                      :x-segments x-segments :y-segments y-segments
                      :normals normals :tex-coords tex-coords)
    (declare (ignore vlen))
    (let ((vert-len (* (1+ y-segments) (1+ x-segments))))
      (ptr-to-lists vptr iptr normals tex-coords vert-len ilen t))))

(defun plain-data (&key (width 1.0) (height 1.0) (normals t) (tex-coords t))
  (lattice-data :width width :height height
               :x-segments 1 :y-segments 1
               :normals normals :tex-coords tex-coords))

(defun cylinder-data (&key (segments 10) (height 1) (radius 0.5)
                        (normals t) (tex-coords t) (cap t))
  (destructuring-bind (vptr vlen iptr ilen)
      (cylinder-foreign :segments segments :height height :radius radius
                        :normals normals :tex-coords tex-coords :cap cap)
    (let* ((elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
           (vert-len (/ vlen elem-size)))
      (ptr-to-lists vptr iptr normals tex-coords vert-len ilen nil))))

(defun cone-data (&key (segments 10) (height 1) (radius 0.5)
                        (normals t) (tex-coords t) (cap t))
  (destructuring-bind (vptr vlen iptr ilen)
      (cone-foreign :segments segments :height height :radius radius
                    :normals normals :tex-coords tex-coords :cap cap)
    (let* ((elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
           (vert-len (/ vlen elem-size)))
      (ptr-to-lists vptr iptr normals tex-coords vert-len ilen nil))))


(defun sphere-data (&key (radius 0.5) (lines-of-latitude 30)
                      (lines-of-longitude 30) (normals t) (tex-coords t))
  (destructuring-bind (vptr vlen iptr ilen)
      (sphere-foreign :radius radius
                      :lines-of-latitude lines-of-latitude
                      :lines-of-longitude lines-of-longitude
                      :normals normals :tex-coords tex-coords)
    (let* ((elem-size (+ 3 (if normals 3 0) (if tex-coords 2 0)))
           (vert-len (/ vlen elem-size)))
      (ptr-to-lists vptr iptr normals tex-coords vert-len ilen nil))))


(defun cap-data (&key (segments 10) (y-pos 0) (up-norm nil) (radius 0.5)
                   (normals t) (tex-coords t) (index-offset 0))
  (let ((angle (/ (* +pi+ 2) segments)))
    (list
     (cons
      (let ((p (v! 0 y-pos 0)))
        (if (not (or normals tex-coords))
            p `(,p ,@(when normals (list (v! 0 (if up-norm 1 -1) 0)))
                   ,@(when tex-coords (list (v! 0.5 0.5))))))
      (loop :for s :upto segments
         :for ang = (* s angle) :collect
         (let ((p (v! (* radius (cos ang)) y-pos (* radius (sin ang)))))
           (if (not (or normals tex-coords))
               p `(,p ,@(when normals (list (v! 0 (if up-norm 1 -1) 0)))
                      ,@(when tex-coords (list (v! (sin ang) (cos ang)))))))))
     (loop :for s :from (+ 1 index-offset)
        :below (+ 1 segments index-offset)
        :append (if up-norm
                    (list index-offset s (1+ s))
                    (list index-offset (1+ s) s))))))

(defun cube-data (&key (size 1.0) (normals t) (tex-coords t))
  (box-data :width size :height size :depth size :normals normals
            :tex-coords tex-coords))

(defun box-data (&key (width 1.0) (height 1.0) (depth 1.0)
                   (normals t) (tex-coords t))
  (let ((width (/ width 2.0))
        (height (/ height 2.0))
        (depth (/ depth 2.0)))
    (list (if (not (or normals tex-coords))
              (list (v! (- width) (- height) depth)
                    (v! width (- height) depth)
                    (v! width height depth)
                    (v! (- width) height depth)
                    (v! width (- height) (- depth))
                    (v! (- width) (- height) (- depth))
                    (v! (- width) height (- depth))
                    (v! width height (- depth))
                    (v! (- width) (- height) (- depth))
                    (v! (- width) (- height) depth)
                    (v! (- width) height depth)
                    (v! (- width) height (- depth))
                    (v! width (- height) depth)
                    (v! width (- height) (- depth))
                    (v! width height (- depth))
                    (v! width height depth)
                    (v! (- width) height depth)
                    (v! width height depth)
                    (v! width height (- depth))
                    (v! (- width) height (- depth))
                    (v! (- width) (- height) (- depth))
                    (v! width (- height) (- depth))
                    (v! width (- height) depth)
                    (v! (- width) (- height) depth))
              (list `(,(v! (- width) (- height) depth)
                       ,@(when normals `(,(v! 0.0 0.0 1.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! width (- height) depth)
                       ,@(when normals `(,(v! 0.0 0.0 1.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! width height depth)
                       ,@(when normals `(,(v! 0.0 0.0 1.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! (- width) height depth)
                       ,@(when normals `(,(v! 0.0 0.0 1.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))
                    `(,(v! width (- height) (- depth))
                       ,@(when normals `(,(v! 0.0 0.0 -1.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! (- width) (- height) (- depth))
                       ,@(when normals `(,(v! 0.0 0.0 -1.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! (- width) height (- depth))
                       ,@(when normals `(,(v! 0.0 0.0 -1.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! width height (- depth))
                       ,@(when normals `(,(v! 0.0 0.0 -1.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))
                    `(,(v! (- width) (- height) (- depth))
                       ,@(when normals `(,(v! -1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! (- width) (- height) depth)
                       ,@(when normals `(,(v! -1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! (- width) height depth)
                       ,@(when normals `(,(v! -1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! (- width) height (- depth))
                       ,@(when normals `(,(v! -1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))
                    `(,(v! width (- height) depth)
                       ,@(when normals `(,(v! 1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! width (- height) (- depth))
                       ,@(when normals `(,(v! 1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! width height (- depth))
                       ,@(when normals `(,(v! 1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! width height depth)
                       ,@(when normals `(,(v! 1.0 0.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))
                    `(,(v! (- width) height depth)
                       ,@(when normals `(,(v! 0.0 1.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! width height depth)
                       ,@(when normals `(,(v! 0.0 1.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! width height (- depth))
                       ,@(when normals `(,(v! 0.0 1.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! (- width) height (- depth))
                       ,@(when normals `(,(v! 0.0 1.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))
                    `(,(v! (- width) (- height) (- depth))
                       ,@(when normals `(,(v! 0.0 -1.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 1.0))))
                    `(,(v! width (- height) (- depth))
                       ,@(when normals `(,(v! 0.0 -1.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 1.0))))
                    `(,(v! width (- height) depth)
                       ,@(when normals `(,(v! 0.0 -1.0 0.0)))
                       ,@(when tex-coords `(,(v! 1.0 0.0))))
                    `(,(v! (- width) (- height) depth)
                       ,@(when normals `(,(v! 0.0 -1.0 0.0)))
                       ,@(when tex-coords `(,(v! 0.0 0.0))))))
          (list 0 1 2 0 2 3 4 5 6 4 6 7 8 9 10 8 10 11 12 13 14 12 14 15 16 17
                18 16 18 19 20 21 22 20 22 23))))

(defun swap-winding-order (data)
  (%swap-winding-order data nil nil))

(defun %swap-winding-order (data accum sub)
  (if data
      (let* ((sub (cons (first data) sub))
             (sub-len (length sub))
             (add (= sub-len 3)))
        (%swap-winding-order
         (rest data)
         (if add
             (append (list (first sub) (third sub) (second sub)) accum)
             accum)
         (unless add sub)))
      (reverse accum)))
