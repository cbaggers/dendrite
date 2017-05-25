(in-package :dendrite.primitives)

;; {TODO} If only position, dont put in list, just return list of :vec3

(defun latice-data (&key (width 1.0) (height 1.0) (x-segments 30)
                      (y-segments 30) (normals t) (tex-coords t))
  (let* ((x-step (/ width x-segments))
         (y-step (- (/ height y-segments)))
         (origin (v! (- (/ width 2)) (/ height 2) 0)))
    (list
     (loop :for y :upto x-segments :append
        (loop :for x :upto y-segments :collect
           (let ((p (v:+ origin (v! (* x x-step) (* y y-step) 0))))
             (if (not (or normals tex-coords))
                 p
                 `(,p
                   ,@(when normals (list (v! 0 1 0)))
                   ,@(when tex-coords
                           (list (v! (/ x x-segments)
                                     (/ y y-segments)))))))))
     (let ((index 0))
       (loop :for y :below x-segments :append
          (loop :for x :below y-segments :append
             (list index
                   (+ index y-segments 1)
                   (+ index y-segments 1 1)
                   index
                   (+ index y-segments 1 1)
                   (+ index 1))
             :do (incf index)))))))

(defun cylinder-data (&key (segments 10) (height 1) (radius 0.5)
                        (normals t) (tex-coords t) (cap t))
  (let* ((angle (/ (* pi 2) segments))
        (cap-data1 (when cap (cap-data :segments segments
                                       :y-pos 0
                                       :up-norm t
                                       :radius radius
                                       :normals normals
                                       :tex-coords tex-coords
                                       :index-offset (* 2 (1+ segments)))))
        (cap-data2 (when cap (cap-data :segments segments
                                       :y-pos height
                                       :up-norm nil
                                       :radius radius
                                       :normals normals
                                       :tex-coords tex-coords
                                       :index-offset (+ (* 2 (1+ segments))
                                                        (length (first cap-data1)))))))
    (list
     (append
      (loop :for s :upto segments
         :for ang = (* s angle)
         :for normal = (v:normalize (v! (cos ang) 0 (sin ang)))
         :for p1 = (v! (* radius (cos ang)) 0 (* radius (sin ang)))
         :for p2 = (v! (* radius (cos ang)) height (* radius (sin ang)))
         :collect
         (if (not (or normals tex-coords))
             p1 `(,p1 ,@(when normals (list normal))
                      ,@(when tex-coords (list (v! 0.5 0.5)))))
         :collect
         (if (not (or normals tex-coords))
             p2 `(,p2 ,@(when normals (list normal))
                      ,@(when tex-coords (list (v! (sin ang) (cos ang)))))))
      (first cap-data1) (first cap-data2))
     (append
      (loop :for s :below segments :for index = (* 2 s) :append
         (list index (+ index 1) (+ index 3)
               index (+ index 3) (+ index 2)))
       (second cap-data1) (second cap-data2)))))


(defun cap-data (&key (segments 10) (y-pos 0) (up-norm nil) (radius 0.5)
                   (normals t) (tex-coords t) (index-offset 0))
  (let ((angle (/ (* pi 2) segments)))
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


(defun cone-data (&key (segments 10) (height 1) (radius 0.5)
                    (normals t) (tex-coords t) (cap t))
  (let ((angle (/ (* pi 2) segments))
        (cap-data (when cap (cap-data :segments segments
                                      :y-pos 0
                                      :up-norm t
                                      :radius radius
                                      :normals normals
                                      :tex-coords tex-coords
                                      :index-offset (1+ (* 2 segments))))))
    (list
     (append
      (loop :for s :upto segments
         :for ang = (* (- s) angle)
         :for normal = (v:normalize (v! (* height (cos ang))
                                        radius
                                        (* height (sin ang))))
         :collect
         (let ((p (v! 0 height 0)))
           (if (not (or normals tex-coords))
               p `(,p ,@(when normals (list normal))
                      ,@(when tex-coords (list (v! 0.5 0.5))))))
         :collect
         (let ((p (v! (* radius (cos ang)) 0 (* radius (sin ang)))))
           (if (not (or normals tex-coords))
               p `(,p ,@(when normals (list normal))
                      ,@(when tex-coords (list (v! (sin ang) (cos ang))))))))
      (first cap-data))
     (append
      (loop :for s :below segments
         :for index = (* 2 s) :append
         (list index (+ 1 index) (+ 3 index)))
      (second cap-data)))))

(defun equilateral-triangle-data (&key (size 1.0) (normals t) (tex-coords t))
  (let* ((hs (/ size 2))
         (height (sqrt (+ (expt size 2) (expt hs 2))))
         (hh (/ height 2))
         (p1 (v! 0 hh 0.0))
         (p2 (v! (- hs) (- hh) 0.0))
         (p3 (v! hs (- hh) 0.0)))
    (list
     (if (not (or normals tex-coords))
         (list p1 p2 p3)
         (list `(,p1
                 ,@(when normals `(,(v! 0.0 0.0 1.0)))
                 ,@(when tex-coords `(,(v! 0.5 0.75))))
               `(,p2
                 ,@(when normals `(,(v! 0.0 0.0 1.0)))
                 ,@(when tex-coords `(,(v! -0.5 0))))
               `(,p3
                 ,@(when normals `(,(v! 0.0 0.0 1.0)))
                 ,@(when tex-coords `(,(v! 0.5 0))))
               ))
     '(0 1 2))))

(defun plain-data (&key (width 1.0) (height 1.0) (normals t) (tex-coords t))
  (latice-data :width width :height height
               :x-segments 1 :y-segments 1
               :normals normals :tex-coords tex-coords))

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

(defun sphere-data (&key (radius 0.5) (lines-of-latitude 30)
                      (lines-of-longitude 30) (normals t) (tex-coords t))
  (declare (type (unsigned-byte 8) lines-of-longitude lines-of-latitude))
  ;; latitude  -  horizontal
  ;; longitude -  vertical
  (let ((faces (make-array (* 6 lines-of-latitude (* (1+ lines-of-longitude)))))
        (lat-angle (/ +pi+ lines-of-latitude))
        (lon-angle (/ (* 2.0 +pi+) lines-of-longitude))
        (f-index 0) (v-index 0))
    (list (loop :for lat :upto lines-of-latitude :append
             (let* ((part (* lat lat-angle))
                    (carry (* radius (sin part)))
                    (y (* radius (cos part))))
               (loop :for lon :upto (1- lines-of-longitude) :collect
                  (let* ((part (* lon lon-angle))
                         (x (* carry (sin part)))
                         (z (* carry (cos part)))
                         (pos (v! x y z)))
                    (when (not (eql lat lines-of-latitude))
                      (let ((part (+ v-index lines-of-longitude)))
                        (setf (aref faces f-index) (1+ part)
                              (aref faces (+ f-index 1))  v-index
                              (aref faces (+ f-index 2)) part
                              (aref faces (+ f-index 3)) (1+ part)
                              (aref faces (+ f-index 4)) (1+ v-index)
                              (aref faces (+ f-index 5)) v-index
                              f-index (+ 6 f-index)
                              v-index (1+ v-index))))
                    (if (not (or normals tex-coords))
                        pos
                        `(,pos
                          ,@(when normals `(,(v3:normalize pos)))
                          ,@(when tex-coords
                                  `(,(v! (/ lon lines-of-longitude)
                                         (/ lat lines-of-latitude))))))))))
          (coerce faces 'list))))

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
