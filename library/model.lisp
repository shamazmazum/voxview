(in-package :voxview/library)

(sera:-> safe-aref ((simple-array bit (* * *))
                    fixnum fixnum fixnum)
         (values bit &optional))
(declaim (inline safe-aref))
(defun safe-aref (array i j k)
  #+sbcl
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (if (and (<= 0 i (1- (array-dimension array 0)))
           (<= 0 j (1- (array-dimension array 1)))
           (<= 0 k (1- (array-dimension array 2))))
      (aref array i j k) 0))

(sera:-> compute-mask ((simple-array bit (* * *))
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum)
         (values (or null (unsigned-byte 8)) &optional))
(declaim (inline compute-mask))
(defun compute-mask (array i j k)
  (if (not (zerop (aref array i j k)))
      (logior (if (zerop (safe-aref array (1- i) j k)) (ash 1 0) 0)
              (if (zerop (safe-aref array (1+ i) j k)) (ash 1 1) 0)
              (if (zerop (safe-aref array i (1- j) k)) (ash 1 2) 0)
              (if (zerop (safe-aref array i (1+ j) k)) (ash 1 3) 0)
              (if (zerop (safe-aref array i j (1- k))) (ash 1 4) 0)
              (if (zerop (safe-aref array i j (1+ k))) (ash 1 5) 0))))

(sera:defconstructor connectivity
  (points (simple-array (unsigned-byte 32) (*)))
  (masks  (simple-array (unsigned-byte  8) (*)))
  (dimensions rtg-math.types:uvec3))

(sera:-> compute-connectivity ((simple-array bit (* * *)))
         (values connectivity &optional))
(defun compute-connectivity (array)
  (declare (optimize (speed 3)))
  (let ((n 0) ps cs)
    (declare (type fixnum n))
    (do-indices (array i j k)
      (let ((mask (compute-mask array i j k)))
        (when (and mask (not (zerop mask)))
          (incf n)
          (setq ps (list* i j k ps))
          (push mask cs))))
    (connectivity
     (make-array (* n 3) :element-type '(unsigned-byte 32) :initial-contents ps)
     (make-array (* n 1) :element-type '(unsigned-byte 8)  :initial-contents cs)
     (apply #'rtg-math.base-vectors:v!uint (array-dimensions array)))))
