(in-package :voxview/library)

(sera:defconstructor connectivity-data
  (coord rtg-math.types:uvec3)
  (mask  (unsigned-byte 8)))

(sera:-> safe-aref ((simple-array bit (* * *))
                    fixnum fixnum fixnum)
         (values bit &optional))
(declaim (inline safe-aref))
(defun safe-aref (array i j k)
  (if (and (<= 0 i (1- (array-dimension array 0)))
           (<= 0 j (1- (array-dimension array 1)))
           (<= 0 k (1- (array-dimension array 2))))
      (aref array i j k) 0))

(sera:-> connectivity ((simple-array bit (* * *))
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum)
         (values (or null (unsigned-byte 8)) &optional))
(declaim (inline connectivity))
(defun connectivity (array i j k)
  (if (not (zerop (aref array i j k)))
      (logior (if (zerop (safe-aref array (1- i) j k)) (ash 1 0) 0)
              (if (zerop (safe-aref array (1+ i) j k)) (ash 1 1) 0)
              (if (zerop (safe-aref array i (1- j) k)) (ash 1 2) 0)
              (if (zerop (safe-aref array i (1+ j) k)) (ash 1 3) 0)
              (if (zerop (safe-aref array i j (1- k))) (ash 1 4) 0)
              (if (zerop (safe-aref array i j (1+ k))) (ash 1 5) 0))))

(sera:-> compute-connectivity ((simple-array bit (* * *)))
         (values list &optional))
(defun compute-connectivity (array)
  (declare (optimize (speed 3)))
  (let (list)
    (do-indices (array i j k)
      (let ((connectivity (connectivity array i j k)))
        (when (and connectivity (not (zerop connectivity)))
          (push (connectivity-data
                 (rtg-math.base-vectors:v!uint i j k)
                 connectivity)
                list))))
    list))

#|
(sera:-> load-connectivity ((or string pathname))
         (values list rtg-math.types:uvec3 &optional))
(defun load-connectivity (filename)
  (let ((data (load-data filename)))
    (values
     (compute-connectivity data)
     (apply #'rtg-math.base-vectors:v!uint (array-dimensions data)))))
|#
