(in-package :voxview)

(sera:-> safe-aref ((simple-array bit (* * *))
                    fixnum fixnum fixnum)
         (values bit &optional))
(declaim (inline safe-aref))
(defun safe-aref (array i j k)
  (if (and (<= 0 i (1- (array-dimension array 0)))
           (<= 0 j (1- (array-dimension array 1)))
           (<= 0 k (1- (array-dimension array 2))))
      (aref array i j k) 0))

(sera:-> boundaryp ((simple-array bit (* * *))
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum
                    alex:non-negative-fixnum)
         (values boolean &optional))
(declaim (inline boundaryp))
(defun boundaryp (array i j k)
  (and (not (zerop (aref array i j k)))
       (or (zerop (safe-aref array i j (1- k)))
           (zerop (safe-aref array i j (1+ k)))

           (zerop (safe-aref array i (1- j) k))
           (zerop (safe-aref array i (1+ j) k))

           (zerop (safe-aref array (1+ i) j k))
           (zerop (safe-aref array (1- i) j k)))))

(sera:-> compute-boundary ((simple-array bit (* * *)))
         (values (model *) &optional))
(defun compute-boundary (array)
  (declare (optimize (speed 3)))
  (let (list)
    (do-indices (array i j k)
      (when (boundaryp array i j k)
        (push (list (float i) (float j) (float k)) list)))
    (make-array (list (length list) 3)
                :element-type 'single-float
                :initial-contents list)))

(sera:-> load-model ((or string pathname))
         (values (model *) rtg-math.types:vec3 &optional))
(defun load-model (filename)
  (let ((data (load-data filename)))
    (values
     (compute-boundary data)
     (apply #'rtg-math.vector3:make (mapcar #'float (array-dimensions data))))))


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

(sera:defconstructor connectivity-data
  (coord rtg-math.types:uvec3)
  (mask  (unsigned-byte 8)))

(deftype connectivity-vector () `(simple-array connectivity-data (*)))

(sera:-> compute-connectivity ((simple-array bit (* * *)))
         (values connectivity-vector &optional))
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
    (make-array (length list)
                :element-type 'connectivity-data
                :initial-contents list)))

(sera:-> load-connectivity ((or string pathname))
         (values connectivity-vector rtg-math.types:uvec3 &optional))
(defun load-connectivity (filename)
  (let ((data (load-data filename)))
    (values
     (compute-connectivity data)
     (apply #'rtg-math.base-vectors:v!uint (array-dimensions data)))))
