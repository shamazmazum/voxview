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
       (or (zerop (aref array i j (1- k)))
           (zerop (aref array i j (1+ k)))

           (zerop (aref array i (1- j) k))
           (zerop (aref array i (1+ j) k))

           (zerop (aref array (1+ i) j k))
           (zerop (aref array (1- i) j k)))))

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
