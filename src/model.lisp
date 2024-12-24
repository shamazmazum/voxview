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
       (/=
        (loop for %i from -1 to 1 sum
              (loop for %j from -1 to 1 sum
                    (loop for %k from -1 to 1 sum
                          (safe-aref array (+ i %i) (+ j %j) (+ k %k)))))
        27)))

(sera:-> compute-boundary ((simple-array bit (* * *)))
         (values (model *) &optional))
(defun compute-boundary (array)
  (declare (optimize (speed 3)))
  (let (list)
    (loop for i fixnum below (array-dimension array 0) do
          (loop for j fixnum below (array-dimension array 1) do
                (loop for k fixnum below (array-dimension array 2) do
                      (when (boundaryp array i j k)
                        (push (list (float i) (float j) (float k)) list)))))
    (make-array (list (length list) 3)
                :element-type 'single-float
                :initial-contents list)))

(sera:-> load-model ((or string pathname))
         (values (model *) &optional))
(defun load-model (filename)
  (compute-boundary
   (load-data filename)))
