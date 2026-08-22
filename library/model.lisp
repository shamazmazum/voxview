(in-package :voxview/library)

(deftype array-rank-3 (type)
  `(simple-array ,type 3))

(deftype allowed-array ()
  `(or (array-rank-3 (unsigned-byte  8))
       (array-rank-3 (unsigned-byte 16))
       (array-rank-3 (unsigned-byte 32))
       (array-rank-3 single-float)))

(sera:defconstructor model
  (texture-data (array-rank-3 single-float))
  (min          single-float)
  (max          single-float))

(sera:-> %normalize (allowed-array fixnum)
         (values (array-rank-3 single-float) &optional))
(declaim (inline %normalize))
(defun %normalize (array max)
  (let ((result (make-array (array-dimensions array)
                            :element-type 'single-float))
        (max (float max)))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i)
            (/ (row-major-aref array i) max)))
    result))

(sera:-> normalize (allowed-array)
         (values (array-rank-3 single-float) &optional))
(defun normalize (array)
  (declare (optimize (speed 3)))
  (typecase array
    ((array-rank-3 (unsigned-byte 8))
     (%normalize array (1- (ash 1 8))))
    ((array-rank-3 (unsigned-byte 16))
     (%normalize array (1- (ash 1 16))))
    ((array-rank-3 (unsigned-byte 32))
     (%normalize array (1- (ash 1 32))))
    ((array-rank-3 single-float)
     array)))

(serapeum:-> compute-min-max ((array-rank-3 single-float))
             (values single-float single-float &optional))
(defun compute-min-max (array)
  (declare (optimize (speed 3)))
  (let ((min float-features:single-float-positive-infinity)
        (max float-features:single-float-negative-infinity))
    (loop for i below (array-total-size array)
          for x = (row-major-aref array i) do
            (setq min (min min x)
                  max (max max x)))
    (values min max)))

(serapeum:-> compute-model (allowed-array)
             (values model &optional))
(defun compute-model (array)
  (declare (optimize (speed 3)))
  (let ((normalized (normalize array)))
    (multiple-value-call #'model normalized
      (compute-min-max normalized))))
