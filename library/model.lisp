(in-package :voxview/library)

(deftype array-rank-3 (type)
  `(simple-array ,type 3))

(deftype allowed-array ()
  `(or (array-rank-3 (unsigned-byte  8))
       (array-rank-3 (unsigned-byte 16))
       (array-rank-3 (unsigned-byte 32))
       (array-rank-3 single-float)))

(sera:defconstructor model
  (texture-data (array-rank-3 single-float)))

(sera:-> normalize (allowed-array fixnum)
         (values (array-rank-3 single-float) &optional))
(declaim (inline normalize))
(defun normalize (array max)
  (let ((result (make-array (array-dimensions array)
                            :element-type 'single-float))
        (max (float max)))
    (loop for i below (array-total-size result) do
      (setf (row-major-aref result i)
            (/ (row-major-aref array i) max)))
    result))

(serapeum:-> compute-model (allowed-array)
             (values model &optional))
(defun compute-model (array)
  (declare (optimize (speed 3)))
  (model
   (typecase array
     ((array-rank-3 (unsigned-byte 8))
      (normalize array (1- (ash 1 8))))
     ((array-rank-3 (unsigned-byte 16))
      (normalize array (1- (ash 1 16))))
     ((array-rank-3 (unsigned-byte 32))
      (normalize array (1- (ash 1 32))))
     ((array-rank-3 single-float)
      array))))
