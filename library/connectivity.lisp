(in-package :voxview/library)

(sera:-> safe-aref ((simple-array (unsigned-byte 32) (* * *))
                    fixnum fixnum fixnum)
         (values (unsigned-byte 32) &optional))
(declaim (inline safe-aref))
(defun safe-aref (array i j k)
  #+sbcl
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (if (array-in-bounds-p array i j k)
      (aref array i j k) 0))

(sera:-> compute-mask ((simple-array (unsigned-byte 32) (* * *))
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum
                       alex:non-negative-fixnum)
         (values (unsigned-byte 32) (or null (unsigned-byte 8)) &optional))
(declaim (inline compute-mask))
(defun compute-mask (array i j k)
  (let ((value (aref array i j k)))
    (values
     value
     (if (not (zerop value))
         (logior (if (zerop (safe-aref array (1- i) j k)) (ash 1 0) 0)
                 (if (zerop (safe-aref array (1+ i) j k)) (ash 1 1) 0)
                 (if (zerop (safe-aref array i (1- j) k)) (ash 1 2) 0)
                 (if (zerop (safe-aref array i (1+ j) k)) (ash 1 3) 0)
                 (if (zerop (safe-aref array i j (1- k))) (ash 1 4) 0)
                 (if (zerop (safe-aref array i j (1+ k))) (ash 1 5) 0))))))

(sera:defconstructor connectivity
  (points (simple-array (unsigned-byte 32) (*)))
  (labelz (simple-array (unsigned-byte 32) (*)))
  (masks  (simple-array (unsigned-byte  8) (*))))

(sera:-> compute-connectivity ((simple-array (unsigned-byte 32) (* * *)))
         (values connectivity &optional))
(defun compute-connectivity (array)
  (declare (optimize (speed 3)))
  (let ((n 0) (lbl 0) ps cs lbls
        (label-table (make-hash-table)))
    (declare (type fixnum lbl n))
    (do-indices (array i j k)
      (multiple-value-bind (value mask)
          (compute-mask array i j k)
        (when (and mask (not (zerop mask)))
          (incf n)
          (setq ps (list* i j k ps))
          (push mask cs)
          (multiple-value-bind (label foundp)
              (gethash value label-table lbl)
            (unless foundp
              (setf (gethash value label-table) label)
              (incf lbl))
            (push label lbls)))))
    (connectivity
     (make-array (* n 3) :element-type '(unsigned-byte 32) :initial-contents ps)
     (make-array (* n 1) :element-type '(unsigned-byte 32) :initial-contents lbls)
     (make-array (* n 1) :element-type '(unsigned-byte 8)  :initial-contents cs))))
