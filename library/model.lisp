(in-package :voxview/library)

(alex:define-constant +vertices+
    '((-1 -1 -1)
      (-1 -1 +1)
      (-1 +1 -1)
      (-1 +1 +1)
      (+1 -1 -1)
      (+1 -1 +1)
      (+1 +1 -1)
      (+1 +1 +1))
  :test #'equalp)

(sera:defconstructor model
  (points  (simple-array single-float       (*)))
  (indices (simple-array (unsigned-byte 32) (*)))
  (labels  (simple-array (unsigned-byte 32) (*))))

(sera:-> safe-aref ((or (simple-array (unsigned-byte 32) (* * *))
                        (simple-array bit                (* * *)))
                    fixnum fixnum fixnum)
         (values (unsigned-byte 32) &optional))
(declaim (inline safe-aref))
(defun safe-aref (array i j k)
  #+sbcl
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (if (array-in-bounds-p array i j k)
      (aref array i j k) 0))

(sera:-> emit-voxel-faces!
    ((sera:-> (alex:array-index
               alex:array-index
               alex:array-index
               alex:array-index)
         (values &optional))
     (or (simple-array (unsigned-byte 32) (* * *))
         (simple-array bit                (* * *)))
     alex:non-negative-fixnum
     alex:non-negative-fixnum
     alex:non-negative-fixnum)
    (values (unsigned-byte 8) &optional))
(declaim (inline emit-voxel-faces!))
(defun emit-voxel-faces! (emitter array i j k)
  (let ((value (aref array i j k)))
    (flet ((id (x) x)
           (emit! (i j k l)
             (funcall emitter i j k l)
             (logior (ash 1 i)
                     (ash 1 j)
                     (ash 1 k)
                     (ash 1 l))))
      (declare (inline id emit!))
      (if (zerop value) 0
          (logior
           (if (zerop (safe-aref array (1- i) (id j) (id k)))
               (emit! 0 1 2 3) 0)      ; z = -1
           (if (zerop (safe-aref array (1+ i) (id j) (id k)))
               (emit! 4 6 5 7) 0)      ; z = +1
           (if (zerop (safe-aref array (id i) (1- j) (id k)))
               (emit! 5 1 4 0) 0)      ; y = -1
           (if (zerop (safe-aref array (id i) (1+ j) (id k)))
               (emit! 7 6 3 2) 0)      ; y = +1
           (if (zerop (safe-aref array (id i) (id j) (1- k)))
               (emit! 4 0 6 2) 0)      ; x = -1
           (if (zerop (safe-aref array (id i) (id j) (1+ k)))
               (emit! 5 7 1 3) 0)))))) ; x = +1

(serapeum:-> emit-voxel-points!
    ((sera:-> (single-float single-float single-float)
         (values &optional))
     alex:array-index alex:array-index
     alex:array-index alex:array-index
      (unsigned-byte 8))
    (values &optional))
(declaim (inline emit-voxel-points!))
(defun emit-voxel-points! (emitter dimension i j k mask)
  (loop for (z y x) in +vertices+
        for idx from 0 by 1
        unless (zerop (ldb (byte 1 idx) mask)) do
          (flet ((transform (position vertex)
                   (declare (type (member +1 -1) vertex)
                            (type alex:array-index position))
                   (let ((position (float position))
                         (vertex   (float vertex)))
                     (+ (/ (* position 2) dimension) -1
                        (/ (1+ vertex) dimension)))))
            (funcall emitter
                     (transform i z)
                     (transform j y)
                     (transform k x))))
  (values))

;; M is an mask of removed points
(serapeum:-> corrected-index ((integer 0 7) (unsigned-byte 8))
             (values (integer 0 7) &optional))
(defun corrected-index (i m)
  (declare (optimize (speed 3)))
  (- i (logcount (logand (1- (ash 1 i)) m))))

(sera:-> %compute-model ((or (simple-array (unsigned-byte 32) (* * *))
                             (simple-array bit                (* * *)))
                         boolean)
         (values model &optional))
(declaim (inline %compute-model))
(defun %compute-model (array labelsp)
  (let ((max-dimension (max (array-dimension array 0)
                            (array-dimension array 1)
                            (array-dimension array 2)))
        (points  (make-array 100 :element-type 'single-float       :adjustable t))
        (labels  (make-array 100 :element-type '(unsigned-byte 32) :adjustable t))
        (indices (make-array 100 :element-type '(unsigned-byte 32) :adjustable t))
        (npoints 0) (nindices 0) (nlabels 0) (lbl 0)
        (label-table (make-hash-table)))
    (declare (type fixnum npoints nindices nlabels lbl))
    (flet ((emit-point (z y x)
             (when (< (length points) (* (1+ npoints) 3))
               (adjust-array points (* (length points) 2)))
             (let ((offset (* npoints 3)))
               (setf (aref points (+ offset 0)) z
                     (aref points (+ offset 1)) y
                     (aref points (+ offset 2)) x)
               (incf npoints))
             (values))
           (emit-label (l)
             (when (< (length labels) (1+ nlabels))
               (adjust-array labels (* (length labels) 2)))
             (setf (aref labels nlabels) l)
             (incf nlabels)
             (values))
           (emit-face (i j k l)
             (when (< (length indices) (+ nindices 6))
               (adjust-array indices (* (length indices) 2)))
             (setf (aref indices (+ nindices 0)) i
                   (aref indices (+ nindices 1)) j
                   (aref indices (+ nindices 2)) k
                   (aref indices (+ nindices 3)) k
                   (aref indices (+ nindices 4)) j
                   (aref indices (+ nindices 5)) l)
             (incf nindices 6)
             (values)))
      (declare (inline emit-point emit-label emit-face))
      (do-indices (array i j k)
        ;; Faces should be emmited first to determine number of
        ;; removed vertices.
        (let ((%nindices nindices)
              (mask (emit-voxel-faces! #'emit-face array i j k))
              (value (aref array i j k)))
          (unless (zerop mask)
            (when labelsp
              (multiple-value-bind (label foundp)
                  (gethash value label-table lbl)
                (unless foundp
                  (setf (gethash value label-table) label)
                  (incf lbl))
                (loop repeat (logcount mask) do
                  (emit-label label))))
            ;; We need to reassign indices for the last face with
            ;; respect to removed vertices (not all vertices of a
            ;; voxel may be stored).
            (loop for l from %nindices below nindices
                  for index = (aref indices l) do
                    (setf (aref indices l)
                          (+ npoints (corrected-index index (logxor mask #xff)))))
            (emit-voxel-points! #'emit-point max-dimension i j k mask))))
      (assert (or (eq (array-element-type array) 'bit)
                  (= npoints nlabels)))
      (model (subseq points  0 (* npoints 3))
             (subseq indices 0 nindices)
             (subseq labels  0 nlabels)))))

(sera:-> compute-model-bw ((simple-array bit (* * *)))
         (values model &optional))
(defun compute-model-bw (array)
  (declare (optimize (speed 3)))
  (%compute-model array nil))

(sera:-> compute-model-gray ((simple-array (unsigned-byte 32) (* * *)))
         (values model &optional))
(defun compute-model-gray (array)
  (declare (optimize (speed 3)))
  (%compute-model array t))

(serapeum:-> compute-model ((or (simple-array (unsigned-byte 32) (* * *))
                                (simple-array bit                (* * *))))
             (values model &optional))
(defun compute-model (array)
  (cond
    ((equalp (array-element-type array)
             'bit)
     (compute-model-bw array))
    ((equalp (array-element-type array)
             '(unsigned-byte 32))
     (compute-model-gray array))
    (t (error "Never happens"))))
