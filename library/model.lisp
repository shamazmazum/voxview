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
    (values boolean &optional))
(declaim (inline emit-voxel-faces!))
(defun emit-voxel-faces! (emitter array i j k)
  (let ((value (aref array i j k)))
    (flet ((id (x) x)
           (emit! (c i j k l)
             (if c (progn (funcall emitter i j k l) 1) 0)))
      (declare (inline id emit!))
      (unless (zerop value)
        (let ((emitted
                (logior
                 (emit! (zerop (safe-aref array (1- i) (id j) (id k)))
                        0 1 2 3)    ; z = -1
                 (emit! (zerop (safe-aref array (1+ i) (id j) (id k)))
                        4 6 5 7)    ; z = +1
                 (emit! (zerop (safe-aref array (id i) (1- j) (id k)))
                        5 1 4 0)    ; y = -1
                 (emit! (zerop (safe-aref array (id i) (1+ j) (id k)))
                        7 6 3 2)    ; y = +1
                 (emit! (zerop (safe-aref array (id i) (id j) (1- k)))
                        4 0 6 2)    ; x = -1
                 (emit! (zerop (safe-aref array (id i) (id j) (1+ k)))
                        5 7 1 3)))) ; x = +1
          (not (zerop emitted)))))))

(serapeum:-> emit-voxel-points!
    ((sera:-> (single-float single-float single-float)
         (values &optional))
     alex:array-index alex:array-index
     alex:array-index alex:array-index)
    (values &optional))
(declaim (inline emit-voxel-points!))
(defun emit-voxel-points! (emitter dimension i j k)
  (loop for (z y x) in +vertices+ do
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
             (setf (aref indices (+ nindices 0))
                   (+ npoints i)
                   (aref indices (+ nindices 1))
                   (+ npoints j)
                   (aref indices (+ nindices 2))
                   (+ npoints k)
                   (aref indices (+ nindices 3))
                   (+ npoints k)
                   (aref indices (+ nindices 4))
                   (+ npoints j)
                   (aref indices (+ nindices 5))
                   (+ npoints l))
             (incf nindices 6)
             (values)))
      (declare (inline emit-point emit-label emit-face))
      (do-indices (array i j k)
        ;; Faces should be emmited first
        (let ((emitted (emit-voxel-faces! #'emit-face array i j k))
              (value (aref array i j k)))
          (when emitted
            (when labelsp
              (multiple-value-bind (label foundp)
                  (gethash value label-table lbl)
                (unless foundp
                  (setf (gethash value label-table) label)
                  (incf lbl))
                (loop repeat 8 do
                  (emit-label label))))
            (emit-voxel-points! #'emit-point max-dimension i j k))))
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
