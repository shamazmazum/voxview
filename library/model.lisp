(in-package :voxview/library)

(sera:defconstructor model)

(serapeum:-> compute-model ((or (simple-array (unsigned-byte 32) (* * *))
                                (simple-array bit                (* * *))))
             (values model &optional))
(defun compute-model (array)
  (declare (ignore array))
  (model))
