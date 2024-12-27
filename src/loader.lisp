(in-package :voxview)

(deftype data-loader ()
  '(sera:-> (pathname)
    (values (simple-array bit (* * *)) &optional)))

(sera:defconstructor loader
  (type        string)
  (description string)
  (loader      data-loader))

(define-condition unknown-format (error)
  ((pathname :initarg :pathname
             :type    pathname
             :reader  unknown-format-pathname))
  (:report (lambda (c s)
             (format s "Cannot load ~a: unknown format"
                     (unknown-format-pathname c)))))

(declaim (ftype data-loader load-npy-model))
(defun load-npy-model (pathname)
  (let ((model (numpy-npy:load-array pathname)))
    (unless (and (eq (array-element-type model) 'bit)
                 (=  (array-rank         model) 3))
      (error 'unknown-format :pathname pathname))
    model))

(declaim (type list *loaders*))
(defparameter *loaders*
  (list
   (loader "npy" "Numpy array (.npy)" #'load-npy-model)))


(sera:-> load-data ((or string pathname))
         (values (simple-array bit (* * *)) &optional))
(defun load-data (filename)
  (let* ((pathname (pathname filename))
         (type     (pathname-type pathname))
         (loader (find type *loaders* :test #'string= :key #'loader-type)))
    (declare (type (or loader null) loader))
    (unless loader
      (error 'unknown-format :pathname pathname))
    (funcall (loader-loader loader) pathname)))
