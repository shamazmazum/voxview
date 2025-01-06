(in-package :voxview)

(deftype data-loader ()
  '(sera:-> (pathname)
    (values (simple-array bit (* * *)) &optional)))

(sera:defconstructor loader
  (type        string)
  (description string)
  (loader      data-loader))

(define-condition loader-error (error)
  ()
  (:documentation "Generic loader error. Not to be instantiated."))

(define-condition unknown-format (loader-error)
  ((pathname :initarg :pathname
             :type    pathname
             :reader  unknown-format-pathname))
  (:report (lambda (c s)
             (format s "Cannot load ~a: unknown format"
                     (unknown-format-pathname c)))))

(define-condition content-error (loader-error)
  ((dimensions :initarg :dimensions
               :type    list
               :reader  content-error-dimensions)
   (type       :initarg :type
               :type    (or symbol list)
               :reader  content-error-type))
  (:report (lambda (c s)
             (format
              s #.(concatenate 'string
                               "Wrong array with dimensions ~a and element type ~a."
                               '(#\NewLine)
                               "Must be three-dimensional array of bits")
              (content-error-dimensions c)
              (content-error-type       c)))))

(declaim (ftype data-loader load-npy-model))
(defun load-npy-model (pathname)
  (let ((model (numpy-npy:load-array pathname)))
    (unless (and (eq (array-element-type model) 'bit)
                 (=  (array-rank         model) 3))
      (error 'content-error
             :dimensions (array-dimensions model)
             :type (array-element-type model)))
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

(sera:-> data-files ((or string pathname))
         (values list &optional))
(defun data-files (directory)
  "Return a list of data files in the directory"
  (remove-if-not
   (lambda (pathname)
     (member (pathname-type pathname) *loaders* :key #'loader-type :test #'string=))
   (cl-fad:list-directory
    (uiop:ensure-directory-pathname directory))))
