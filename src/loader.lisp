(in-package :voxview)

(defparameter *loaders*
  '(("npy" . #'numpy-npy:load-array)))

(define-condition unknown-format (error)
  ((pathname :initarg :pathname
             :type    pathname
             :reader  unknown-format-pathname))
  (:report (lambda (c s)
             (format s "Cannot load ~a: unknown format"
                     (unknown-format-pathname c)))))

(sera:-> load-data ((or string pathname))
         (values (simple-array bit (* * *)) &optional))
(defun load-data (filename)
  (let* ((pathname (pathname filename))
         (type     (pathname-type pathname))
         (loader (cdr (assoc type *loaders* :test #'string=))))
    (unless loader
      (error 'unknown-format :pathname pathname))
    (let ((model (funcall loader pathname)))
      (unless (and (eq (array-element-type model) 'bit)
                   (=  (array-rank         model) 3))
        (error 'unknown-format :pathname pathname))
      model)))
