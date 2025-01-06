(in-package :voxview)

;; Functional pointers to an element in a list
;; https://wiki.haskell.org/index.php?title=Zipper

(sera:defconstructor list-zipper
  (rest list)
  (seen list))

(sera:-> zipper-to-head (list)
         (values list-zipper &optional))
(defun zipper-to-head (list)
  (list-zipper list nil))

(sera:-> current (list-zipper)
         (values t &optional))
(defun current (zipper)
  (car (list-zipper-rest zipper)))

(sera:-> current-or-previous (list-zipper)
         (values t &optional))
(defun current-or-previous (zipper)
  (or (car (list-zipper-rest zipper))
      (car (list-zipper-seen zipper))))

(sera:-> step-forward (list-zipper)
         (values list-zipper &optional))
(defun step-forward (zipper)
  (if (list-zipper-rest zipper)
      (list-zipper (cdr (list-zipper-rest zipper))
                   (cons (car (list-zipper-rest zipper))
                         (list-zipper-seen zipper)))
      zipper))

(sera:-> step-backward (list-zipper)
         (values list-zipper &optional))
(defun step-backward (zipper)
  (if (list-zipper-seen zipper)
      (list-zipper (cons (car (list-zipper-seen zipper))
                         (list-zipper-rest zipper))
                   (cdr (list-zipper-seen zipper)))
      zipper))

(sera:-> goto-element (list-zipper t &key (:test (sera:-> (t t) (values boolean &optional))))
         (values (or list-zipper null) &optional))
(defun goto-element (zipper element &key (test #'equal))
  (cond
    ((null (list-zipper-rest zipper)) nil)
    ((funcall test element (current zipper))
     zipper)
    (t (goto-element (step-forward zipper) element :test test))))

(define-condition file-not-found (loader-error)
  ((pathname :initarg :pathname
             :type    pathname
             :reader  file-not-found-pathname))
  (:report (lambda (c s)
             (format s "Strange enough, but I cannot find file ~a in its directory"
                     (file-not-found-pathname c)))))

(sera:-> zipper-to-model ((or string pathname))
         (values list-zipper &optional))
(defun zipper-to-model (filename)
  "Return a zipper pointing to a file with a specific name in a list
of all models in its directory. Signal FILE-NOT-FOUND is the directory
cannot be listed for some reason."
  (let* ((pathname (pathname filename))
         (directory (make-pathname :device    (pathname-device    pathname)
                                   :directory (pathname-directory pathname)))
         (files (data-files directory))
         (zipper (goto-element (zipper-to-head files)
                               (truename pathname)
                               :test #'equalp)))
    (unless zipper
      (error 'file-not-found :pathname pathname))
    zipper))
