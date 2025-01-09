(in-package :voxview/library)

;; Functional pointers to an element in a list
;; https://wiki.haskell.org/index.php?title=Zipper

(sera:defconstructor list-zipper
  (rest list)
  (seen list))

(deftype stepper () '(sera:-> (list-zipper) (values list-zipper &optional)))

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
