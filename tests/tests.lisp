(in-package :voxview/library/tests)

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(zippers))))

(def-suite zippers :description "Test zippers")

(in-suite zippers)

(test goto-end
  (loop repeat 1000
        for n = (+ 100 (random 200))
        for list = (loop repeat n collect (random 1000))
        for m = (+ n (random 100))
        for zipper = (si:foldl
                      (lambda (zipper n)
                        (declare (ignore n))
                        (step-forward zipper))
                      (zipper-to-head list)
                      (si:range 0 m))
        do
        (is (eq (current-or-previous zipper) (car (last list))))
        (is (eq (current zipper) nil))))

(test goto-start
  (loop repeat 1000
        for n = (+ 100 (random 200))
        for list = (loop repeat n collect (random 1000))
        for m = (random 100)
        for zipper = (si:foldl
                      (lambda (zipper n)
                        (declare (ignore n))
                        (step-backward zipper))
                      (zipper-to-head list)
                      (si:range 0 m))
        do
        (is (eq (current-or-previous zipper) (car list)))
        (is (eq (current zipper) (car list)))))

(test goto-middle
  (loop repeat 1000
        for n = (+ 100 (random 200))
        for list = (loop repeat n collect (random 1000))
        for m = (random n)
        for zipper = (si:foldl
                      (lambda (zipper n)
                        (declare (ignore n))
                        (step-forward zipper))
                      (zipper-to-head list)
                      (si:range 0 m))
        do
        (is (eq (current-or-previous zipper) (nth m list)))
        (is (eq (current zipper) (nth m list)))))

(test go-back-and-forth
  (loop repeat 1000
        for n = (+ 100 (random 200))
        for list = (loop repeat n collect (random 1000))
        for k = (1+ (random (1- n)))
        for l = (random k)
        for zipper = (si:foldl
                      (lambda (zipper n)
                        (declare (ignore n))
                        (step-backward zipper))
                      (si:foldl
                       (lambda (zipper n)
                         (declare (ignore n))
                         (step-forward zipper))
                       (zipper-to-head list)
                       (si:range 0 k))
                      (si:range 0 l))
        do
        (is (eq (current-or-previous zipper) (nth (- k l) list)))
        (is (eq (current zipper) (nth (- k l) list)))))
        
