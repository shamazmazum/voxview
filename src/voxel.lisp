(in-package :voxview)

(deftype model (n) `(simple-array single-float (,n 3)))

(declaim (type (model #.(* 3 12)) *cube-vertices*))
(defparameter *cube-vertices*
  (make-array '(#.(* 3 12) 3)
              :element-type 'single-float
              :initial-contents
              '(( 1f0 -1f0  1f0)
                (-1f0 -1f0 -1f0)
                ( 1f0 -1f0 -1f0)

                (-1f0  1f0 -1f0)
                ( 1f0  1f0  1f0)
                ( 1f0  1f0 -1f0)

                ( 1f0  1f0 -1f0)
                ( 1f0 -1f0  1f0)
                ( 1f0 -1f0 -1f0)

                ( 1f0  1f0  1f0)
                (-1f0 -1f0  1f0)
                ( 1f0 -1f0  1f0)

                (-1f0 -1f0  1f0)
                (-1f0  1f0 -1f0)
                (-1f0 -1f0 -1f0)

                ( 1f0 -1f0 -1f0)
                (-1f0  1f0 -1f0)
                ( 1f0  1f0 -1f0)

                ( 1f0 -1f0  1f0)
                (-1f0 -1f0  1f0)
                (-1f0 -1f0 -1f0)

                (-1f0  1f0 -1f0)
                (-1f0  1f0  1f0)
                ( 1f0  1f0  1f0)

                ( 1f0  1f0 -1f0)
                ( 1f0  1f0  1f0)
                ( 1f0 -1f0  1f0)

                ( 1f0  1f0  1f0)
                (-1f0  1f0  1f0)
                (-1f0 -1f0  1f0)

                (-1f0 -1f0  1f0)
                (-1f0  1f0  1f0)
                (-1f0  1f0 -1f0)

                ( 1f0 -1f0 -1f0)
                (-1f0 -1f0 -1f0)
                (-1f0  1f0 -1f0))))
