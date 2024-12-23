(in-package :voxview)

(declaim (type (simple-array single-float (#.(* 3 3 12))) +cube-vertices+))
(alex:define-constant +cube-vertices+
    (map '(vector single-float)
         #'float
         '( 1 -1  1
           -1 -1 -1
            1 -1 -1

           -1  1 -1
            1  1  1
            1  1 -1

            1  1 -1
            1 -1  1
            1 -1 -1

            1  1  1
           -1 -1  1
            1 -1  1

           -1 -1  1
           -1  1 -1
           -1 -1 -1

            1 -1 -1
           -1  1 -1
            1  1 -1

            1 -1  1
           -1 -1  1
           -1 -1 -1

           -1  1 -1
           -1  1  1
            1  1  1

            1  1 -1
            1  1  1
            1 -1  1

            1  1  1
           -1  1  1
           -1 -1  1

           -1 -1  1
           -1  1  1
           -1  1 -1

            1 -1 -1
           -1 -1 -1
           -1  1 -1))

  :test #'equalp)
