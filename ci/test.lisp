(handler-case
    (progn
      (asdf:load-system :voxview)
      (asdf:load-system :voxview/library/tests)
      (uiop:quit
       (if (uiop:call-function "voxview/library/tests:run-tests")
           0 1)))
  (error () (uiop:quit 1)))
