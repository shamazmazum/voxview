(handler-case
    (progn
      (asdf:make :voxview)
      (uiop:quit 0))
  (error () (uiop:quit 1)))
