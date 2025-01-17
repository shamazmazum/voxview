(defsystem :voxview/library
    :name :voxview/library
    :version "0.2.2"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "library"
    :components ((:file "package")
                 (:file "do-indices")
                 (:file "model")
                 (:file "list-zipper"))
    :depends-on (:alexandria
                 :serapeum
                 :stateless-iterators)
    :in-order-to ((test-op (load-op "voxview/library/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :voxview/library/tests '#:run-tests)))

(defsystem :voxview/library/tests
    :name :voxview/library/tests
    :version "0.2.2"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "tests"
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:voxview/library
                 :fiveam))

(defsystem :voxview
    :name :voxview
    :version "0.2.2"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool to view voxel geometry"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "utilities")
                 (:file "loader")
                 (:file "voxel")
                 (:file "shaders")
                 (:file "render")
                 (:file "application"))
    :depends-on (:voxview/library
                 :cl-gtk4
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :alexandria
                 :serapeum
                 :numpy-npy
                 :cl-fad
                 :cl-value-noise)
    :build-operation program-op
    :build-pathname "voxview"
    :entry-point "voxview:voxview")


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
