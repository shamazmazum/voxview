(defsystem :voxview
    :name :voxview
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool to view voxel geometry"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "utilities")
                 (:file "loader")
                 (:file "model")
                 (:file "voxel")
                 (:file "shaders")
                 (:file "render")
                 (:file "application"))
    :depends-on (:cl-gtk4
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :alexandria
                 :serapeum
                 :numpy-npy

                 :cl-value-noise
                 :stateless-iterators
                 :array-operations)
    :build-operation program-op
    :build-pathname "voxview"
    :entry-point "voxview:voxview")



#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression -1))
