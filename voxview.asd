(defsystem :voxview
    :name :voxview
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "A tool to view voxel geometry"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :components ((:file "package")
                 (:file "voxel")
                 (:file "shaders")
                 (:file "gl-state")
                 (:file "widget")
                 (:file "loader")
                 (:file "model")
                 (:file "application"))
    :depends-on (:cl-gtk4
                 :varjo
                 :cl-opengl
                 :rtg-math
                 :alexandria
                 :serapeum
                 :numpy-npy))
