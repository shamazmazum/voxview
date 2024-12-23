(in-package :voxview)

(gtk4:define-application (:name voxview :id "org.fatimp.voxview")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Voxview")
    (let ((area (make-drawing-area
                 (make-array 9 :element-type 'single-float :initial-contents
                             '(0.0 0.0 0.0 0.1 0.0 0.0 0.1 0.1 0.1))
                 (make-camera)
                 (rtg-math.vector3:make 0.1 0.1 0.1))))
      (setf (gtk4:window-child window) area))
    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
