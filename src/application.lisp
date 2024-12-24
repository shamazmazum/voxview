(in-package :voxview)

(defparameter *tmp-model*
  (make-array '(2 3)
              :element-type 'single-float
              :initial-contents
              '((0.0 0.0 0.0) (1.0 1.0 0.0))))

(gtk4:define-application (:name voxview :id "org.fatimp.voxview")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Voxview")
    (let ((area (make-drawing-area
                 *tmp-model*
                 (make-scene)
                 (rtg-math.vector3:make 128.0 128.0 128.0))))
      (setf (gtk4:window-child window) area))
    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
