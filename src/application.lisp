(in-package :voxview)

(defparameter *tmp-model*
  (load-model "~/test/grains/blobs-neuro/concave/blob-321-oc.npy"))

(defun expand-horizontally (widget)
  (setf (gtk4:widget-hexpand-p widget) t
        (gtk4:widget-halign    widget) gtk4:+align-fill+))

(defun scale (min max value step)
  (let ((scale (gtk4:make-scale :min min :max max :step step
                                :orientation gtk4:+orientation-horizontal+)))
    (setf (gtk4:widget-size-request scale) '(200 10)
          (gtk4:widget-margin-start scale) 15
          (gtk4:widget-margin-end   scale) 15
          (gtk4:range-value         scale) (float value 0d0)
          (gtk4:scale-digits scale) 3
          (gtk4:scale-draw-value-p scale) t)
    scale))

(defun append-with-label (toplevel scale label)
  (let ((box (gtk4:make-box :orientation gtk4:+orientation-horizontal+ :spacing 5))
        (label (gtk4:make-label :str label)))
    (gtk4:box-append box scale)
    (gtk4:box-append box label)
    (gtk4:box-append toplevel box)))

(gtk4:define-application (:name voxview :id "org.fatimp.voxview")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Voxview")
    (let* ((scene (make-scene))
           (area (make-drawing-area *tmp-model* scene
                                    (rtg-math.vector3:make 128.0 128.0 128.0)))
           (control-frame (gtk4:make-frame :label "Controls"))
           (camera-frame (gtk4:make-frame :label "Camera"))
           (light-frame  (gtk4:make-frame :label "Light"))
           (toplevel-box (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                        :spacing 0))
           (control-box  (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                        :spacing 10))
           (camera-box   (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                        :spacing 5))
           (light-box    (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                        :spacing 5))

           (camera-ϕ (scale 0d0 (* 2 pi)
                            (scene-camera-ϕ scene)
                            1d-1))
           (camera-ψ (scale (+ (- (/ pi 2)) 0.01)
                            (- (+ (/ pi 2)) 0.01)
                            (scene-camera-ψ scene)
                            1d-2))
           (camera-r (scale 5d-1 2d0 (scene-camera-r scene) 1d-1))

           (light-ϕ (scale 0d0 (* 2 pi)
                           (scene-light-ϕ scene) 1d-1))
           (light-ψ (scale (+ (- (/ pi 2)) 0.01)
                           (- (+ (/ pi 2)) 0.01)
                           (scene-light-ψ scene)
                           1d-2)))

      (setf (gtk4:window-child window) toplevel-box
            (gtk4:frame-child control-frame) control-box
            (gtk4:frame-child camera-frame) camera-box
            (gtk4:frame-child light-frame) light-box)

      (expand-horizontally area)
      (gtk4:box-append toplevel-box area)
      (gtk4:box-append toplevel-box control-frame)
      (gtk4:box-append control-box camera-frame)
      (gtk4:box-append control-box light-frame)

      (append-with-label camera-box camera-ϕ "ϕ")
      (append-with-label camera-box camera-ψ "ψ")
      (append-with-label camera-box camera-r "r")

      (append-with-label light-box light-ϕ "ϕ")
      (append-with-label light-box light-ψ "ψ")

      ;; Connect scale signals
      (macrolet ((%go (scale setter)
                   `(gtk4:connect ,scale "value-changed"
                                  (lambda (widget)
                                    (declare (ignore widget))
                                    (let ((value (float (gtk4:range-value ,scale) 0f0)))
                                      (setf (,setter scene) value))
                                    (gtk4:gl-area-queue-render area)))))
        (%go camera-ϕ scene-camera-ϕ)
        (%go camera-ψ scene-camera-ψ)
        (%go camera-r scene-camera-r)
        (%go light-ϕ  scene-light-ϕ)
        (%go light-ψ  scene-light-ψ)))

    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
