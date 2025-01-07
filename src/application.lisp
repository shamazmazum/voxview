(in-package :voxview)

(sera:-> navigation-button-handler
         (gir::object-instance model-gpu-uploader getter setter stepper)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun navigation-button-handler (area uploader model-getter model-setter stepper)
  (lambda (widget)
    (declare (ignore widget))
    (let ((pointer (funcall stepper (funcall model-getter))))
      (funcall model-setter pointer)
      (handler-case
          (multiple-value-bind (model nvoxels)
              (load-connectivity (current-or-previous pointer))
            (funcall uploader model nvoxels)
            (gtk4:gl-area-queue-render area))
        (loader-error (c)
          (show-error-dialog c))))))

(defun show-error-dialog (condition)
  (let ((dialog (gtk4:make-dialog)))
    (gtk4:dialog-add-button dialog "Close" gtk4:+response-type-close+)
    (let ((label (gtk4:make-label
                  :str (with-output-to-string (out)
                         (princ condition out)))))
      (gtk4:box-append (gtk4:dialog-content-area dialog) label))
    (gtk4:connect dialog "response"
                  (lambda (widget id)
                    (declare (ignore widget id))
                    (gtk4:window-destroy dialog)))
    (setf (gtk4:window-modal-p dialog) t
          (gtk4:window-resizable-p dialog) nil
          (gtk4:window-title dialog) "Error")
    (gtk4:widget-show dialog)))

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

(defun add-filters-to-file-chooser-dialog (dialog)
  (loop for loader in *loaders*
        for filter = (gtk4:make-file-filter) do
        (gtk4:file-filter-add-suffix filter (loader-type loader))
        (setf (gtk4:file-filter-name filter) (loader-description loader))
        (gtk4:file-chooser-add-filter dialog filter))
  (when (> (length *loaders*) 1)
    (loop with filter = (gtk4:make-file-filter)
          for loader in *loaders* do
          (gtk4:file-filter-add-suffix filter (loader-type loader)) finally
          (setf (gtk4:file-filter-name filter) "All supported formats")
          (gtk4:file-chooser-add-filter dialog filter)))
  (values))

(gtk4:define-application (:name voxview :id "org.fatimp.voxview")
  (gtk4:define-main-window (window (gtk4:make-application-window
                                    :application gtk4:*application*))
    (setf (gtk4:window-title window) "Voxview")
    (let* ((scene (make-scene))
           (light-follows-camera-p t)
           (area-+-uploader (multiple-value-call #'cons (make-drawing-area scene)))
           (area     (car area-+-uploader))
           (uploader (cdr area-+-uploader))
           (control-frame (gtk4:make-frame :label "Controls"))
           (camera-frame  (gtk4:make-frame :label "Camera"))
           (light-frame   (gtk4:make-frame :label "Light"))
           (toplevel-box   (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                          :spacing 0))
           (control-box    (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 10))
           (camera-box     (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 5))
           (light-box      (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 5))
           (navigation-box (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                          :spacing 2))

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
                           1d-2))
           (follow-camera (gtk4:make-check-button :label "Follow camera"))
           (open-model (gtk4:make-button :label "Open model"))
           (next-model (gtk4:make-button :icon-name "go-next"))
           (prev-model (gtk4:make-button :icon-name "go-previous")))

      (setf (gtk4:window-child window) toplevel-box
            (gtk4:frame-child control-frame) control-box
            (gtk4:frame-child camera-frame) camera-box
            (gtk4:frame-child light-frame) light-box
            (gtk4:widget-sensitive-p prev-model) nil
            (gtk4:widget-sensitive-p next-model) nil)

      (expand-horizontally area)
      (gtk4:box-append toplevel-box area)
      (gtk4:box-append toplevel-box control-frame)
      (gtk4:box-append control-box camera-frame)
      (gtk4:box-append control-box light-frame)
      (gtk4:box-append control-box navigation-box)

      (append-with-label camera-box camera-ϕ "ϕ")
      (append-with-label camera-box camera-ψ "ψ")
      (append-with-label camera-box camera-r "r")

      (append-with-label light-box light-ϕ "ϕ")
      (append-with-label light-box light-ψ "ψ")
      (gtk4:box-append   light-box follow-camera)

      ;; Looks ugly
      (gtk4:box-append navigation-box prev-model)
      (gtk4:box-append navigation-box open-model)
      (gtk4:box-append navigation-box next-model)

      ;; Connect follow camera signal
      (gtk4:connect follow-camera "toggled"
                    (lambda (widget)
                      (declare (ignore widget))
                      (let ((state (gtk4:check-button-active-p follow-camera)))
                        (setf (gtk4:widget-sensitive-p light-ϕ) (not state)
                              (gtk4:widget-sensitive-p light-ψ) (not state)
                              light-follows-camera-p state))
                      (when light-follows-camera-p
                        (setf (gtk4:range-value light-ϕ)
                              (gtk4:range-value camera-ϕ)
                              (gtk4:range-value light-ψ)
                              (gtk4:range-value camera-ψ)))))

      ;; Connect scale signals
      (macrolet ((%go (scale setter &optional aux-scale)
                   `(gtk4:connect ,scale "value-changed"
                                  (lambda (widget)
                                    (declare (ignore widget))
                                    (let ((value (gtk4:range-value ,scale)))
                                      (setf (,setter scene) (float value 0f0))
                                      ,@(when aux-scale
                                          `((when light-follows-camera-p
                                              (setf (gtk4:range-value ,aux-scale) value)))))
                                    (gtk4:gl-area-queue-render area)))))
        (%go camera-ϕ scene-camera-ϕ light-ϕ)
        (%go camera-ψ scene-camera-ψ light-ψ)
        (%go camera-r scene-camera-r)
        (%go light-ϕ  scene-light-ϕ)
        (%go light-ψ  scene-light-ψ))

      ;; Set "light follows camera" knob to a correct position
      (setf (gtk4:check-button-active-p follow-camera) light-follows-camera-p)

      (with-place (model-pointer-getter model-pointer-setter)
        (gtk4:connect
         prev-model "clicked"
         (navigation-button-handler area uploader
                                    #'model-pointer-getter #'model-pointer-setter
                                    #'step-backward))
        (gtk4:connect
         next-model "clicked"
         (navigation-button-handler area uploader
                                    #'model-pointer-getter #'model-pointer-setter
                                    #'step-forward))

        (gtk4:connect
         open-model "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (let ((dialog (gtk4:make-file-chooser-native
                          :title "Choose a model"
                          :parent window
                          :action gtk4:+file-chooser-action-open+
                          :accept-label "Open"
                          :cancel-label "Cancel")))

             (add-filters-to-file-chooser-dialog dialog)
             (gtk4:connect dialog "response"
                           (lambda (widget response)
                             (declare (ignore widget))
                             (when (= response gtk4:+response-type-accept+)
                               (let* ((file (gio:file-path
                                             (gtk4:file-chooser-file dialog)))
                                      (model-pointer (zipper-to-model file)))
                                 (handler-case
                                     (multiple-value-bind (model nvoxels)
                                         (load-connectivity
                                          (current-or-previous model-pointer))
                                       (funcall uploader model nvoxels)
                                       (model-pointer-setter model-pointer)
                                       (setf
                                        (gtk4:widget-sensitive-p next-model) t
                                        (gtk4:widget-sensitive-p prev-model) t)
                                       (gtk4:gl-area-queue-render area))
                                   (loader-error (c)
                                     (show-error-dialog c)))))))
             (gtk4:native-dialog-show dialog))))))

    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
