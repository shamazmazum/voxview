(in-package :voxview)

(sera:-> navigation-button-handler
         (model-gpu-uploader getter setter stepper)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun navigation-button-handler (uploader model-getter model-setter stepper)
  (lambda (widget)
    (declare (ignore widget))
    (let ((pointer (funcall stepper (funcall model-getter))))
      (handler-case
          (progn
            (funcall uploader (load-connectivity (current-or-previous pointer)))
            (funcall model-setter pointer))
        (loader-error (c)
          (show-error-dialog c))))))

(defun format-status-line (zipper)
  (format nil "Model file: ~a"
          (enough-namestring (current-or-previous zipper)
                             (truename #p"~/"))))

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

(defun expand-widget (widget)
  (setf (gtk4:widget-hexpand-p widget) t
        (gtk4:widget-vexpand-p widget) t
        (gtk4:widget-halign    widget) gtk4:+align-fill+
        (gtk4:widget-valign    widget) gtk4:+align-fill+))

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
    ;; KLUDGE: this setter is broken in cl-gtk4, so use gir directly
    (gir:invoke (window "set_size_request") 1200 700)
    (let* ((scene (make-scene))
           (light-follows-camera-p t)
           (renderer (make-drawing-area scene))
           (control-frame (gtk4:make-frame :label "Controls"))
           (camera-frame  (gtk4:make-frame :label "Camera"))
           (light-frame   (gtk4:make-frame :label "Light"))
           (toplevel-box   (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 2))
           (big-box        (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                          :spacing 0))
           (control-box    (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 10))
           (camera-box     (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 5))
           (light-box      (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 5))
           (navigation-box (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                          :spacing 2))
           (buttons-box    (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                          :spacing 0))

           (camera-ϕ (scale 0d0 (* 2 pi)
                            (scene-camera-ϕ scene)
                            1d-1))
           (camera-ψ (scale (+ (- (/ pi 2)) 0.01)
                            (- (+ (/ pi 2)) 0.01)
                            (scene-camera-ψ scene)
                            1d-2))
           (camera-r (scale 5d-1 3d0 (scene-camera-r scene) 1d-1))

           (light-ϕ (scale 0d0 (* 2 pi)
                           (scene-light-ϕ scene) 1d-1))
           (light-ψ (scale (+ (- (/ pi 2)) 0.01)
                           (- (+ (/ pi 2)) 0.01)
                           (scene-light-ψ scene)
                           1d-2))
           (follow-camera (gtk4:make-check-button :label "Follow camera"))
           (show-light    (gtk4:make-check-button :label "Show light source"))
           (open-model (gtk4:make-button :label "Open model"))
           (next-model (gtk4:make-button :icon-name "go-next"))
           (prev-model (gtk4:make-button :icon-name "go-previous"))
           (palette    (gtk4:make-button :label "Randomize palette"))
           (status-label (gtk4:make-label :str "Welcome to Voxview"))

           (motion-controller (gtk4:make-event-controller-motion))
           (wheel-controller (gtk4:make-event-controller-scroll
                              :flags gtk4:+event-controller-scroll-flags-vertical+))
           (button-controller (gtk4:make-gesture-click)))

      (setf (gtk4:window-child window) toplevel-box
            (gtk4:frame-child control-frame) control-box
            (gtk4:frame-child camera-frame) camera-box
            (gtk4:frame-child light-frame) light-box
            (gtk4:widget-sensitive-p prev-model) nil
            (gtk4:widget-sensitive-p next-model) nil)

      (expand-widget (renderer-area renderer))
      (gtk4:box-append toplevel-box big-box)
      (gtk4:box-append toplevel-box status-label)
      (gtk4:box-append big-box (renderer-area renderer))
      (gtk4:box-append big-box control-frame)
      (gtk4:box-append control-box camera-frame)
      (gtk4:box-append control-box light-frame)
      (gtk4:box-append control-box buttons-box)
      (gtk4:box-append buttons-box open-model)
      (gtk4:box-append buttons-box palette)
      (gtk4:box-append buttons-box navigation-box)

      (append-with-label camera-box camera-ϕ "ϕ")
      (append-with-label camera-box camera-ψ "ψ")
      (append-with-label camera-box camera-r "r")

      (append-with-label light-box light-ϕ "ϕ")
      (append-with-label light-box light-ψ "ψ")
      (gtk4:box-append light-box follow-camera)
      (gtk4:box-append light-box show-light)

      ;; Looks ugly
      (gtk4:box-append navigation-box prev-model)
      (gtk4:box-append navigation-box next-model)

      ;; Connect event controllers to the GL area
      (gtk4:widget-add-controller (renderer-area renderer) motion-controller)
      (gtk4:widget-add-controller (renderer-area renderer) button-controller)
      (gtk4:widget-add-controller (renderer-area renderer) wheel-controller)
      ;; Catch events from all buttons (there are no fucking touchpads here! ;)
      (setf (gtk4:gesture-single-button button-controller) 0)

      ;; Randomize palette
      (gtk4:connect
       palette "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (funcall (renderer-palette-uploader renderer))
         (gtk4:gl-area-queue-render (renderer-area renderer))))

      ;; Connect follow camera signal
      (gtk4:connect
       follow-camera "toggled"
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

      ;; Connect "show light source" signal
      (gtk4:connect
       show-light "toggled"
       (lambda (widget)
         (declare (ignore widget))
         (setf (scene-show-light-p scene)
               (gtk4:check-button-active-p show-light))
         (gtk4:gl-area-queue-render (renderer-area renderer))))

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
                                    (gtk4:gl-area-queue-render (renderer-area renderer))))))
        (%go camera-ϕ scene-camera-ϕ light-ϕ)
        (%go camera-ψ scene-camera-ψ light-ψ)
        (%go camera-r scene-camera-r)
        (%go light-ϕ  scene-light-ϕ)
        (%go light-ψ  scene-light-ψ))

      ;; Set "light follows camera" knob to a correct position
      (setf (gtk4:check-button-active-p follow-camera) light-follows-camera-p)
      ;; Same for "show light source"
      (setf (gtk4:check-button-active-p show-light) (scene-show-light-p scene))

      (with-place (model-pointer-getter model-pointer-setter)
        (gtk4:connect
         prev-model "clicked"
         (navigation-button-handler (renderer-model-uploader renderer)
                                    #'model-pointer-getter #'model-pointer-setter
                                    #'step-backward))
        (gtk4:connect
         next-model "clicked"
         (navigation-button-handler (renderer-model-uploader renderer)
                                    #'model-pointer-getter #'model-pointer-setter
                                    #'step-forward))

        (dolist (button (list next-model prev-model))
          (gtk4:connect
           button "clicked"
           (lambda (widget)
             (declare (ignore widget))
             (gtk4:gl-area-queue-render (renderer-area renderer))
             (setf (gtk4:label-text status-label)
                   (format-status-line (model-pointer-getter))))))

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
             (gtk4:connect
              dialog "response"
              (lambda (widget response)
                (declare (ignore widget))
                (when (= response gtk4:+response-type-accept+)
                  (let* ((file (gio:file-path
                                (gtk4:file-chooser-file dialog)))
                         (model-pointer (zipper-to-model file)))
                    (handler-case
                        (progn
                          (funcall (renderer-model-uploader renderer)
                                   (load-connectivity
                                    (current-or-previous model-pointer)))
                          (model-pointer-setter model-pointer)
                          (setf
                           (gtk4:widget-sensitive-p next-model) t
                           (gtk4:widget-sensitive-p prev-model) t
                           (gtk4:label-text status-label)
                           (format-status-line model-pointer))
                          (gtk4:gl-area-queue-render (renderer-area renderer)))
                      (loader-error (c)
                        (show-error-dialog c)))))))
             (gtk4:native-dialog-show dialog)))))

      ;; "Mouse look"
      ;; TODO: A separate control for sensitivity?
      (let ((mouse-sensitivity 10.0)
            old-x old-y moving-camera-p moving-light-p)
        (flet ((set-state! (value)
                 (let ((button (gtk4:gesture-single-current-button button-controller)))
                   (cond
                     ((= button 1)
                      (setq moving-camera-p value))
                     ((= button 3)
                      (setq moving-light-p  value))))))
          (gtk4:connect
           motion-controller "motion"
           (lambda (widget x y)
             (declare (ignore widget))
             (when (or moving-camera-p moving-light-p)
               (with-screen-size (width height)
                   (renderer-area renderer)
                 (let ((Δϕ (* mouse-sensitivity (/ (- x old-x) width)))
                       (Δψ (* mouse-sensitivity (/ (- y old-y) height))))
                   (flet ((modify-controls (control-ϕ control-ψ)
                            (let ((ϕ (gtk4:range-value control-ϕ)))
                              (setf (gtk4:range-value control-ϕ)
                                    (mod (+ ϕ Δϕ) (* 2 pi)))
                              (incf (gtk4:range-value control-ψ) Δψ))))
                     (when moving-camera-p
                       (modify-controls camera-ϕ camera-ψ))
                     (when (and moving-light-p (not light-follows-camera-p))
                       (modify-controls light-ϕ light-ψ)))))
               (setq old-x x old-y y))))

          (gtk4:connect
           button-controller "pressed"
           (lambda (widget nbuttons x y)
             (declare (ignore widget nbuttons))
             (setq old-x x old-y y)
             (set-state! t)))

          (gtk4:connect
           button-controller "released"
           (lambda (widget nbuttons x y)
             (declare (ignore widget nbuttons x y))
             (set-state! nil)))))

      ;; Zooming with a mouse wheel
      (let ((wheel-sensitivity 1f-1))
        (gtk4:connect
         wheel-controller "scroll"
         (lambda (widget x y)
           (declare (ignore widget x))
           (incf (gtk4:range-value camera-r)
                 (* y wheel-sensitivity)))))

      ;; Add hotkeys
      (flet ((add-action (name button hotkey)
               (let ((action (gio:make-simple-action
                              :name name :parameter-type nil)))
                 (gtk4:connect
                  action "activate"
                  (lambda (action param)
                    (declare (ignore action param))
                    (when (gtk4:widget-sensitive-p button)
                      (gtk4:widget-activate button))))
                 (gio:action-map-add-action window action))
               (gir:invoke (gtk4:*application* "set_accels_for_action")
                           (format nil "win.~a" name)
                           (list hotkey))))
        (add-action "next-model" next-model "<Control>l")
        (add-action "prev-model" prev-model "<Control>h")))

    (unless (gtk4:widget-visible-p window)
      (gtk4:window-present window))))
