(in-package :voxview)

(sera:-> call-with-screen-size ((sera:-> (integer integer) t) gir::object-instance) t)
(declaim (inline call-with-screen-size))
(defun call-with-screen-size (f area)
  (let* ((allocation (gtk4:widget-allocation area))
         (width  (gir:field allocation 'width))
         (height (gir:field allocation 'height)))
    (funcall f width height)))

(defmacro with-screen-size ((width height) area &body body)
  `(call-with-screen-size
    (lambda (,width ,height)
      ,@body)
    ,area))

(sera:-> camera-projection-matrix (gir::object-instance scene)
         (values rtg-math.types:mat4 &optional))
(defun camera-projection-matrix (area scene)
  (with-screen-size (width height) area
    (projection-matrix (camera-position-vector scene) width height)))

(deftype model-gpu-uploader () '(sera:-> (model) (values &optional)))
(sera:-> make-gpu-uploader (gir::object-instance getter scene)
         (values model-gpu-uploader &optional))
(defun make-gpu-uploader (area state-getter scene)
  (declare (ignore state-getter))
  (lambda (model)
    (declare (ignore model))
    (gtk4:gl-area-make-current area)
    ;; TODO:: Here do something for model loading (e.g. upload textures)
    (setf (scene-loaded-p scene) t)
    (values)))

(sera:-> make-realize-handler (setter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (setter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    ;; TODO:: Do something to initialize GL state
    (gl:clear-color 0.0 0.0 0.0 0.0)
    ;; Set GL state
    (funcall setter (gl-state))
    (values)))

(sera:-> make-unrealize-handler (getter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (state-getter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    ;; Clear GL state
    (values)))

(sera:-> make-draw-handler (getter scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (state-getter scene)
  (lambda (area context)
    (declare (ignore area context))
    (when (scene-loaded-p scene)
      (let ((gl-state (funcall state-getter)))
        (declare (ignore gl-state))
        ;; Draw the scene
        (gl:clear :color-buffer-bit)
        ;; T indicates that we are done
        t))))

;; KLUDGE: There are no GLArea.get_allowed_apis on Ubuntu
(defun (setf maybe-gl-area-allowed-apis) (value area)
  ;; First of all, find this method in the gir files
  (let ((gl-area-methods
         (gir:list-methods-desc
          (gir:nget-desc (gir:require-namespace "Gtk" "4.0") "GLArea"))))
    (when (find "set_allowed_apis" gl-area-methods
                :test #'string=
                :key (alex:compose #'gir:info-get-name #'gir::info-of))
      (gir:invoke (area "set_allowed_apis") value)))
  value)

(sera:defconstructor renderer
  (area             gir::object-instance)
  (model-uploader   model-gpu-uploader))

(sera:-> make-drawing-area (scene)
         (values renderer &optional))
(defun make-drawing-area (scene)
  (let ((area (gtk4:make-gl-area)))
    (setf (maybe-gl-area-allowed-apis area) 1) ; OpenGL Only
    (with-place (state-getter state-setter)
      (gtk4:connect area "realize"   (make-realize-handler   #'state-setter))
      (gtk4:connect area "unrealize" (make-unrealize-handler #'state-getter))
      (gtk4:connect area "render"    (make-draw-handler      #'state-getter scene))
      (renderer area (make-gpu-uploader area #'state-getter scene)))))
