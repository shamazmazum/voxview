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
  (lambda (model)
    (gtk4:gl-area-make-current area)

    (let ((state (funcall state-getter)))
      (gl:bind-texture :texture-3d (gl-state-model-texture state))
      (fast-upload-voxels (model-texture-data model))

      (gl:use-program (gl-state-program state))
      (set-float-uniform (gl-state-program state) "MIN"
                         (model-min model))
      (set-float-uniform (gl-state-program state) "MAX"
                         (model-max model)))

    (setf (scene-loaded-p scene) t)
    (values)))

(sera:-> make-realize-handler (setter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (setter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)

    ;; Set GL state
    (let ((program  (create-program *shaders*))
          (texture  (gl:gen-texture))
          (colormap (gl:gen-texture))
          (vao      (gl:gen-vertex-array)))
      (gl:bind-texture :texture-3d texture)
      (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-3d :texture-min-filter :linear)
      (gl:tex-parameter :texture-3d :texture-wrap-s :clamp-to-border)
      (gl:tex-parameter :texture-3d :texture-wrap-t :clamp-to-border)
      (gl:tex-parameter :texture-3d :texture-wrap-r :clamp-to-border)

      (gl:bind-texture :texture-1d colormap)
      (gl:tex-parameter :texture-1d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-1d :texture-min-filter :linear)
      (gl:tex-parameter :texture-1d :texture-wrap-s :clamp-to-border)
      (fast-upload-colormap *viridis*)

      (funcall setter (gl-state vao texture colormap program))
      (values))))

(sera:-> make-unrealize-handler (getter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (state-getter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    ;; Clear GL state
    (let ((state (funcall state-getter)))
      (gl:delete-textures
       (list (gl-state-model-texture state)
             (gl-state-colormap      state)))
      (gl:delete-program (gl-state-program state))
      (gl:delete-vertex-arrays
       (list (gl-state-vao state))))

    (values)))

(sera:-> make-draw-handler (getter scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (state-getter scene)
  (lambda (area context)
    (declare (ignore context))

    (gl:clear :color-buffer-bit)
    (when (scene-loaded-p scene)
      (let ((state (funcall state-getter)))
        (gl:use-program (gl-state-program state))

        ;; Set uniforms
        (set-mat-uniform (gl-state-program state) "PROJECTION"
                         (camera-projection-matrix area scene))
        (set-vec-uniform  (gl-state-program state) "CP"
                          (cutting-plane scene))
        (set-bool-uniform (gl-state-program state) "USE_CP_P"
                          (scene-plane-p scene))
        (set-int-uniform (gl-state-program state) "MODEL_TEXTURE" 0)
        (set-int-uniform (gl-state-program state) "COLORMAP" 1)
        (set-float-uniform (gl-state-program state) "MULTIPLIER"
                           (scene-multiplier scene))
        (set-float-uniform (gl-state-program state) "THRESHOLD"
                           (scene-threshold scene))
        (set-mat-uniform (gl-state-program state) "PLANAR"
                         (planar-space-basis scene))

        ;; Bind textures
        (gl:active-texture :texture0)
        (gl:bind-texture :texture-3d (gl-state-model-texture state))
        (gl:active-texture :texture1)
        (gl:bind-texture :texture-1d (gl-state-colormap state))

        ;; Render scene
        (gl:bind-vertex-array (gl-state-vao state))
        (gl:draw-arrays-instanced :triangle-strip 0 4 +n-planes+)))

    ;; T indicates that we are done
    t))

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
