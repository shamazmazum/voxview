(in-package :voxview)

(declaim (type alex:positive-fixnum +shadow-width+ +shadow-height+))
(defconstant +shadow-width+  1500)
(defconstant +shadow-height+ 1500)

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

(sera:-> light-projection-matrix (scene)
         (values rtg-math.types:mat4 &optional))
(defun light-projection-matrix (scene)
  (projection-matrix (light-position-vector scene)
                     +shadow-width+ +shadow-height+))

(deftype model-gpu-uploader () '(sera:-> (connectivity) (values &optional)))

(sera:-> make-gpu-uploader (gir::object-instance getter scene)
         (values model-gpu-uploader &optional))
(defun make-gpu-uploader (area state-getter scene)
  (lambda (connectivity)
    (gtk4:gl-area-make-current area)

    (let ((gl-state (funcall state-getter)))
      ;; Fill voxel positions buffer
      (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
      (fast-upload-buffer (connectivity-points connectivity) 4)

      ;; Fill connectivity data
      (gl:bind-buffer :array-buffer (gl-state-connbuffer gl-state))
      (fast-upload-buffer (connectivity-masks connectivity) 1)

      ;; Set number of voxels
      (setf (scene-nvoxels scene) (length (connectivity-masks connectivity)))

      ;; Set model dimensions
      (flet ((%go (program)
               (gl:use-program program)
               (gl:uniformf (gl:get-uniform-location program "NVOXELS")
                            (connectivity-max-dimension connectivity))))
        (%go (gl-state-pass-0 gl-state))
        (%go (gl-state-pass-1 gl-state))))
    (values)))

(sera:-> make-realize-handler (setter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (setter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    ;; Create resources
    (let ((pass-0 (create-program *pass-0*)) ; Shadowmap program
          (pass-1 (create-program *pass-1*)) ; Rendering program
          (ls-program
           (create-program
            *light-source-shaders*))         ; Light source rendering program
          (vao (gl:gen-vertex-array))        ; Vertex array object for a model
          (posbuffer  (gl:gen-buffer))       ; Voxel positions
          (connbuffer (gl:gen-buffer))       ; Connectivity information
          (texture (gl:gen-texture))         ; Model texture
          (framebuffer (gl:gen-framebuffer)) ; Shadow framebuffer
          (shadowmap (gl:gen-texture)))      ; Shadowmap texture

      ;; Enable depth test
      (gl:enable :depth-test)
      (gl:clear-color 0.0 0.0 0.0 0.0)

      ;; Upload model texture
      (gl:bind-texture :texture-3d texture)
      (gl:tex-image-3d :texture-3d 0 :red
                       (array-dimension *noise* 0)
                       (array-dimension *noise* 1)
                       (array-dimension *noise* 2)
                       0 :red :float (flatten *noise*))
      (gl:tex-parameter :texture-3d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-3d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-3d :texture-wrap-s :mirrored-repeat)
      (gl:tex-parameter :texture-3d :texture-wrap-t :mirrored-repeat)
      (gl:tex-parameter :texture-3d :texture-wrap-r :mirrored-repeat)

      ;; Prepare shadowmap
      (gl:bind-texture :texture-2d shadowmap)
      (gl:tex-image-2d :texture-2d 0 :depth-component +shadow-width+ +shadow-height+ 0
                       :depth-component :float (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
      (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
      (gl:bind-framebuffer :framebuffer framebuffer)
      (gl:framebuffer-texture-2d :framebuffer :depth-attachment :texture-2d shadowmap 0)
      (gl:draw-buffer :none)
      (gl:read-buffer :none)
      (gl:bind-framebuffer :framebuffer 0)

      (funcall setter
               (gl-state vao posbuffer connbuffer
                         pass-0 framebuffer shadowmap
                         pass-1 texture ls-program)))
    (values)))

(sera:-> make-unrealize-handler (getter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (state-getter)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (let ((gl-state (funcall state-getter)))
      (gl:delete-textures (list (gl-state-texture gl-state)
                                (gl-state-shadowmap gl-state)))
      (gl:delete-framebuffer (gl-state-framebuffer gl-state))
      (gl:delete-buffers (list (gl-state-connbuffer gl-state)
                               (gl-state-posbuffer  gl-state)))
      (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
      (gl:delete-program (gl-state-pass-0 gl-state))
      (gl:delete-program (gl-state-pass-1 gl-state))
      (gl:delete-program (gl-state-ls-program gl-state)))
    (values)))

(defun render-scene (gl-state scene)
  (gl:bind-vertex-array (gl-state-vao gl-state))
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
  (gl:vertex-attrib-pointer 0 3 :unsigned-int nil 0 0)

  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (gl-state-connbuffer gl-state))
  (gl:vertex-attrib-ipointer 1 1 :unsigned-byte 0 0)

  (gl:draw-arrays :points 0 (scene-nvoxels scene))

  (gl:disable-vertex-attrib-array 1)
  (gl:disable-vertex-attrib-array 0))
  

(sera:-> make-draw-handler (getter scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (state-getter scene)
  (lambda (area context)
    (declare (ignore context))

    (cond
      ((zerop (scene-nvoxels scene)) nil)
      (t
       (let ((gl-state (funcall state-getter))
             ;; GTK reassigns the framebuffer almost each frame. This is really stupid
             (framebuffer (gl:get-integer :framebuffer-binding)))
         (gl:enable :cull-face)
         ;; Pass 0: Render shadows
         (gl:bind-framebuffer :framebuffer (gl-state-framebuffer gl-state))
         (gl:viewport 0 0 +shadow-width+ +shadow-height+)
         (gl:clear :depth-buffer-bit)
         (gl:use-program (gl-state-pass-0 gl-state))

         ;; Set light space projection matrix
         (set-mat4-uniform (gl-state-pass-0 gl-state) "PROJECTION"
                           (light-projection-matrix scene))

         ;; Render pass 0
         (render-scene gl-state scene)

         ;; Pass 1: Render the scene from the viewer's perspective
         (gl:bind-framebuffer :framebuffer framebuffer)
         (with-screen-size (width height) area
            (gl:viewport 0 0 width height))
         (gl:clear :color-buffer-bit :depth-buffer-bit)

         ;; Set uniforms
         (gl:use-program (gl-state-pass-1 gl-state))

         ;; Camera projection
         (set-mat4-uniform (gl-state-pass-1 gl-state) "C_PROJECTION"
                           (camera-projection-matrix area scene))

         ;; Light projection
         (set-mat4-uniform (gl-state-pass-1 gl-state) "L_PROJECTION"
                           (light-projection-matrix scene))

         ;; Light color
         (set-vec3-uniform (gl-state-pass-1 gl-state) "LIGHT_COLOR"
                           (scene-light-color scene))

         ;; Light position
         (set-vec3-uniform (gl-state-pass-1 gl-state) "LIGHT_POSITION"
                           (light-position-vector scene))

         ;; Texture sampler
         (gl:uniformi
          (gl:get-uniform-location (gl-state-pass-1 gl-state) "TEXTURE_SAMPLER") 0)

         ;; Shadowmap sampler
         (gl:uniformi
          (gl:get-uniform-location (gl-state-pass-1 gl-state) "SHADOW_SAMPLER") 1)

         ;; Activate textures
         (gl:active-texture :texture0)
         (gl:bind-texture :texture-3d (gl-state-texture gl-state))
         (gl:active-texture :texture1)
         (gl:bind-texture :texture-2d (gl-state-shadowmap gl-state))

         ;; Render pass 1
         (render-scene gl-state scene)

         (when (scene-show-light-p scene)
           ;; Render light source
           (gl:disable :cull-face)

           (gl:use-program (gl-state-ls-program gl-state))

           ;; Set light position
           (set-vec3-uniform (gl-state-ls-program gl-state) "LIGHT_POSITION"
                             (light-position-vector scene))

           ;; Set projection matrix
           (set-mat4-uniform (gl-state-ls-program gl-state) "PROJECTION"
                             (camera-projection-matrix area scene))

           ;; Render a triangle
           (gl:draw-arrays :triangles 0 3)))

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

(sera:-> make-drawing-area (scene)
         (values gir::object-instance
                 (sera:-> (list rtg-math.types:uvec3)
                          (values &optional))
                 &optional))
(defun make-drawing-area (scene)
  (let ((area (gtk4:make-gl-area)))
    (setf (gtk4:gl-area-has-depth-buffer-p area) t ; Enable depth buffer
          ;; (gtk4:gl-area-allowed-apis area) 1
          (maybe-gl-area-allowed-apis area) 1)     ; OpenGL Only
    (with-place (state-getter state-setter)
      (gtk4:connect area "realize"   (make-realize-handler   #'state-setter))
      (gtk4:connect area "unrealize" (make-unrealize-handler #'state-getter))
      (gtk4:connect area "render"    (make-draw-handler      #'state-getter scene))
      (values area (make-gpu-uploader area #'state-getter scene)))))
