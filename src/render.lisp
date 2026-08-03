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

(deftype model-gpu-uploader () '(sera:-> (model) (values &optional)))
(sera:-> make-gpu-uploader (gir::object-instance getter scene)
         (values model-gpu-uploader &optional))
(defun make-gpu-uploader (area state-getter scene)
  (lambda (model)
    (gtk4:gl-area-make-current area)

    (setf (scene-nelements scene) (length (model-indices model)))

    (let ((gl-state (funcall state-getter)))
      ;; Fill vertex positions buffer
      (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
      (fast-upload-buffer (model-points model) 4)

      ;; Fill voxel label buffer
      (gl:bind-buffer :array-buffer (gl-state-labelbuffer gl-state))
      (fast-upload-buffer (model-labels model) 4)

      ;; Fill indices of the vertices
      (gl:bind-buffer :element-array-buffer (gl-state-indbuffer gl-state))
      (fast-upload-buffer (model-indices model) 4 :target :element-array-buffer)

      ;; Do we need to draw in color?
      (let ((program (gl-state-pass-1 gl-state)))
        (gl:use-program program)
        (set-bool-uniform program "USE_COLOR_P"
                          (not (zerop (length (model-labels model)))))))
    (values)))

(defun upload-new-palette (palbuffer)
  (gl:bind-buffer :texture-buffer palbuffer)
  (fast-upload-buffer (make-palette) 4 :target :texture-buffer))

(deftype palette-uploader () '(sera:-> () (values &optional)))
(sera:-> make-palette-uploader (gir::object-instance getter)
         (values palette-uploader &optional))
(defun make-palette-uploader (area state-getter)
  (lambda ()
    (gtk4:gl-area-make-current area)
    (upload-new-palette (gl-state-palbuffer (funcall state-getter)))
    (values)))

(sera:-> make-realize-handler (setter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (setter)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    ;; Create resources
    (let ((pass-0 (create-program *pass-0*)) ; Shadowmap program
          (pass-1 (create-program *pass-1*)) ; Rendering program
          (pass-2 (create-program *pass-2*)) ; Preparation for cutting plane rendering
          (cp-program
            (create-program
             *plane-shaders*))               ; Cutting plane rendering
          (ls-program
           (create-program
            *light-source-shaders*))         ; Light source rendering program
          (vao (gl:gen-vertex-array))        ; Vertex array object for a model
          (posbuffer   (gl:gen-buffer))      ; Position of the vertices
          (labelbuffer (gl:gen-buffer))      ; Voxel label
          (palbuffer   (gl:gen-buffer))      ; Palette colors
          (indbuffer   (gl:gen-buffer))      ; Indices into position array
          (palette (gl:gen-texture))         ; Palette texture
          (texture (gl:gen-texture))         ; Model texture
          (framebuffer (gl:gen-framebuffer)) ; Shadow framebuffer
          (shadowmap (gl:gen-texture)))      ; Shadowmap texture

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

      ;; Upload palette
      (upload-new-palette palbuffer)
      (gl:bind-texture :texture-buffer palette)
      (%gl:tex-buffer :texture-buffer :rgb32f palbuffer)

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
               (gl-state vao posbuffer labelbuffer indbuffer palbuffer
                         pass-0 framebuffer shadowmap
                         pass-1 texture palette pass-2 cp-program ls-program)))
    (values)))

(sera:-> make-unrealize-handler (getter)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (state-getter)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (let ((gl-state (funcall state-getter)))
      (gl:delete-textures (list (gl-state-texture gl-state)
                                (gl-state-palette gl-state)
                                (gl-state-shadowmap gl-state)))
      (gl:delete-framebuffer (gl-state-framebuffer gl-state))
      (gl:delete-buffers (list (gl-state-indbuffer   gl-state)
                               (gl-state-labelbuffer gl-state)
                               (gl-state-posbuffer   gl-state)
                               (gl-state-palbuffer   gl-state)))
      (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
      (gl:delete-program (gl-state-pass-0 gl-state))
      (gl:delete-program (gl-state-pass-1 gl-state))
      (gl:delete-program (gl-state-pass-2 gl-state))
      (gl:delete-program (gl-state-cp-program gl-state))
      (gl:delete-program (gl-state-ls-program gl-state)))
    (values)))

(defun render-scene (gl-state scene)
  (gl:bind-vertex-array (gl-state-vao gl-state))
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
  (gl:bind-buffer :element-array-buffer (gl-state-indbuffer gl-state))
  (gl:vertex-attrib-pointer 0 3 :float nil 0 0)

  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (gl-state-labelbuffer gl-state))
  (gl:vertex-attrib-ipointer 1 1 :unsigned-int 0 0)

  (%gl:draw-elements :triangles (scene-nelements scene) :unsigned-int 0)

  (gl:disable-vertex-attrib-array 1)
  (gl:disable-vertex-attrib-array 0))

(deftype uniform ()
  `(member :cp :use-cp-p
           :l-projection
           :c-projection
           :l-position
           :texture-sampler
           :shadowmap-sampler
           :palette-sampler))

(sera:-> set-uniform (gir::object-instance scene t uniform)
         (values &optional))
(defun set-uniform (area scene program what)
  (declare (optimize (speed 3)))
  (case what
    (:cp
     ;; Set cutting plane
     (set-vec-uniform
      program "CP"
      (cutting-plane scene)))
    (:use-cp-p
     ;; Do we use the cutting plane?
     (set-bool-uniform
      program "USE_CP_P"
      (scene-plane-p scene)))
    (:l-projection
     ;; Light projection
     (set-mat4-uniform
      program "L_PROJECTION"
      (light-projection-matrix scene)))
    (:c-projection
     ;; Camera projection
     (set-mat4-uniform
      program "C_PROJECTION"
      (camera-projection-matrix area scene)))
    (:l-position
     ;; Light position
     (set-vec-uniform
      program "LIGHT_POSITION"
      (light-position-vector scene)))
    (:texture-sampler
     ;; Texture sampler
     (set-int-uniform
      program "TEXTURE_SAMPLER" 0))
    (:shadowmap-sampler
     ;; Shadowmap sampler
     (set-int-uniform
      program "SHADOW_SAMPLER" 1))
    (:palette-sampler
     ;; Palette sampler
     (set-int-uniform
      program "PALETTE_SAMPLER" 2))))

(sera:-> make-draw-handler (getter scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (state-getter scene)
  (lambda (area context)
    (declare (ignore context))
    (cond
      ((zerop (scene-nelements scene)) nil)
      (t
       (let ((gl-state (funcall state-getter))
             ;; GTK reassigns the framebuffer almost each frame. This is really stupid
             (framebuffer (gl:get-integer :framebuffer-binding)))
         (gl:enable :cull-face :depth-test)
         ;; Pass 0: Render shadows
         (gl:cull-face :front)
         (gl:bind-framebuffer :framebuffer (gl-state-framebuffer gl-state))
         (gl:viewport 0 0 +shadow-width+ +shadow-height+)
         (gl:clear :depth-buffer-bit)
         (gl:use-program (gl-state-pass-0 gl-state))

         (set-uniform area scene (gl-state-pass-0 gl-state) :l-projection)
         (set-uniform area scene (gl-state-pass-0 gl-state) :cp)
         (set-uniform area scene (gl-state-pass-0 gl-state) :use-cp-p)

         ;; Render pass 0
         (render-scene gl-state scene)

         ;; Pass 1: Render the scene from the viewer's perspective
         (gl:cull-face :back)
         (gl:bind-framebuffer :framebuffer framebuffer)
         (with-screen-size (width height) area
            (gl:viewport 0 0 width height))
         (gl:clear :color-buffer-bit :depth-buffer-bit)

         ;; Set uniforms
         (gl:use-program (gl-state-pass-1 gl-state))
         (set-uniform area scene (gl-state-pass-1 gl-state) :c-projection)
         (set-uniform area scene (gl-state-pass-1 gl-state) :l-projection)
         (set-uniform area scene (gl-state-pass-1 gl-state) :cp)
         (set-uniform area scene (gl-state-pass-1 gl-state) :use-cp-p)
         (set-uniform area scene (gl-state-pass-1 gl-state) :l-position)
         (set-uniform area scene (gl-state-pass-1 gl-state) :texture-sampler)
         (set-uniform area scene (gl-state-pass-1 gl-state) :shadowmap-sampler)
         (set-uniform area scene (gl-state-pass-1 gl-state) :palette-sampler)

         ;; Activate textures
         (gl:active-texture :texture0)
         (gl:bind-texture :texture-3d (gl-state-texture gl-state))
         (gl:active-texture :texture1)
         (gl:bind-texture :texture-2d (gl-state-shadowmap gl-state))
         (gl:active-texture :texture2)
         (gl:bind-texture :texture-buffer (gl-state-palette gl-state))

         ;; Render pass 1
         (render-scene gl-state scene)

         ;; Draw the caps if needed
         (when (scene-plane-p scene)
           ;; Set uniforms
           (gl:use-program (gl-state-pass-2 gl-state))
           (set-uniform area scene (gl-state-pass-2 gl-state) :c-projection)
           (set-uniform area scene (gl-state-pass-2 gl-state) :cp)

           ;; Fill the stencil buffer. It is non-zero only when back
           ;; faces are visible
           (gl:cull-face :front)
           (gl:enable :stencil-test)
           (gl:clear :stencil-buffer-bit)
           (gl:stencil-func :always 0 #xff)
           (gl:stencil-op :keep :keep :incr)
           (render-scene gl-state scene)

           ;; Render the cutting plane
           (gl:use-program (gl-state-cp-program gl-state))
           (set-uniform area scene (gl-state-cp-program gl-state) :c-projection)
           (set-uniform area scene (gl-state-cp-program gl-state) :l-projection)
           (set-uniform area scene (gl-state-cp-program gl-state) :cp)
           (set-uniform area scene (gl-state-cp-program gl-state) :l-position)
           (set-uniform area scene (gl-state-cp-program gl-state) :texture-sampler)
           (set-uniform area scene (gl-state-cp-program gl-state) :shadowmap-sampler)

           ;; Random vectors
           (set-vec-uniform (gl-state-cp-program gl-state) "V1"
                            (random-vec3))
           (set-vec-uniform (gl-state-cp-program gl-state) "V2"
                            (random-vec3))

           ;; Activate textures
           (gl:active-texture :texture0)
           (gl:bind-texture :texture-3d (gl-state-texture gl-state))
           (gl:active-texture :texture1)
           (gl:bind-texture :texture-2d (gl-state-shadowmap gl-state))
           
           (gl:stencil-func :notequal 0 #xff)
           (gl:stencil-op :keep :keep :keep)

           (gl:disable :cull-face)
           (gl:draw-arrays :triangle-strip 0 4)

           ;; Disable stencil tests
           (gl:disable :stencil-test))

         (when (scene-show-light-p scene)
           ;; Render light source
           (gl:disable :cull-face)

           (gl:use-program (gl-state-ls-program gl-state))
           (gl:use-program (gl-state-cp-program gl-state))
           (set-uniform area scene (gl-state-ls-program gl-state) :c-projection)
           (set-uniform area scene (gl-state-ls-program gl-state) :l-position)

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

(sera:defconstructor renderer
  (area             gir::object-instance)
  (model-uploader   model-gpu-uploader)
  (palette-uploader palette-uploader))

(sera:-> make-drawing-area (scene)
         (values renderer &optional))
(defun make-drawing-area (scene)
  (let ((area (gtk4:make-gl-area)))
    (setf (gtk4:gl-area-has-depth-buffer-p   area) t ; Enable depth buffer
          (gtk4:gl-area-has-stencil-buffer-p area) t ; Enable stencil buffer
          ;; (gtk4:gl-area-allowed-apis area) 1
          (maybe-gl-area-allowed-apis area) 1)     ; OpenGL Only
    (with-place (state-getter state-setter)
      (gtk4:connect area "realize"   (make-realize-handler   #'state-setter))
      (gtk4:connect area "unrealize" (make-unrealize-handler #'state-getter))
      (gtk4:connect area "render"    (make-draw-handler      #'state-getter scene))
      (renderer area
                (make-gpu-uploader area #'state-getter scene)
                (make-palette-uploader area #'state-getter)))))
