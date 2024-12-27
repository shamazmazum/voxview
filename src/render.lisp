(in-package :voxview)

(declaim (type alex:positive-fixnum +shadow-width+ +shadow-height+))
(defconstant +shadow-width+  1500)
(defconstant +shadow-height+ 1500)

(deftype model-loader () '(sera:-> ((model *) rtg-math.types:vec3) (values &optional)))

(sera:-> call-with-screen-size ((sera:-> (integer integer) t) gir::object-instance) t)
(declaim (inline call-with-screen-size))
(defun call-with-screen-size (f area)
  (let* ((allocation (gtk4:widget-allocation area))
         (width  (gir:field allocation 'width))
         (height (gir:field allocation 'height)))
    (funcall f width height)))

(sera:-> camera-projection-matrix (gir::object-instance scene)
         (values rtg-math.types:mat4 &optional))
(defun camera-projection-matrix (area scene)
  (call-with-screen-size
   (lambda (width height)
     (projection-matrix (scene-camera-r scene)
                        (scene-camera-ϕ scene)
                        (scene-camera-ψ scene)
                        width height))
   area))

(sera:-> light-projection-matrix (scene)
         (values rtg-math.types:mat4 &optional))
(defun light-projection-matrix (scene)
  (projection-matrix (scene-light-r scene)
                     (scene-light-ϕ scene)
                     (scene-light-ψ scene)
                     +shadow-width+ +shadow-height+))

(sera:-> make-model-loader (gir::object-instance gl-state scene)
         (values model-loader &optional))
(defun make-model-loader (area gl-state scene)
  (lambda (model dimensions)
    (gtk4:gl-area-make-current area)

    ;; Fill voxel positions buffer
    (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
    (with-gl-array (posarray model)
      (gl:buffer-data :array-buffer :static-draw posarray))

    ;; Set number of voxels
    (setf (scene-nvoxels scene)
          (array-dimension model 0))

    ;; Set model dimensions
    (flet ((%go (program)
             (gl:use-program program)
             (gl:uniformf
              (gl:get-uniform-location program "NVOXELS")
              (aref dimensions 0)
              (aref dimensions 1)
              (aref dimensions 2))))
      (%go (gl-state-pass-0 gl-state))
      (%go (gl-state-pass-1 gl-state)))
    (values)))

(sera:-> make-realize-handler (gl-state)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (gl-state)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    (with-accessors ((pass-0      gl-state-pass-0)
                     (pass-1      gl-state-pass-1)
                     (vao         gl-state-vao)
                     (posbuffer   gl-state-posbuffer)
                     (vertbuffer  gl-state-vertbuffer)
                     (normbuffer  gl-state-normbuffer)
                     (texture     gl-state-texture)
                     (framebuffer gl-state-framebuffer)
                     (shadowmap   gl-state-shadowmap))
        gl-state

      ;; Enable depth test
      (gl:enable :depth-test :cull-face)
      (gl:clear-color 0.0 0.0 0.0 0.0)

      ;; Create programs
      (setf pass-0 (create-program (first *pass-0*) (second *pass-0*)))
      (setf pass-1 (create-program (first *pass-1*) (second *pass-1*)))

      ;; Create vertex array
      (setf vao (gl:gen-vertex-array))
      (gl:bind-vertex-array vao)

      ;; Create voxel position buffer
      (setf posbuffer (gl:gen-buffer))

      ;; Fill model vertices
      (setf vertbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer vertbuffer)
      (with-gl-array (vertarray *cube-vertices*)
        (gl:buffer-data :array-buffer :static-draw vertarray))

      ;; Fill normals
      (setf normbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer normbuffer)
      (with-gl-array (normarray *cube-normals*)
        (gl:buffer-data :array-buffer :static-draw normarray))

      ;; Create texture
      (setf texture (gl:gen-texture))
      (gl:bind-texture :texture-3d texture)
      (gl:tex-image-3d :texture-3d 0 :red
                       (array-dimension *noise* 0)
                       (array-dimension *noise* 1)
                       (array-dimension *noise* 2)
                       0 :red :float (aops:flatten *noise*))
      (gl:tex-parameter :texture-3d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-3d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-3d :texture-wrap-s :mirrored-repeat)
      (gl:tex-parameter :texture-3d :texture-wrap-t :mirrored-repeat)
      (gl:tex-parameter :texture-3d :texture-wrap-r :mirrored-repeat)

      ;; Prepare shadowmap
      (setf framebuffer (gl:gen-framebuffer))
      (setf shadowmap (gl:gen-texture))
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
      (gl:bind-framebuffer :framebuffer 0))
    (values)))

(sera:-> make-unrealize-handler (gl-state)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (gl-state)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:delete-textures (list (gl-state-texture gl-state)
                              (gl-state-shadowmap gl-state)))
    (gl:delete-framebuffer (gl-state-framebuffer gl-state))
    (gl:delete-buffers (list (gl-state-vertbuffer gl-state)
                             (gl-state-posbuffer  gl-state)
                             (gl-state-normbuffer gl-state)))
    (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
    (gl:delete-program (gl-state-pass-0 gl-state))
    (gl:delete-program (gl-state-pass-1 gl-state))
    (values)))

(defun render-scene (gl-state scene)
  (gl:enable-vertex-attrib-array 0)
  (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
  (gl:vertex-attrib-pointer 0 3 :float nil 0
                            (cffi:null-pointer))
  (%gl:vertex-attrib-divisor 0 1)

  (gl:enable-vertex-attrib-array 1)
  (gl:bind-buffer :array-buffer (gl-state-vertbuffer gl-state))
  (gl:vertex-attrib-pointer 1 3 :float nil 0
                            (cffi:null-pointer))

  (gl:enable-vertex-attrib-array 2)
  (gl:bind-buffer :array-buffer (gl-state-normbuffer gl-state))
  (gl:vertex-attrib-pointer 2 3 :float nil 0
                            (cffi:null-pointer))
  (gl:draw-arrays-instanced :triangles 0
                            (array-dimension *cube-vertices* 0)
                            (scene-nvoxels scene))
  (gl:disable-vertex-attrib-array 2)
  (gl:disable-vertex-attrib-array 1)
  (gl:disable-vertex-attrib-array 0))
  

(sera:-> make-draw-handler (gl-state scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (gl-state scene)
  (lambda (area context)
    (declare (ignore context))

    (cond
      ((zerop (scene-nvoxels scene)) nil)
      (t
       ;; GTK reassigns the framebuffer almost each frame. This is really stupid
       (let ((framebuffer (gl:get-integer :framebuffer-binding)))
         ;; Pass 0: Render shadows
         (gl:bind-framebuffer :framebuffer (gl-state-framebuffer gl-state))
         (gl:viewport 0 0 +shadow-width+ +shadow-height+)
         (gl:clear :depth-buffer-bit)
         (gl:use-program (gl-state-pass-0 gl-state))

         ;; Set light space projection matrix
         (let ((matrix (light-projection-matrix scene)))
           (gl:uniform-matrix
            (gl:get-uniform-location (gl-state-pass-0 gl-state) "TRANSFORM")
            4 (vector matrix) nil))

         ;; Render pass 0
         (render-scene gl-state scene)

         ;; Pass 1: Render the scene from the viewer's perspective
         (gl:bind-framebuffer :framebuffer framebuffer)
         (call-with-screen-size
          (lambda (width height)
            (gl:viewport 0 0 width height))
          area)
         (gl:clear :color-buffer-bit :depth-buffer-bit)

         ;; Set uniforms
         (gl:use-program (gl-state-pass-1 gl-state))

         ;; Camera projection
         (let ((matrix (camera-projection-matrix area scene)))
           (gl:uniform-matrix
            (gl:get-uniform-location (gl-state-pass-1 gl-state) "W_TRANSFORM")
            4 (vector matrix) nil))

         ;; Light projection
         (let ((matrix (light-projection-matrix scene)))
           (gl:uniform-matrix
            (gl:get-uniform-location (gl-state-pass-1 gl-state) "L_TRANSFORM")
            4 (vector matrix) nil))

         ;; Light color
         (let ((color (scene-light-color scene)))
           (gl:uniformf
            (gl:get-uniform-location (gl-state-pass-1 gl-state) "LIGHT_COLOR")
            (aref color 0)
            (aref color 1)
            (aref color 2)))

         ;; Light position
         (let ((position (object-position
                          (scene-light-r scene)
                          (scene-light-ϕ scene)
                          (scene-light-ψ scene))))
           (gl:uniformf
            (gl:get-uniform-location (gl-state-pass-1 gl-state) "LIGHT_POSITION")
            (aref position 0)
            (aref position 1)
            (aref position 2)))

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
         (render-scene gl-state scene))
       ;; T indicates that we are done
       t))))

(sera:-> make-drawing-area (scene)
         (values gir::object-instance (sera:-> ((model *)) (values &optional)) &optional))
(defun make-drawing-area (scene)
  (let ((area (gtk4:make-gl-area))
        (gl-state (make-gl-state)))
    (setf (gtk4:gl-area-allowed-apis area) 1         ; OpenGL Only
          (gtk4:gl-area-has-depth-buffer-p area) t)  ; Enable depth buffer
    (gtk4:connect area "realize"   (make-realize-handler   gl-state))
    (gtk4:connect area "unrealize" (make-unrealize-handler gl-state))
    (gtk4:connect area "render"    (make-draw-handler      gl-state scene))
    (values area (make-model-loader area gl-state scene))))
