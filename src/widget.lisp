(in-package :voxview)

(sera:-> make-realize-handler (gl-state (model *) rtg-math.types:vec3)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-realize-handler (gl-state model nvoxels)
  (lambda (area)
    (gtk4:gl-area-make-current area)

    (with-accessors ((program     gl-state-program)
                     (vao         gl-state-vao)
                     (posbuffer   gl-state-posbuffer)
                     (vertbuffer  gl-state-vertbuffer)
                     (trans-loc   gl-state-trans-loc)
                     (nvoxels-loc gl-state-nvoxels-loc))
        gl-state

      ;; Create program
      (setf program (gl:create-program))
      (let ((vertex-shader   (gl:create-shader :vertex-shader))
            (fragment-shader (gl:create-shader :fragment-shader)))
        (gl:shader-source vertex-shader   (varjo:glsl-code (first  *compiled-shaders*)))
        (gl:shader-source fragment-shader (varjo:glsl-code (second *compiled-shaders*)))
        (gl:compile-shader vertex-shader)
        (gl:compile-shader fragment-shader)
        (gl:attach-shader program vertex-shader)
        (gl:attach-shader program fragment-shader)
        (gl:link-program program)
        (gl:detach-shader  program vertex-shader)
        (gl:detach-shader program fragment-shader)
        (gl:delete-shader vertex-shader)
        (gl:delete-shader fragment-shader))

      (let ((status (gl:get-program program :link-status)))
        (unless status
          (error "Program linkage failure: ~a"
                 (gl:get-program-info-log program))))

      ;; Create vertex array
      (setf vao (gl:gen-vertex-array))
      (gl:bind-vertex-array vao)

      ;; Fill model vertices
      (setf vertbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer vertbuffer)
      (with-gl-array (vertarray *cube-vertices*)
        (gl:buffer-data :array-buffer :static-draw vertarray))

      ;; Fill voxel positions
      (setf posbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer posbuffer)
      (with-gl-array (posarray model)
        (gl:buffer-data :array-buffer :static-draw posarray))

      ;; Set locations of the uniforms
      (setf trans-loc   (gl:get-uniform-location program "TRANSFORM")
            nvoxels-loc (gl:get-uniform-location program "NVOXELS"))

      ;; Use our only program
      (gl:use-program program)

      ;; Set voxel size
      (gl:uniformf nvoxels-loc
                   (aref nvoxels 0)
                   (aref nvoxels 1)
                   (aref nvoxels 2)))

    (values)))

(sera:-> make-unrealize-handler (gl-state)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (gl-state)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:delete-buffers (list (gl-state-vertbuffer gl-state)
                             (gl-state-posbuffer  gl-state)))
    (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
    (gl:delete-program (gl-state-program gl-state))
    (values)))

(sera:-> make-draw-handler (gl-state scene (model *))
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (gl-state scene model)
  (lambda (area context)
    (declare (ignore context))
    ;; Clear buffers
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear :color-buffer-bit)
    (gl:clear-color 0.0 0.0 0.0 0.0)
    (gl:clear :depth-buffer-bit)
    (gl:enable :depth-test :cull-face)

    ;; Set uniforms
    (let ((world->screen
           (let* ((allocation (gtk4:widget-allocation area))
                  (width  (gir:field allocation 'width))
                  (height (gir:field allocation 'height)))
             (world->screen scene width height))))
      (gl:uniform-matrix
       (gl-state-trans-loc gl-state)
       4 (vector world->screen) nil))

    ;; Draw
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer (gl-state-posbuffer gl-state))
    (gl:vertex-attrib-pointer 0 3 :float nil 0
                              (cffi:null-pointer))
    ;; FIXME: Why %gl?
    (%gl:vertex-attrib-divisor 0 1)
    (gl:enable-vertex-attrib-array 1)
    (gl:bind-buffer :array-buffer (gl-state-vertbuffer gl-state))
    (gl:vertex-attrib-pointer 1 3 :float nil 0
                              (cffi:null-pointer))
    (gl:draw-arrays-instanced :triangles 0
                              (array-dimension *cube-vertices* 0)
                              (array-dimension model 0))
    (gl:disable-vertex-attrib-array 1)
    (gl:disable-vertex-attrib-array 0)

    ;; T indicates that we are done
    t))

(sera:-> make-drawing-area ((model *) scene rtg-math.types:vec3)
         (values gir::object-instance &optional))
(defun make-drawing-area (model scene nvoxels)
  (let ((area (gtk4:make-gl-area))
        (gl-state (make-gl-state)))
    (setf (gtk4:gl-area-allowed-apis area) 1) ; OpenGL Only
    (gtk4:connect area "realize"   (make-realize-handler   gl-state model nvoxels))
    (gtk4:connect area "unrealize" (make-unrealize-handler gl-state))
    (gtk4:connect area "render"    (make-draw-handler      gl-state scene model))
    area))
