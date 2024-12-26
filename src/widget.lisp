(in-package :voxview)

(deftype model-loader () '(sera:-> ((model *) rtg-math.types:vec3) (values &optional)))

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
    (gl:use-program (gl-state-pass-1 gl-state))
    (gl:uniformf
     (gl:get-uniform-location (gl-state-pass-1 gl-state) "NVOXELS")
     (aref dimensions 0)
     (aref dimensions 1)
     (aref dimensions 2))
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
                     (normbuffer  gl-state-normbuffer))
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

      ;; Fill model vertices
      (setf vertbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer vertbuffer)
      (with-gl-array (vertarray *cube-vertices*)
        (gl:buffer-data :array-buffer :static-draw vertarray))

      ;; Create voxel position buffer
      (setf posbuffer (gl:gen-buffer))

      ;; Fill normals
      (setf normbuffer (gl:gen-buffer))
      (gl:bind-buffer :array-buffer normbuffer)
      (with-gl-array (normarray *cube-normals*)
        (gl:buffer-data :array-buffer :static-draw normarray)))
    (values)))

(sera:-> make-unrealize-handler (gl-state)
         (values (sera:-> (gir::object-instance) (values &optional)) &optional))
(defun make-unrealize-handler (gl-state)
  (lambda (area)
    (gtk4:gl-area-make-current area)
    (gl:delete-buffers (list (gl-state-vertbuffer gl-state)
                             (gl-state-posbuffer  gl-state)
                             (gl-state-normbuffer gl-state)))
    (gl:delete-vertex-arrays (list (gl-state-vao gl-state)))
    (gl:delete-program (gl-state-pass-0 gl-state))
    (gl:delete-program (gl-state-pass-1 gl-state))
    (values)))

(sera:-> make-draw-handler (gl-state scene)
         (values (sera:-> (gir::object-instance gir::object-instance)
                          (values boolean &optional))
                 &optional))
(defun make-draw-handler (gl-state scene)
  (lambda (area context)
    (declare (ignore context))
    ;; Clear buffers
    (gl:clear :color-buffer-bit :depth-buffer-bit)

    (unless (zerop (scene-nvoxels scene))

      ;; Set uniforms
      (gl:use-program (gl-state-pass-1 gl-state))
      ;; Projection
      (let ((world->screen
             (let* ((allocation (gtk4:widget-allocation area))
                    (width  (gir:field allocation 'width))
                    (height (gir:field allocation 'height)))
               (world->screen scene width height))))
        (gl:uniform-matrix
         (gl:get-uniform-location (gl-state-pass-1 gl-state) "TRANSFORM")
         4 (vector world->screen) nil))

      ;; Light color
      (let ((color (scene-light-color scene)))
        (gl:uniformf
         (gl:get-uniform-location (gl-state-pass-1 gl-state) "LIGHT_COLOR")
         (aref color 0)
         (aref color 1)
         (aref color 2)))

      ;; Light position
      (let ((position (object-position 2.0 (scene-light-ϕ scene) (scene-light-ψ scene))))
        (gl:uniformf
         (gl:get-uniform-location (gl-state-pass-1 gl-state) "LIGHT_POSITION")
         (aref position 0)
         (aref position 1)
         (aref position 2)))

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

      (gl:enable-vertex-attrib-array 2)
      (gl:bind-buffer :array-buffer (gl-state-normbuffer gl-state))
      (gl:vertex-attrib-pointer 2 3 :float nil 0
                                (cffi:null-pointer))
      (gl:draw-arrays-instanced :triangles 0
                                (array-dimension *cube-vertices* 0)
                                (scene-nvoxels scene))
      (gl:disable-vertex-attrib-array 2)
      (gl:disable-vertex-attrib-array 1)
      (gl:disable-vertex-attrib-array 0)
      ;; T indicates that we are done
      t)))

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
