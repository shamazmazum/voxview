(in-package :voxview)

(defstruct scene
  ;; Camera
  (camera-fov 75.0 :type single-float)
  (camera-ϕ 0.0 :type single-float)
  (camera-ψ 0.0 :type single-float)
  (camera-r 1.5 :type single-float)

  ;; Light
  (light-ϕ 0.0 :type single-float)
  (light-ψ 0.0 :type single-float)

  ;; Light color
  (light-color (rtg-math.vector3:make 1.0 0.0 1.0)
               :type rtg-math.types:vec3)

  ;; Scene parameters
  (nvoxels  0 :type fixnum))

(defstruct gl-state
  ;; Pass 0: A shadow map
  (pass-0      -1 :type fixnum)
  (framebuffer -1 :type fixnum)
  (shadowmap   -1 :type fixnum)
  ;; Pass 1: actual rendering
  (pass-1      -1 :type fixnum)
  (vao         -1 :type fixnum)
  (posbuffer   -1 :type fixnum)
  (vertbuffer  -1 :type fixnum)
  (normbuffer  -1 :type fixnum))

(sera:-> object-position (single-float single-float single-float)
         (values rtg-math.types:vec3 &optional))
(defun object-position (r ϕ ψ)
  (declare (optimize (speed 3)))
  (let ((sin-ϕ (sin ϕ))
        (cos-ϕ (cos ϕ))
        (sin-ψ (sin ψ))
        (cos-ψ (cos ψ)))
    (rtg-math.vector3:make
     (* r cos-ϕ cos-ψ)
     (* r sin-ψ)
     (* r sin-ϕ cos-ψ))))

(sera:-> array->gl ((model *))
         (values gl:gl-array &optional))
(defun array->gl (array)
  "Convert one dimensional lisp array of floats to foreign array"
  (declare (optimize (speed 3)))
  (let ((gl-array (gl:alloc-gl-array :float (array-total-size array))))
    (loop for i below (array-total-size array) do
          (setf (gl:glaref gl-array i)
                (row-major-aref array i)))
    gl-array))

(defmacro with-gl-array ((var lisp-array) &rest body)
  `(let ((,var (array->gl ,lisp-array)))
     (unwind-protect
          (progn ,@body)
       (gl:free-gl-array ,var))))

(sera:-> world->screen
         (scene alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun world->screen (scene width height)
  "Return world -> screen projection matrix. WIDTH and HEIGHT are
dimensions of the GtkGLArea widget."
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 3.0
    (scene-camera-fov scene))
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    (object-position (scene-camera-r scene)
                     (scene-camera-ϕ scene)
                     (scene-camera-ψ scene))
    (rtg-math.vector3:make 0.0 0.0 0.0))))

;; TODO: Refactor
(sera:-> world->light
         (scene alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun world->light (scene width height)
  "Return world -> light projection matrix. WIDTH and HEIGHT are
dimensions of the shadow map."
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 3.0 75.0)
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    ;; FIXME: Must coincide with a number in MAKE-DRAW-HANDLER
    (object-position 2.0
                     (scene-light-ϕ scene)
                     (scene-light-ψ scene))
    (rtg-math.vector3:make 0.0 0.0 0.0))))

(defun create-program (vertex fragment)
  (let* ((program (gl:create-program))
         (vertex-shader   (gl:create-shader :vertex-shader))
         (fragment-shader (gl:create-shader :fragment-shader)))
    (gl:shader-source vertex-shader   (varjo:glsl-code vertex))
    (gl:shader-source fragment-shader (varjo:glsl-code fragment))
    (gl:compile-shader vertex-shader)
    (gl:compile-shader fragment-shader)
    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)
    (gl:detach-shader  program vertex-shader)
    (gl:detach-shader program fragment-shader)
    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    (let ((status (gl:get-program program :link-status)))
      (unless status
        (error "Program linkage failure: ~a"
               (gl:get-program-info-log program))))
    program))
