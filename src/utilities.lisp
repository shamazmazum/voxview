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
  (light-r 2.0 :type single-float)

  ;; Light color
  (light-color (rtg-math.vector3:make 1.0 1.0 1.0)
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
  (normbuffer  -1 :type fixnum)
  (texture     -1 :type fixnum))

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

;; Voxel texture
(defmacro do-indices ((array &rest indices) &body body)
  (let ((a (gensym)))
    `(let ((,a ,array))
       ,(first
         (si:foldr
          (lambda (enumerated-index acc)
            (destructuring-bind (i . var)
                enumerated-index
              `((loop for ,var fixnum below (array-dimension ,a ,i) do ,@acc))))
          body (si:enumerate (si:list->iterator indices)))))))

(sera:-> create-noise (alex:positive-fixnum single-float fixnum)
         (values (simple-array single-float (* * *)) &optional))
(defun create-noise (side scale seed)
  (declare (optimize (speed 3)))
  (let ((noise (make-array (list side side side)
                           :element-type 'single-float)))
    (flet ((trans (n)
             (declare (type fixnum n))
             (/ (* scale n) side)))
    (do-indices (noise i j k)
      (setf (aref noise i j k)
            (cl-value-noise:value-noise (trans i) (trans j) (trans k)
                                        :seed seed :octaves 7)))
    noise)))

(defparameter *noise*  (create-noise 128 20.0 43543))

(sera:-> projection-matrix
         (single-float single-float single-float alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun projection-matrix (r ϕ ψ width height)
  "Return a projection matrix from a perspective of an object with
coordinates (R, Φ, Ψ) looking at the origin. WIDTH and HEIGHT are
dimensions of the screen."
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 3.0 75.0)
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    (object-position r ϕ ψ)
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
