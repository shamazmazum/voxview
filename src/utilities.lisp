(in-package :voxview)

(defmacro with-place ((getter setter) &body body)
  (let ((place (gensym))
        (value (gensym "VAL")))
    `(let (,place)
       (flet ((,getter ()
                (assert ,place)
                ,place)
              (,setter (,value)
                (setq ,place ,value)))
         ,@body))))

(deftype getter () '(sera:-> () (values t &optional)))
(deftype setter () '(sera:-> (t) (values t &optional)))

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

(sera:defconstructor gl-state
  ;; Common resources
  (vao         fixnum)
  (posbuffer   fixnum)
  (connbuffer  fixnum)
  ;; Pass 0: A shadow map
  (pass-0      fixnum)
  (framebuffer fixnum)
  (shadowmap   fixnum)
  ;; Pass 1: actual rendering
  (pass-1      fixnum)
  (texture     fixnum))

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

(sera:defconstructor connectivity-data
  (coord rtg-math.types:uvec3)
  (mask  (unsigned-byte 8)))

(defun fill-positions-buffer (list)
  (declare (optimize (speed 3))
           (type list list))
  (let* ((length (length list))
         (gl-array (gl:alloc-gl-array :uint32 (* length 3))))
    (loop for n below (length list)
          for i = (* n 3)
          for connectivity in list
          for coord = (connectivity-data-coord connectivity) do
          (loop for j below 3 do
                (setf (gl:glaref gl-array (+ i j))
                      (aref coord j))))
    gl-array))

(defun fill-connectivity-buffer (list)
  (declare (optimize (speed 3))
           (type list list))
  (let* ((length (length list))
         (gl-array (gl:alloc-gl-array :uint8 length)))
    (loop for i below (length list)
          for connectivity in list
          for mask = (connectivity-data-mask connectivity) do
          (setf (gl:glaref gl-array i) mask))
    gl-array))

(defmacro with-gl-array ((var init-form) &rest body)
  `(let ((,var ,init-form))
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

(defun create-shader (stage compiled-shader)
  (let ((shader (gl:create-shader stage)))
    (gl:shader-source shader (varjo:glsl-code compiled-shader))
    (gl:compile-shader shader)
    (let ((status (gl:get-shader shader :compile-status)))
      (unless status
        (error "Shader compile failure: ~a ~a"
               shader
               (gl:get-shader-info-log shader))))
    shader))

(defun create-program (stage)
  (let* ((program (gl:create-program))
         (vertex-shader   (create-shader :vertex-shader   (first  stage)))
         (geometry-shader (create-shader :geometry-shader (second stage)))
         (fragment-shader (create-shader :fragment-shader (third  stage))))
    (mapc (lambda (shader) (gl:attach-shader program shader))
          (list vertex-shader geometry-shader fragment-shader))
    (gl:link-program program)
    (mapc (lambda (shader) (gl:detach-shader program shader) (gl:delete-shader shader))
          (list vertex-shader geometry-shader fragment-shader))

    (let ((status (gl:get-program program :link-status)))
      (unless status
        (error "Program linkage failure: ~a"
               (gl:get-program-info-log program))))
    program))
