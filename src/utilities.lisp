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

(defparameter *light-Δϕ* 0.15
  "Difference between positions of the camera and the light source
when the latter tracks the first.")

(defstruct scene
  ;; Camera
  (camera-fov 75.0 :type single-float)
  (camera-ϕ 0.0 :type single-float)
  (camera-ψ 0.0 :type single-float)
  (camera-r 2.8 :type single-float)

  ;; Light
  (light-ϕ *light-Δϕ* :type single-float)
  (light-ψ 0.0        :type single-float)
  (light-r 2.8        :type single-float)

  ;; Light settings
  (show-light-p nil :type boolean)

  ;; Scene parameters
  (nvoxels 0 :type fixnum))

(sera:defconstructor gl-state
  ;; Common resources
  (vao         fixnum)
  (posbuffer   fixnum)
  (labelbuffer fixnum)
  (connbuffer  fixnum)
  (palbuffer   fixnum)
  ;; Pass 0: A shadow map
  (pass-0      fixnum)
  (framebuffer fixnum)
  (shadowmap   fixnum)
  ;; Pass 1: actual rendering
  (pass-1      fixnum)
  (texture     fixnum)
  (palette     fixnum)
  ;; Light source rendering
  (ls-program  fixnum))

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

(sera:-> light-position-vector (scene)
         (values rtg-math.types:vec3 &optional))
(defun light-position-vector (scene)
  (object-position (scene-light-r scene)
                   (scene-light-ϕ scene)
                   (scene-light-ψ scene)))

(sera:-> camera-position-vector (scene)
         (values rtg-math.types:vec3 &optional))
(defun camera-position-vector (scene)
  (object-position (scene-camera-r scene)
                   (scene-camera-ϕ scene)
                   (scene-camera-ψ scene)))

(defun fast-upload-buffer (vector element-size &key (target :array-buffer))
  (cffi:with-pointer-to-vector-data (ptr vector)
    (%gl:buffer-data target (* element-size (length vector)) ptr :static-draw)))

;; Voxel texture
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
         (rtg-math.types:vec3  alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun projection-matrix (position width height)
  "Return a projection matrix from a perspective of an object with
coordinates POSITION looking at the origin. WIDTH and HEIGHT are
dimensions of the screen."
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 6.2 75.0)
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    position
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

(defun create-program (shaders)
  (let* ((program (gl:create-program))
         (what (if (= (length shaders) 2)
                   '(:vertex-shader :fragment-shader)
                   '(:vertex-shader :geometry-shader :fragment-shader)))
         (gl-shaders (mapcar #'create-shader what shaders)))
    (mapc (lambda (shader) (gl:attach-shader program shader)) gl-shaders)
    (gl:link-program program)
    (mapc (lambda (shader) (gl:detach-shader program shader) (gl:delete-shader shader))
          gl-shaders)

    (let ((status (gl:get-program program :link-status)))
      (unless status
        (error "Program linkage failure: ~a"
               (gl:get-program-info-log program))))
    program))

(defun set-mat4-uniform (program uniform matrix)
  (gl:uniform-matrix
   (gl:get-uniform-location program uniform)
   4 (vector matrix) nil))

(defun set-vec3-uniform (program uniform vector)
  (gl:uniformf
   (gl:get-uniform-location program uniform)
   (aref vector 0)
   (aref vector 1)
   (aref vector 2)))

(declaim (inline flatten))
(defun flatten (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset 0))

(defconstant +palette-color-number+ 64
  "NUmber of colors in the palette")

(defun make-palette ()
  "Make random colors for different labels"
  (let ((state (make-random-state t)))
    (make-array (* +palette-color-number+ 4)
                :element-type 'single-float
                :initial-contents
                (loop repeat (* +palette-color-number+ 4)
                      collect (random 1.0 state)))))
