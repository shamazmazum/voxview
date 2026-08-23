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
  ;; Voxel settings
  (voxel-size-x 1.0 :type single-float)
  (voxel-size-y 1.0 :type single-float)
  (voxel-size-z 1.0 :type single-float)

  ;; Density settings
  (multiplier 1.0 :type single-float)
  (threshold  0.0 :type single-float)

  ;; Camera
  (camera-fov 75.0 :type single-float)
  (camera-ϕ 0.0 :type single-float)
  (camera-ψ 0.0 :type single-float)
  (camera-r 2.8 :type single-float)

  ;; Cutting plane
  (plane-ϕ 0.0 :type single-float)
  (plane-ψ 0.0 :type single-float)
  (plane-d 0.0 :type single-float)
  (plane-p nil :type boolean)

  ;; Is the scene loaded?
  (loaded-p nil :type boolean))

(sera:defconstructor gl-state
  (vao           fixnum)
  (model-texture fixnum)
  (colormap      fixnum)
  (program       fixnum))

(sera:-> random-vec3 ()
         (values rtg-math.types:vec3 &optional))
(defun random-vec3 ()
  (rtg-math.vector3:make
   (random 1.0)
   (random 1.0)
   (random 1.0)))

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

(sera:-> camera-position-vector (scene)
         (values rtg-math.types:vec3 &optional))
(defun camera-position-vector (scene)
  (object-position (scene-camera-r scene)
                   (scene-camera-ϕ scene)
                   (scene-camera-ψ scene)))

(sera:-> cutting-plane (scene)
         (values rtg-math.types:vec4 &optional))
(defun cutting-plane (scene)
  (let* ((ϕ (scene-plane-ϕ scene))
         (ψ (scene-plane-ψ scene))
         (d (scene-plane-d scene))
         (cisϕ (cis ϕ))
         (cisψ (cis ψ)))
    (rtg-math.vector4:make
     (* (realpart cisϕ) (realpart cisψ))
     (* (imagpart cisϕ) (realpart cisψ))
     (* (imagpart cisψ))
     (- d))))

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

(sera:-> planar-space-basis (scene)
         (values rtg-math.types:mat3 &optional))
(defun planar-space-basis (scene)
  (let* ((position (camera-position-vector scene))
         (u (random-vec3))
         (v (random-vec3))
         ;; Normal to the planes
         (n (v3:normalize position))
         ;; Tangent space basis vector #1
         (t1 (v3:- u (v3:*s n (v3:dot n u))))
         (t1 (v3:normalize t1))
         ;; Tangent space basis vector #2
         (t2 (v3:- v (v3:*s n (v3:dot n v)) (v3:*s t1 (v3:dot t1 v))))
         (t2 (v3:normalize t2)))
    (m3:from-columns t1 t2 n)))

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
  (destructuring-bind (vertex fragment)
      shaders
    (let ((program   (gl:create-program))
          (svertex   (create-shader :vertex-shader   vertex))
          (sfragment (create-shader :fragment-shader fragment)))
      (gl:attach-shader program svertex)
      (gl:attach-shader program sfragment)
      (gl:link-program  program)
      (gl:detach-shader program svertex)
      (gl:detach-shader program sfragment)
      (gl:delete-shader svertex)
      (gl:delete-shader sfragment)

      (let ((status (gl:get-program program :link-status)))
        (unless status
          (error "Program linkage failure: ~a"
                 (gl:get-program-info-log program))))
      program)))

(sera:-> set-int-uniform (t string integer)
         (values &optional))
(defun set-int-uniform (program uniform value)
  (gl:uniformi
   (gl:get-uniform-location program uniform)
   value)
  (values))

(sera:-> set-float-uniform (t string single-float)
         (values &optional))
(defun set-float-uniform (program uniform value)
  (gl:uniformf
   (gl:get-uniform-location program uniform)
   value)
  (values))

(sera:-> set-bool-uniform (t string boolean)
         (values &optional))
(defun set-bool-uniform (program uniform value)
  (set-int-uniform
   program uniform
   (if value 1 0)))

(sera:-> set-mat-uniform (t string (simple-array single-float (*)))
         (values &optional))
(defun set-mat-uniform (program uniform matrix)
  (gl:uniform-matrix
   (gl:get-uniform-location program uniform)
   (ecase (length matrix)
     (9  3)
     (16 4))
   (vector matrix)
   nil)
  (values))

(sera:-> set-vec-uniform (t string (simple-array single-float (*)))
         (values &optional))
(defun set-vec-uniform (program uniform vector)
  (let ((location (gl:get-uniform-location program uniform)))
    (ecase (length vector)
      (1 (gl:uniformf location
                      (aref vector 0)))
      (2 (gl:uniformf location
                      (aref vector 0)
                      (aref vector 1)))
      (3 (gl:uniformf location
                      (aref vector 0)
                      (aref vector 1)
                      (aref vector 2)))
      (4 (gl:uniformf location
                      (aref vector 0)
                      (aref vector 1)
                      (aref vector 2)
                      (aref vector 3)))))
  (values))

(declaim (inline flatten))
(defun flatten (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset 0))

(serapeum:-> fast-upload-voxels ((simple-array single-float (* * *)))
             (values &optional))
(defun fast-upload-voxels (array)
  (declare (optimize (speed 3)))
  #-sbcl
  (gl:tex-image-3d :texture-3d 0 :red
                   (array-dimension array 2)
                   (array-dimension array 1)
                   (array-dimension array 0)
                   0 :red :float (flatten array))
  #+sbcl
  (cffi:with-pointer-to-vector-data (ptr (sb-ext:array-storage-vector array))
    (gl:tex-image-3d :texture-3d 0 :red
                     (array-dimension array 2)
                     (array-dimension array 1)
                     (array-dimension array 0)
                     0 :red :float ptr)))

(serapeum:-> fast-upload-colormap ((simple-array single-float (* 3)))
             (values &optional))
(defun fast-upload-colormap (array)
  (declare (optimize (speed 3)))
  #-sbcl
  (gl:tex-image-1d :texture-1d 0 :rgb (array-dimension array 0) 0 :rgb :float
                   (flatten array))
  #+sbcl
  (cffi:with-pointer-to-vector-data (ptr (sb-ext:array-storage-vector array))
    (gl:tex-image-1d :texture-1d 0 :rgb (array-dimension array 0) 0 :rgb :float ptr)))

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
