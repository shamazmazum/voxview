(in-package :voxview)

(defstruct scene
  ;; Camera position
  (camera-position (rtg-math.vector3:make 0.0 1.0 -1.5)
                   :type rtg-math.types:vec3)
  ;; Camera fov
  (camera-fov      75.0
                   :type single-float)
  ;; Light position
  (light-position  (rtg-math.vector3:make 0.0 100.0 -50.0)
                   :type rtg-math.types:vec3)
  ;; Light color
  (light-color     (rtg-math.vector3:make 1.0 0.0 1.0)
                   :type rtg-math.types:vec3))

(defstruct gl-state
  (program     -1 :type fixnum)
  (vao         -1 :type fixnum)
  (posbuffer   -1 :type fixnum)
  (vertbuffer  -1 :type fixnum)
  (normbuffer  -1 :type fixnum)
  (trans-loc   -1 :type fixnum)
  (nvoxels-loc -1 :type fixnum)
  (lpos-loc    -1 :type fixnum)
  (lcolor-loc  -1 :type fixnum))

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
    0.1 10.0
    (scene-camera-fov scene))
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    (scene-camera-position scene)
    (rtg-math.vector3:make 0.0 0.0 0.0))))
