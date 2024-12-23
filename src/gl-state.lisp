(in-package :voxview)

(defstruct camera
  (position (rtg-math.vector3:make 1.0 0.0 0.0)
            :type rtg-math.types:vec3)
  (fov 75.0 :type single-float))

(defstruct gl-state
  (program     -1 :type fixnum)
  (vao         -1 :type fixnum)
  (posbuffer   -1 :type fixnum)
  (vertbuffer  -1 :type fixnum)
  (trans-loc   -1 :type fixnum)
  (voxsize-loc -1 :type fixnum))

(sera:-> array->gl
         ((simple-array single-float (*)))
         (values gl:gl-array &optional))
(defun array->gl (array)
  "Convert one dimensional lisp array of floats to foreign array"
  (let ((gl-array (gl:alloc-gl-array :float (length array))))
    (loop for i from 0 by 1
          for x across array do
          (setf (gl:glaref gl-array i) x))
    gl-array))

(defmacro with-gl-array ((var lisp-array) &rest body)
  `(let ((,var (array->gl ,lisp-array)))
     (unwind-protect
          (progn ,@body)
       (gl:free-gl-array ,var))))

(sera:-> world->screen
         (camera alex:positive-fixnum alex:positive-fixnum)
         (values rtg-math.types:mat4 &optional))
(defun world->screen (camera width height)
  "Return world -> screen projection matrix. WIDTH and HEIGHT are
dimensions of the GtkGLArea widget."
  (rtg-math.matrix4:*
   (rtg-math.projection:perspective
    (float width)
    (float height)
    0.1 10.0
    (camera-fov camera))
   (rtg-math.matrix4:look-at
    (rtg-math.vector3:make 0.0 1.0 0.0)
    (camera-position camera)
    (rtg-math.vector3:make 0.0 0.0 0.0))))
