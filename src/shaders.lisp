(in-package :voxview)

(declaim (type varjo.internals:vertex-stage *vertex-shader*))
(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '((position :vec3)   ; Position of a voxel in the world system, changed once by an instance
     (vertex   :vec3))  ; Position of a voxel's vertex in the model system
   '((transform :mat4)  ; World -> Camera transform
     (voxsize   :vec3)) ; Size of a voxel (in world units)
   '(:450)
   '((let ((coord (+ position (* (/ voxsize 2) (1+ vertex)))))
       (values
        (* transform (vari:vec4 coord 1.0))))))) ; gl_Position

(declaim (type varjo.internals:fragment-stage *fragment-shader*))
(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '()
   '()
   '(:450)
   '((vari:vec4 0.0 1.0 0.0 1.0))))

(defparameter *compiled-shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))
