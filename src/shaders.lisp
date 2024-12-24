(in-package :voxview)

(declaim (type varjo.internals:vertex-stage *vertex-shader*))
(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '((position :vec3)   ; Position of a voxel in the world system, changed once by an instance
     (vertex   :vec3)   ; Position of a voxel's vertex in the model system
     (normal   :vec3))  ; Vertex normals
   '((transform :mat4)  ; World -> Camera transform
     (nvoxels   :vec3)) ; Model space dimensions (in voxels)
   '(:450)
   '((let* ((eps (* 0.1 (/ 2 nvoxels)))
            (recip (/ nvoxels))
            (coord (+ (* 2 position recip) -1
                      recip
                      (* vertex (+ eps recip)))))
       (values
        (* transform (vari:vec4 coord 1)) ; gl_Position
        coord                             ; Vertex coordinate in the world space
        normal)))))

(declaim (type varjo.internals:fragment-stage *fragment-shader*))
(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord  :vec3)
     (normal :vec3))
   '((light-position :vec3)
     (light-color    :vec3))
   '(:450)
   '((let* ((r (- light-position coord))
            (cosphi (/ (vari:dot r normal) (vari:length r))))
       (vari:vec4
        (* light-color
           (+ 0.1 (* 0.9 (vari:clamp cosphi 0 1))))
        1)))))

(defparameter *compiled-shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))
