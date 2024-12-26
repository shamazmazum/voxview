(in-package :voxview)

(declaim (type varjo.internals:vertex-stage *vertex-pass-0*))
(defparameter *vertex-pass-0*
  (varjo:make-stage
   :vertex
   '((position :vec3)   ; Position of a voxel in the world system, changed once by an instance
     (vertex   :vec3))  ; Position of a voxel's vertex in the model system
   '((transform :mat4)  ; World -> Light source transform
     (nvoxels   :vec3)) ; Model space dimensions (in voxels)
   '(:450)
   '((let* ((eps (* 0.1 (/ 2 nvoxels)))
            (recip (/ nvoxels))
            (coord (+ (* 2 position recip) -1
                      recip
                      (* vertex (+ eps recip)))))
       (* transform (vari:vec4 coord 1)))))) ; gl_Position

(declaim (type varjo.internals:fragment-stage *fragment-pass-0*))
(defparameter *fragment-pass-0*
  (varjo:make-stage
   :fragment
   '()
   '()
   '(:450)
   '((values))))

(defparameter *pass-0*
  (varjo:rolling-translate
   (list *vertex-pass-0*
         *fragment-pass-0*)))

(declaim (type varjo.internals:vertex-stage *vertex-pass-1*))
(defparameter *vertex-pass-1*
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
        normal)))))

(declaim (type varjo.internals:fragment-stage *fragment-pass-1*))
(defparameter *fragment-pass-1*
  (varjo:make-stage
   :fragment
   '((normal :vec3))
   '((light-direction :vec3)
     (light-color    :vec3))
   '(:450)
   '((let ((cosphi (vari:dot light-direction normal)))
       (vari:vec4
        (* light-color
           (+ 0.2 (* 0.8 (vari:clamp cosphi 0 1))))
        1)))))

(defparameter *pass-1*
  (varjo:rolling-translate
   (list *vertex-pass-1*
         *fragment-pass-1*)))
