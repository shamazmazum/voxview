(in-package :voxview)

(varjo:define-vari-function transform-coords ((position :vec3)
                                              (vertex   :vec3)
                                              (divisor  :vec3))
  (+ (/ (* position 2) divisor) -1
     (/ (1+ vertex) divisor)))

(varjo:define-vari-function in-shadow-p ((vector  :vec4)
                                         (sampler :sampler-2d))
  (let* ((normalized (/ (vari:swizzle vector :xyz)
                        (vari:swizzle vector :w)))
         (text-coords (/ (1+ normalized) 2))
         (sample (vari:swizzle
                  (vari:texture sampler (vari:swizzle text-coords :xy)) :r)))
    (> (- (vari:swizzle text-coords :z) 0.005) sample)))

;; Pass 0: Rendering shadows

(declaim (type varjo.internals:vertex-stage *vertex-pass-0*))
(defparameter *vertex-pass-0*
  (varjo:make-stage
   :vertex
   '((position :vec3)  ; Position of a voxel in the world system.
     (mask     :uint)) ; Connectivity mask. Used bits are 0 to 7
   '()
   '(:430)
   ;; A simple pass-through shader
   '((values (vari:vec4 position 1) mask))
   t :points))

(declaim (type varjo.internals:geometry-stage *geometry-pass-0*))
(defparameter *geometry-pass-0*
  (varjo:make-stage
   :geometry
   '((mask (:uint *)))   ; Connectivity mask (passed through the previous stage)
   '((nvoxels    :vec3)  ; Dimensionality of the scene
     (projection :mat4)) ; Projection matrix
   '(:430)
   '((declare (vari:output-primitive :kind :triangle-strip :max-vertices 26))
     (with-vertices-bound vertices
       (with-normals-bound normals
         (let ((connectivity (aref mask 0)))
           (dotimes (plane 6)
             (unless (zerop (logand connectivity (vari.cl::<< 1 plane)))
               (dotimes (vertex-idx 4)
                 (let ((coord (transform-coords
                               (vari:swizzle (vari:gl-position (aref vari:gl-in 0)) :xyz)
                               (aref (aref vertices plane) vertex-idx)
                               nvoxels)))
                   ;; Only emit coordinates of a voxel
                   (setf vari:gl-position
                         (* projection (vari:vec4 coord 1))))
                 (vari:emit-vertex))
               (vari:end-primitive)))))))
   t :points))

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
         *geometry-pass-0*
         *fragment-pass-0*)))

;; Pass 1: Render the scene

(declaim (type varjo.internals:vertex-stage *vertex-pass-1*))
;; Vertex stage is shared with the first pass
(defparameter *vertex-pass-1* *vertex-pass-0*)

(declaim (type varjo.internals:geometry-stage *geometry-pass-1*))
(defparameter *geometry-pass-1*
  (varjo:make-stage
   :geometry
   '((mask (:uint *)))
   '((nvoxels      :vec3)  ; The same meaning as in the first pass
     (c-projection :mat4)  ; Camera->screen projection
     (l-projection :mat4)) ; Light->shadow map projection
   '(:430)
   '((declare (vari:output-primitive :kind :triangle-strip :max-vertices 26))
     (with-vertices-bound vertices
       (with-normals-bound normals
           (let ((connectivity (aref mask 0)))
             (dotimes (plane 6)
               (unless (zerop (logand connectivity (vari.cl::<< 1 plane)))
                 (dotimes (vertex-idx 4)
                   (let ((coord (transform-coords
                                 (vari:swizzle (vari:gl-position (aref vari:gl-in 0)) :xyz)
                                 (aref (aref vertices plane) vertex-idx)
                                 nvoxels)))
                     (setf vari:gl-position
                           (* c-projection (vari:vec4 coord 1)))
                     ;; This shader is much like the shader in the
                     ;; first pass, only now it also emits coordinates
                     ;; of a vertex in the world space + normal vector.
                     (vari:emit-data
                      (values coord
                              (aref normals plane)
                              ;; + Also projection of this vertex onto the shadow map.
                              (* l-projection (vari:vec4 coord 1)))))
                   (vari:emit-vertex))
                 (vari:end-primitive)))))))
   t :points))

(declaim (type varjo.internals:fragment-stage *fragment-pass-1*))
(defparameter *fragment-pass-1*
  (varjo:make-stage
   :fragment
   '((coord      :vec3)
     (normal     :vec3)
     (light-proj :vec4))
   '((light-position  :vec3)
     (light-color     :vec3)
     (texture-sampler :sampler-3d)
     (shadow-sampler  :sampler-2d))
   '(:430)
   '((let* ((r (- light-position coord))
            (cosphi (/ (vari:dot r normal) (vari:length r)))
            (texture-coord (/ (1+ coord) 2))
            (texture-color (vari:swizzle (vari:texture texture-sampler texture-coord) :r)))
       (vari:vec4
        (+ (if (in-shadow-p light-proj shadow-sampler)
               (vari:vec3 0)                                ; We are in shadow, add nothing
               (* 0.3 light-color (vari:clamp cosphi 0 1))) ; Otherwise add diffuse light
           (* 0.7 texture-color))                           ; Ambient light
        1)))))

(defparameter *pass-1*
  (varjo:rolling-translate
   (list *vertex-pass-1*
         *geometry-pass-1*
         *fragment-pass-1*)))
