(in-package :voxview)

(varjo:define-vari-function transform-coords ((position :vec3)
                                              (vertex   :vec3)
                                              (divisor  :float))
  (+ (/ (* position 2) divisor) -1
     (/ (1+ vertex) divisor)))

(varjo:define-vari-function illumination ((vector :vec4) (sampler :sampler-2d))
  (let* ((step (/ 1.0 (vari:texture-size sampler 0)))
         (illumination 0.0)
         (%normalized (/ (vari:swizzle vector :xyz)
                         (vari:swizzle vector :w)))
         (normalized (/ (1+ %normalized) 2))
         (current-depth (vari:swizzle normalized :z))
         (shadowmap-coords (vari:swizzle normalized :xy)))
    (dotimes (i 5)
      (dotimes (j 5)
        (let ((sample (vari:swizzle
                       (vari:texture
                        sampler (+ shadowmap-coords
                                   (* (vari:vec2 (- i 2)
                                                 (- j 2))
                                      step)))
                       :r)))
          (incf illumination (if (< (- current-depth 0.0005) sample) 1 0)))))
    (/ illumination 25)))

;; Pass 0: Rendering shadows

(declaim (type varjo.internals:vertex-stage *vertex-pass-0*))
(defparameter *vertex-pass-0*
  (varjo:make-stage
   :vertex
   '((position :vec3)  ; Position of a voxel in the world system.
     (label    :uint)  ; Label of a voxel. Not used in this stage
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
   '((nvoxels    :float) ; Maximal dimension of a scene
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
(defparameter *vertex-pass-1*
  (varjo:make-stage
   :vertex
   '((position :vec3)  ; Position of a voxel in the world system.
     (label    :uint)  ; Label of a voxel. Pass-through
     (mask     :uint)) ; Connectivity mask. Used bits are 0 to 7. Pass-through
   '()
   '(:430)
   ;; A simple pass-through shader
   '((values (vari:vec4 position 1) label mask))
   t :points))

(declaim (type varjo.internals:geometry-stage *geometry-pass-1*))
(defparameter *geometry-pass-1*
  (varjo:make-stage
   :geometry
   '((label (:uint *))
     (mask  (:uint *)))
   '((nvoxels      :float) ; The same meaning as in the first pass
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
                              (:flat (aref label 0))
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
     (label      :uint :flat)
     (normal     :vec3)
     (light-proj :vec4))
   '((light-position  :vec3)
     (texture-sampler :sampler-3d)
     (shadow-sampler  :sampler-2d)
     (palette-sampler :sampler-buffer))
   '(:430)
   `((let* ((r (- light-position coord))
            (cosphi (/ (vari:dot r normal) (vari:length r)))
            (texture-coord (/ (1+ coord) 2))
            (texture-color (vari:swizzle (vari:texture texture-sampler texture-coord) :r))
            (palette-color (vari:swizzle
                            (vari:texel-fetch
                             palette-sampler
                             (glsl-symbols.operators:% (vari:int label)
                                                       ,+palette-color-number+))
                            :rgb)))
       (vari:vec4
        (* palette-color
           (+ (* 0.3
                 (illumination light-proj shadow-sampler) ; Determine if we are illuminated
                 (vari:clamp cosphi 0 1))                 ; Add diffuse light
              (* 0.7 texture-color)))                     ; Ambient light
        1)))))

(defparameter *pass-1*
  (varjo:rolling-translate
   (list *vertex-pass-1*
         *geometry-pass-1*
         *fragment-pass-1*)))


;; Light source shader
(declaim (type varjo.internals:vertex-stage *vertex-light-source*))
(defparameter *vertex-light-source*
  (varjo:make-stage
   :vertex
   '() ; No inputs
   '((light-position :vec3)
     (projection     :mat4))
   '(:430)
   '((let* ((x (vari:swizzle light-position :x))
            (y (vari:swizzle light-position :y))
            (z (vari:swizzle light-position :z))
            (v1 (vari:vec3 z 0 (- x)))
            (v2 (vari:vec3 (- (* x y)) (+ (expt x 2) (expt z 2)) (- (* y z))))
            (scale 0.08)
            (m (vari:mat3 (* scale (vari:normalize v1))
                          (* scale (vari:normalize v2))
                          light-position))
            (points (vector (vari:vec3 -1 -1 1) (vari:vec3 1 -1 1) (vari:vec3 0 1 1))))
       (* projection
          (vari:vec4 (* m (aref points vari:gl-vertex-id)) 1))))))

(declaim (type varjo.internals:fragment-stage *fragment-light-source*))
(defparameter *fragment-light-source*
  (varjo:make-stage
   :fragment
   '()
   '()
   '(:430)
   '((vari:vec4 1 0 0 1))))

(defparameter *light-source-shaders*
  (varjo:rolling-translate
   (list *vertex-light-source*
         *fragment-light-source*)))
