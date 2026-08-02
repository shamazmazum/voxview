(in-package :voxview)

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
          (incf illumination (if (< current-depth sample) 1 0)))))
    (/ illumination 25)))

;; Pass 0: Rendering shadows

(declaim (type varjo.internals:vertex-stage *vertex-pass-0*))
(defparameter *vertex-pass-0*
  (varjo:make-stage
   :vertex
   '((position   :vec3)  ; Position of a vertex in the world system.
     (label      :uint)) ; Label of a voxel. Not used in this stage
   '((projection :mat4)  ; On screen projection operator
     (cp         :vec4)) ; Cutting plane
   '(:430)
   `((let ((pos4 (vari:vec4 position 1)))
       (values
        (* projection pos4)
        (vari:dot pos4 cp))))))

(declaim (type varjo.internals:fragment-stage *fragment-pass-0*))
(defparameter *fragment-pass-0*
  (varjo:make-stage
   :fragment
   '((cp-distance :float))
   '((use-cp-p    :bool))
   '(:450)
   '((when (and use-cp-p (< cp-distance 0))
       (vari:discard))
     (values))))

(defparameter *pass-0*
  (varjo:rolling-translate
   (list *vertex-pass-0*
         *fragment-pass-0*)))

;; Pass 1: Render the scene

(declaim (type varjo.internals:vertex-stage *vertex-pass-1*))
(defparameter *vertex-pass-1*
  (varjo:make-stage
   :vertex
   '((position     :vec3)  ; Position of a vertex in the world system.
     (label        :uint)) ; Label of a voxel. Pass-through
   '((c-projection :mat4)  ; Camera->screen projection
     (l-projection :mat4)  ; Light->shadow map projection
     (cp           :vec4)) ; Cutting plane
   '(:430)
   `((let ((pos4 (vari:vec4 position 1)))
       (values
        (* c-projection pos4)
        position
        (:flat label)
        ;; + Also projection of this vertex onto the shadow map.
        (* l-projection pos4)
        ;; And distance to the cutting plane
        (vari:dot pos4 cp))))))

(declaim (type varjo.internals:fragment-stage *fragment-pass-1*))
(defparameter *fragment-pass-1*
  (varjo:make-stage
   :fragment
   '((coord       :vec3)
     (label       :uint :flat)
     (light-proj  :vec4)
     (cp-distance :float))
   '((light-position  :vec3)
     (texture-sampler :sampler-3d)
     (shadow-sampler  :sampler-2d)
     (palette-sampler :sampler-buffer)
     (use-color-p     :bool)
     (use-cp-p        :bool))
   '(:430)
   `((when (and use-cp-p (< cp-distance 0))
       (vari:discard))
     (let* ((r (- light-position coord))
            ;; What a naming! D-FDX!
            (normal (vari:normalize
                     (vari:cross (vari:d-fdx coord)
                                 (vari:d-fdy coord))))
            (cosphi (/ (vari:dot r normal) (vari:length r)))
            (texture-coord (/ (1+ coord) 2))
            (texture-color (vari:swizzle (vari:texture texture-sampler texture-coord) :r))
            (palette-color (if use-color-p
                               (vari:swizzle
                                (vari:texel-fetch
                                 palette-sampler
                                 (glsl-symbols.operators:% (vari:int label)
                                                           ,+palette-color-number+))
                                :rgb)
                               (vari:vec3 1))))
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
