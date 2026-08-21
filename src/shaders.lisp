(in-package :voxview)

(varjo:define-vari-function calculate-color ((light-position  :vec3)
                                             (coord           :vec3)
                                             (texture-sampler :sampler-3d)
                                             (illumination    :float))
  (let* ((r (- light-position coord))
         ;; What a naming! D-FDX!
         (normal (vari:normalize
                  (vari:cross (vari:d-fdx coord)
                              (vari:d-fdy coord))))
         (cosphi (/ (vari:dot r normal) (vari:length r)))
         (texture-coord (/ (1+ coord) 2))
         (texture-color (vari:swizzle (vari:texture texture-sampler texture-coord) :r)))
    (+ (* 0.3
          illumination                             ; Determines if we are illuminated
          (vari:clamp cosphi 0 1))                 ; Add diffuse light
       (* 0.7 texture-color))))                    ; Ambient light

(varjo:define-vari-function palette-color ((label :uint) (sampler :sampler-buffer))
  (vari:swizzle
   (vari:texel-fetch
    sampler (glsl-symbols.operators:%
             (vari:int label)
             #.+palette-color-number+))
   :rgb))

(varjo:define-vari-function illumination ((coord-light-proj :vec4) (sampler :sampler-2d))
  (let* ((step (/ 1.0 (vari:texture-size sampler 0)))
         (illumination 0.0)
         (%normalized (/ (vari:swizzle coord-light-proj :xyz)
                         (vari:swizzle coord-light-proj :w)))
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
   '((position     :vec3)  ; Position of a vertex in the world system.
     (label        :uint)) ; Label of a voxel. Not used in this stage
   '((l-projection :mat4)  ; On screen projection operator
     (cp           :vec4)) ; Cutting plane
   '(:430)
   `((let ((pos4 (vari:vec4 position 1)))
       (values
        (* l-projection pos4)
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
     (let ((palette-color (if use-color-p
                              (palette-color label palette-sampler)
                              (vari:vec3 1))))
       (vari:vec4
        (* palette-color
           (calculate-color
            light-position coord texture-sampler
            (illumination light-proj shadow-sampler)))
        1)))))

(defparameter *pass-1*
  (varjo:rolling-translate
   (list *vertex-pass-1*
         *fragment-pass-1*)))

;; Pass 2: Prepare to render the slice
;; TODO: Lighting
(declaim (type varjo.internals:vertex-stage *vertex-pass-2*))
(defparameter *vertex-pass-2*
  (varjo:make-stage
   :vertex
   '((position     :vec3)
     (label        :uint)) ; Not used
   '((c-projection :mat4)  ; On screen projection operator
     (cp           :vec4)) ; Cutting plane
   '(:430)
   `((let ((pos4 (vari:vec4 position 1)))
       (values (* c-projection pos4)
               (vari:dot cp pos4))))))

(declaim (type varjo.internals:fragment-stage *fragment-pass-2*))
(defparameter *fragment-pass-2*
  (varjo:make-stage
   :fragment
   '((cp-distance :float))
   '()
   '(:430)
   ;; This shader has no output
   `((when (< cp-distance 0)
       (vari:discard)))))

(defparameter *pass-2*
  (varjo:rolling-translate
   (list *vertex-pass-2*
         *fragment-pass-2*)))

;; Render the cutting plane
(declaim (type varjo.internals:vertex-stage *vertex-plane*))
(defparameter *vertex-plane*
  (varjo:make-stage
   :vertex
   '()
   '((c-projection :mat4)  ; Camera->screen projection
     (l-projection :mat4)  ; Light->shadow map projection
     (v1           :vec3)  ; Just any random vector 1
     (v2           :vec3)  ; Just any random vector 2
     (cp           :vec4)) ; Equation for the cutting plane
   '(:430)
   `((let* ((points (vector
                     (vari:vec3 -2 -2 +1) ; Make it bigger than -1, +1 box
                     (vari:vec3 +2 -2 +1)
                     (vari:vec3 -2 +2 +1)
                     (vari:vec3 +2 +2 +1)))
            ;; Normal to the cutting plane
            (n (vari:swizzle cp :xyz))
            ;; Distance between the cutting plane and the origin
            (d (- (vari:swizzle cp :w)))
            ;; The first tangent vector
            (t1 (vari:normalize (- v1 (* (vari:dot v1 n) n))))
            ;; The second tangent vector. t1, t2 form the tangent space
            (t2 (vari:normalize (- v2 (* (vari:dot v2 n) n) (* (vari:dot v2 t1) t1))))
            ;; Transform planar (u, v, 1) coordinates into world (x, y, z) coords
            (m (vari:mat3 t1 t2 (* n d)))
            ;; Find a point on a cutting plane
            (point (* m (aref points vari:gl-vertex-id)))
            (point4 (vari:vec4 point 1)))
       (values
        (* c-projection point4)
        ;; Position of a point in the world system
        point
        ;; + Also projection of this vertex onto the shadow map.
        (* l-projection point4))))))

(declaim (type varjo.internals:fragment-stage *fragment-plane*))
(defparameter *fragment-plane*
  (varjo:make-stage
   :fragment
   '((coord       :vec3)
     (light-proj  :vec4))
   '((light-position  :vec3)
     (texture-sampler :sampler-3d)
     (shadow-sampler  :sampler-2d))
   '(:430)
   `((vari:vec4
      (vari:vec3
       (calculate-color
        light-position coord texture-sampler
        (illumination light-proj shadow-sampler)))
      1))))

(defparameter *plane-shaders*
  (varjo:rolling-translate
   (list *vertex-plane*
         *fragment-plane*)))

;; Light source shader
(declaim (type varjo.internals:vertex-stage *vertex-light-source*))
(defparameter *vertex-light-source*
  (varjo:make-stage
   :vertex
   '() ; No inputs
   '((light-position :vec3)
     (c-projection   :mat4))
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
       (* c-projection
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
