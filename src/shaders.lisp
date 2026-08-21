(in-package :voxview)

(defconstant +n-planes+ 512)

(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '()                   ; No input
   '((projection :mat4)  ; On screen projection operator
     (cp         :vec4)) ; Cutting plane
   '(:430)
   ;; This is a very simple case with planes which are always
   ;; perpendicular to the Z axis.
   `((let* ((square (vector
                     (vari:vec2 -1.0 -1.0)
                     (vari:vec2 +1.0 -1.0)
                     (vari:vec2 -1.0 +1.0)
                     (vari:vec2 +1.0 +1.0)))
            (svertex (aref square vari:gl-vertex-id))
            (z (- (* 2 (/ (float vari:gl-instance-id) ,+n-planes+)) 1))
            (vertex (vari:vec3 svertex z)))
       (values
        (* projection (vari:vec4 vertex 1))
        vertex)))))

(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '((coord         :vec3))      ; Fragment coordinate in the world space
   '((model-texture :sampler-3d) ; Voxel data
     (threshold     :float)      ; Minimum density for fragment to be visible
     (multiplier    :float))     ; Density alpha multiplier
   '(:430)
   `(;; We will need this check in the future
     (let* ((min (vari:vec3 -1.0))
            (max (vari:vec3 +1.0))
            (clamped (vari:clamp coord min max)))
       (when (not (vari:all (vari:equal coord clamped)))
         (vari:discard)))
     (let* ((tex-coord (/ (1+ coord) 2))
            (density (vari:swizzle (vari:texture model-texture tex-coord) :r)))
       (when (< density threshold)
         (vari:discard))
       (vari:vec4
        (vari:mix
         (vari:vec3 1.0 0.0 0.0)
         (vari:vec3 0.0 0.0 1.0)
         (vari:clamp (* 3 density) 0 1))
        (* multiplier density))))))

(defparameter *shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))
