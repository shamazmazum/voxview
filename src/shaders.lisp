(in-package :voxview)

(defconstant +n-planes+ 512)

(defparameter *vertex-shader*
  (varjo:make-stage
   :vertex
   '()                   ; No input
   '((projection :mat4)  ; On screen projection operator
     (cp         :vec4)  ; Cutting plane
     (planar     :mat3)) ; Transform from planar space to the world space
   '(:430)
   ;; This is a very simple case with planes which are always
   ;; perpendicular to the Z axis.
   `((let* ((square (vector
                     (vari:vec2 -1.0 -1.0)
                     (vari:vec2 +1.0 -1.0)
                     (vari:vec2 -1.0 +1.0)
                     (vari:vec2 +1.0 +1.0)))
            (svertex (aref square vari:gl-vertex-id))
            (dist (- (* 2 (/ (float vari:gl-instance-id) ,+n-planes+)) 1))
            (vertex (* planar (* (vari:vec3 svertex dist) 2)))
            (vertex4 (vari:vec4 vertex 1)))
       (values
        (* projection vertex4)
        (vari:dot cp vertex4)
        vertex)))))

(defparameter *fragment-shader*
  (varjo:make-stage
   :fragment
   '((cp-dist       :float)      ; Distance to the cutting plane
     (coord         :vec3))      ; Fragment coordinate in the world space
   '((model-texture :sampler-3d) ; Voxel data
     (colormap      :sampler-1d) ; Colormap sampler
     (min           :float)      ; Minimum density value in the image
     (max           :float)      ; Maximum density value in the image
     (threshold     :float)      ; Minimum density for fragment to be visible
     (multiplier    :float)      ; Density alpha multiplier
     (use-cp-p      :bool))      ; Do we use cutting plane?
   '(:430)
   `((when (and use-cp-p (< cp-dist 0))
       ;; Discard fragments behind the cutting plane
       (vari:discard))
     (let* ((min (vari:vec3 -1.0))
            (max (vari:vec3 +1.0))
            (clamped (vari:clamp coord min max)))
       (when (not (vari:all (vari:equal coord clamped)))
         ;; Discard fragments outside (vec3 -1) ... (vec3 +1) cube
         (vari:discard)))
     (let* ((tex-coord (/ (1+ coord) 2))
            (density (vari:swizzle (vari:texture model-texture tex-coord) :r)))
       (when (< density threshold)
         ;; Discard too transparent fragments
         (vari:discard))
       (vari:vec4
        (vari:swizzle
         (vari:texture
          colormap
          (/ (- density min) (- max min))) ; Normalized density
         :rgb)
        (* multiplier density))))))

(defparameter *shaders*
  (varjo:rolling-translate
   (list *vertex-shader*
         *fragment-shader*)))
