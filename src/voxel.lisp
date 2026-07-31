(in-package :voxview)

(varjo:define-vari-macro with-vertices-bound (vertices-sym &body body)
  `(let ((,vertices-sym
          (vector
           ;; x = -1
           (vector
            (vari:vec3 -1 -1 -1)
            (vari:vec3 -1 -1  1)
            (vari:vec3 -1  1 -1)
            (vari:vec3 -1  1  1))
           ;; x = 1
           (vector
            (vari:vec3  1 -1 -1)
            (vari:vec3  1  1 -1)
            (vari:vec3  1 -1  1)
            (vari:vec3  1  1  1))
           ;; y = -1
           (vector
            (vari:vec3 -1 -1 -1)
            (vari:vec3  1 -1 -1)
            (vari:vec3 -1 -1  1)
            (vari:vec3  1 -1  1))
           ;; y = 1
           (vector
            (vari:vec3 -1  1 -1)
            (vari:vec3 -1  1  1)
            (vari:vec3  1  1 -1)
            (vari:vec3  1  1  1))
           ;; z = -1
           (vector
            (vari:vec3 -1 -1 -1)
            (vari:vec3 -1  1 -1)
            (vari:vec3  1 -1 -1)
            (vari:vec3  1  1 -1))
           ;; z = 1
           (vector
            (vari:vec3 -1 -1  1)
            (vari:vec3  1 -1  1)
            (vari:vec3 -1  1  1)
            (vari:vec3  1  1  1)))))
     ,@body))
