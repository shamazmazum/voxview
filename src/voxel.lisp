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


(varjo:define-vari-macro with-normals-bound (normals-sym &body body)
  `(let ((,normals-sym
          (vector
           (vari:vec3 -1  0  0)
           (vari:vec3  1  0  0)
           (vari:vec3  0 -1  0)
           (vari:vec3  0  1  0)
           (vari:vec3  0  0 -1)
           (vari:vec3  0  0  1))))
     ,@body))
