(in-package :voxview/library)

(defmacro do-indices ((array &rest indices) &body body)
  (let ((a (gensym)))
    `(let ((,a ,array))
       ,(first
         (si:foldr
          (lambda (enumerated-index acc)
            (destructuring-bind (i . var)
                enumerated-index
              `((loop for ,var fixnum below (array-dimension ,a ,i) do ,@acc))))
          body (si:enumerate (si:list->iterator indices)))))))
