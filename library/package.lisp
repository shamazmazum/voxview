(defpackage voxview/library
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:si   #:stateless-iterators))
  (:export #:do-indices
           ;; Model
           #:model
           #:model-points
           #:model-indices
           #:model-labels
           ;; Model computation
           #:compute-model
           ;; List zippers
           #:list-zipper
           #:stepper
           #:zipper-to-head
           #:current
           #:current-or-previous
           #:step-forward
           #:step-backward
           #:goto-element))
