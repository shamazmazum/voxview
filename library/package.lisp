(defpackage voxview/library
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:si   #:stateless-iterators))
  (:export #:do-indices
           ;; Connectivity data
           #:connectivity
           #:connectivity-points
           #:connectivity-masks
           #:connectivity-max-dimension
           ;; Connectivity computation
           #:compute-connectivity
           ;; List zippers
           #:list-zipper
           #:stepper
           #:zipper-to-head
           #:current
           #:current-or-previous
           #:step-forward
           #:step-backward
           #:goto-element))
