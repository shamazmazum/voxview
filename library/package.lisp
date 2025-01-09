(defpackage voxview/library
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:si   #:stateless-iterators))
  (:export #:do-indices
           ;; Connectivity data
           #:connectivity-data
           #:connectivity-data-coord
           #:connectivity-data-mask
           ;; Connectivity computation
           #:compute-connectivity))
