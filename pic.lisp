;;; Used to generate the picture in the README.
;;; Uses the 'sketch' graphics library, and runs from
;;; within the sketch package.

(defsketch noise-test
            ((width 400)
             (height 400)
             (curve-height 100))
          (background +white+)
          (loop repeat 4
                for offset = 0 then (+ offset curve-height)
                for (lod falloff) in '((4 0.5)
                                       (2 0.5)
                                       (1 1)
                                       (8 0.3))
                do (noisy:noise-detail :lod lod :falloff falloff)
                do (with-pen (make-pen :stroke +black+)
                     (apply #'polyline
                            (apply #'append
                                    (loop for x = 0 then (+ x 5)
                                          while (<= x width)
                                          collect (list x
                                                        (+ offset
                                                           (* curve-height
                                                              (noisy:noise (* 0.04 x))))))))))
          (stop-loop))
