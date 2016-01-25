(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (sequence-crossmix tm
                                 (map 'vector
                                      (lambda (p) (apply 'partial p))
                                      '((tri 128)
                                        (tri 224)
                                        (tri 192)
                                        (tri 150)
                                        (tri 256)))
                                 1/2 1 'fade)))

(play-vec (sample-region 'the-sound 0.0 10.0))
