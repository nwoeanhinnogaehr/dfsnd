(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (sequence-crossmix tm
                                 (map 'vector
                                      (lambda (p) (apply 'partial p))
                                      (list (list 'tri 128)
                                            (list 'tri 224)
                                            (list 'tri 192)
                                            (list 'tri 256)))
                                 1/4 1/4 'fade)))

(play-vec (sample-region 'the-sound 0.0 10.0))
