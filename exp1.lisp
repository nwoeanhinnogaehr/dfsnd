(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (sequence-cut tm
                            (map 'vector
                                 (lambda (p) (apply 'partial p))
                                 (list (list 'pulse 120 (abs (osc tm 1)))
                                       (list 'pulse 300 (abs (osc tm 1/2)))
                                       (list 'pulse 160 (abs (osc tm 1/3)))
                                       (list 'tri 140)
                                       (list 'pulse 60 (abs (osc tm 1/4)))
                                       (list 'pulse 60 (abs (osc tm 1/4)))))
                            1/4)))

(play-vec (sample-region 'the-sound 0.0 10.0))
