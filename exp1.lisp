(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (sequence-cut tm
                            (map 'vector
                                 (lambda (hz) (partial 'osc hz))
                                 '(440 220 280 320 500))
                            1/6)))

(play-vec (sample-region 'the-sound 0.0 5.0))
