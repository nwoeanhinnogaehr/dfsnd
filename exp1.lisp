(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (mix-tracks (list (osc tm 100) (osc tm 225) (osc tm 350) (osc tm 475)))))

(play-vec (sample-region 'the-sound 0.0 5.0))
