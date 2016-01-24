(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (osc tm 440)))

(play-vec (sample-region 'the-sound 0.0 5.0))
