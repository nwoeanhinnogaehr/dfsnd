(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (pulse 128 (+ 0.5 (* 0.5 (osc tm 1/4))) tm)))

(write-vec (sample-region 'the-sound 0.0 10.0) "out.wav")
