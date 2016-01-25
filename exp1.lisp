(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (saw 128 tm)))

(write-vec (sample-region 'the-sound 0.0 10.0) "out.wav")
