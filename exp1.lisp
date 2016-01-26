(load "lib.lisp")

(defun saw-harmonics (hz tm n)
  (loop for i from 1 to n
        collect (/ (osc (* hz i) tm) i)))

(defun the-sound (tm)
  (stereo-disperse-tracks* (saw-harmonics 64 tm 200) tm (* tm 0.2) 200))

(write-vec (normalize (sample-region 'the-sound 0.0 5.0)) "out.wav")
