(load "lib.lisp")

(defun square (tm hz n)
  (if (= n 0)
    nil
    (cons (osc tm (* hz n)) (square tm hz (- n 1)))))

(defun the-sound (tm)
  (stereo-disperse-tracks* (square tm 64 15) tm tm 15))

(play-vec (sample-region 'the-sound 0.0 5.0))
