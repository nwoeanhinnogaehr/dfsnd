(load "lib.lisp")

(defun add-square (tm hz n)
  (if (= n 0)
    0.0
    (+ (/ (osc tm (* hz n 2)) n) (add-square tm (+ hz (* n (sin (+ n tm)))) (- n 1)))))

(defun the-sound (tm)
  (channel-up (add-square tm 30 15)))

(play-vec (sample-region 'the-sound 0.0 5.0))
