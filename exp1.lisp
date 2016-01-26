(load "lib.lisp")

(defun the-sound (tm)
  (channel-up (sequence-crossmix tm
                                 (lambda (n)
                                   (partial 'tri (note-freq (floor (* 9 (tri n 1/16))) 9)))
                                 1/16
                                 1/4
                                 'fade)))

(write-vec (normalize (sample-region 'the-sound 0.0 5.0)) "out.wav")
