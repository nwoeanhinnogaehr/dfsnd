(load "lib.lisp")

(defun snare (tm)
  (fade
    (list (noise) (noise))
    (expt (+ 1 tm) -20)))

(defun kick (tm)
  (channel-up
    (fade
      (osc (note-freq (+ (* 4 (expt (+ 1 tm) -50)) -2) 1) tm)
      (expt (+ 1 tm) -5))))

(defun synth (tm)
  (let ((scale 12))
    (stereo-disperse-tracks
      (loop for i from 1 below 5
            collect (sequence-crossmix tm
                                       (lambda (n)
                                         (partial 'saw
                                                  (note-freq
                                                    (+ (- (* 1 scale)) (* i 6) (floor (* scale (tri n 1/16))))
                                                    scale)))
                                       (expt 2 (- i))
                                       (expt 2 (+ -4 i))
                                       'fade))
      (* tm 1/2))))

(defun the-sound (tm)
  (mix-tracks (list
                (synth tm)
                (snare (fract (+ tm 0.5)))
                (kick (fract tm)))))

(write-vec (normalize (sample-region 'the-sound 0.0 8.0)) "out.wav")
