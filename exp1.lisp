(load "lib.lisp")

(defun snare (vel len tm)
  (fade
    (list (noise) (noise))
    (* vel (expt (+ 1 tm) len))))

(defun hat (vel len tm)
  (fade
    (sum-frames (list (max 0.7 (noise)) (max 0.7 (noise))) -0.7)
    (* vel (expt (+ 1 tm) len))))

(defun kick (vel tm)
  (channel-up
    (fade
      (osc (note-freq (+ (* 4 (expt (+ 1 tm) -50)) -2) 1) tm)
      (* vel (expt (+ 1 tm) -5)))))

(defun beep (note vel spd tm)
  (let ((hz (note-freq (- note 10) 4)))
    (fade
      (stereo-disperse-tracks (list (pulse hz 0.5 tm) (pulse hz (fract tm) tm)) tm)
      (* vel (to-unsigned (squ spd tm))))))

(defun the-sound (tm)
  (mix-frames
    (loop-beat #((0 0.2 2) (0 0 0) (4 0.4 3) (4 0.3 7) (2 0.3 5)) 'beep 5/4 tm)
    (loop-beat #((1 -50) (0 0) (0.5 -10) (0.25 -40) (0 0)) 'snare 1/4 tm)
    (loop-beat #((1 -100) (0 0) (0.25 -100) (1 -70)) 'hat 1/7 tm)
    (loop-beat #((1) (0) (0) (0.5) (0) (0.5) (0) (1)) 'kick 1/4 tm)))

(write-vec (normalize (sample-region 'the-sound 0.0 12.0)) "out.wav")
