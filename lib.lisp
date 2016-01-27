(ql:quickload :cl-sndfile)

; UTILITIES
(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))

(defun to-signed (x)
  (- 1 (* 2 x)))
(defun to-unsigned (x)
  (+ 0.5 (* 0.5 x)))

(defun fract (x)
  (mod x 1))

; PLAYBACK
(defconstant +sample-rate+ 44100d0)
(defconstant +num-channels+ 2)

(defun write-vec (input filename)
  (sf:with-open-sndfile (snd filename
                             :direction :output
                             :chanls +num-channels+
                             :sr (floor +sample-rate+))
                        (sf:write-frames-float input snd)))

(defun sample-region (fun start end)
  (loop for i from 0 below (round (* (- end start) +sample-rate+))
        append (map 'list (lambda (x) (coerce x 'single-float))
                    (funcall fun (+ start (/ i +sample-rate+))))))

(defun normalize (samples)
  (let ((maximum (reduce 'max (map 'list 'abs samples))))
    (if (= 0 maximum)
      samples
      (loop for frame in samples
            collect (/ frame maximum)))))

; MIXING
(defun channel-up (x)
  (su:dup x +num-channels+))

(defgeneric frame-apply (func frame &rest args))

(defmethod frame-apply (func (frame number) &rest args)
  (apply func (cons frame args)))

(defun non-atom-apply (func)
  (lambda (x)
    (if (atom x)
      x
      (funcall func x))))

(defmethod frame-apply (func (frame list) &rest args)
  (and frame
       (cons (apply func
                    (car frame)
                    (map 'list (non-atom-apply 'car) args))
             (apply 'frame-apply
                    func
                    (cdr frame)
                    (map 'list (non-atom-apply 'cdr) args)))))

(defun fade (frame fader)
  (frame-apply '* frame fader))

(defun sum-frames (&rest frames)
  (apply 'frame-apply '+ frames))

(defun mix-frames (&rest frames)
  (fade (apply 'sum-frames frames) (/ 1 (length frames))))

(defun stereo-pan (frame pos)
  (list (* pos frame) (* (- 1 pos) frame)))

(defun stereo-disperse-tracks* (tracks angle offset n)
  (let ((mixed (stereo-pan (car tracks)
                           (to-unsigned (sin (* angle 2 pi))))))
    (if (null (cdr tracks))
      mixed
      (mix-frames mixed
                  (stereo-disperse-tracks* (cdr tracks)
                                           (+ angle offset)
                                           offset
                                           n)))))

(defun stereo-disperse-tracks (tracks angle)
  (stereo-disperse-tracks* tracks angle (/ 1 (length tracks)) (length tracks)))

; SEQUENCING
(defgeneric sref (seq idx))

(defmethod sref ((seq sequence) idx)
  (elt seq (floor (mod idx (length seq)))))

(defmethod sref ((seq function) idx)
  (funcall seq (floor idx)))

(defun sequence-cut (idx tracks interval tm)
  (funcall (sref tracks (/ idx interval)) tm))

(defun sequence-crossmix (tm tracks interval crossover mixer)
  (let ((fade (min 1 (/ (mod tm interval) (* interval crossover)))))
    (mix-frames (funcall mixer
                         (sequence-cut tm tracks interval tm)
                         (- 1 fade))
                (funcall mixer
                         (sequence-cut (+ tm interval) tracks interval tm)
                         fade))))

(defun loop-beat (beat fun interval tm)
  (apply fun
         (append (sref beat (/ tm interval))
                 (list (mod tm interval)))))

; SYNTHESIS
(defun osc (hz tm)
  (sin (* hz tm 2 pi)))

(defun tri (hz tm)
  (to-signed (abs (to-signed (fract (* hz tm))))))

(defun squ (hz tm)
  (to-signed (round (fract (* hz tm)))))

(defun saw (hz tm)
  (to-signed (fract (* hz tm))))

(defun pulse (hz width tm)
  (to-signed (floor (+ width (fract (* hz tm))))))

(defun noise ()
  (- (random 2.0) 1))

; SCALES
(defun note-freq (note scale &key (base-freq 256) (base 2) (octave 0))
  (* (expt base (/ (+ note (* scale octave)) scale)) base-freq))
