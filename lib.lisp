(ql:quickload :cl-sndfile)

; UTILITIES
(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
    ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun to-signed (x)
  (- 1 (* 2 x)))
(defun to-unsigned (x)
  (+ 0.5 (* 0.5 x)))

; PLAYBACK
(defconstant +sample-rate+ 44100d0)
(defconstant +num-channels+ 2)

(defun write-vec (input filename)
  (sf:with-open-sndfile (snd filename
                             :direction :output
                             :chanls +num-channels+
                             :sr (floor +sample-rate+))
                        (sf:write-frames-float (flatten input) snd)))

(defun sample-region (fun start end)
  (loop for i from 0 below (round (* (- end start) +sample-rate+))
        collect (map 'list (lambda (x) (coerce x 'single-float))
                     (funcall fun (+ start (/ i +sample-rate+))))))

; MIXING
(defun channel-up (x)
  (su:dup x +num-channels+))

(defgeneric mix-frames (a b))

(defmethod mix-frames ((a number) (b number))
  (+ a b))

(defmethod mix-frames ((a list) (b number))
  (map 'list (partial '+ b) a))

(defmethod mix-frames ((a number) (b list))
  (mix-frames b a))

(defmethod mix-frames ((a list) (b list))
  (map 'list '+ a b))

(defun sum-tracks (tracks)
  (if (null (cdr tracks))
    (car tracks)
    (mix-frames (car tracks) (sum-tracks (cdr tracks)))))

(defun mix-tracks (tracks)
  (/ (sum-tracks tracks) (length tracks)))

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

(defgeneric fade (frame fader))

(defmethod fade ((frame number) (fader number))
  (* frame fader))

(defmethod fade ((frame list) (fader number))
  (map 'list (partial '* fader) frame))

(defmethod fade ((frame list) (fader list))
  (map 'list '* frame fader))

(defun normalize (samples)
  (let ((maximum (reduce 'max (map 'list 'abs (flatten samples)))))
    (if (= 0 maximum)
      samples
      (loop for frame in samples
            collect (fade frame (/ 1 maximum))))))

; SEQUENCING
(defun sequence-cut (tm tracks interval)
  (funcall (aref tracks (mod (floor (/ tm interval)) (length tracks))) tm))

(defun sequence-cut-zero-index (tm tracks interval)
  (funcall (aref tracks (mod (floor (/ tm interval)) (length tracks))) (mod tm interval)))

(defun sequence-crossmix (tm tracks interval crossover mixer)
  (let ((fade (min 1 (/ (mod tm interval) (* interval crossover)))))
    (mix-frames (funcall mixer
                         (sequence-cut tm tracks interval)
                         (- 1 fade))
                (funcall mixer
                         (sequence-cut (+ tm interval) tracks interval)
                         fade))))

; SYNTHESIS
(defun osc (hz tm)
  (sin (* hz tm 2 pi)))

(defun tri (hz tm)
  (to-signed (abs (to-signed (mod (* hz tm) 1)))))

(defun squ (hz tm)
  (to-signed (round (mod (* hz tm) 1))))

(defun saw (hz tm)
  (to-signed (mod (* hz tm) 1)))

(defun pulse (hz width tm)
  (to-signed (floor (+ width (mod (* hz tm) 1)))))
