(ql:quickload :cl-sndfile)

; UTILITIES
(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))

(defun to-signed (x)
  (- 1 (* 2 x)))
(defun to-unsigned (x)
  (+ 0.5 (* 0.5 x)))

; PLAYBACK
(defconstant +sample-rate+ 44100d0)
(defconstant +num-channels+ 2)

(defun num-channels (x)
  (array-dimension x 0))
(defun num-samples (x)
  (array-dimension x 1))

(defun get-frame-list (input i j)
  (if (= j 0)
    nil
    (cons (aref input (- j 1) i) (get-frame-list input i (- j 1)))))

(defun write-vec (input filename)
  (sf:with-open-sndfile (snd filename
                             :direction :output
                             :chanls +num-channels+
                             :sr (floor +sample-rate+))
                        (sf:write-frames-float
                          (loop for i from 0 below (num-samples input)
                                append (get-frame-list input i +num-channels+))
                          snd)))

(defun sample-region (fun start end)
  (let* ((num-samples (round (* (- end start) +sample-rate+)))
         (buffer (make-array (list +num-channels+ num-samples))))
    (dotimes (i num-samples)
      (let ((frame (funcall fun (+ start (/ i +sample-rate+)))))
        (dotimes (j +num-channels+)
          (setf (aref buffer j i)
                (coerce (aref frame j) 'single-float)))))
    buffer))

; MIXING
(defun channel-up (x)
  (make-array +num-channels+ :initial-element x))

(defgeneric mix-frames (a b))

(defmethod mix-frames ((a number) (b number))
  (+ a b))

(defmethod mix-frames ((a vector) (b number))
  (map 'vector (partial '+ b) a))

(defmethod mix-frames ((a number) (b vector))
  (mix-frames b a))

(defmethod mix-frames ((a vector) (b vector))
  (map 'vector '+ a b))

(defun sum-tracks (tracks)
  (if (null (cdr tracks))
    (car tracks)
    (mix-frames (car tracks) (sum-tracks (cdr tracks)))))

(defun mix-tracks (tracks)
  (/ (sum-tracks tracks) (length tracks)))

(defun stereo-pan (frame pos)
  (vector (* pos frame) (* (- 1 pos) frame)))

(defun stereo-disperse-tracks* (tracks angle offset n)
  (let ((mixed (stereo-pan (/ (car tracks) n)
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

(defun fade (frame fader)
  (if (typep frame 'vector)
    (map 'vector (partial '* fader) frame)
    (* frame fader)))

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
