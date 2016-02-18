(ql:quickload :cl-sndfile)
(ql:quickload :lparallel)

(setf lparallel:*kernel* (lparallel:make-kernel 8))

; UTILITIES
(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))

(defun to-signed (x)
  (- (* 2 x) 1))
(defun to-unsigned (x)
  (+ 0.5 (* 0.5 x)))

(defun fract (x)
  (mod x 1))

; PLAYBACK
(defconstant +sample-rate+ 44100d0)
(defconstant +num-channels+ 2)

(defun write-vec (input filename)
  (sf:with-open-sndfile
    (snd filename
         :direction :output
         :chanls +num-channels+
         :sr (floor +sample-rate+))
    (sf:write-frames-float input snd)))

(defun sample-region (fun start end)
  (loop for i from 0 below (round (* (- end start) +sample-rate+))
        append (map 'list (lambda (x) (coerce x 'single-float))
                    (funcall fun (+ start (/ i +sample-rate+))))))

(defun normalize (samples)
  (let ((maximum (lparallel:preduce 'max (map 'list 'abs samples))))
    (if (= 0 maximum)
      samples
      (lparallel:pmap 'list (lambda (x) (/ x maximum)) samples))))

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
  (and frames
       (apply 'frame-apply '+ frames)))

(defun mix-frames (&rest frames)
  (and frames
       (fade (apply 'sum-frames frames) (/ 1 (length frames)))))

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

; BUFFERING
(defun make-buffer ()
  (make-array (floor (* +sample-rate+ 60)) :initial-element nil))

(defun read-buffer (buf tm fallback)
  (let ((idx (floor (+ (* tm +sample-rate+) (/ (length buf) 2)))))
    (assert (and (<= 0 idx) (> (length buf) idx)))
    (or (elt buf idx) (setf (aref buf idx) (funcall fallback tm)))))

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

; FILTERS
(defun gen-lowpass-coeff* (freq i d1)
  (if (= i 0)
    nil
    (cons (let ((d2 (- i d1)))
            (if (= d2 0)
              (/ freq pi)
              (/ (sin (* freq d2)) (* pi d2))))
          (gen-lowpass-coeff* freq (- i 1) d1))))

(defun gen-lowpass-coeff (cutoff n)
  (gen-lowpass-coeff* (/ (* 2 pi cutoff) +sample-rate+) n (/ (- n 1) 2)))

(defun apply-fir (sampler coeff tm)
  (if (null coeff)
    0
    (+ (* (funcall sampler tm) (car coeff))
       (apply-fir sampler (cdr coeff) (+ tm (/ 1 +sample-rate+))))))

; EVENTS

(defstruct event
  (time)
  (death)
  (inst)
  (args))

(defstruct queue
  (key)
  (ord)
  (data))

(defun insert-sorted (q x ord key)
  (cond
    ((null q) (list x))
    ((funcall ord (funcall key (car q)) (funcall key x))
     (cons (car q) (insert-sorted (cdr q) x ord key)))
    (t (cons x q))))

(defmacro queue-push (q items)
  `(setf (queue-data ,q)
         (reduce
           (lambda (queue item)
             (insert-sorted queue
                            item
                            (queue-ord ,q)
                            (queue-key ,q)))
           ,items
           :initial-value (queue-data ,q))))

(defun split-pred (q pred)
  (cond
    ((null q) '(nil nil))
    ((funcall pred (car q))
     (destructuring-bind (popped left) (split-pred (cdr q) pred)
       (list (cons (car q) popped) left)))
    (t (list nil q))))

(defmacro queue-pop (q tm)
  `(flet ((pred (a) (funcall (queue-ord ,q) (funcall (queue-key ,q) a) ,tm)))
     (when (and (queue-data ,q) (pred (car (queue-data ,q))))
       (destructuring-bind
         (popped left)
         (split-pred (queue-data ,q) #'pred)
         (setf (queue-data ,q) left)
         popped))))

(defun interval (interval func)
  (let ((trigger 0)
        (counter 0))
    (lambda (tm)
      (when (>= tm trigger)
        (incf trigger interval)
        (incf counter)
        (funcall func tm counter)))))

