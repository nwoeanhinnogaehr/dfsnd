(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-portaudio)
(use-package :portaudio)

(defun partial (func &rest args1)
  (lambda (&rest args2) (apply func (append args1 args2))))

(defconstant +frames-per-buffer+ 2048)
(defconstant +sample-rate+ 44100d0)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 2)

(defun num-channels (x)
  (array-dimension x 0))
(defun num-samples (x)
  (array-dimension x 1))

(defun get-buffer-region (in out start)
  (dotimes (i +frames-per-buffer+)
    (dotimes (j +num-channels+)
      (setf (aref out j i)
            (let ((idx (+ i (* start +frames-per-buffer+))))
              (if (<= (num-samples in) idx)
                0.0
                (aref in j idx)))))))

(defun play-vec (input)
  (format t "Playing ~D samples.~%" (num-samples input))
  (let ((buffer (make-array (list +num-channels+ +frames-per-buffer+))))
    (with-audio
      (with-default-audio-stream (astream +num-channels+ +num-channels+ :sample-format +sample-format+ :sample-rate +sample-rate+ :frames-per-buffer +frames-per-buffer+)
                                 (dotimes (i (round (/ (num-samples input) +frames-per-buffer+)))
                                   (get-buffer-region input buffer i)
                                   (write-stream astream
                                                 (merge-channels-into-array astream buffer)))))))

(defun sample-region (fun start end)
  (let* ((num-samples (round (* (- end start) +sample-rate+)))
         (buffer (make-array (list +num-channels+ num-samples))))
    (dotimes (i num-samples)
      (let ((frame (funcall fun (+ start (/ i +sample-rate+)))))
        (dotimes (j +num-channels+)
          (setf (aref buffer j i)
                (coerce (aref frame j) 'single-float)))))
    buffer))

(defun channel-up (x)
  (make-array +num-channels+ :initial-element x))

(defun mix-frames (a b)
  (if (typep a 'vector)
    (map 'vector '+ a b)
    (+ a b)))

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
                           (+ 0.5 (* 0.5 (sin (* angle 2 pi)))))))
    (if (null (cdr tracks))
      mixed
      (mix-frames mixed
                  (stereo-disperse-tracks* (cdr tracks)
                                           (+ angle offset)
                                           offset
                                           n)))))

(defun stereo-disperse-tracks (tracks angle)
  (stereo-disperse-tracks* tracks angle (/ 1 (length tracks)) (length tracks)))

(defun sequence-cut (tm tracks interval)
  (funcall (aref tracks (mod (floor (/ tm interval)) (length tracks))) (mod tm interval)))

(defun osc (hz tm)
  (sin (* hz tm 2 pi)))

(defun tri (hz tm)
  (- (abs (- (* 4 (mod (* hz tm) 1)) 2)) 1))

(defun squ (hz tm)
  (- (* 2 (round (mod (* hz tm) 1))) 1))

(defun saw (hz tm)
  (- (* 2 (mod (* hz tm) 1)) 1))

(defun pulse (hz width tm)
  (- (* 2 (floor (+ width (mod (* hz tm) 1)))) 1))
