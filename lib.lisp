(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-portaudio)
(use-package :portaudio)

(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 2)

(defun num-channels (x)
  (array-dimension x 0))
(defun num-samples (x)
  (array-dimension x 1))

(defun get-buffer-region (x start)
  (let ((o (make-array (list +num-channels+ +frames-per-buffer+))))
    (dotimes (i +frames-per-buffer+)
      (dotimes (j +num-channels+)
        (setf (aref o j i)
              (let ((idx (+ i (* start +frames-per-buffer+))))
                (if (<= (num-samples x) idx)
                  0.0
                  (aref x j idx))))))
    o))

(defun play-vec (input)
  (format t "Playing ~D samples.~%" (num-samples input))
  (with-audio
    (with-default-audio-stream (astream +num-channels+ +num-channels+ :sample-format +sample-format+ :sample-rate +sample-rate+ :frames-per-buffer +frames-per-buffer+)
                               (dotimes (i (round (/ (num-samples input) +frames-per-buffer+)))
                                 (write-stream astream (merge-channels-into-array astream
                                                                                  (get-buffer-region input i)))))))

(defun sample-region (fun start end)
  (let* ((num-samples (round (* (- end start) 44100)))
         (buffer (make-array (list +num-channels+ num-samples))))
    (dotimes (i num-samples)
      (let ((frame (funcall fun (+ start (/ i +sample-rate+)))))
        (dotimes (j +num-channels+)
          (setf (aref buffer j i)
                (coerce (aref frame j) 'single-float)))))
    buffer))

(defun channel-up (x)
  (make-array +num-channels+ :initial-element x))

(defun osc (tm hz)
  (sin (* tm hz 2 pi)))

