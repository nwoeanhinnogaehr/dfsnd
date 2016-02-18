(load "lib.lisp")

(defun snare (tm vel len)
  (fade
    (list (noise) (noise))
    (* vel (expt (+ 1 tm) len))))

(defun hat (tm vel len)
  (fade
    (sum-frames (list (max 0.7 (noise)) (max 0.7 (noise))) -0.7)
    (* vel (expt (+ 1 tm) len))))

(defun kick (tm vel)
  (channel-up
    (fade
      (osc (note-freq (+ (* 4 (expt (+ 1 tm) -50)) -2) 1) tm)
      (* vel (expt (+ 1 tm) -5)))))

(defun beep (tm vel note)
  (channel-up
    (fade
      (squ (note-freq note 12 :base 2 :octave -1) tm)
      (* vel (expt (+ 1 tm) -10)))))

(let*
  ((buffer (make-buffer))
   (waitq (make-queue :key 'event-time :ord '<))
   (playq (make-queue :key 'event-death :ord '<))
   (triggers (loop for i from 1 below 4
                   collecting (interval
                                (/ 1 i)
                                (let ((i i))
                                  (lambda (tm idx)
                                    (queue-push waitq (list (make-event :time (+ tm 0)
                                                                        :death (+ tm (/ 1 i))
                                                                        :inst 'beep
                                                                        :args `(1 ,(mod idx i)))
                                                            )))))))
   )

  (defun the-sound (tm)
    ; process event triggers
    (dolist (i triggers)
      (funcall i tm))

    ; shuffle queues
    (queue-push playq (queue-pop waitq tm))
    (queue-pop playq tm)

    ; compute
    (reduce 'mix-frames
            (queue-data playq)
            :key (lambda (e)
                   (apply (event-inst e)
                          (- tm (event-time e))
                          (event-args e)))))

  (defun buffer-proxy (tm)
    (read-buffer buffer tm 'the-sound))

  (write-vec (time (normalize (sample-region 'buffer-proxy 0.0 8.0))) "out.wav"))
