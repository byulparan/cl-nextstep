(in-package :cl-nextstep)

(defvar *dispatch-id-map* (make-id-map))

(cffi:defcallback dispatch-callback :void ((id :int))
  (let* ((task (id-map-free-object *dispatch-id-map* id)))
    (when task
      (handler-case (funcall task)
	(error (c) (break (format nil "catch signal while Dispatching Event: ~s " c)))))))

(defun start-event-loop ()
  (trivial-main-thread:call-in-main-thread
    (lambda ()
      (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	(cffi:foreign-funcall "start_event_loop" :pointer (cffi:callback dispatch-callback))))))

(defmacro with-event-loop ((&key waitp nil) &body body)
  (alexandria:with-gensyms (result semaphore id) 
    `(cond ((eql (trivial-main-thread:find-main-thread) (bt:current-thread)) (progn ,@body))
	   (,waitp (let* ((,result nil)
			  (,id (assign-id-map-id *dispatch-id-map*
						 (lambda ()
						   (setf ,result (progn ,@body))))))
		     (%execute-in-event-loop-sync ,id)
		     ,result))
	   (t (let* ((,id (assign-id-map-id *dispatch-id-map* (lambda () ,@body))))
		(%execute-in-event-loop-async ,id))))))


