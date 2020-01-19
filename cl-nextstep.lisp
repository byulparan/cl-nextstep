(in-package :cl-nextstep)

(defvar *dispatch-id-map* (make-id-map))

(cffi:defcallback dispatch-callback :void ((id :int))
  (let* ((task (id-map-free-object *dispatch-id-map* id)))
    (when task
      (funcall task))))

(defun start-event-loop ()
  (trivial-main-thread:call-in-main-thread
    (lambda ()
      (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	(cffi:foreign-funcall "start_event_loop" :pointer (cffi:callback dispatch-callback))))))

(defmacro with-event-loop ((&key waitp nil) &body body)
  (alexandria:with-gensyms (result semaphore id) 
    `(cond ((eql (trivial-main-thread:find-main-thread) (bt:current-thread)) (progn ,@body))
	   (,waitp (let* ((,result nil)
			  (,semaphore #+sbcl (sb-thread:make-semaphore)
				      #+ccl (ccl:make-semaphore))
			  (,id (assign-id-map-id *dispatch-id-map*
						 (lambda ()
						   (setf ,result (progn ,@body))
						   #+sbcl (sb-thread:signal-semaphore ,semaphore)
						   #+ccl (ccl:signal-semaphore ,semaphore)))))
		     (cffi:foreign-funcall "execute_in_event_loop" :int ,id)
		     #+sbcl (sb-thread:wait-on-semaphore ,semaphore)
		     #+ccl (ccl:wait-on-semaphore ,semaphore)
		     ,result))
	   (t (let* ((,id (assign-id-map-id *dispatch-id-map* (lambda () ,@body))))
		(cffi:foreign-funcall "execute_in_event_loop" :int ,id))))))


