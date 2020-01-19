(in-package :cl-nextstep)

(defvar *task-table* (make-hash-table))
(defvar *id* 0)

(cffi:defcallback event-callback :void ((id :int))
  (let* ((task (gethash id *task-table*)))
    (when task
      (funcall task))))

(defun start-event-loop ()
  (trivial-main-thread:call-in-main-thread
    (lambda ()
      (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	(cffi:foreign-funcall "start_event_loop" :pointer (cffi:callback event-callback))))))

(defmacro with-event-loop ((&key waitp nil) &body body)
  (alexandria:with-gensyms (result semaphore) 
    `(cond ((sb-thread:main-thread-p) (funcall (lambda () ,@body)))
	   (,waitp  (let* ((,result nil)
			   (,semaphore (sb-thread:make-semaphore)))
		      (incf *id*)
		      (setf (gethash *id* *task-table*)
			(lambda ()
			  (setf ,result (progn ,@body))
			  (sb-thread:signal-semaphore ,semaphore)))
		      (cffi:foreign-funcall "execute_in_event_loop" :int *id*)
		      (sb-thread:wait-on-semaphore ,semaphore)
		      ,result))
	   (t (incf *id*)
	      (setf (gethash *id* *task-table*)
		(lambda ()
		  ,@body))
	      (cffi:foreign-funcall "execute_in_event_loop" :int *id*)))))


