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
    `(cond ((eql (trivial-main-thread:find-main-thread) (bt:current-thread))  (funcall (lambda () ,@body)))
	   (,waitp  (let* ((,result nil)
			   (,semaphore #+sbcl (sb-thread:make-semaphore)
				       #+ccl (ccl:make-semaphore)))
		      (incf *id*)
		      (setf (gethash *id* *task-table*)
			(lambda ()
			  (setf ,result (progn ,@body))
			  #+sbcl (sb-thread:signal-semaphore ,semaphore)
			  #+ccl (ccl:signal-semaphore ,semaphore)))
		      (cffi:foreign-funcall "execute_in_event_loop" :int *id*)
		      #+sbcl (sb-thread:wait-on-semaphore ,semaphore)
		      #+ccl (ccl:wait-on-semaphore ,semaphore)
		      ,result))
	   (t (incf *id*)
	      (setf (gethash *id* *task-table*)
		(lambda ()
		  ,@body))
	      (cffi:foreign-funcall "execute_in_event_loop" :int *id*)))))


