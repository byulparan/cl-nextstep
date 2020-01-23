(in-package :cl-nextstep)


(cffi:define-foreign-library lib-nextstep
  (:darwin "libcl_nextstep.dylib"))

(cffi:use-foreign-library lib-nextstep)


(cffi:defcfun ("objc_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defgeneric cocoa-ref (self))

(defmethod cocoa-ref ((self t))
  self)

(defmethod cocoa-ref ((self string))
  (cls self))

(defmacro objc (class sel &rest rest)
  (with-gensyms (cls selector)
    `(let* ((,cls (cocoa-ref ,class))
	    (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,cls)) nil "Can't find NSClass: '~a' with SEL: '~a'" ,class ,sel)
       (cffi:foreign-funcall "objc_msgSend" :pointer ,cls :pointer ,selector ,@rest))))


(cffi:defcfun ("start_event_loop" %start-event-loop) :void
  (event-callback :pointer))

(cffi:defcfun ("execute_in_event_loop_async" %execute-in-event-loop-async) :void
  (id :int))

(cffi:defcfun ("execute_in_event_loop_sync" %execute-in-event-loop-sync) :void
  (id :int))



(defvar *dispatch-id-map* (make-id-map))

(cffi:defcallback dispatch-callback :void ((id :int))
  (let* ((task (id-map-free-object *dispatch-id-map* id)))
    (when task
      (handler-case (funcall task)
	(error (c)
	  (break (format nil "catch signal while Dispatching Event: \"~a\"" c)))))))

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


(let* ((running-p nil))
  (defun start-event-loop ()
    (unless running-p
      (trivial-main-thread:call-in-main-thread
       (lambda ()
	 (setf running-p t)
	 (defun trivial-main-thread:call-in-main-thread (function &key blocking (runner trivial-main-thread::*runner*))
	   (declare (ignore runner))
	   (ns:with-event-loop (:waitp blocking)
	     (funcall function)))
	 (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	   (cffi:foreign-funcall "start_event_loop" :pointer (cffi:callback dispatch-callback))))))))


