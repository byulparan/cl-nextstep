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
  (let* ((object (cls self)))
    (assert (not (cffi:null-pointer-p object)) nil "Can't find NSClass: '~a'" object)
    object))

(defmacro objc (instance sel &rest rest)
  (with-gensyms (object selector)
    `(let* ((,object (cocoa-ref ,instance))
	    (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil "`ns:objc` accept NullPointer with SEL: \"~a\"" ,sel)
       (cffi:foreign-funcall "objc_msgSend" :pointer ,object :pointer ,selector ,@rest))))

(defun retain (instance)
  (ns:objc instance "retain" :pointer))

(defun release (instance)
  (ns:objc instance "release" :pointer))

(defun autorelease (instance)
  (ns:objc instance "autorelease" :pointer))


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


