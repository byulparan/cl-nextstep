(in-package :cl-nextstep)

(cffi:defcfun ("make_timer" %make-timer) :pointer
  (id :int)
  (timer-fn :pointer)
  (interval :double))

(defvar *timer-table* (make-hash-table))

(cffi:defcallback timer-callback :void ((id :int))
  (let* ((timer (gethash id *timer-table*)))
    (funcall (timer-fn timer))))

(defclass timer ()
  ((id :accessor id)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (timer-fn :initarg :timer-fn :initform (error "timer-fn should be specified") :reader timer-fn)
   (cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after ((self timer) &key (interval 1.0))
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *timer-table*) self)
  (setf (cocoa-ref self)
    (objc (%make-timer (id self) (cffi:callback timer-callback) (float interval 1.0d0))
	  "autorelease" :pointer)))

(defun invalidate (timer)
  (objc timer "invalidate"))


