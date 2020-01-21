(in-package :cl-nextstep)

(defvar *window-callback-table* (make-hash-table))

(cffi:defcallback window-callback :void ((id :int))
  (when-let* ((window (gethash id *window-callback-table*))
	      (close-fn (close-fn window)))
    (handler-case (funcall close-fn)
      (error (c) (break (format nil "catch signal while Closing Window: ~s " c))))))

(defclass window ()
  ((cocoa-ref :reader cocoa-ref)
   (id :reader id)
   (g-id :initform 0 :reader g-id :allocation :class)
   (title :initarg :title :initform "" :reader title)
   (close-fn :initarg :close-fn :initform nil :reader close-fn)))

(defmethod initialize-instance :after ((self window) &key (x 0) (y 0) (w 100) (h 200))
  (with-slots (cocoa-ref id g-id title close-fn) self
    (setf id g-id)
    (incf g-id)
    (setf cocoa-ref (%make-window id title x y w h (cffi:callback window-callback)))
    (when close-fn
      (setf (gethash id *window-callback-table*) self))))

(defun window-show (window)
  (%window-show (cocoa-ref window)))

(defun toggle-fullscreen (window)
  (ns:objc window "toggleFullscreen"))


