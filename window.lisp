(in-package :cl-nextstep)

(defvar *window-table* (make-hash-table))

(cffi:defcallback window-callback :void ((id :int))
  (when-let* ((window (gethash id *window-table*))
	      (close-fn (close-fn window)))
    (handler-case (funcall close-fn)
      (error (c) (break (format nil "catch signal while Closing Window: ~s " c))))))

(defclass window ()
  ((cocoa-ref :reader cocoa-ref)
   (id :reader id)
   (g-id :initform 0 :reader g-id :allocation :class)
   (title :initarg :title :initform "" :reader title)
   (close-fn :initarg :close-fn :initform nil :accessor close-fn)))

(defmethod initialize-instance :after ((self window) &key (x 0) (y 0) (w 400) (h 200))
  (with-slots (cocoa-ref id g-id title close-fn) self
    (setf id g-id)
    (incf g-id)
    (setf cocoa-ref (ns:objc (ns:objc "LispWindow" "alloc" :pointer)
			     "initWithID:frame:closeFn:"
			     :int id
			     (:struct rect) (make-rect x y w h)
			     :pointer (cffi:callback window-callback)
			     :pointer))
    (ns:objc cocoa-ref "setTitle:" :pointer (autorelease (make-ns-string title)))
    (ns:objc cocoa-ref "setDelegate:" :pointer cocoa-ref)
    (setf (gethash id *window-table*) self)))

(defun window-show (window)
  (ns:objc window "makeKeyAndOrderFront:" :pointer (cffi:null-pointer)))

(defun toggle-fullscreen (window)
  (ns:objc window "toggleFullscreen"))

(defun content-view (window)
  (ns:objc window "contentView" :pointer))

(defun (setf content-view) (view window)
  (ns:objc window "setContentView:" :pointer (ns:objc view "autorelease" :pointer)))

(defun add-subviews (superview subview &rest subviews)
  (dolist (sub (cons subview subviews))
    (ns:objc superview "addSubview:" :pointer (ns:objc sub "autorelease" :pointer))))

