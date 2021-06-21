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

(defun in-screen-rect (rect)
  (let* ((screen
	   #+x86-64 (ns:objc-stret ns:rect  (ns:objc "NSScreen" "mainScreen" :pointer) "frame")
	   #+arm64 (ns:objc (ns:objc "NSScreen" "mainScreen" :pointer) "frame" (:struct ns:rect))
	   ))
      (let* ((in-x (- (ns:rect-width screen) (ns:rect-width rect)))
	     (in-y (- (ns:rect-height screen) (ns:rect-height rect))))
	(ns:make-rect (clamp (ns:rect-x rect) 0 in-x)
		      (clamp (ns:rect-y rect) 0 in-y)
		      (ns:rect-width rect)
		      (ns:rect-height rect)))))

(defmethod initialize-instance :after ((self window) &key rect (x 0) (y 0) (w 400) (h 200)
				       style-mask (closable t) (resizable t) (miniaturizable t))
  (with-slots (cocoa-ref id g-id title close-fn) self
    (setf id g-id)
    (incf g-id)
    (setf cocoa-ref (ns:objc (ns:objc "LispWindow" "alloc" :pointer)
			     "initWithID:frame:styleMask:closeFn:"
			     :int id
			     (:struct rect) (if rect rect (ns:make-rect x y w h))
			     :int (if style-mask style-mask
				    (logior (ash 1 0) ;; Titled
					    (if closable (ash 1 1) 0)
					    (if resizable (ash 1 3) 0)
					    (if miniaturizable (ash 1 2) 0)))
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

(defun set-always-on-top (window flag)
  (ns:objc window "setLevel:" :int (if flag
				       9 ;; kCGStatusWindowLevelKey
				     0 ;; kCGBaseWindowLevelKey
				     )))


