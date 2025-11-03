(in-package :cl-nextstep)

(defvar *window-table* (make-hash-table))

(cffi:defcallback window-callback :void ((id :int) (event-type :int))
  (when-let* ((window (gethash id *window-table*))
	      (handle-fn (case event-type
			  (0 (close-fn window)))))
    (unwind-protect
	 (handler-case (funcall handle-fn)
	   (error (c) (break (format nil "catch signal while call Window Event: ~s " c))))
      (remhash id *window-table*))))

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
	(ns:rect (clamp (ns:rect-x rect) 0 in-x)
		      (clamp (ns:rect-y rect) 0 in-y)
		      (ns:rect-width rect)
		      (ns:rect-height rect)))))


(defun window-style-mask (&key (titled t) (closable t) (resizable t) (miniaturizable t) full-size-content-view full-screen)
  (logior (if titled (ash 1 0) 0)
	  (if closable (ash 1 1) 0)
	  (if resizable (ash 1 3) 0)
	  (if miniaturizable (ash 1 2) 0)
	  (if full-size-content-view (ash 1 15) 0)
	  (if full-screen (ash 1 14) 0)))



(defmethod initialize-instance :after ((self window) &key rect (x 0) (y 0) (w 400) (h 200)
						       style-mask (closable t) (resizable t) (miniaturizable t)
						       full-size-content-view)
  (with-slots (cocoa-ref id g-id title close-fn) self
    (setf id g-id)
    (incf g-id)
    (setf cocoa-ref (ns:objc (ns:objc "LispWindow" "alloc" :pointer)
			     "initWithID:frame:styleMask:handleFn:"
			     :int id
			     (:struct rect) (if rect rect (ns:rect x y w h))
			     :int (if style-mask style-mask
				    (window-style-mask
				     :titled t
				     :closable closable
				     :resizable resizable
				     :miniaturizable miniaturizable
				     :full-size-content-view full-size-content-view))
			     :pointer (cffi:callback window-callback)
			     :pointer))
    (ns:objc cocoa-ref "setTitle:" :pointer (autorelease (make-ns-string title)))
    (ns:objc cocoa-ref "setDelegate:" :pointer cocoa-ref)
    (setf (gethash id *window-table*) self)))

(defun window-show (window)
  (ns:objc window "makeKeyAndOrderFront:" :pointer (cffi:null-pointer)))

(defun window-close (window)
  (ns:objc window "performClose:" :pointer (cffi:null-pointer)))

(defun toggle-fullscreen (window)
  (ns:objc window "toggleFullscreen"))

(defun titlebar-appears-transparent (window)
  (ns:objc window "titlebarAppearsTransparent" :bool))

(defun (setf titlebar-appears-transparent) (value window)
  (ns:objc window "setTitlebarAppearsTransparent:" :bool value))

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

(defun window-screen (window)
  (objc window "screen" :pointer))


;; NSScreen
(defun main-screen ()
  (objc "NSScreen" "mainScreen" :pointer))

(defun list-screens ()
  (let* ((screens (objc "NSScreen" "screens" :pointer)))
    (loop for i below (objc screens "count" :int)
	  collect (objc screens "objectAtIndex:" :int i :pointer))))

(defun screen-display (screen)
  (ns:objc
   (ns:objc (ns:objc screen "deviceDescription" :pointer)
	    "objectForKey:" :pointer (ns:autorelease (ns:make-ns-string "NSScreenNumber")) :pointer)
   "intValue" :unsigned-int))




