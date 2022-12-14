(in-package :cl-nextstep)

(defvar *view-table* (make-hash-table))

(cffi:defcallback draw-callback :void ((id :int) (draw-flag :int) (cgl-context :pointer) (cgl-pixel-format :pointer) (width :int) (height :int))
  (let* ((view (gethash id *view-table*)))
    (setf (cgl-context view) cgl-context
	  (cgl-pixel-format view) cgl-pixel-format
	  (width view) width
	  (height view) height)
    (handler-case 
    	(ecase draw-flag
    	  (0 (init view))
    	  (1 (draw view))
    	  (2 (reshape view))
    	  (3 (release view)))
      (error (c) (break (format nil "catch signal while Drawing view: \"~a\"" c))))))

(cffi:defcallback mouse-callback :void ((id :int) (mouse-flag :int) (event :pointer) (x :double) (y :double))
  (let* ((view (gethash id *view-table*)))
    (handler-case
	(ecase mouse-flag
	  (0 (mouse-down view event x y))
	  (1 (mouse-dragged view event x y))
	  (2 (mouse-up view event x y))
	  (3 (mouse-moved view event x y))
	  (4 (mouse-wheel view event x y)))
      (error (c) (break (format nil "catch signal while Handling Mouse: \"~a\"" c))))))

(defclass base-view ()
  ((id :accessor id)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (cgl-context :accessor cgl-context)
   (cgl-pixel-format :accessor cgl-pixel-format)
   (width :accessor width )
   (heigh :accessor height)
   (cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after ((self base-view) &key)
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *view-table*) self))

(defmethod init ((self base-view))
  ())

(defmethod draw ((self base-view))
  ())

(defmethod release ((self base-view))
  ())

(defmethod mouse-down ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defmethod mouse-dragged ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defmethod mouse-up ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defmethod mouse-moved ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defmethod mouse-wheel ((self base-view) event location-x location-y)
  (declare (ignorable event location-x location-y)))

(defun command-p (event)
  (not (zerop (logand (ns:objc event "modifierFlags" :unsigned-int) (ash 1 20)))))

(defun shift-p (event)
  (not (zerop (logand (ns:objc event "modifierFlags" :unsigned-int) (ash 1 17)))))

(defun ctrl-p (event)
  (not (zerop (logand (ns:objc event "modifierFlags" :unsigned-int) (ash 1 18)))))

(defun opt-p (event)
  (not (zerop (logand (ns:objc event "modifierFlags" :unsigned-int) (ash 1 19)))))

(defun redisplay (view)
  (ns:objc view "setNeedsDisplayInRect:" (:struct ns:rect) (ns:make-rect 0 0 (ns:width view) (ns:height view))))

;; view
(defclass view (base-view)
  ())

(defmethod initialize-instance :after ((self view) &key (x 0) (y 0) (w 400) (h 200))
  (setf (cocoa-ref self) (ns:objc (ns:objc "LispView" "alloc" :pointer)
				  "initWithID:frame:drawFn:mouseFn:"
				  :int (id self)
				  (:struct rect) (make-rect x y w h)
				  :pointer (cffi:callback draw-callback)
				  :pointer (cffi:callback mouse-callback)
				  :pointer)))

(defun current-cg-context ()
  (ns:objc (ns:objc "NSGraphicsContext" "currentContext" :pointer) "CGContext" :pointer))
