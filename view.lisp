(in-package :cl-nextstep)

(defvar *view-table* (make-hash-table))

(cffi:defcallback view-draw-callback :void ((id :int) (draw-flag :int) (cgl-context :pointer) (cgl-pixel-format :pointer) (width :int) (height :int))
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

(cffi:defcallback view-event-callback :void ((id :int) (mouse-flag :int) (event :pointer) (x :double) (y :double))
  (let* ((view (gethash id *view-table*)))
    (handler-case
	(ecase mouse-flag
	  (0 (mouse-down view event x y))
	  (1 (mouse-dragged view event x y))
	  (2 (mouse-up view event x y))
	  (3 (mouse-moved view event x y))
	  (4 (mouse-wheel view event x y))
	  (5 (key-down view event (char-code (aref (ns-string-to-lisp (ns:objc event "characters" :pointer)) 0)))))
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


(defmethod key-down ((self base-view) event key-code)
  (declare (ignorable event key-code)))

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
  (with-sb-alien-rect (rect (ns:rect 0 0 (ns:width view) (ns:height view)))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer
							      (sb-alien:struct rect)))
     (cocoa-ref view) (sel "setNeedsDisplayInRect:") rect)))

;; view
(defclass view (base-view)
  ())

(defmethod initialize-instance :after ((self view) &key rect (x 0) (y 0) (w 400) (h 200))
  (with-sb-alien-rect (rect (if rect rect (rect x y w h)))
    (setf (cocoa-ref self)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								sb-alien:int
								(sb-alien:struct rect)
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer))
       (ns:objc "LispView" "alloc" :pointer)
       (sel "initWithID:frame:drawFn:eventFn:")
       (id self)
       rect
       (cffi:callback view-draw-callback)
       (cffi:callback view-event-callback)))))


(defun current-cg-context ()
  (let* ((graphic-context (ns:objc "NSGraphicsContext" "currentContext" :pointer)))
    (if (cffi:null-pointer-p graphic-context) (error "can't get current NSGraphicContext")
      (ns:objc graphic-context "CGContext" :pointer))))



;; mtk-view
(defclass mtk-view (base-view)
  ((%device :accessor %device)))

(defmethod reshape ((self mtk-view))
  ())

(defmethod initialize-instance :after ((self mtk-view) &key (x 0) (y 0) (w 400) (h 200))
  (let* ((device (cffi:foreign-funcall "MTLCreateSystemDefaultDevice" :pointer))
	 (view (with-sb-alien-rect (rect (rect x y w h))
		 (sb-alien:alien-funcall
		  (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
									   sb-alien:system-area-pointer
									   sb-alien:system-area-pointer
									   (sb-alien:struct rect)
									   sb-alien:system-area-pointer
									   sb-alien:int
									   sb-alien:system-area-pointer
									   sb-alien:system-area-pointer))
		  (objc "LispMTKView" "alloc" :pointer)
		  (sel "initWithFrame:device:id:drawFn:eventFn:")
		  rect 
		  device
		  (id self)
		  (cffi:callback view-draw-callback)
		  (cffi:callback view-event-callback)))))
    (setf (%device self) device)
    (objc view "setDelegate:" :pointer view)
    (setf (cocoa-ref self) view)
    (init self)))


(defun device (mtk-view)
  (%device mtk-view))


(defun color-pixel-format (mtk-view)
  (objc mtk-view "colorPixelFormat" :int))

(defun depth-stencil-pixel-format (mtk-view)
  (objc mtk-view "depthStencilPixelFormat" :int))

(defun set-depth-stencil-pixel-format (mtk-view pixel-format)
  (objc mtk-view "setDepthStencilPixelFormat:" :int pixel-format))

(defun drawable-size (mtk-view)
  (let* ((%size (sb-alien:alien-funcall
		 (sb-alien:extern-alien "objc_msgSend" (sb-alien:function (sb-alien:struct size)
									  sb-alien:system-area-pointer
									  sb-alien:system-area-pointer))
		 (cocoa-ref mtk-view)
		 (sel "drawableSize"))))
    (ns:size (sb-alien:slot %size 'width) (sb-alien:slot %size 'height))))


(defun (setf drawable-size) (size mtk-view)
  (sb-alien:with-alien ((%size (sb-alien:struct size)))
    (setf (sb-alien:slot %size 'width) (float (size-width size) 1.0d0)
	  (sb-alien:slot %size 'height) (float (size-height size) 1.0d0))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer
							      (sb-alien:struct size)))
     (cocoa-ref mtk-view)
     (sel "setDrawableSize:")
     %size)))


(defun current-drawable (mtk-view)
  (objc mtk-view "currentDrawable" :pointer))

(defun current-render-pass-descriptor (mtk-view)
  (objc mtk-view "currentRenderPassDescriptor" :pointer))
