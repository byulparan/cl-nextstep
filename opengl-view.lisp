(in-package :cl-nextstep)

(defclass opengl-view (base-view)
  ((core-profile :initarg :core-profile
		 :initform nil
		 :reader core-profile)
   (animate :initarg :animate
	    :initform nil
	    :reader animate)
   (retina :initarg :retina
	   :initform nil
	   :reader retina)))

(defmethod reshape ((self opengl-view))
  ())


(defmethod initialize-instance :after ((self opengl-view) &key (x 0) (y 0) (w 400) (h 200))
  (let* ((pixel-format (cgl:make-pixel-format (cgl:list-attributes
					       :core-profile (core-profile self)))))
    (unwind-protect (let* ((gl-view (with-sb-alien-rect (rect (ns:rect x y w h))
				      (sb-alien:alien-funcall
				       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
												sb-alien:system-area-pointer
												sb-alien:system-area-pointer
												sb-alien:int
												(sb-alien:struct rect)
												sb-alien:system-area-pointer
												sb-alien:unsigned-char
												sb-alien:system-area-pointer
												sb-alien:system-area-pointer))
				       (ns:objc "LispOpenGLView" "alloc" :pointer)
				       (sel "initWithID:frame:pixelFormat:isAnimate:drawFn:eventFn:")
				       (id self)
				       rect
				       pixel-format
				       (if (animate self) 1 0)
				       (cffi:callback view-draw-callback)
				       (cffi:callback view-event-callback)))))
		      (set-gl-best-resolution gl-view (retina self))
		      (setf (cocoa-ref self) gl-view))
      (cgl:destroy-pixel-format pixel-format))))

(defun set-gl-best-resolution (opengl-view setup)
  (ns:objc (cocoa-ref opengl-view) "setWantsBestResolutionOpenGLSurface:" :bool setup))

(defun display-link (opengl-view)
  (objc opengl-view "link" :pointer))
