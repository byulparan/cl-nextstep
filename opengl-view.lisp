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
    (unwind-protect (let* ((gl-view (ns:objc
				     (ns:objc "LispOpenGLView" "alloc" :pointer)
				     "initWithID:frame:pixelFormat:isAnimate:drawFn:mouseFn:"
				     :int (id self)
				     (:struct ns:rect) (ns:rect x y w h)
				     :pointer pixel-format
				     :unsigned-char (if (animate self) 1 0)
				     :pointer (cffi:callback draw-callback)
				     :pointer (cffi:callback mouse-callback)
				     :pointer)))
		      (set-gl-best-resolution gl-view (retina self))
		      (setf (cocoa-ref self) gl-view))
      (cgl:destroy-pixel-format pixel-format))))

(defun set-gl-best-resolution (opengl-view setup)
  (ns:objc (cocoa-ref opengl-view) "setWantsBestResolutionOpenGLSurface:" :bool setup))
