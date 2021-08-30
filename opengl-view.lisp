(in-package :cl-nextstep)

(defclass opengl-view (base-view)
  ((core-profile :initarg :core-profile
		 :initform t
		 :reader core-profile)))

(defmethod reshape ((self opengl-view))
  ())


(defmethod initialize-instance :after ((self opengl-view) &key (x 0) (y 0) (w 400) (h 200))
  (let* ((pixel-format (cgl:make-pixel-format (cgl:make-attributes
					       :core-profile (core-profile self)))))
    (unwind-protect
	 (setf (cocoa-ref self) (ns:objc
				 (ns:objc "LispOpenGLView" "alloc" :pointer)
				 "initWithID:frame:pixelFormat:isAnimate:drawFn:mouseFn:"
				 :int (id self)
				 (:struct ns:rect) (ns:make-rect x y w h)
				 :pointer pixel-format
				 :unsigned-char 1
				 :pointer (cffi:callback draw-callback)
				 :pointer (cffi:callback mouse-callback)
				 :pointer))
      (cgl:destroy-pixel-format pixel-format))))


(defun set-best-resolution (opengl-view setup)
  (ns:objc (cocoa-ref opengl-view) "setWantsBestResolutionOpenGLSurface:" :bool setup))
