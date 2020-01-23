(in-package :cl-nextstep)

(cffi:defcfun ("make_opengl_view" %make-opengl-view) :pointer
  (id :int)
  (cgl-pixel-format :pointer)
  (animate-p :bool)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (draw-fn :pointer)
  (mouse-fn :pointer))

(defclass opengl-view (base-view)
  ((core-profile :initarg :core-profile
		 :initform t
		 :reader core-profile)))

(defmethod reshape ((self opengl-view))
  ())


(defmethod initialize-instance :after ((self opengl-view) &key (x 0) (y 0) (w 400) (h 200))
  (setf (cocoa-ref self) (%make-opengl-view (id self)
					    (cgl:make-pixel-format (cgl:make-attributes
								    :core-profile (core-profile self)))
					    t ;; animate
					    x y w h
					    (cffi:callback draw-callback)
					    (cffi:callback mouse-callback))))



