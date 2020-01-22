(in-package :cl-nextstep)

(cffi:defcfun ("make_opengl_view" %make-opengl-view) :pointer
  (id :int)
  (attributes :pointer)
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

(defconstant +cgl-pfa-double-buffer+ 5)
(defconstant +cgl-pfa-accelerated+ 73)
(defconstant +cgl-pfa-color-size+ 8)
(defconstant +cgl-pfa-depth-size+ 12)
(defconstant +cgl-pfa-stencil-size+ 13)
(defconstant +cgl-pfa-sample-buffers+ 55)
(defconstant +cgl-pfa-samples+ 56)
(defconstant +cgl-pfa-no-recovery+ 72)
(defconstant +cgl-pfa-opengl-profile+ 99)
(defconstant +cgl-opengl-profile-version3-2-core+ 12800)

(defvar *default-cgl-pixel-format*
  (list
   +cgl-pfa-double-buffer+ +cgl-pfa-accelerated+
   +cgl-pfa-color-size+ 32
   +cgl-pfa-depth-size+ 32
   +cgl-pfa-stencil-size+ 8
   +cgl-pfa-sample-buffers+ 1
   +cgl-pfa-samples+ 4
   +cgl-pfa-no-recovery+))

(defmethod initialize-instance :after ((self opengl-view) &key (x 0) (y 0) (w 400) (h 200))
  (let* ((attributes *default-cgl-pixel-format*))
    (when (core-profile self)
      (setf attributes (append attributes
  			       (list +cgl-pfa-opengl-profile+
  				     +cgl-opengl-profile-version3-2-core+))))
    (let* ((nattribute (length attributes)))
      (cffi:with-foreign-objects ((attrib-object :unsigned-int (1+ nattribute)))
  	(loop for attr in attributes
  	      for i from 0 
  	      do (setf (cffi:mem-aref attrib-object :int i) attr)
  	      finally (setf (cffi:mem-aref attrib-object :int nattribute) 0))
  	(setf (cocoa-ref self) (%make-opengl-view (id self)
  						  attrib-object
  						  t ;; animate
  						  x y w h
  						  (cffi:callback draw-callback)
  						  (cffi:callback mouse-callback)))))))



