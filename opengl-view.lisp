(in-package :cl-nextstep)

(defvar *opengl-view-table* (make-hash-table))

(defclass opengl-view ()
  ((id :reader id)
   (g-id :initform 0 :reader g-id :allocation :class)
   (context :accessor context)
   (pixel-format :accessor pixel-format)
   (width :accessor width )
   (heigh :accessor height)
   (core-profile :initarg :core-profile
		 :initform t
		 :reader core-profile)
   (cocoa-ref :accessor cocoa-ref)))

(defmethod init ((self opengl-view))
  ())

(defmethod draw ((self opengl-view))
  ())

(defmethod reshape ((self opengl-view))
  ())

(defmethod shutdown ((self opengl-view))
  ())


(cffi:defcallback opengl-callback :void ((id :int) (draw-flag :int) (context :pointer) (pixel-format :pointer) (width :int) (height :int))
  (let* ((view (gethash id *opengl-view-table*)))
    (setf (context view) context
	  (pixel-format view) pixel-format
	  (width view) width
	  (height view) height)
    (handler-case 
    	(ecase draw-flag
    	  (0 (init view))
    	  (1 (draw view))
    	  (2 (reshape view))
    	  (3 (shutdown view)))
      (error (c) (break (format nil "catch signal while Drawing OpenGL: ~s " c))))))


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
  (with-slots (id g-id) self
    (setf id g-id)
    (incf g-id))
  (setf (gethash (id self) *opengl-view-table*) self)
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
						  (cffi:callback opengl-callback)
						  attrib-object
						  x y w h))))))



