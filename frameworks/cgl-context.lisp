(defpackage :cgl-context
  (:nicknames :cgl)
  (:use :cl)
  (:export #:make-context
	   #:make-pixel-format
	   #:make-attributes
	   #:+pfa-double-buffer+
	   #:+pfa-accelerated+
	   #:+pfa-color-size+ 
	   #:+pfa-depth-size+ 
	   #:+pfa-stencil-size+ 
	   #:+pfa-sample-buffers+ 
	   #:+pfa-samples+ 
	   #:+pfa-no-recovery+ 
	   #:+pfa-opengl-profile+ 
	   #:+opengl-profile-version3-2-core+ 
	   #:destroy-context
	   #:destroy-pixel-format
	   #:set-current-context
	   #:get-current-context
	   #:tex-image-io-surface-2d))

(in-package :cgl-context)

(defconstant +pfa-double-buffer+ 5)
(defconstant +pfa-accelerated+ 73)
(defconstant +pfa-color-size+ 8)
(defconstant +pfa-depth-size+ 12)
(defconstant +pfa-stencil-size+ 13)
(defconstant +pfa-sample-buffers+ 55)
(defconstant +pfa-samples+ 56)
(defconstant +pfa-no-recovery+ 72)
(defconstant +pfa-opengl-profile+ 99)
(defconstant +opengl-profile-version3-2-core+ 12800)

(defun make-attributes (&key (core-profile t))
  (append 
   (list
    +pfa-double-buffer+
    +pfa-accelerated+
    +pfa-color-size+ 32
    +pfa-depth-size+ 32
    +pfa-stencil-size+ 8
    +pfa-sample-buffers+ 1
    +pfa-samples+ 4
    +pfa-no-recovery+)
   (when core-profile
     (list +pfa-opengl-profile+ +opengl-profile-version3-2-core+))))

(defun make-context (cgl-pixel-format)
  (cffi:with-foreign-objects ((new-context :pointer))
    (let* ((err (cffi:foreign-funcall "CGLCreateContext" :pointer cgl-pixel-format
							 :pointer (cffi:null-pointer)
							 :pointer new-context
							 :int)))
      (unless (zerop err)
	(error "Error on cgl:make-context with code: ~d" err))
      (cffi:mem-ref new-context :pointer))))


(defun make-pixel-format (attributes)
  (let* ((len (length attributes)))
    (cffi:with-foreign-objects ((attrib :int (1+ len))
				(new-pixel-format :pointer)
				(n-pix :int))
      (loop for attr in attributes
	    for i from 0
	    do (setf (cffi:mem-aref attrib :int i) attr)
	    finally (setf (cffi:mem-aref attrib :int len) 0))
      (cffi:foreign-funcall "CGLChoosePixelFormat" :pointer attrib
						   :pointer new-pixel-format
						   :pointer n-pix)
      (when (zerop (cffi:mem-ref n-pix :int))
	(error "Error on cgl:make-pixel-format"))
      (cffi:mem-ref new-pixel-format :pointer))))

(defun destroy-context (cgl-context)
  (cffi:foreign-funcall "CGLDestroyContext" :pointer cgl-context))

(defun destroy-pixel-format (cgl-pixel-format)
  (cffi:foreign-funcall "CGLDestroyPixelFormat" :pointer cgl-pixel-format))

(defun get-current-context ()
  (cffi:foreign-funcall "CGLGetCurrentContext" :pointer))

(defun set-current-context (cgl-context)
  (cffi:foreign-funcall "CGLSetCurrentContext" :pointer cgl-context))

(defun tex-image-io-surface-2d (cgl-context target internal-format width height format type io-surface plane)
  (cffi:foreign-funcall "CGLTexImageIOSurface2D"
			:pointer cgl-context
			:int (cffi:foreign-enum-value '%gl:enum target)
			:int (cffi:foreign-enum-value '%gl:enum internal-format)
			:int width
			:int height
			:int (cffi:foreign-enum-value '%gl:enum format)
			:int (cffi:foreign-enum-value '%gl:enum type)
			:pointer io-surface
			:unsigned-int plane))
