(defpackage :cg
  (:use :cl)
  (:export #:point
	   #:make-point
	   #:point-x
	   #:point-y
	   #:size
	   #:make-size
	   #:size-width
	   #:size-height
	   #:rect
	   #:make-rect
	   #:rect-x
	   #:rect-y
	   #:rect-width
	   #:rect-height
	   #:load-image
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-bitmap-data
	   #:retain-image
	   #:release-image
	   #:make-image-from-screen

	   #:context-set-rgb-fill-color
	   #:context-set-rgb-stroke-color
	   #:context-fill-rect))

(in-package :cg)

(cffi:defcstruct (point :class %point)
  (x :double)
  (y :double))

(defstruct (point
	    (:constructor make-point (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %point))
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (make-point x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %point) p)
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (setf x (coerce (point-x point) 'double-float)
	  y (coerce (point-y point) 'double-float))))


(cffi:defcstruct (size :class %size)
  (width :double)
  (height :double))

(defstruct (size
	    (:constructor make-size (width height)))
  width height)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (make-size width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (setf width (coerce (size-width size) 'double-float)
	  height (coerce (size-height size) 'double-float))))



(cffi:defcstruct (rect :class %rect)
  (origin (:struct point))
  (size (:struct size)))

(defstruct (rect
	    (:constructor make-rect (x y width height)))
  x y width height)

(defmethod cffi:translate-from-foreign (p (type %rect))
  (cffi:with-foreign-slots ((origin size) p (:struct rect))
    (make-rect (point-x origin)
	       (point-y origin)
	       (size-width size)
	       (size-height size))))

(defmethod cffi:translate-into-foreign-memory (rect (type %rect) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct rect) 'origin))
	 (size (cffi:foreign-slot-pointer p '(:struct rect) 'size)))
    (cffi:with-foreign-slots ((x y) origin (:struct point))
      (cffi:with-foreign-slots ((width height) size (:struct size))
	(setf x (coerce (rect-x rect) 'double-float)
	      y (coerce (rect-y rect) 'double-float)
	  width (coerce (rect-width rect) 'double-float)
	  height (coerce (rect-height rect) 'double-float))))))

(cffi:defcfun ("cg_load_bitmap_image" %load-cg-image) :pointer
  (path :string))

(defun load-image (path)
  (let* ((full-path (uiop/driver:truename* path)))
    (assert full-path nil "~s : Can't find image file" path)
    (%load-cg-image (namestring full-path))))

(cffi:defcfun ("CGImageGetWidth" image-width) :sizet
  (image :pointer))

(cffi:defcfun ("CGImageGetHeight" image-height) :sizet
  (image :pointer))

(cffi:defcfun ("CGImageGetBitsPerPixel" image-bpp) :sizet
  (image :pointer))

(cffi:defcfun ("bitmap_data" image-bitmap-data) :pointer
  (image :pointer))

(cffi:defcfun ("CGImageRetain" retain-image) :pointer
  (image :pointer))

(cffi:defcfun ("CGImageRelease" release-image) :void
  (image :pointer))

(cffi:defcfun ("cg_get_cgimage_from_screen" make-image-from-screen) :pointer
  (rect (:struct rect)))


;; drwing-call
(defun context-set-rgb-fill-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun context-set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))


(defun context-fill-rect (context rect)
  (cffi:foreign-funcall "CGContextFillRect" :pointer context (:struct cg:rect) rect))


