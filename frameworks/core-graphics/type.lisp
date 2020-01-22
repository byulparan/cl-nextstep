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
