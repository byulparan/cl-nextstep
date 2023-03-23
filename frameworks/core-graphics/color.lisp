(in-package :cg)

(defun make-color-generic-rgb (r g b a)
  (cffi:foreign-funcall "CGColorCreateGenericRGB" :double (float r 1.0d0) :double (float g 1.0d0)
						  :double (float b 1.0d0) :double (float a 1.0d0)
						  :pointer))




