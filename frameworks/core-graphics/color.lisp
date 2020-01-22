(in-package :cg)

(defun color-create-generic-rgb (r g b a)
  (cffi:foreign-funcall "CGColorCreateGenericRGB" :double (cgfloat r) :double (cgfloat g) :double (cgfloat b)
			:double (cgfloat a) :pointer))




