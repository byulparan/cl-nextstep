(in-package :cg)

(defun color-space-create (name)
  (let* ((name (case name
		 (:color-space-srgb "kCGColorSpaceSRGB")
		 (t name))))
    (cffi:foreign-funcall "CGColorSpaceCreateWithName"
			  :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer)
			  :pointer)))

(defun color-space-release (color-space)
  (cffi:foreign-funcall "CGColorSpaceRelease" :pointer color-space))

