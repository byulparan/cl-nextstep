(in-package :cg)

(defun make-color-space (name)
  (let* ((name (case name
		 (:color-space-srgb "kCGColorSpaceSRGB")
		 (t name))))
    (cffi:foreign-funcall "CGColorSpaceCreateWithName"
			  :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer)
			  :pointer)))

(defun release-color-space (color-space)
  (cffi:foreign-funcall "CGColorSpaceRelease" :pointer color-space))

