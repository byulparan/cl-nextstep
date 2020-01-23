(defpackage :ci
  (:use :cl)
  (:export #:make-context
	   #:draw-image
	   #:render-to-bitmap
	   #:load-image
	   #:make-image-from-texture
	   #:to-ciimage
	   #:extent
	   #:make-filter
	   #:set-filter-param
	   #:apply-filter))

(in-package :ci)

(defun make-context (cgl-context cgl-pixel-format)
  (let* ((color-space (cg:color-space-create :color-space-srgb))
	 (ci-context (ns:objc
		      (ns:objc "CIContext" "contextWithCGLContext:pixelFormat:colorSpace:options:"
			       :pointer cgl-context
			       :pointer cgl-pixel-format
			       :pointer color-space
			       :pointer (cffi:null-pointer)
			       :pointer)
		      "retain" :pointer)))
    (cg:color-space-release color-space)
    ci-context))



