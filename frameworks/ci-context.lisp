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

;; ci-context
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

(defun draw-image (context image in-rect from-rect)
  (ns:objc context "drawImage:inRect:fromRect:"
	   :pointer image
	   (:struct cg:rect) in-rect
	   (:struct cg:rect) from-rect))

(defun render-to-bitmap (context image data row-bytes format bounds)
  (ns:objc context "render:toBitmap:rowBytes:bounds:format:colorSpace:"
	   :pointer image
	   :pointer data
	   :int row-bytes
	   :int (case format
		  (:ci-format-argb8 265)
		  (t format))
	   :pointer (cffi:null-pointer)))

;; ci-image






