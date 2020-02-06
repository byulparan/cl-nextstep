(in-package :cg)

(defun load-image (path)
  (let* ((path (uiop:truenamize path)))
    (unless (probe-file path)
      (assert path nil "can't find file: ~s" path))
    (ns:with-event-loop (:waitp t)
      (let* ((ns-image (ns:objc (ns:objc "NSImage" "alloc" :pointer)
				"initWithContentsOfFile:"
				:pointer (ns:autorelease (ns:make-ns-string (namestring path)))
				:pointer)))
	(ns:objc ns-image "CGImageForProposedRect:context:hints:"
		 :pointer (cffi:null-pointer)
		 :pointer (cffi:null-pointer)
		 :pointer (cffi:null-pointer)
		 :pointer)))))

(cffi:defcfun ("CGImageRetain" retain-image) :pointer
  (cg-image :pointer))

(cffi:defcfun ("CGImageRelease" release-image) :void
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetWidth" image-width) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetHeight" image-height) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetBitsPerPixel" image-bits-per-pixel) :sizet
  (cg-image :pointer))

(cffi:defcfun ("CGImageGetBytesPerRow" image-bytes-per-row) :sizet
  (cg-image :pointer))



(defun image-bitmap-data (cg-image)
  "this function should be call in EventLoop"
  (let* ((ns-bitmap (ns:autorelease (ns:objc (ns:objc "NSBitmapImageRep" "alloc" :pointer)
					     "initWithCGImage:"
					     :pointer cg-image
					     :pointer))))
    (ns:objc ns-bitmap "bitmapData" :pointer)))


(defun image-from-screen (rect)
  (cffi:foreign-funcall "CGWindowListCreateImage"
			(:struct ns:rect) rect
			:int 12 	; kCGWindowListOptionIncludingWindow | kCGWindowListOptionOnScreenBelowWindow
			:int 0		; kCGNullWindowID
			:int 16		; kCGWindowImageNominalResolution
			:pointer))
