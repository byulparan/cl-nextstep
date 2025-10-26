(in-package :cg)

;; CGImage
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


(defun make-image-from-screen (rect)
  (cffi:foreign-funcall "CGWindowListCreateImage"
			(:struct ns:rect) rect
			:int 12 	; kCGWindowListOptionIncludingWindow | kCGWindowListOptionOnScreenBelowWindow
			:int 0		; kCGNullWindowID
			:int 16		; kCGWindowImageNominalResolution
			:pointer))


(cffi:defcfun ("CGBitmapContextCreateImage" make-image-from-context) :pointer
  (bitmap-context :pointer))

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

(defun image-data (cg-image)
  "this function should be call in EventLoop"
  (let* ((ns-bitmap (ns:autorelease (ns:objc (ns:objc "NSBitmapImageRep" "alloc" :pointer)
					     "initWithCGImage:"
					     :pointer cg-image
					     :pointer))))
    (ns:objc ns-bitmap "bitmapData" :pointer)))

(defun write-to-png-file (image path)
  (ns:with-event-loop (:waitp t)
    (let* ((image-destination
	     (cffi:foreign-funcall "CGImageDestinationCreateWithURL"
				   :pointer 
				   (ns:objc "NSURL" "fileURLWithPath:" :pointer (ns:autorelease (ns:make-ns-string (uiop:native-namestring path))) :pointer)
				   :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "kUTTypePNG") :pointer)
				   :sizet 1
				   :pointer (cffi:null-pointer)
				   :pointer)))
      (unwind-protect (progn
			(cffi:foreign-funcall "CGImageDestinationAddImage" :pointer image-destination
									   :pointer image
									   :pointer (cffi:null-pointer))
			(cffi:foreign-funcall "CGImageDestinationFinalize" :pointer image-destination)))
      (ns:cf-release image-destination))))




;; CGBitmapContext
(defun make-bitmap-context (width height &key (data (cffi:null-pointer)) (color-space :color-space-srgb) (alpha-info :last) (bitmap-info :order-default))
  (ns:with-event-loop (:waitp t)
    (let* ((color-space (cg:make-color-space color-space)))
      (prog1
	  (cffi:foreign-funcall "CGBitmapContextCreate"
				:pointer data
				:sizet width
				:sizet height
				:sizet 8
				:sizet (* width 4)
				:pointer color-space 
				:unsigned-int (logior (ecase bitmap-info
							(:order-default 0)
							(:order-little 8192)
							(:order-big 16384))
						      (ecase alpha-info
							(:last 1)
							(:first 2)))
				:pointer)
	(cg:release-color-space color-space)))))
      

(cffi:defcfun ("CGBitmapContextGetData" context-data) :pointer
  (context :pointer))

(cffi:defcfun ("CGBitmapContextGetWidth" context-width) :sizet
  (context :pointer))

(cffi:defcfun ("CGBitmapContextGetHeight" context-height) :sizet
  (context :pointer))

(cffi:defcfun ("CGContextRelease" release-context) :void
  (context :pointer))
