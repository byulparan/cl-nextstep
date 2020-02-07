(defpackage :io-surface
  (:use :cl)
  (:export #:make-surface
	   #:decrement-use-count
	   #:alloc-size
	   #:base-address
	   #:bytes-per-element
	   #:bytes-per-row
	   #:height
	   #:id
	   #:pixel-format
	   #:plane-count
	   #:type-id
	   #:use-count
	   #:width
	   #:increment-use-count
	   #:lookup))

(in-package :io-surface)

(defun make-surface (width height &optional (pixel-format "ARGB"))
  (let* ((dictionary (cffi:foreign-funcall "CFDictionaryCreateMutable"
					   :pointer (cffi:null-pointer)
					   :long 0
					   :pointer (cffi:foreign-symbol-pointer "kCFTypeDictionaryKeyCallBacks")
					   :pointer (cffi:foreign-symbol-pointer "kCFTypeDictionaryValueCallBacks")
					   :pointer))
	 (bytes-per-element 4)
	 (kCFNumberSInt32Type 3))
    (cffi:with-foreign-objects ((%bytes-per-row :int) 
				(%bytes-per-element :int)
				(%width :int)
				(%height :int)
				(%pixel-format :char 4)
				(%size :int))
      (setf (cffi:mem-ref %bytes-per-row :int) (* width bytes-per-element)
	    (cffi:mem-ref %bytes-per-element :int) bytes-per-element
	    (cffi:mem-ref %width :int) width
	    (cffi:mem-ref %height :int) height
	    (cffi:mem-ref %size :int) (* width height bytes-per-element))
      (loop for pix in (map 'list #'char-code pixel-format)
	    for i from 0 do (setf (cffi:mem-aref %pixel-format :char i) pix))
      (macrolet ((cf-dict-set (key value)
		   `(cffi:foreign-funcall "CFDictionarySetValue"
					  :pointer dictionary
					  :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer ,key) :pointer)
					  :pointer (ns:cf-autorelease
						    (cffi:foreign-funcall "CFNumberCreate"
									  :pointer (cffi:null-pointer)
									  :long kCFNumberSInt32Type
									  :pointer ,value
									  :pointer)))))
	(cf-dict-set "kIOSurfaceBytesPerRow" %bytes-per-row)
	(cf-dict-set "kIOSurfaceBytesPerElement" %bytes-per-element)
	(cf-dict-set "kIOSurfaceWidth" %width)
	(cf-dict-set "kIOSurfaceHeight" %height)
	(cf-dict-set "kIOSurfacePixelFormat" %pixel-format)
	(cf-dict-set "kIOSurfaceAllocSize" %size)
	(cffi:foreign-funcall "IOSurfaceCreate" :pointer dictionary :pointer)))))

(cffi:defcfun ("IOSurfaceDecrementUseCount" decrement-use-count) :void
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetAllocSize" alloc-size) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetBaseAddress" base-address) :pointer
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetBytesPerElement" bytes-per-element) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetBytesPerRow" bytes-per-row) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetHeight" height) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetID" id) :unsigned-int
  (buffer :pointer))

(defun pixel-format (buffer)
  (let* ((os-type (cffi:foreign-funcall "IOSurfaceGetPixelFormat"
					:pointer buffer
					:unsigned-int)))
    (map 'string #'code-char (list (logand os-type #xff)
				   (logand (ash os-type -8) #xff)
				   (logand (ash os-type -16) #xff)
				   (ash os-type -24)))))

(cffi:defcfun ("IOSurfaceGetPlaneCount" plane-count) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetTypeID" type-id) :unsigned-long)

(cffi:defcfun ("IOSurfaceGetUseCount" use-count) :int
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceGetWidth" width) :sizet
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceIncrementUseCount" increment-use-count) :void
  (buffer :pointer))

(cffi:defcfun ("IOSurfaceLookup" lookup) :pointer
  (csid :unsigned-int))






