(defpackage #:io-surface
  (:use #:cl)
  (:nicknames :ios)
  (:export #:make-surface))

(in-package :io-surface)

(defun make-surface (width height)
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
      (loop for pix in (map 'list #'char-code "ARGB")
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

