(defpackage :core-video
  (:use :cl :alexandria)
  (:export #:make-buffer
	   #:buffer-create
	   #:buffer-create-bytes
	   #:buffer-create-io-surface
	   #:buffer-base-address
	   #:buffer-bytes-per-row
	   #:buffer-height
	   #:buffer-width
	   #:buffer-data-size
	   #:buffer-pixel-format
	   #:buffer-io-sruface
	   #:buffer-lock-base-address
	   #:buffer-unlock-base-address
	   #:buffer-retain
	   #:buffer-release))

(in-package :core-video)

;; =======================================================
;; CVBuffer

;; Retaining and Releasing Buffers
(cffi:defcfun ("CVBufferRetain" %buffer-retain) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVBufferRelease" %buffer-release) :pointer
  (buffer :pointer))


;; =======================================================
;; CVPixelBuffer

;; Creating Pixel Buffers
(cffi:defcfun ("CVPixelBufferCreate" buffer-create) :int
  (allocator :pointer)
  (width :sizet)
  (height :sizet)
  (pixel-format-type :unsigned-int)
  (pixel-format-attributes :pointer)
  (out-buffer :pointer))

(defun make-buffer (width height &optional (pixel-format "ARGB"))
  (flet ((encode-type (type)
	   (assert (= 4 (length type)) nil "~s's size is not 4" type)
	   (cond ((string= pixel-format "ARGB") 32)
		 (t (let ((codes (map 'list #'char-code type)))
		      (+ (ash (nth 0 codes) 24)
			 (ash (nth 1 codes) 16)
			 (ash (nth 2 codes)  8)
			 (nth 3 codes)))))))
    (cffi:with-foreign-objects ((buffer :pointer))
      (let* ((result (buffer-create (cffi:null-pointer) width height (encode-type pixel-format)
				   (cffi:null-pointer) buffer)))
	(assert (zerop result) nil "can't make CVPixelBuffer. err: ~d" result)
	(cffi:mem-ref buffer :pointer)))))

(cffi:defcfun ("CVPixelBufferCreateWithBytes" buffer-create-bytes) :int
  (allocator :pointer)
  (width :sizet)
  (height :sizet)
  (pixel-format-type :pointer)
  (base-address :pointer)
  (bytes-per-row :sizet)
  (release-callback :pointer)
  (release-refcon :pointer)
  (pixel-buffer-attributes :pointer)
  (out-buffer :pointer))

(cffi:defcfun ("CVPixelBufferCreateWithIOSurface" buffer-create-io-surface) :int
  (allocator :pointer)
  (surface :pointer)
  (pixel-buffer-attributes :pointer)
  (out-buffer :pointer))

;; Inspecting Pixel Buffers
(cffi:defcfun ("CVPixelBufferGetBaseAddress" buffer-base-address) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferGetBytesPerRow" buffer-bytes-per-row) :sizet
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferGetHeight" buffer-height) :sizet
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferGetWidth" buffer-width) :sizet
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferGetDataSize" buffer-data-size) :sizet
  (buffer :pointer))

(defun buffer-pixel-format (buffer)
  (flet ((decode-type (type)
	   (cond ((= type 32) "ARGB")
		 (t (map 'string #'code-char
			 (list (ash type -24)
			       (logand (ash type -16) #xff)
			       (logand (ash type -8) #xff)
			       (logand type #xff)))))))
    (decode-type
     (cffi:foreign-funcall "CVPixelBufferGetPixelFormatType"
			   :pointer buffer
			   :unsigned-int))))

(cffi:defcfun ("CVPixelBufferGetIOSurface" buffer-io-surface) :pointer
  (buffer :pointer))


;; Modifying Pixel Buffers
(cffi:defcfun ("CVPixelBufferLockBaseAddress" buffer-lock-base-address) :int
  (buffer :pointer)
  (lock-flags :int))

(cffi:defcfun ("CVPixelBufferUnlockBaseAddress" buffer-unlock-base-address) :int
  (buffer :pointer)
  (unlock-flags :int))

;; Retaining and Releasing Pixel Buffers
(cffi:defcfun ("CVPixelBufferRetain" buffer-retain) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferRelease" buffer-release) :void
  (buffer :pointer))
