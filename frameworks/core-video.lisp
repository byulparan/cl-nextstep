(defpackage :core-video
  (:use :cl :alexandria)
  (:export #:+pixel-format-type-24-rgb+
	   #:+pixel-format-type-24-bgr+
	   #:+pixel-format-type-32-argb+
	   #:+pixel-format-type-32-rgba+
	   #:+pixel-format-type-32-bgra+
	   #:+pixel-format-type-32-abgr+
	   
	   #:make-buffer
	   #:make-buffer-with-bytes
	   #:make-buffer-with-io-surface
	   #:buffer-base-address
	   #:buffer-bytes-per-row
	   #:buffer-height
	   #:buffer-width
	   #:buffer-data-size
	   #:buffer-pixel-format-type
	   #:buffer-io-surface
	   #:buffer-lock-base-address
	   #:buffer-unlock-base-address
	   #:retain-buffer
	   #:release-buffer
	   #:make-texture-cache
	   #:texture-cache-texture
	   #:texture-cache-flush
	   #:retain-texture-cache
	   #:release-texture-cache
	   #:texture-name
	   #:texture-target
	   #:retain-texture
	   #:release-texture))

(in-package :core-video)


;; CVPixelFormatType
(defconstant +pixel-format-type-24-rgb+ #x00000018)
(defconstant +pixel-format-type-24-bgr+ 842285639)
(defconstant +pixel-format-type-32-argb+ #x00000020)
(defconstant +pixel-format-type-32-rgba+ 1380401729)
(defconstant +pixel-format-type-32-bgra+ 1111970369)
(defconstant +pixel-format-type-32-abgr+ 1094862674)

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

(defun make-buffer (width height &optional (pixel-format-type :argb))
  (cffi:with-foreign-objects ((buffer :pointer))
    (let* ((result (buffer-create (cffi:null-pointer) width height (case pixel-format-type
								     (:rgb +pixel-format-type-24-rgb+)
								     (:bgr +pixel-format-type-24-bgr+)
								     (:argb +pixel-format-type-32-argb+)
								     (:rgba +pixel-format-type-32-rgba+)
								     (:bgra +pixel-format-type-32-bgra+)
								     (:abgr +pixel-format-type-32-abgr+)
								     (t pixel-format-type))
				  (cffi:null-pointer) buffer)))
      (assert (zerop result) nil "can't make CVPixelBuffer. err: ~d" result)
      (cffi:mem-ref buffer :pointer))))

(cffi:defcfun ("CVPixelBufferCreateWithBytes" make-buffer-with-bytes) :int
  (allocator :pointer)
  (width :sizet)
  (height :sizet)
  (pixel-format-type :unsigned-int)
  (base-address :pointer)
  (bytes-per-row :sizet)
  (release-callback :pointer)
  (release-refcon :pointer)
  (pixel-buffer-attributes :pointer)
  (out-buffer :pointer))

(cffi:defcfun ("CVPixelBufferCreateWithIOSurface" make-buffer-with-io-surface) :int
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


(cffi:defcfun ("CVPixelBufferGetPixelFormatType" buffer-pixel-format-type) :unsigned-int
  (buffer :pointer))


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
(cffi:defcfun ("CVPixelBufferRetain" retain-buffer) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferRelease" release-buffer) :void
  (buffer :pointer))

;; =======================================================
;; CVOpenGLTextureCache
(defun make-texture-cache (cgl-context cgl-pixel-format)
  (cffi:with-foreign-objects ((cache-out :pointer))
    (cffi:foreign-funcall "CVOpenGLTextureCacheCreate"
			  :pointer (cffi:null-pointer) ;; allocator
			  :pointer (cffi:null-pointer) ;; cache-attributes
			  :pointer cgl-context
			  :pointer cgl-pixel-format
			  :pointer (cffi:null-pointer) ;; texture-attributes
			  :pointer cache-out
			  :int)
    (cffi:mem-ref cache-out :pointer)))

(defun texture-cache-texture (texture-cache buffer)
  (cffi:with-foreign-objects ((texture-out :pointer))
    (cffi:foreign-funcall "CVOpenGLTextureCacheCreateTextureFromImage"
			   :pointer (cffi:null-pointer)
			   :pointer texture-cache
			   :pointer buffer
			   :pointer (cffi:null-pointer)
			   :pointer texture-out
			   :int)
    (cffi:mem-ref texture-out :pointer)))

(cffi:defcfun ("CVOpenGLTextureCacheFlush" texture-cache-flush) :void
  (texture-cache :pointer)
  (options :int))

(cffi:defcfun ("CVOpenGLTextureCacheRetain" retain-texture-cache) :pointer
  (texture-cache :pointer))

(cffi:defcfun ("CVOpenGLTextureCacheRelease" release-texture-cache) :void
  (texture-cache :pointer))

;; =======================================================
;; CVOpenGLTexture
(cffi:defcfun ("CVOpenGLTextureGetName" texture-name) :unsigned-int
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureGetTarget" texture-target) :int
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureRetain" retain-texture) :pointer
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureRelease" release-texture) :void
  (texture :pointer))
