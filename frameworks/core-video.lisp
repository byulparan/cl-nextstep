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

	   #:display-link-set-current-display
	   #:display-link-set-current-display-from-opengl-context
	   #:display-link-current-display
	   #:display-link-current-time
	   #:display-link-actual-output-video-refresh-period
	   #:display-link-nominal-output-video-refresh-period
	   #:display-link-output-video-latency
	   #:display-link-is-running
	   #:display-link-type-id
	   #:display-link-start
	   #:display-link-stop
	   
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


;; =======================================================
;; Data Processing
;; =======================================================


;; CVPixelFormatType
(defconstant +pixel-format-type-24-rgb+ #x00000018)
(defconstant +pixel-format-type-24-bgr+ 842285639)
(defconstant +pixel-format-type-32-argb+ #x00000020)
(defconstant +pixel-format-type-32-rgba+ 1380401729)
(defconstant +pixel-format-type-32-bgra+ 1111970369)
(defconstant +pixel-format-type-32-abgr+ 1094862674)


;; CVBuffer
(cffi:defcfun ("CVBufferRetain" %buffer-retain) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVBufferRelease" %buffer-release) :pointer
  (buffer :pointer))


;; CVPixelBuffer
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

(cffi:defcfun ("CVPixelBufferLockBaseAddress" buffer-lock-base-address) :int
  (buffer :pointer)
  (lock-flags :int))

(cffi:defcfun ("CVPixelBufferUnlockBaseAddress" buffer-unlock-base-address) :int
  (buffer :pointer)
  (unlock-flags :int))

(cffi:defcfun ("CVPixelBufferRetain" retain-buffer) :pointer
  (buffer :pointer))

(cffi:defcfun ("CVPixelBufferRelease" release-buffer) :void
  (buffer :pointer))



;; =======================================================
;; Time Management
;; =======================================================

(cffi:defcstruct cv-smpte-time
  (counter :uint32)
  (flags :uint32)
  (frames :int16)
  (hours :int16)
  (minutes :int16)
  (seconds :int16)
  (subframe-divisor :int16)
  (subframes :int16)
  (type :uint32))

(cffi:defcstruct cv-time-stamp
  (flags :uint64)
  (host-time :uint64)
  (rate-scalar :double)
  (reserved :uint64)
  (smpte-time (:struct cv-smpte-time))
  (version :uint32)
  (vido-refresh-period :int64)
  (video-time :int64)
  (video-time-scale :int32))

(cffi:defcstruct cv-time
  (flags :int)
  (time-scale :int)
  (time-value :int64))

;; Configuring Display Links
(cffi:defcfun ("CVDisplayLinkSetCurrentCGDisplay" display-link-set-current-display) :int32
  "Sets the current display of a display link."
  (display-link :pointer)
  (display :uint32))

(cffi:defcfun ("CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext" display-link-set-current-display-from-opengl-context) :int32
  "Selects the display link most optimal for the current renderer of an OpenGL context. This function chooses the display with the lowest refresh rate."
  (display-link :pointer)
  (cgl-context :pointer)
  (pixel-format :pointer))

;; Inspecting Display Links
(cffi:defcfun ("CVDisplayLinkGetCurrentCGDisplay" display-link-current-display) :uint32
  "Gets the current display associated with a display link."
  (display-link :pointer))

(defun display-link-current-time (display-link)
  "Retrieves the current (“now”) time of a given display link"
  (cffi:with-foreign-object (out '(:struct cv-time-stamp))
    (cffi:foreign-funcall "CVDisplayLinkGetCurrentTime" :pointer display-link
							:pointer out)
    (cffi:mem-ref out '(:struct cv-time-stamp))))

(cffi:defcfun ("CVDisplayLinkGetActualOutputVideoRefreshPeriod" display-link-actual-output-video-refresh-period) :double
  "Retrieves the actual output refresh period of a display as measured by the system time. This call returns the actual output refresh period computed relative to the system time (as measured using the CVGetCurrentHostTime function)."
  (display-link :pointer))

(cffi:defcfun ("CVDisplayLinkGetNominalOutputVideoRefreshPeriod" display-link-nominal-output-video-refresh-period) (:struct cv-time)
  "Retrieves the nominal refresh period of a display link. This call allows one to retrieve the device's ideal refresh period. For example, an NTSC output device might report 1001/60000 to represent the exact NTSC vertical refresh rate."
  (display-link :pointer))

(cffi:defcfun ("CVDisplayLinkGetOutputVideoLatency" display-link-output-video-latency) (:struct cv-time)
  "Retrieves the nominal latency of a display link. This call allows you to retrieve the device’s built-in output latency. For example, an NTSC device with one frame of latency might report back 1001/30000 or 2002/60000."
  (display-link :pointer))

(cffi:defcfun ("CVDisplayLinkIsRunning" display-link-is-running) :bool
  "Indicates whether a given display link is running. Returns true if the display link is running, false otherwise."
  (display-link :pointer))

(cffi:defcfun ("CVDisplayLinkGetTypeID" display-link-type-id) :unsigned-long
  "Obtains the Core Foundation ID for the display link data type.")


;; Managing Display Links
(cffi:defcfun ("CVDisplayLinkStart" display-link-start) :int32
  "Activates a display link. Calling this function starts the display link thread, which then periodically calls back to your application to request that you display frames. If the specified display link is already running, CVDisplayLinkStart returns an error."
  (display-link :pointer))

(cffi:defcfun ("CVDisplayLinkStop" display-link-stop) :int32
  "Stops a display link. If the specified display link is already stopped, CVDisplayLinkStop returns an error.

In macOS 10.4 and later, the display link thread is automatically stopped if the user employs Fast User Switching. The display link is restarted when switching back to the original user."
  (display-link :pointer))


;; =======================================================
;; OpenGL
;; =======================================================

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

(cffi:defcfun ("CVOpenGLTextureGetName" texture-name) :unsigned-int
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureGetTarget" texture-target) :int
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureRetain" retain-texture) :pointer
  (texture :pointer))

(cffi:defcfun ("CVOpenGLTextureRelease" release-texture) :void
  (texture :pointer))

