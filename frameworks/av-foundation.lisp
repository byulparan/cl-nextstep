(defpackage :av-foundation
  (:nicknames :av)
  (:use :cl)
  (:export #:with-media-data
	   #:ready
	   #:image-buffer
	   
	   #:list-camera-device
	   #:capture
	   #:release-capture
	   #:make-camera-capture
	   #:make-screen-capture
	   #:crop-rect
	   #:min-frame-duration
	   #:start-capture
	   #:stop-capture
	   
	   #:player
	   #:make-player
	   #:release-player
	   #:status
	   #:play
	   #:pause
	   #:volume
	   #:seek-to-zero))

(in-package :av-foundation)

;; CoreMedia
(cffi:defcstruct cm-time
  (value :int64)
  (timescale :int)
  (flags :unsigned-int)
  (epoch :int64))

(defgeneric image-buffer (av-media))
(defgeneric ready (av-media))

(defmacro with-media-data ((av-media width height data) &body body)
  (alexandria:with-gensyms (m-head)
    `(when (ready ,av-media)
       (let* ((,m-head (image-buffer ,av-media)))
	 (unless (cffi:null-pointer-p ,m-head)
	   (cffi:foreign-funcall "CVPixelBufferLockBaseAddress" :pointer ,m-head :int 0)
	   (unwind-protect (let* ((,width (cffi:foreign-funcall "CVPixelBufferGetWidth" :pointer ,m-head :sizet))
				  (,height (cffi:foreign-funcall "CVPixelBufferGetHeight" :pointer ,m-head :sizet))
				  (,data (cffi:foreign-funcall "CVPixelBufferGetBaseAddress" :pointer ,m-head :pointer)))
			     ,@body)
	     (cffi:foreign-funcall "CVPixelBufferUnlockBaseAddress" :pointer ,m-head :int 0)))))))

;; Capture
(defun list-camera-device ()
  (ns:with-event-loop nil
    (let* ((devices (ns:objc "AVCaptureDevice" "devicesWithMediaType:"
			     :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "AVMediaTypeVideo") :pointer)
			     :pointer)))
      (loop for i from 0 below (ns:objc devices "count" :unsigned-int)
	    do (format t "[~d]: ~a~%" i
		       (ns:ns-string-to-lisp 
			(ns:objc (ns:objc devices "objectAtIndex:" :unsigned-int i :pointer) "localizedName"
				 :pointer)))))))

(defun make-capture-camera-input (index)
  (let* ((devices (ns:objc "AVCaptureDevice" "devicesWithMediaType:"
			   :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "AVMediaTypeVideo") :pointer)
			   :pointer)))
    (assert (> (ns:objc devices "count" :unsigned-int) index) nil "out of index camera device list")
    (let* ((dev (ns:objc devices "objectAtIndex:" :unsigned-int index :pointer)))
      (cffi:with-foreign-objects ((err :int))
	(let* ((input (ns:objc "AVCaptureDeviceInput" "deviceInputWithDevice:error:"
			       :pointer dev :pointer err :pointer))
	       (code (cffi:mem-ref err :int)))
	  (assert (zerop code) nil "Error while make camera capture: ~a" code)
	  input)))))

(defun make-capture-screen-input (&optional crop-rect)
  (let* ((capture (ns:objc (ns:alloc "AVCaptureScreenInput") "initWithDisplayID:"
			   :int 2077750265 ;; kCGDirectMainDisplay
			   :pointer)))
    (when crop-rect
      (ns:objc capture "setCropRect:" (:struct ns:rect) crop-rect))
    (ns:autorelease capture)))

(defun make-capture-video-data-output (capture-delegate)
  (let* ((video-output (ns:autorelease (ns:objc (ns:alloc "AVCaptureVideoDataOutput") "init" :pointer)))
	 (dictionary (ns:objc "NSMutableDictionary" "dictionaryWithCapacity:" :unsigned-int 2 :pointer)))
    (macrolet ((ns-number (value)
		 `(ns:objc "NSNumber" "numberWithInt:" :int ,value :pointer))
	       (ns-key (key)
		 `(cffi:mem-ref (cffi:foreign-symbol-pointer ,key) :pointer)))
      (ns:objc video-output "setSampleBufferDelegate:queue:"
	       :pointer capture-delegate :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q"))
      (ns:objc dictionary "setObject:forKey:" :pointer (ns-number #x00000020) ;; kCVPixelFormatType_32ARGB
					      :pointer (ns-key "kCVPixelBufferPixelFormatTypeKey"))
      (ns:objc dictionary "setObject:forKey:" :pointer (ns-number 1)
					      :pointer (ns-key "kCVPixelBufferOpenGLCompatibilityKey"))
      (ns:objc video-output "setVideoSettings:" :pointer dictionary)
      video-output)))

(defstruct (capture (:constructor %make-capture (&key session delegate input-type)))
  session delegate input-type)

(defmethod ready ((av-media capture))
  t)

(defmethod image-buffer ((av-media capture))
  (ns:objc (capture-delegate av-media) "getImageBuffer" :pointer))

(defun make-capture (input input-type)
  (let* ((session (ns:objc (ns:alloc "AVCaptureSession") "init" :pointer))
	 (capture-delegate (ns:objc (ns:alloc "CaptureDelegate") "init" :pointer))
	 (output (make-capture-video-data-output capture-delegate)))
    (ns:objc session "addInput:" :pointer input)
    (ns:objc session "addOutput:" :pointer output)
    (%make-capture :session session :delegate capture-delegate :input-type input-type)))

(defun make-camera-capture (index)
  (ns:with-event-loop (:waitp t)
    (make-capture (make-capture-camera-input index) :camera)))

(defun make-screen-capture (&optional rect)
  (ns:with-event-loop (:waitp t)
    (make-capture (make-capture-screen-input rect) :screen)))

(defun crop-rect (screen-capture rect)
  (assert (eql :screen (capture-input-type screen-capture)) nil)
  (ns:with-event-loop nil
    (let* ((input (ns:objc 
		   (ns:objc (capture-session screen-capture) "inputs" :pointer)
		   "objectAtIndex:" :int 0 :pointer)))
      (ns:objc input "setCropRect:" (:struct ns:rect) rect))))

(defun min-frame-duration (screen-capture)
  (ns:with-event-loop (:waitp t)
    (let* ((input (ns:objc 
		   (ns:objc (capture-session screen-capture) "inputs" :pointer)
		   "objectAtIndex:" :int 0 :pointer)))
      (ns:objc-stret cm-time input "minFrameDuration"))))

(defun (setf min-frame-duration) (framerate screen-capture)
  (ns:with-event-loop nil
    (let* ((input (ns:objc 
		   (ns:objc (capture-session screen-capture) "inputs" :pointer)
		   "objectAtIndex:" :int 0 :pointer)))
      (ns:objc input "setMinFrameDuration:"
	       (:struct cm-time) (cffi:foreign-funcall "CMTimeMake"
						       :int64 1
						       :int framerate
						       (:struct cm-time))))))

(defun release-capture (capture)
  (ns:with-event-loop (:waitp t)
    (ns:release (capture-session capture))
    (ns:release (capture-delegate capture))))

(defun start-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "startRunning")))

(defun stop-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "stopRunning")))


;; Player
(defvar *player-table* (make-hash-table))

(defstruct (player (:constructor %make-player (&key id manager load-fn end-fn)))
  id manager load-fn end-fn)

(defmethod image-buffer ((av-media player))
  (ns:objc (player-manager av-media) "getImageBuffer" :pointer))

(cffi:defcallback player-handler :void ((id :int) (command :int))
  (alexandria:when-let* ((player (gethash id *player-table*))
			 (handler (case command
				    (0 (player-load-fn player))
				    (1 (player-end-fn player)))))
    (funcall handler)))

(let* ((id 0))
  (defun make-player (path &key load-fn end-fn)
    (let* ((path (uiop:truenamize path))
	   (player nil)
	   (load-object nil)
	   (load-handle load-fn))
      (assert (probe-file path) nil "can't find file: ~a" path)
      (unless (or (eql (bt:current-thread) (trivial-main-thread:find-main-thread))
		  load-handle)
	(setf load-object (sb-thread:make-semaphore)
	      load-handle (lambda () (sb-thread:signal-semaphore load-object))))
      (ns:with-event-loop (:waitp t)
	(setf player (%make-player :id id :load-fn load-handle :end-fn end-fn))
	(setf (gethash id *player-table*) player)
	(setf (player-manager player) (ns:objc (ns:alloc "PlayerManager")
					       "initWithID:path:handlerFn:"
					       :int id
					       :pointer (ns:autorelease (ns:make-ns-string (namestring path)))
					       :pointer (cffi:callback player-handler)
					       :pointer))
	(incf id))
      (when load-object
	(sb-thread:wait-on-semaphore load-object))
      player)))

(defun release-player (player)
  (ns:release (player-manager player)))

(defmethod ready ((av-media player))
  (ns:with-event-loop (:waitp t)
    (= 1 (ns:objc (player-manager av-media) "ready" :int))))

(defun play (player)
  (ns:with-event-loop nil
    (ns:objc (ns:objc (player-manager player) "player" :pointer) "play")))

(defun pause (player)
  (ns:with-event-loop nil
    (ns:objc (ns:objc (player-manager player) "player" :pointer) "pause")))

(defun volume (player volume)
  (ns:with-event-loop nil
    (ns:objc (ns:objc (player-manager player) "player" :pointer) "setVolume:" :float (float volume 1.0))))

(defun seek-to-zero (player)
  (ns:with-event-loop nil
    (ns:objc (ns:objc (player-manager player) "player" :pointer) "seekToTime:"
	     (:struct cm-time) (cffi:mem-ref (cffi:foreign-symbol-pointer "kCMTimeZero") '(:struct cm-time)))))

