(defpackage :av-foundation
  (:nicknames :av)
  (:use :cl)
  (:export #:ready
	   #:pixel-buffer
	   
	   #:list-camera-device
	   #:capture
	   #:release-capture
	   #:make-camera-capture
	   #:make-screen-capture
	   #:crop-rect
	   #:min-frame-duration
	   #:scale-factor
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

(defgeneric pixel-buffer (av-media))
(defgeneric ready (av-media))

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

(defun make-capture-screen-input (&optional crop-rect min-frame-duration)
  (let* ((capture (ns:objc (ns:alloc "AVCaptureScreenInput") "initWithDisplayID:"
			   :unsigned-int (cffi:foreign-funcall "CGMainDisplayID"
							       :unsigned-int)
			   :pointer)))
    (when crop-rect
      (ns:objc capture "setCropRect:" (:struct ns:rect) crop-rect))
    (when min-frame-duration
      (ns:objc capture "setMinFrameDuration:"
	       (:struct cm-time) (cffi:foreign-funcall "CMTimeMake"
						       :int64 1
						       :int min-frame-duration
						       (:struct cm-time))))
    (ns:autorelease capture)))

(defun make-capture-video-data-output (capture-delegate pixel-format-type request-size)
  (let* ((video-output (ns:autorelease (ns:objc (ns:alloc "AVCaptureVideoDataOutput") "init" :pointer)))
	 (dictionary (ns:objc "NSMutableDictionary" "dictionaryWithCapacity:" :unsigned-int 10 :pointer)))
    (ns:objc video-output "setSampleBufferDelegate:queue:"
	     :pointer capture-delegate :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q"))
    (macrolet ((ns-number-bool (value)
		 `(ns:objc "NSNumber" "numberWithBool:" :bool ,value :pointer))
	       (ns-number-int (value)
		 `(ns:objc "NSNumber" "numberWithInt:" :int ,value :pointer))
	       (ns-number-double (value)
		 `(ns:objc "NSNumber" "numberWithDouble:" :double (float ,value 1.0d0) :pointer))
	       (ns-key (key)
		 `(cffi:mem-ref (cffi:foreign-symbol-pointer ,key) :pointer))
	       (set-option (key value)
		 `(ns:objc dictionary "setObject:forKey:" :pointer ,value
							  :pointer (ns-key ,key))))
      (set-option "kCVPixelBufferPixelFormatTypeKey" (ns-number-int (case pixel-format-type
								    (:bgr core-video:+pixel-format-type-24-bgr+)
								    (:rgb core-video:+pixel-format-type-24-rgb+)
								    (:abgr core-video:+pixel-format-type-32-abgr+)
								    (:argb core-video:+pixel-format-type-32-argb+)
								    (:bgra core-video:+pixel-format-type-32-bgra+)
								    (:rgba core-video:+pixel-format-type-32-rgba+)
								    (t pixel-format-type))))
      (set-option "kCVPixelBufferOpenGLCompatibilityKey" (ns-number-bool t))
      (when request-size
	(set-option "kCVPixelBufferWidthKey" (ns-number-double (first request-size)))
	(set-option "kCVPixelBufferHeightKey" (ns-number-double (second request-size)))))
    (ns:objc video-output "setVideoSettings:" :pointer dictionary)
    video-output))

(defstruct (capture (:constructor %make-capture (&key session delegate input-type)))
  session delegate input-type)

(defmethod ready ((av-media capture))
  t)

(defmethod pixel-buffer ((av-media capture))
  (ns:objc (capture-delegate av-media) "getPixelBuffer" :pointer))

(defun make-capture (input input-type pixel-format-type request-size)
  (let* ((session (ns:objc (ns:alloc "AVCaptureSession") "init" :pointer))
	 (capture-delegate (ns:objc (ns:alloc "CaptureDelegate") "init" :pointer))
	 (output (make-capture-video-data-output capture-delegate pixel-format-type request-size)))
    (ns:objc session "addInput:" :pointer input)
    (ns:objc session "addOutput:" :pointer output)
    

    (%make-capture :session session :delegate capture-delegate :input-type input-type)))

(defun make-camera-capture (index &key (pixel-format-type :argb) request-size)
  (ns:with-event-loop (:waitp t)
    (make-capture (make-capture-camera-input index) :camera pixel-format-type request-size)))

(defun make-screen-capture (&key crop-rect min-frame-duration (pixel-format-type :argb) request-size)
  (ns:with-event-loop (:waitp t)
    (make-capture (make-capture-screen-input crop-rect min-frame-duration)
		  :screen pixel-format-type request-size)))

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
      #+x86-64 (ns:objc-stret cm-time input "minFrameDuration")
      #+arm64 (ns:objc input "minFrameDuration" (:struct cm-time)))))

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

(defun scale-factor (screen-capture)
  (ns:with-event-loop (:waitp t)
    (let* ((input (ns:objc 
		   (ns:objc (av::capture-session screen-capture) "inputs" :pointer)
		   "objectAtIndex:" :int 0 :pointer)))
      (ns:objc input "scaleFactor" :double))))

(defun (setf scale-factor) (scale-factor screen-capture)
  (ns:with-event-loop nil
    (let* ((input (ns:objc 
		   (ns:objc (av::capture-session screen-capture) "inputs" :pointer)
		   "objectAtIndex:" :int 0 :pointer)))
      (ns:objc input "setScaleFactor:" :double (float scale-factor 1.0d0)))))


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

(defstruct (player (:constructor %make-player (&key id manager ready-fn end-fn)))
  id manager ready-fn end-fn)

(defmethod pixel-buffer ((av-media player))
  (ns:objc (player-manager av-media) "getPixelBuffer" :pointer))

(cffi:defcallback player-handler :void ((id :int) (command :int))
  (alexandria:when-let* ((player (gethash id *player-table*))
			 (handler (case command
				    (0 (player-ready-fn player))
				    (1 (player-end-fn player)))))
    (funcall handler)))

(let* ((id 0))
  (defun make-player (path &key ready-fn end-fn request-size)
    (let* ((path (uiop:truenamize path))
	   (player nil)
	   (ready-object nil)
	   (ready-handle ready-fn))
      (assert (probe-file path) nil "can't find file: ~a" path)
      (unless (or (eql (bt:current-thread) (trivial-main-thread:main-thread))
		  ready-handle)
	(setf ready-object
	  #+sbcl (sb-thread:make-semaphore)
	  #+ccl (ccl:make-semaphore)
	  #+lispworks (mp:make-semaphore)
	  #+ecl (mp:make-semaphore)
	  ready-handle (lambda ()
			#+sbcl(sb-thread:signal-semaphore ready-object)
			#+ccl (ccl:signal-semaphore ready-object)
			#+lispworks (mp:semaphore-release ready-object)
			#+ecl (mp:signal-semaphore ready-object))))
      (ns:with-event-loop (:waitp t)
	(setf player (%make-player :id id :ready-fn ready-handle :end-fn end-fn))
	(setf (gethash id *player-table*) player)
	(setf (player-manager player) (ns:objc (ns:alloc "PlayerManager")
					       "initWithID:path:requestSize:handlerFn:"
					       :int id
					       :pointer (ns:autorelease (ns:make-ns-string (namestring path)))
					       (:struct ns:size) (if request-size (ns:size (first request-size) (second request-size))
								   (ns:size -1 -1))
					       :pointer (cffi:callback player-handler)
					       :pointer))
	(incf id))
      (when ready-object
	#+sbcl (sb-thread:wait-on-semaphore ready-object))
      #+ccl (ccl:wait-on-semaphore ready-object)
      #+lispworks (mp:semaphore-acquire ready-object)
      #+ecl (mp:wait-on-semaphore ready-object)
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

