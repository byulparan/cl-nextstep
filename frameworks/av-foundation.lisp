(defpackage :av
  (:use :cl)
  (:export #:with-media-data
	   #:list-camera-device
	   #:capture
	   #:release-capture
	   #:make-camera-capture
	   #:start-capture
	   #:stop-capture

	   #:player
	   #:make-player
	   #:release-player
	   #:status
	   #:play
	   #:pause
	   #:volume
	   #:seek-to-zero
	   #:ready?))

(in-package :av)

(defgeneric get-delegate (av-media))

(defmacro with-media-data ((av-media width height data) &body body)
  (alexandria:with-gensyms (m-head)
    `(let* ((,m-head (ns:objc (get-delegate ,av-media) "getImageBuffer" :pointer)))
       (unless (cffi:null-pointer-p ,m-head)
	 (cffi:foreign-funcall "CVPixelBufferLockBaseAddress" :pointer ,m-head :int 0)
	 (unwind-protect (let* ((,width (cffi:foreign-funcall "CVPixelBufferGetWidth" :pointer ,m-head :sizet))
				(,height (cffi:foreign-funcall "CVPixelBufferGetHeight" :pointer ,m-head :sizet))
				(,data (cffi:foreign-funcall "CVPixelBufferGetBaseAddress" :pointer ,m-head :pointer)))
			   ,@body)
	   (cffi:foreign-funcall "CVPixelBufferUnlockBaseAddress" :pointer ,m-head :int 0))))))

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

(defun make-capture-video-data-output (capture-delegate)
  (let* ((video-output (ns:autorelease (ns:objc (ns:alloc "AVCaptureVideoDataOutput") "init" :pointer))))
    (ns:objc video-output "setSampleBufferDelegate:queue:"
	     :pointer capture-delegate :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q"))
    (ns:objc video-output "setVideoSettings:"
	     :pointer (ns:objc "NSDictionary" "dictionaryWithObject:forKey:"
			       :pointer (ns:objc "NSNumber" "numberWithInt:" :int #x00000020 ;; kCVPixelFormatType_32ARGB
						 :pointer)
			       :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "kCVPixelBufferPixelFormatTypeKey")
						      :pointer)
			       :pointer))
    video-output))

(defstruct (capture (:constructor %make-capture (&key session delegate)))
  session delegate)

(defmethod get-delegate ((av-media capture))
  (capture-delegate av-media))

(defun make-capture (input)
  (let* ((session (ns:objc (ns:alloc "AVCaptureSession") "init" :pointer))
	 (capture-delegate (ns:objc (ns:alloc "CaptureDelegate") "init" :pointer))
	 (output (make-capture-video-data-output capture-delegate)))
    (ns:objc session "addInput:" :pointer input)
    (ns:objc session "addOutput:" :pointer output)
    (%make-capture :session session :delegate capture-delegate)))

(defun release-capture (capture)
  (ns:with-event-loop (:waitp t)
    (ns:release (capture-session capture))
    (ns:release (capture-delegate capture))))

(defun make-camera-capture (index)
  (ns:with-event-loop (:waitp t)
    (make-capture (make-capture-camera-input index))))

(defun start-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "startRunning")))

(defun stop-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "stopRunning")))




;; Player
(defvar *player-table* (make-hash-table))

(defstruct (player (:constructor %make-player (&key id object item delegate did-end)))
  id object item delegate did-end)

(defmethod get-delegate ((av-media player))
  (player-delegate  av-media))

(cffi:defcallback player-did-reach-end :void ((id :int))
  (alexandria:when-let* ((player (gethash id *player-table*))
			 (did-end (player-did-end player)))
    (funcall did-end)))

(let* ((id 0))
  (defun make-player (path &key did-end)
    (let* ((path (uiop:truenamize path)))
      (assert (probe-file path) nil "can't find file: ~a" path)
      (ns:with-event-loop (:waitp t)
	(let* ((av-player (ns:objc (ns:alloc "AVPlayer")
				   "initWithURL:" :pointer (ns:objc "NSURL" "fileURLWithPath:"
								    :pointer (ns:autorelease (ns:make-ns-string (namestring path)))
								    :pointer)
				   :pointer))
	       (av-player-item (ns:objc av-player "currentItem" :pointer))
	       (player-delegate (ns:objc (ns:alloc "PlayerDelegate")
					 "initWithID:endFn:" :int id
					 :pointer (cffi:callback player-did-reach-end)
					 :pointer)))
	  (ns:objc av-player-item "addObserver:forKeyPath:options:context:"
		   :pointer player-delegate
		   :pointer (ns:autorelease (ns:make-ns-string "status"))
		   :int 0
		   :pointer (cffi:null-pointer))
	  (let* ((player (%make-player :id id :object av-player :item av-player-item :delegate player-delegate)))
	    (setf (gethash id *player-table*) player)
	    (incf id)
	    player))))))

(defun release-player (player)
  (ns:release (player-object player))
  (ns:release (player-delegate player)))

(defun status (player)
  (ns:with-event-loop (:waitp t)
    (let* ((status (ns:objc (player-object player) "status" :int)))
      (case status
	(0 :unknown)
	(1 :ready-to-play)
	(2 :failed)))))

(defun play (player)
  (ns:with-event-loop nil
    (ns:objc (player-object player) "play")))

(defun pause (player)
  (ns:with-event-loop nil
    (ns:objc (player-object player) "pause")))

(defun volume (player volume)
  (ns:with-event-loop nil
    (ns:objc (player-object player) "setVolume:" :float (float volume 1.0))))

(defun ready? (player)
  (= 1 (ns:objc (player-delegate player) "ready" :int)))

;; CoreMedia
(cffi:defcstruct cm-time
  (value :int64)
  (timescale :int)
  (flags :unsigned-int)
  (epoch :int64))

(defun seek-to-zero (player)
  (ns:with-event-loop nil
    (ns:objc (player-object player) "seekToTime:"
	     (:struct cm-time) (cffi:mem-ref (cffi:foreign-symbol-pointer "kCMTimeZero") '(:struct cm-time)) )))

