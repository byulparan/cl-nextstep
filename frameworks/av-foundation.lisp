(defpackage :av
  (:use :cl)
  (:export #:with-media-data
	   #:list-camera-device
	   #:capture
	   #:make-camera-capture
	   #:start-capture
	   #:stop-capture
	   #:release-capture

	   #:player
	   #:make-player
	   #:status
	   #:play
	   #:pause
	   #:volume
	   #:release-player))

(in-package :av)

(defgeneric get-delegate (av-media))

(defmacro with-media-data ((av-media bpr width height data) &body body)
  (alexandria:with-gensyms (m-head)
    `(let* ((,m-head (ns:objc (get-delegate ,av-media) "getImageBuffer" :pointer)))
       (unless (cffi:null-pointer-p ,m-head)
	 (cffi:foreign-funcall "CVPixelBufferLockBaseAddress" :pointer ,m-head :int 0)
	 (unwind-protect (let* ((,bpr (cffi:foreign-funcall "CVPixelBufferGetBytesPerRow" :pointer ,m-head :sizet))
				(,width (cffi:foreign-funcall "CVPixelBufferGetWidth" :pointer ,m-head :sizet))
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

(defun make-capture-input-camera (index)
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

(defstruct capture
  session delegate)

(defmethod get-delegate ((av-media capture))
  (capture-delegate av-media))

(defun make-capture-device (input)
  (let* ((session (ns:objc (ns:alloc "AVCaptureSession") "init" :pointer))
	 (capture-delegate (ns:objc (ns:alloc "CaptureDelegate") "init" :pointer))
	 (output (make-capture-video-data-output capture-delegate)))
    (ns:objc session "addInput:" :pointer input)
    (ns:objc session "addOutput:" :pointer output)
    (make-capture :session session :delegate capture-delegate)))

(defun make-camera-capture (index)
  (ns:with-event-loop (:waitp t)
    (make-capture-device (make-capture-input-camera index))))

(defun start-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "startRunning")))

(defun stop-capture (capture)
  (ns:with-event-loop nil
    (ns:objc (capture-session capture) "stopRunning")))

(defun release-capture (capture)
  (ns:with-event-loop (:waitp t)
    (ns:release (capture-session capture))
    (ns:release (capture-delegate capture))))


;; Player
(defvar *player-table* (make-hash-table))

(defclass player ()
  ((id :accessor id)
   (g-id :initform 0 :accessor g-id :allocation :class)
   (player :accessor player)
   (player-item :accessor player-item)
   (player-delegate :accessor player-delegate)
   (did-end :initarg :did-end :initform nil :accessor did-end)))

(defmethod get-delegate ((av-media player))
  (player-delegate av-media))

(cffi:defcallback player-did-reach-end :void ((id :int))
  (alexandria:when-let* ((player (gethash id *player-table*))
			 (did-end (did-end player)))
    (funcall did-end)))

(defmethod initialize-instance :after ((self player) &key did-end)
  (setf (id self) (g-id self))
  (incf (g-id self))
  (setf (gethash (id self) *player-table*) self))

(defun make-player (path &key did-end)
  (let* ((path (uiop:truenamize path)))
    (assert (probe-file path) nil "can't find file: ~a" path)
    (ns:with-event-loop (:waitp t)
      (let* ((player (make-instance 'player :did-end did-end))
	     (av-player (ns:objc (ns:alloc "AVPlayer")
				 "initWithURL:" :pointer (ns:objc "NSURL" "fileURLWithPath:"
								  :pointer (ns:autorelease (ns:make-ns-string (namestring path)))
								  :pointer)
				 :pointer))
	     (av-player-item (ns:objc av-player "currentItem" :pointer))
	     (player-delegate (ns:objc (ns:alloc "PlayerDelegate")
	     				    "initWithID:endFn:" :int (id player)
	     				    :pointer (cffi:callback player-did-reach-end)
	     				    :pointer)))
	(ns:objc av-player-item "addObserver:forKeyPath:options:context:"
		 :pointer player-delegate
		 :pointer (ns:autorelease (ns:make-ns-string "status"))
		 :int 0
		 :pointer (cffi:null-pointer))
	(setf (player player) av-player
	      (player-item player) av-player-item
	      (player-delegate player) player-delegate)
	player))))

(defun status (player)
  (ns:with-event-loop (:waitp t)
    (let* ((status (ns:objc (player player) "status" :int)))
      (case status
	(0 :unknown)
	(1 :ready-to-play)
	(2 :failed)))))

(defun play (player)
  (ns:with-event-loop nil
    (ns:objc (player player) "play")))

(defun pause (player)
  (ns:with-event-loop nil
    (ns:objc (player player) "pause")))

(defun volume (player volume)
  (ns:with-event-loop nil
    (ns:objc (player player) "setVolume:" :float (float volume 1.0))))

(defun release-player (player)
  (ns:release (player player))
  (ns:release (player-delegate player)))



