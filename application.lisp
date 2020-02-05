(in-package :cl-nextstep)

(defconstant +NSActivityIdleDisplaySleepDisabled+ (ash 1 40))
(defconstant +NSActivityIdleSystemSleepDisabled+ (ash 1 20)) 
(defconstant +NSActivitySuddenTerminationDisabled+ (ash 1 14)) 
(defconstant +NSActivityAutomaticTerminationDisabled+ (ash 1 15)) 
(defconstant +NSActivityUserInitiated+ (logior #x00FFFFFF +NSActivityIdleSystemSleepDisabled+)) 
(defconstant +NSActivityUserInitiatedAllowingIdleSystemSleep+ (logand 
							       +NSActivityUserInitiated+
							       (lognot +NSActivityIdleSystemSleepDisabled+))) 
(defconstant +NSActivityBackground+ #x000000FF) 
(defconstant +NSActivityLatencyCritical+ #xFF00000000)

(defconstant +NSApplicationActivationPolicyRegular+ 0)
(defconstant +NSApplicationActivationPolicyAccessory+ 1)
(defconstant +NSApplicationActivationPolicyProhibited+ 2)


(defvar *dispatch-id-map* (make-id-map))
(defvar *startup-hooks* nil)

(cffi:defcallback delegate-callback :void ((id :int))
  (case id
    (0 (dolist (hook *startup-hooks*)
	 (funcall hook)))
    (1 (dolist (hook sb-ext:*exit-hooks*)
	 (funcall hook)))))

(cffi:defcallback dispatch-callback :void ((id :pointer))
  (let* ((id (cffi:pointer-address id)))
    (let* ((task (id-map-free-object *dispatch-id-map* id)))
      (when task
	(handler-case (funcall task)
	  (error (c)
	    (break (format nil "catch signal while Dispatching Event: \"~a\"" c))))))))


(defmacro with-event-loop ((&key waitp nil) &body body)
  (alexandria:with-gensyms (result semaphore id) 
    `(cond ((eql (trivial-main-thread:find-main-thread) (bt:current-thread)) (progn ,@body))
	   (,waitp (let* ((,result nil)
			  (,id (assign-id-map-id *dispatch-id-map*
						 (lambda ()
						   (setf ,result (progn ,@body))))))
		     (cffi:foreign-funcall "dispatch_sync_f" :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q")
					   :pointer (cffi:make-pointer ,id)
					   :pointer (cffi:callback dispatch-callback))
		     ,result))
	   (t (let* ((,id (assign-id-map-id *dispatch-id-map* (lambda () ,@body))))
		(cffi:foreign-funcall "dispatch_async_f" :pointer (cffi:foreign-symbol-pointer "_dispatch_main_q")
							 :pointer (cffi:make-pointer ,id)
							 :pointer (cffi:callback dispatch-callback)))))))


(let* ((running-p nil))
  (defun start-event-loop ()
    (unless running-p
      (defun trivial-main-thread:call-in-main-thread (function &key blocking (runner trivial-main-thread::*runner*))
	(declare (ignore runner))
	(ns:with-event-loop (:waitp blocking)
	  (funcall function)))
      (trivial-main-thread:swap-main-thread 
       (lambda ()
	 (setf running-p t)
	 (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	   (let* ((pool (new "NSAutoreleasePool"))
		  (ns-app (objc "LispApplication" "sharedApplication" :pointer)))
	     (objc ns-app "setActivationPolicy:" :long +nsapplicationactivationpolicyregular+)
	     (objc ns-app "setLispApplicationDispatch:" :pointer (cffi:callback delegate-callback))
	     (let* ((activity-options (logior +NSActivityIdleDisplaySleepDisabled+
					      +NSActivityIdleSystemSleepDisabled+
					      +NSActivitySuddenTerminationDisabled+
					      +NSActivityAutomaticTerminationDisabled+
					      +NSActivityUserInitiated+
					      +NSActivityUserInitiatedAllowingIdleSystemSleep+
					      +NSActivityBackground+
					      +NSActivityLatencyCritical+)))
	       (set-process-activity activity-options "NONE REASON"))
	     (objc ns-app "setDelegate:" :pointer ns-app)
	     (make-default-menubar ns-app)
	     (objc ns-app "run")
	     (release pool)))))
      :start-event-loop)))

(defun enable-foreground ()
  (ns:with-event-loop nil
    (ns:objc (ns:objc "NSApplication" "sharedApplication" :pointer)
	     "activateIgnoringOtherApps:" :bool t)))

(defun set-process-activity (options reason)
  (objc
   (objc "NSProcessInfo" "processInfo" :pointer)
   "beginActivityWithOptions:reason:"
   :unsigned-long-long options
   :pointer (autorelease (ns:make-ns-string reason))))

(defun make-menu-item (name &key action key)
  (objc (alloc "NSMenuItem")
	"initWithTitle:action:keyEquivalent:"
	:pointer (autorelease (make-ns-string name))
	:pointer (sel action)
	:pointer (autorelease (make-ns-string key))
	:pointer))

(defun make-default-menubar (ns-app)
  (let* ((menubar (autorelease (new "NSMenu")))
	 (app-menu-item (autorelease (new "NSMenuItem")))
	 (edit-menu-item (autorelease (new "NSMenuItem"))))
    (objc ns-app "setMainMenu:" :pointer menubar)
    (objc menubar "addItem:" :pointer app-menu-item)
    (objc menubar "addItem:" :pointer edit-menu-item)
    (let* ((app-menu (autorelease (new "NSMenu")))
	   (quit-menu-item (autorelease (make-menu-item
					 (ns-string-to-lisp
					  (objc (objc "NSProcessInfo" "processInfo" :pointer)
						"processName" :pointer))
					 :action "terminate:"
					 :key "q"))))
      (objc app-menu "addItem:" :pointer quit-menu-item)
      (objc app-menu-item "setSubmenu:" :pointer app-menu))
    (let* ((edit-menu (autorelease (objc (alloc "NSMenu") "initWithTitle:"
					 :pointer (autorelease (make-ns-string "Edit"))
					 :pointer)))
	   (close-menu-item (autorelease (make-menu-item "Close"
							 :action "performClose:"
							 :key "w")))
	   (fullscreen-menu-item (autorelease (make-menu-item "ToggleFullscreen"
							 :action "toggleFullscreen"
							 :key "f"))))
      (objc edit-menu "addItem:" :pointer close-menu-item)
      (objc edit-menu "addItem:" :pointer fullscreen-menu-item)
      (objc edit-menu-item "setSubmenu:" :pointer edit-menu))))

