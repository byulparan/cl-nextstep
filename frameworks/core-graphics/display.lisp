(in-package :cg)

;; ================================================================================
;; Finding Displays
;; ================================================================================

(cffi:defcfun ("CGMainDisplayID" main-display-id) :unsigned-int
  "The display ID assigned to the main display.
The main display is the display with its screen location at (0,0) in the global display coordinate space. In a system without display mirroring, the display with the menu bar is typically the main display.

If mirroring is enabled and the menu bar appears on more than one display, this function provides a reliable way to find the main display.

In case of hardware mirroring, the drawable display becomes the main display. In case of software mirroring, the display with the highest resolution and deepest pixel depth typically becomes the main display.")


(defconstant +max-displays+ 10)

(defun online-display-list ()
  "Provides a list of displays that are online (active, mirrored, or sleeping)."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetOnlineDisplayList" :int32 +max-displays+
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun active-display-list ()
  "Provides a list of displays that are active for drawing."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetActiveDisplayList" :int32 +max-displays+
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun displays-with-point (point)
  "Provides a list of online displays with bounds that include the specified point."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetDisplaysWithPoint" (:struct ns:point) point
			  :int32 +max-displays+
			  :pointer ids
			  :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

(defun displays-with-rect (rect)
  "Gets a list of online displays with bounds that intersect the specified rectangle."
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetDisplaysWithRect" (:struct ns:rect) rect
			  :int32 +max-displays+
			  :pointer ids
			  :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))




;; ================================================================================
;; Creating Images from the Display
;; ================================================================================

(cffi:defcfun ("CGDisplayCreateImage" display-create-image) :pointer
  "An image containing the contents of the specified display. If the display ID is invalid, the return value is NULL. The caller is responsible for releasing the image created by calling CGImageRelease."
  (display :uint32))


(cffi:defcfun ("CGDisplayCreateImageForRect" display-create-image-for-rect) :pointer
  "An image containing the contents of the specified rectangle. If the display ID is invalid, the return value is NULL. The caller is responsible for releasing the image created by calling CGImageRelease."
  (display :uint32)
  (rect (:struct ns:rect)))




;; ================================================================================
;; Getting the Display Configuration
;; ================================================================================

(cffi:defcfun ("CGDisplayCopyColorSpace" display-copy-color-space) :pointer
  "The current color space for the specified display. The caller is responsible for releasing the color space with the CGColorSpaceRelease function."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsActive" display-is-active) :bool
  "Returns a Boolean value indicating whether a display is active. If true, the specified display is active; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsAlwaysInMirrorSet" display-is-always-in-mirror-set) :bool
  "Returns a Boolean value indicating whether a display is always in a mirroring set. If true, the specified display is in a mirroring set and cannot be removed from this set."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsAsleep" display-is-asleep) :bool
  "If YES, the specified display is in sleep mode; otherwise, NO. A display is sleeping when its framebuffer and the attached monitor are in reduced power mode. A sleeping display is still considered to be a part of global display (desktop) space, but it is not drawable."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsBuiltin" display-is-builtin) :bool
  "If true, the specified display is considered to be a built-in display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsInHWMirrorSet" display-is-in-hw-mirror-set) :bool
  "If true, the specified display is a member of a hardware mirroring set; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsInMirrorSet" display-is-in-mirror-set) :bool
  "If true, the specified display is a member of a software or hardware mirroring set; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsMain" display-is-main) :bool
  "If true, the specified display is currently the main display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsOnline" display-is-online) :bool
  "If true, the specified display is connected; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayIsStereo" display-is-stereo) :bool
  "If true, the specified display is running in a stereo graphics mode; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayMirrorsDisplay" display-mirrors-display) :uint32
  "For a secondary display in a mirroring set, returns the primary display. Returns the primary display in the mirroring set. Returns kCGNullDirectDisplay if the specified display is actually the primary display or is not in a mirroring set."
  (display :uint32))

(cffi:defcfun ("CGDisplayModelNumber" display-model-number) :uint32
  "A model number for the monitor associated with the specified display, or a constant to indicate an exception"
  (display :uint32))

(cffi:defcfun ("CGDisplayPrimaryDisplay" display-primary-display) :uint32
  "The primary display in the mirror set. If display is not hardware-mirrored, this function simply returns display."
  (display :uint32))

(cffi:defcfun ("CGDisplayRotation" display-rotation) :double
  "The rotation angle of the display in degrees, or 0 if the display is not valid."
  (display :uint32))

(cffi:defcfun ("CGDisplayScreenSize" display-screen-size) (:struct ns:size)
  "The size of the specified display in millimeters, or 0 if the display is not valid."
  (display :uint32))

(cffi:defcfun ("CGDisplaySerialNumber" display-serial-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))

(cffi:defcfun ("CGDisplayUnitNumber" display-unit-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))

(cffi:defcfun ("CGDisplayUsesOpenGLAcceleration" display-uses-opengl-acceleration) :bool
  "If true, Quartz Extreme is used to render in the specified display; otherwise, false."
  (display :uint32))

(cffi:defcfun ("CGDisplayVendorNumber" display-vendor-number) :uint32
  "The identifier of the display to be accessed."
  (display :uint32))



;; ================================================================================
;; Getting the Display Configuration
;; ================================================================================

(cffi:defcfun ("CGDisplayBounds" display-bounds) (:struct ns:rect)
  "The bounds of the display, expressed as a rectangle in the global display coordinate space (relative to the upper-left corner of the main display)."
  (display :uint32))


(cffi:defcfun ("CGDisplayPixelsHigh" display-pixels-high) :sizet
  "The display height in pixel units."
  (display :uint32))

(cffi:defcfun ("CGDisplayPixelsWide" display-pixels-wide) :sizet
  "The display width in pixel units."
  (display :uint32))


;; ================================================================================
;; Creating and Managing Display Modes
;; ================================================================================

(cffi:defcfun ("CGDisplayCopyDisplayMode" display-copy-display-mode) :pointer
  "A display-mode opaque-type reference, or NULL if the display is invalid. The caller is responsible for releasing the display mode using CGDisplayModeRelease."
  (display :uint32))

;; (cffi:defcfun ("CGDisplayCopyAllDisplayModes" display-copy-all-display-modes) :pointer
;;   (display :uint32)
;;   (options :pointer))

(cffi:defcfun ("CGDisplaySetDisplayMode" display-set-display-mode) :int32
  "This function switches the display mode of the specified display. The operation is always synchronous; the function doesn’t return until the mode switch is complete. Note that after switching, display parameters and addresses may change.

The selected display mode persists for the life of the calling program. When the program terminates, the display mode automatically reverts to the permanent setting in the Displays panel of System Preferences.

When you change the display mode of a display in a mirroring set, your change switches other displays in the mirroring set to a mode capable of mirroring the bounds of the adjusted display. To avoid this automatic behavior, you can use the following procedure: call CGBeginDisplayConfiguration, call CGConfigureDisplayWithDisplayMode for each display to explicitly set the mode, and finally call CGCompleteDisplayConfiguration."
  (diplay :uint32)
  (mode :pointer)
  (options :pointer))

(cffi:defcfun ("CGDisplayModeRetain" retain-display-mode) :pointer
  "Retains a Core Graphics display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeRelease" release-display-mode) :void
  "Releases a Core Graphics display mode. This function is equivalent to CFRelease, except that it does not cause an error if the mode parameter is NULL."
  (mode :pointer))


;; ================================================================================
;; Getting Information About a Display Mode
;; ================================================================================

(cffi:defcfun ("CGDisplayModeGetWidth" display-mode-width) :sizet
  "The width, in pixels, of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetHeight" display-mode-height) :sizet
  "The height, in pixels, of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetRefreshRate" display-mode-refresh-rate) :double
  "The refresh rate, in hertz, of the specified display mode for a CRT display. Some displays may not use conventional video vertical and horizontal sweep in painting the screen; for these displays, the return value is 0."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetIOFlags" display-mode-io-flags) :uint32
  "Returns the I/O Kit flags of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetIODisplayModeID" display-mode-io-display-mode-id) :int32
  "Returns the I/O Kit display mode ID of the specified display mode."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeIsUsableForDesktopGUI" display-mode-is-usable-for-desktop-gui) :bool
  "Returns a Boolean value indicating whether the specified display mode is usable for a desktop graphical user interface. If true, the specified display mode is usable for a desktop graphical user interface; otherwise, false."
  (mode :pointer))

(cffi:defcfun ("CGDisplayModeGetTypeID" display-mode-type-id) :unsigned-long
  "Returns the type identifier of Quartz display modes. The type identifier of the CGDisplayMode opaque type.")


;; ================================================================================
;; Controlling the Mouse Cursor
;; ================================================================================

(cffi:defcfun ("CGDisplayHideCursor" display-hide-cursor) :int32
  "This function hides the cursor regardless of its current location. The display parameter has no effect. In most cases, the caller must be the foreground application to affect the cursor."
  (display :uint32))

(cffi:defcfun ("CGDisplayShowCursor" display-show-cursor) :int32
  "If the hide cursor count is 0, this function shows the cursor regardless of its current location. The display parameter has no effect. In most cases, the caller must be the foreground application to affect the cursor."
  (display :uint32))

(cffi:defcfun ("CGDisplayMoveCursorToPoint" display-move-cursor-to-point) :int32
  "Moves the mouse cursor to a specified point relative to the upper-left corner of the display."
  (display :uint32)
  (point (:struct ns:point)))

(cffi:defcfun ("CGAssociateMouseAndMouseCursorPosition" associate-mouse-and-mouse-cursor-position) :int32
  "Connects or disconnects the mouse and cursor while an application is in the foreground. Call this function to disconnect the mouse from the cursor. When you call this function, the events your application receives from the system have a constant absolute location but contain delta updates to the X and Y coordinates of the mouse. You can hide the cursor or change it into something appropriate for your application. You can reposition the cursor by using the function CGDisplayMoveCursorToPoint or the function CGWarpMouseCursorPosition."
  (connected :bool))

(cffi:defcfun ("CGWarpMouseCursorPosition" wrap-mouse-cursor-position) :int32
  "Moves the mouse cursor without generating events. You can use this function to “warp” or alter the cursor position without generating or posting an event. For example, this function is often used to move the cursor position back to the center of the screen by games that do not want the cursor pinned by display edges."
  (new-cursor-position (:struct ns:point)))

(defun last-mouse-delta ()
  "Reports the change in mouse position since the last mouse movement event received by the application. This function is not recommended for general use. Instead, you should use the mouse-tracking functions provided by the NSEvent class."
  (cffi:with-foreign-objects ((delta-x :int)
			      (delta-y :int))
    (cffi:foreign-funcall "CGGetLastMouseDelta" :pointer delta-x :pointer delta-y)
    (list (cffi:mem-ref delta-x :int) (cffi:mem-ref delta-y :int))))
 

