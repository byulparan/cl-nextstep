(in-package :cl-nextstep)

(defclass text-field ()
  ((cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after ((self text-field) &key (x 0) (y 0) (w 10) (h 10) editable text back-color fore-color)
  (let* ((ref (with-sb-alien-rect (rect (rect x y w h))
		(sb-alien:alien-funcall
		 (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
									  sb-alien:system-area-pointer
									  sb-alien:system-area-pointer
									  (sb-alien:struct rect)))
		 (alloc "NSTextField")
		 (sel "initWithFrame:")
		 rect))))
    (objc ref "setEditable:" :bool editable)
    (when back-color (objc ref "setBackgroundColor:" :pointer back-color))
    (when fore-color (objc ref "setTextColor:" :pointer fore-color))
    (when text (objc ref "setStringValue:" :pointer (ns:autorelease (ns:make-ns-string text))))
    (setf (cocoa-ref self) ref)))


(defclass slider ()
  ((cocoa-ref :accessor cocoa-ref)))


(defmethod initialize-instance :after ((self slider) &key (x 0) (y 0) (w 10) (h 10) action value color)
  (let* ((ref (with-sb-alien-rect (rect (rect x y w h))
		(sb-alien:alien-funcall
		 (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
									  sb-alien:system-area-pointer
									  sb-alien:system-area-pointer
									  (sb-alien:struct rect)))
		 (alloc "NSSlider")
		 (sel "initWithFrame:")
		 rect))))
    (setf (gethash (cffi:pointer-address ref) *user-action-table*) action)
    (when value
      (objc ref "setFloatValue:" :float (float value 1.0)))
    (when color
      (objc ref "setTrackFillColor:" :pointer color))
    (objc ref "setTarget:" :pointer *app*)
    (objc ref "setAction:" :pointer (sel "lispUserAction:"))
    (setf (cocoa-ref self) ref)))
