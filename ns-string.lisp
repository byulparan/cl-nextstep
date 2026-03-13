(in-package :cl-nextstep)

(defmacro with-ns-graphics ((context) &body body)
  (alexandria:with-gensyms (ctx)
    `(let ((,ctx (ns:objc "NSGraphicsContext" "graphicsContextWithGraphicsPort:flipped:"
			  :pointer ,context :bool nil :pointer)))
       (ns:objc "NSGraphicsContext" "saveGraphicsState")
       (ns:objc "NSGraphicsContext" "setCurrentContext:" :pointer ,ctx)
       (unwind-protect (progn ,@body)
	 (ns:objc "NSGraphicsContext" "restoreGraphicsState")))))



(defun attributed-string (string &key (font "Arial") (size 14.0) (color (ns:color 1.0 1.0 1.0)))
  (let* ((dict (ns:objc "NSMutableDictionary" "dictionaryWithCapacity:"
			:unsigned-long 3
			:pointer)))
    (ns:objc dict "setValue:forKey:"
	     :pointer (ns:objc "NSFont" "fontWithName:size:" :pointer (ns:autorelease (ns:make-ns-string font))
							     :double (float size 1.0d0)
							     :pointer)
	     :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "NSFontAttributeName") :pointer))
    (ns:objc dict "setValue:forKey:"
	     :pointer color
	     :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "NSForegroundColorAttributeName") :pointer))
    (ns:autorelease
     (ns:objc (ns:alloc "NSAttributedString") "initWithString:attributes:"
	      :pointer (ns:autorelease (ns:make-ns-string string))
	      :pointer dict
	      :pointer))))


(defun attributed-string-size (attributed-string)
  (sb-alien:with-alien ((%size (sb-alien:struct size)))
    (sb-alien:alien-funcall-into
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function (sb-alien:struct size)
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer))
     (sb-alien:alien-sap %size) 
     (cocoa-ref attributed-string)
     (sel "size"))
    (size (sb-alien:slot %size 'width) (sb-alien:slot %size 'height))))


(defun draw-string (cg-context attributed-string rect)
  (with-ns-graphics (cg-context)
    (with-sb-alien-rect (rect rect)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								(sb-alien:struct rect)))
       (cocoa-ref attributed-string)
       (sel "drawInRect:")
       rect))))




