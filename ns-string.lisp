(in-package :cl-nextstep)

(defmacro with-ns-graphics ((context) &body body)
  (alexandria:with-gensyms (ctx)
    `(let ((,ctx (ns:objc "NSGraphicsContext" "graphicsContextWithGraphicsPort:flipped:"
			  :pointer ,context :bool nil :pointer)))
       (ns:objc "NSGraphicsContext" "saveGraphicsState")
       (ns:objc "NSGraphicsContext" "setCurrentContext:" :pointer ,ctx)
       (unwind-protect (progn ,@body)
	 (ns:objc "NSGraphicsContext" "restoreGraphicsState")))))



(defun make-attributed-string (string &key (font "Arial") (size 14.0) (r 1.0) (g 1.0) (b 1.0) (a 1.0))
  (let* ((dict (ns:objc "NSMutableDictionary" "dictionaryWithCapacity:"
			:unsigned-long 3
			:pointer)))
    (ns:objc dict "setValue:forKey:"
	     :pointer (ns:objc "NSFont" "fontWithName:size:" :pointer (ns:autorelease (ns:make-ns-string font))
							     :double (float size 1.0d0)
							     :pointer)
	     :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "NSFontAttributeName") :pointer))
    (ns:objc dict "setValue:forKey:"
	     :pointer (ns:objc "NSColor" "colorWithCalibratedRed:green:blue:alpha:"
			       :double (float r 1.0d0) :double (float g 1.0d0) :double (float b 1.0d0) :double (float a 1.0d0) :pointer)
	     :pointer (cffi:mem-ref (cffi:foreign-symbol-pointer "NSForegroundColorAttributeName") :pointer))
    (ns:autorelease
     (ns:objc (ns:alloc "NSAttributedString") "initWithString:attributes:"
	      :pointer (ns:autorelease (ns:make-ns-string string))
	      :pointer dict
	      :pointer))))


(defun attributed-string-size (attributed-string)
  (ns:objc attributed-string "size" (:struct ns:size)))


(defun draw-string (cg-context attributed-string rect)
  (with-ns-graphics (cg-context)
    (ns:objc attributed-string "drawInRect:" (:struct ns:rect) rect)))




