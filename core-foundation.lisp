(in-package :cl-nextstep)

;; =======================================================
;; object management

(cffi:defcfun ("objc_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defgeneric cocoa-ref (self))

(defmethod cocoa-ref ((self #+sbcl sb-sys:system-area-pointer
			    #+ccl ccl:macptr
			    #+lispworks flii:pointer
			    #+ecl si:foreign-data))
  self)

(defmethod cocoa-ref ((self string))
  (let* ((object (cls self)))
    (assert (not (cffi:null-pointer-p object)) nil "Can't find NSClass: '~a'" object)
    object))

(defmacro objc (instance sel &rest rest)
  (with-gensyms (object selector)
    `(let* ((,object (cocoa-ref ,instance))
	    (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil "`ns:objc` accept NullPointer with SEL: \"~a\"" ,sel)
       (cffi:foreign-funcall "objc_msgSend" :pointer ,object :pointer ,selector ,@rest))))

#+x86-64
(defmacro objc-stret (return-type instance sel &rest rest)
  (with-gensyms (object selector result)
    `(let* ((,object (cocoa-ref ,instance))
	    (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil "`ns:objc` accept NullPointer with SEL: \"~a\"" ,sel)
       (cffi:with-foreign-objects ((,result '(:struct ,return-type)))
	 (cffi:foreign-funcall "objc_msgSend_stret"
			       :pointer ,result
			       :pointer ,object :pointer ,selector ,@rest)
	 (cffi:mem-ref ,result '(:struct ,return-type))))))

(defun alloc (cls)
  (objc cls "alloc" :pointer))

(defmethod init ((instance #+sbcl sb-sys:system-area-pointer
			      #+ccl ccl:macptr
			      #+lispworks flii:pointer
			      #+ecl si:foreign-data))
  (objc instance "init" :pointer))

(defun new (cls)
  (objc (alloc cls) "init" :pointer))

(defun retain (instance)
  (objc instance "retain" :pointer))

(defmethod release ((instance #+sbcl sb-sys:system-area-pointer
			      #+ccl ccl:macptr
			      #+lispworks flii:pointer
			      #+ecl si:foreign-data))
  (objc instance "release"))

(defun autorelease (instance)
  (objc instance "autorelease" :pointer))

(defun cf-retain (cf-instance)
  (cffi:foreign-funcall "CFRetain" :pointer cf-instance :pointer))

(defun cf-release (cf-instance)
  (cffi:foreign-funcall "CFRelease" :pointer cf-instance))

(defun cf-autorelease (cf-instance)
  (cffi:foreign-funcall "CFAutorelease" :pointer cf-instance :pointer))

(defun retain-count (instance)
  (objc instance "retainCount" :unsigned-int))



;; =======================================================
;; string utilities

(defun make-ns-string (string)
  (cffi:with-foreign-strings ((s string))
    (ns:objc (ns:objc "NSString" "alloc" :pointer)
	     "initWithUTF8String:" :pointer s :pointer)))

(defun ns-string-to-lisp (ns-string)
  (cffi:foreign-string-to-lisp (ns:objc ns-string "UTF8String" :pointer)))

(defun make-cf-string (string)
  (cffi:foreign-funcall "CFStringCreateWithCString"
			:pointer (cffi:null-pointer)
			:string string
			:int 134217984	;NSUTF8StringEncoding
			:pointer))

(defun cf-string-to-lisp (cf-string)
  (let* ((len (1+ (* 3 (cffi:foreign-funcall "CFStringGetLength" :pointer cf-string :int)))))
    (cffi:with-foreign-objects ((buffer :char len))
      (cffi:foreign-funcall "CFStringGetCString" :pointer cf-string
						 :pointer buffer
						 :int len
						 :int 134217984	;NSUTF8StringEncoding
						 )
      (cffi:foreign-string-to-lisp buffer))))

;; =======================================================
;; color
(defun color (&key (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (ns:objc "NSColor" "colorWithCalibratedRed:green:blue:alpha:"
	   :double (float r 1.0d0)
	   :double (float g 1.0d0)
	   :double (float b 1.0d0)
	   :double (float a 1.0d0)
	   :pointer))


;; =======================================================
;; structure

(cffi:defcstruct (point :class %point)
  (x :double)
  (y :double))

(defstruct (point
	    (:constructor point (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %point))
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (point x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %point) p)
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (setf x (coerce (point-x point) 'double-float)
	  y (coerce (point-y point) 'double-float))))


(cffi:defcstruct (size :class %size)
  (width :double)
  (height :double))

(defstruct (size
	    (:constructor size (width height)))
  width height)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (size width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (setf width (coerce (size-width size) 'double-float)
	  height (coerce (size-height size) 'double-float))))



(cffi:defcstruct (rect :class %rect)
  (origin (:struct point))
  (size (:struct size)))

(defstruct (rect
	    (:constructor rect (x y width height)))
  x y width height)

(defmethod cffi:translate-from-foreign (p (type %rect))
  (cffi:with-foreign-slots ((origin size) p (:struct rect))
    (rect (point-x origin)
	  (point-y origin)
	  (size-width size)
	  (size-height size))))

(defmethod cffi:translate-into-foreign-memory (rect (type %rect) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct rect) 'origin))
	 (size (cffi:foreign-slot-pointer p '(:struct rect) 'size)))
    (cffi:with-foreign-slots ((x y) origin (:struct point))
      (cffi:with-foreign-slots ((width height) size (:struct size))
	(setf x (coerce (rect-x rect) 'double-float)
	      y (coerce (rect-y rect) 'double-float)
	      width (coerce (rect-width rect) 'double-float)
	      height (coerce (rect-height rect) 'double-float))))))


