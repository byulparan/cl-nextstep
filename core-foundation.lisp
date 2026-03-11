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

(defmethod release (instance)
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
(defun color (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (ns:objc "NSColor" "colorWithCalibratedRed:green:blue:alpha:"
	   :double (float r 1.0d0)
	   :double (float g 1.0d0)
	   :double (float b 1.0d0)
	   :double (float a 1.0d0)
	   :pointer))


;; =======================================================
;; structure
(defstruct (point
	    (:constructor point (x y)))
  x y)

(sb-alien:define-alien-type nil
    (sb-alien:struct point
	    (x sb-alien:double)
	    (y sb-alien:double)))


(defstruct (size
	    (:constructor size (width height)))
  width height)

(sb-alien:define-alien-type nil
    (sb-alien:struct size
		     (width sb-alien:double)
		     (height sb-alien:double)))


(defstruct (rect
	    (:constructor rect (x y width height)))
  x y width height)

(sb-alien:define-alien-type nil
    (sb-alien:struct rect
		     (origin (sb-alien:struct point))
		     (size (sb-alien:struct size))))



(defmacro with-sb-alien-rect ((name rect) &body body)
  (with-unique-names (%origin %size)
    (once-only (rect)
      `(sb-alien:with-alien ((,name (sb-alien:struct rect))
			     (,%origin (sb-alien:struct point))
			     (,%size (sb-alien:struct size)))
	 (setf (sb-alien:slot ,%origin 'x) (float (rect-x ,rect) 1.0d0)
	       (sb-alien:slot ,%origin 'y) (float (rect-y ,rect) 1.0d0))
	 (setf (sb-alien:slot ,%size 'width) (float (rect-width ,rect) 1.0d0)
	       (sb-alien:slot ,%size 'height) (float (rect-height ,rect) 1.0d0))
	 (setf (sb-alien:slot ,name 'origin) ,%origin
	       (sb-alien:slot ,name 'size) ,%size)
	 ,@body))))


