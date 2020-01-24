(in-package :cl-nextstep)

;; =======================================================
;; object management

(cffi:defcfun ("objc_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defgeneric cocoa-ref (self))

(defmethod cocoa-ref ((self t))
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

(defmacro objc-stret (return-type instance sel &rest rest)
  (with-gensyms (object selector result)
    `(let* ((,object (cocoa-ref ,instance))
	    (,selector (sel ,sel)))
       (assert (not (cffi:null-pointer-p ,object)) nil "`ns:objc` accept NullPointer with SEL: \"~a\"" ,sel)
       (cffi:with-foreign-objects ((,result '(:pointer (:struct ,return-type))))
	 (cffi:foreign-funcall "objc_msgSend_stret"
			       :pointer ,result
			       :pointer ,object :pointer ,selector ,@rest)
	 (cffi:mem-ref ,result '(:struct ,return-type))))))

(defun retain (instance)
  (ns:objc instance "retain" :pointer))

(defun release (instance)
  (ns:objc instance "release"))

(defun autorelease (instance)
  (ns:objc instance "autorelease" :pointer))

(defun cf-retain (cf-instance)
  (cffi:foreign-funcall "CFRetain" :pointer cf-instance :pointer))

(defun cf-release (cf-instance)
  (cffi:foreign-funcall "CFRelease" :pointer cf-instance))

(defun cf-autorelease (cf-instance)
  (cffi:foreign-funcall "CFAutorelease" :pointer cf-instance :pointer))



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

