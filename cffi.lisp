(in-package :cl-nextstep)

(cffi:load-foreign-library "/Users/byul/Desktop/temp/cl-nextstep/C/build/libcl_nextstep.dylib")

(cffi:defcfun ("start_event_loop" %start-event-loop) :void
  (event-callback :pointer))

(cffi:defcfun ("execute_in_event_loop" %execute-in-event-loop) :void
  (id :int))

(cffi:defcfun ("objc_getClass" cls) :pointer
  (name :string))

(cffi:defcfun ("sel_getUid" sel) :pointer
  (name :string))

(defmacro objc (class sel &rest rest)
  (with-gensyms (cls selector)
    `(let* ((,cls (if (stringp ,class) (cls ,class) ,class))
	    (,selector (if (stringp ,sel) (sel ,sel) ,sel)))
       (assert (not (cffi:null-pointer-p ,cls)) nil "Can't find NSClass: ~a" ,class)
       (assert (not (cffi:null-pointer-p ,selector)) nil "Can't find Selector: ~a" ,sel)
       (cffi:foreign-funcall "objc_msgSend" :pointer ,cls :pointer ,selector ,@rest))))


