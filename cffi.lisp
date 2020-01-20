(in-package :cl-nextstep)

(cffi:define-foreign-library lib-nextstep
  (:darwin "libcl_nextstep.dylib"))

(cffi:use-foreign-library lib-nextstep)


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


(cffi:defcfun ("start_event_loop" %start-event-loop) :void
  (event-callback :pointer))

(cffi:defcfun ("execute_in_event_loop" %execute-in-event-loop) :void
  (id :int))

;; Window
(cffi:defcfun ("make_window" %make-window) :pointer
  (id :int)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (close-fn :pointer))

(cffi:defcfun ("window_show" %window-show) :void
  (window :pointer))

