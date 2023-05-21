(in-package :cg)

(cffi:defcfun ("CGMainDisplayID" main-display-id) :unsigned-int)


(defun active-display-list ()
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetActiveDisplayList" :int32 10 ; max displays
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))


(defun online-display-list ()
  (cffi:with-foreign-objects ((ids :uint32 10)
			      (count :uint32))
    (cffi:foreign-funcall "CGGetOnlineDisplayList" :int32 10 ; max displays
						   :pointer ids
						   :pointer count)
    (loop for i below (cffi:mem-ref count :uint32)
	  collect (cffi:mem-aref ids :uint32 i) )))

