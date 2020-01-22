(asdf:defsystem #:cl-nextstep
  :depends-on (#:alexandria
	       #:cffi
	       #:cffi-libffi
	       #:cffi-shared-libs
	       #:float-features
	       #:bordeaux-threads
	       #:trivial-main-thread)
  :serial t
  :components ((:file "package")
	       #-ccl (:file "id-map")
	       (:file "core-graphics")
	       (:file "cl-nextstep")
	       (:file "window")
	       (:file "view")
	       (:file "opengl-view")))
