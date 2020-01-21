(asdf:defsystem #:cl-nextstep
  :depends-on (#:alexandria
	       #:cffi
	       #:cffi-shared-libs
	       #:float-features
	       #:bordeaux-threads
	       #:trivial-main-thread)
  :serial t
  :components ((:file "package")
	       (:file "cffi")
	       #-ccl (:file "id-map")
	       (:file "cl-nextstep")
	       (:file "window")
	       (:file "opengl-view")))
