(asdf:defsystem :cl-nextstep
  :depends-on (:alexandria
	       :cffi
	       :cffi-libffi
	       :cffi-shared-libs
	       :float-features
	       :bordeaux-threads
	       :trivial-main-thread)
  :serial t
  :components ((:module "frameworks"
		:components
		((:file "core-graphics")))
	       (:file "package")
	       #-ccl (:file "id-map")
	       (:file "cl-nextstep")
	       (:file "timer")
	       (:file "window")
	       (:file "view")
	       (:file "opengl-view")))
