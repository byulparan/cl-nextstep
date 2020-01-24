(asdf:defsystem :cl-nextstep
  :depends-on (:alexandria
	       :cffi
	       :cffi-libffi
	       :cffi-shared-libs
	       :float-features
	       :bordeaux-threads
	       :trivial-main-thread)
  :serial t
  :components ((:file "package")
	       #-ccl (:file "id-map")
	       (:file "cl-nextstep")
	       (:file "timer")
	       (:file "window")
	       (:module "frameworks"
		:components
		((:module "core-graphics"
		  :components ((:file "package")
			       (:file "type")
			       (:file "color")
			       (:file "color-space")
			       (:file "context")
			       (:file "image")))
		 (:file "cgl-context")
		 (:file "core-image")
		 (:file "ci-filter-db")))
	       (:file "view")
	       (:file "opengl-view")))
