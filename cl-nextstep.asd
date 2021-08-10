
(let ((lisp-app-path
	"/Users/byul/quicklisp/local-projects/lib/cl-nextstep/sbcl.app/Contents/MacOS/sbcl"))
  #+sbcl (sb-posix:setenv "CFProcessPath" lisp-app-path 1)
  #+ccl (ccl:setenv "CFProcessPath" lisp-app-path 1)
  #+ecl (si:setenv  "CFProcessPath" lisp-app-path))

(asdf:defsystem :cl-nextstep
  :depends-on (:alexandria
	       :cffi
	       :cffi-libffi
	       :float-features
	       :bordeaux-threads
	       :trivial-main-thread
	       :cl-opengl)
  :serial t
  :components ((:file "package")
	       #-ccl (:file "id-map")
	       (:file "library")
	       (:file "core-foundation")
	       (:file "application")
	       (:module "frameworks"
		:components
		((:module "core-graphics"
		  :components ((:file "package")
			       (:file "color")
			       (:file "color-space")
			       (:file "context")
			       (:file "image")))
		 (:file "cgl-context")
		 (:file "core-video")
		 (:file "core-image")
		 (:file "ci-filter-db")
		 (:file "av-foundation")
		 (:file "io-surface")))
	       (:file "timer")
	       (:file "window")
	       (:file "view")
	       (:file "opengl-view")
	       (:file "widget")))

(pushnew :cl-nextstep *features*)

