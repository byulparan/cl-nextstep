(asdf:defsystem #:cl-nextstep
  :depends-on (#:alexandria
	       #:cffi
	       #:float-features
	       #:bordeaux-threads
	       #:trivial-main-thread)
  :serial t
  :components ((:file "package")
	       (:file "cffi")
	       #-ccl (:file "id-map")
	       (:file "cl-nextstep")))
