(asdf:defsystem #:cl-nextstep
  :depends-on (:alexandria :cffi :float-features :trivial-main-thread)
  :serial t
  :components ((:file "package")
	       (:file "cffi")
	       (:file "cl-nextstep")))
