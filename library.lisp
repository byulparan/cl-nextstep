(in-package :cl-nextstep)

(cffi:load-foreign-library
 (concatenate 'string
	      (namestring (asdf:system-source-directory :cl-nextstep))
	      "libcl-nextstep.dylib"))


(cffi:define-foreign-library webkit
  (:darwin (:framework "WebKit")))

(cffi:use-foreign-library webkit)


