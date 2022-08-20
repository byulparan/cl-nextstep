(in-package :cl-nextstep)

(cffi:load-foreign-library
 (concatenate 'string
	      (namestring (asdf:system-source-directory :cl-nextstep))
	      "libcl-nextstep.dylib"))


