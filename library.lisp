(in-package :cl-nextstep)

(cffi:define-foreign-library lib-nextstep
  (:darwin "libcl_nextstep.dylib"))

(cffi:use-foreign-library lib-nextstep)


