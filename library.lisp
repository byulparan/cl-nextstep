(in-package :cl-nextstep)

(cffi:define-foreign-library lib-nextstep
  (:darwin "libcl-nextstep.dylib"))

(cffi:use-foreign-library lib-nextstep)


