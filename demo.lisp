(require :cl-nextstep)
(ns:start-event-loop)

(ns:with-event-loop (:waitp t)
  (ns:objc (ns:objc "NSColor" "redColor" :pointer) "redComponent" :double))

(ns:with-event-loop (:waitp t)
  (ql:quickload :sc-internal))

(ns:with-event-loop (:waitp t)
  (setf sc:*s* (sc:make-internal-server "localhost"))
  (sc:server-boot sc:*s*))

(in-package :sc-user)

(proxy :foo
  (pan2.ar (sin-osc.ar 440 0 .1)))





