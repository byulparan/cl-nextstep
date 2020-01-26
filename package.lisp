(defpackage :cl-nextstep
  (:use :cl :alexandria)
  #+ccl (:import-from #:ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  (:nicknames :ns)
  (:export
   ;; core-foundation.lisp
   #:cls
   #:sel
   #:objc
   #:objc-stret
   #:retain
   #:release
   #:autorelease
   #:cf-retain
   #:cf-release
   #:cf-autorelease
   #:make-ns-string
   #:ns-string-to-lisp
   #:make-cf-string
   #:cf-string-to-lisp
   ;; application.lisp
   #:start-event-loop 
   #:with-event-loop
   ;; timer.lisp
   #:timer
   #:invalidate
   ;; window.lisp
   #:window
   #:window-show
   #:close-fn
   #:toggle-fullscreen
   #:content-view
   #:add-subviews
   ;; view.lisp
   #:width
   #:height
   #:cgl-context
   #:cgl-pixel-format
   #:init
   #:draw
   #:shutdown
   #:mouse-down
   #:mouse-dragged
   #:mouse-up
   #:mouse-moved
   #:mouse-wheel
   #:command-p
   #:shift-p
   #:ctrl-p
   #:opt-p
   #:redisplay
   #:view
   #:current-cg-context
   ;; opengl-view.lisp
   #:opengl-view
   #:reshape))

