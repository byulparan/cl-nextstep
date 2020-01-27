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
   #:alloc
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
   #:make-color
   #:point
   #:make-point
   #:point-x
   #:point-y
   #:size
   #:make-size
   #:size-width
   #:size-height
   #:rect
   #:make-rect
   #:rect-x
   #:rect-y
   #:rect-width
   #:rect-height
   ;; application.lisp
   #:start-event-loop 
   #:with-event-loop
   #:enable-foreground
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
   #:set-always-on-top
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
   #:reshape
   ;; widget.lisp
   #:text-field))

