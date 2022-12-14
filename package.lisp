(defpackage :cl-nextstep
  (:nicknames :ns)
  (:use :cl :alexandria)
  #+ccl (:import-from #:ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  (:export
   ;; core-foundation.lisp
   #:cls
   #:sel
   #:objc
   #+x86-64 #:objc-stret
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
   #:*startup-hooks*
   #:start-event-loop
   #:quit
   #:queue-for-event-loop
   #:with-event-loop
   #:enable-foreground
   ;; timer.lisp
   #:timer
   #:invalidate
   ;; window.lisp
   #:window
   #:window-show
   #:window-close
   #:close-fn
   #:toggle-fullscreen
   #:content-view
   #:add-subviews
   #:set-always-on-top
   #:in-screen-rect
   ;; view.lisp
   #:width
   #:height
   #:cgl-context
   #:cgl-pixel-format
   #:init
   #:draw
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
   #:set-gl-best-resolution
   ;; widget.lisp
   #:text-field
   ;;wk-webview.lisp
   #:wk-webview
   #:reload
   #:url))

