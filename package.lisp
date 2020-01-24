(defpackage :cl-nextstep
  (:use :cl :alexandria)
  #+ccl (:import-from #:ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  (:nicknames :ns)
  (:export #:start-event-loop 
	   #:with-event-loop
	   #:cls
	   #:sel
	   #:objc
	   #:retain
	   #:release
	   #:autorelease
	   #:cf-release
	   #:make-nsstring
	   #:nsstring-to-lisp
	   #:make-cfstring
	   #:cfstring-to-lisp
	   
	   #:timer
	   #:invalidate
	   
	   #:window
	   #:window-show
	   #:toggle-fullscreen
	   #:content-view
	   #:add-subviews

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
	   
	   #:opengl-view
	   #:reshape))

