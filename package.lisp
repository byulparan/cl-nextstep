(defpackage #:cl-nextstep
  (:use #:cl #:alexandria)
  #+ccl (:import-from #:ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  (:nicknames :ns)
  (:export #:start-event-loop 
	   #:with-event-loop
	   #:cls
	   #:sel
	   #:objc

	   #:window
	   #:window-show
	   #:toggle-fullscreen

	   #:width
	   #:height
	   #:init
	   #:draw
	   #:reshape
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
	   
	   #:view
	   #:opengl-view))

