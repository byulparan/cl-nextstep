(defpackage #:cl-nextstep
  (:use #:cl #:alexandria)
  #+ccl (:import-from #:ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  (:nicknames :ns)
  (:export #:start-event-loop 
	   #:with-event-loop
	   #:cls
	   #:sel
	   #:objc

	   #:timer
	   #:invalidate
	   
	   #:window
	   #:window-show
	   #:toggle-fullscreen
	   #:content-view
	   #:add-subviews

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

(defpackage #:cg
  (:use #:cl)
  (:export #:point
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
	   #:load-image
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-bitmap-data
	   #:retain-image
	   #:release-image
	   #:make-image-from-screen))

