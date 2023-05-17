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
   #:init
   #:new
   #:retain
   #:release
   #:autorelease
   #:cf-retain
   #:cf-release
   #:cf-autorelease
   #:retain-count
   
   #:make-ns-string
   #:ns-string-to-lisp
   #:make-cf-string
   #:cf-string-to-lisp
   #:color
   #:point
   #:point-x
   #:point-y
   #:size
   #:size-width
   #:size-height
   #:rect
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

   #:mtk-view
   #:device
   #:color-pixel-format
   #:depth-stencil-pixel-format
   #:set-depth-stencil-pixel-format
   #:current-drawable
   #:current-render-pass-descriptor
   ;; opengl-view.lisp
   #:opengl-view
   #:reshape
   #:set-gl-best-resolution
   ;; widget.lisp
   #:text-field
   #:slider
   ;;wk-webview.lisp
   #:wk-webview
   #:reload
   #:url))


(defpackage #:mtl
  (:use #:cl)
  (:export
   #:make-command-queue
   #:get-command-buffer
   #:get-render-command-encoder
   
   #:present-drawable
   #:commit

   #:set-viewport
   #:set-render-pipeline-state
   #:set-depth-stencil-state
   #:set-vertex-buffer
   #:set-fragment-buffer
   #:draw-primitives
   #:end-encoding

   #:make-buffer

   #:make-library
   #:make-function
   #:make-render-pipeline-descriptor
   #:set-vertex-function
   #:set-vertex-descriptor
   #:set-fragment-function
   #:set-color-attachment-pixel-format
   #:set-depth-attachment-pixel-format
   #:make-vertex-descriptor
   #:set-vertex-descriptor-attribute
   #:set-vertex-descriptor-layout
   #:make-render-pipeline-state

   #:make-depth-stencil-descriptor
   #:set-depth-compare-function
   #:set-depth-write-enabled
   #:make-depth-stencil-state

   
   #:get-texture2d-descriptor
   #:make-texture
   #:replace-region
   
   #:clear-color
   #:origin
   #:size
   #:region
   #:viewport))
