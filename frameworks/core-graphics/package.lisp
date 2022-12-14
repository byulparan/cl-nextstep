(defpackage #-lispworks :core-graphics
	    #+lispworks :next-core-graphics
  (:nicknames :cg)
  (:use :cl)
  (:export #:color-create-generic-rgb
	   #:color-space-create
	   #:color-space-release
	   
	   #:save-gstate
	   #:restore-gstate
	   #:scale-ctm
	   #:translate-ctm
	   #:rotate-ctm
	   #:set-line-width
	   #:set-line-cap
	   #:set-line-join
	   #:set-miter-limit
	   #:set-flatness
	   #:set-alpha
	   #:set-blend-mode
	   #:begin-path
	   #:move-to-point
	   #:add-line-to-point
	   #:add-curve-to-point
	   #:add-quad-curve-to-point
	   #:close-path
	   #:add-arc
	   #:draw-path
	   #:fill-path
	   #:eo-fill-path
	   #:stroke-path
	   #:fill-rect
	   #:stroke-rect
	   #:stroke-rect-with-width
	   #:clear-rect
	   #:fill-ellipse-in-rect
	   #:stroke-ellipse-in-rect
	   #:set-fill-color-with-color
	   #:set-stroke-color-with-color
	   #:set-fill-color-space
	   #:set-stroke-color-space
	   #:set-gray-fill-color
	   #:set-gray-stroke-color
	   #:set-rgb-fill-color
	   #:set-rgb-stroke-color
	   #:set-cmyk-fill-color
	   #:set-cmyk-stroke-color
	   #:draw-image
	   #:set-character-spacing
	   #:set-text-position
	   #:set-text-drawing-mode
	   #:set-font
	   #:set-font-size
	   #:select-font
	   #:show-text
	   #:show-text-at-point
	   
	   #:load-image
	   #:retain-image
	   #:release-image
	   #:image-width
	   #:image-height
	   #:image-bits-per-pixel
	   #:image-bytes-per-row
	   #:image-bitmap-data
	   #:image-from-screen

	   #:make-bitmap-context
	   #:bitmap-data
	   #:bitmap-width
	   #:bitmap-height
	   #:release-context))


