(defpackage #-lispworks :core-graphics
	    #+lispworks :next-core-graphics
  (:nicknames :cg)
  (:use :cl)
  (:export #:main-display-id
	   #:online-display-list
	   #:active-display-list
	   #:displays-with-point
	   #:displays-with-rect
	   #:display-create-image
	   #:display-create-image-for-rect
	   #:display-copy-color-space
	   #:display-is-active
	   #:display-is-always-in-mirror-set
	   #:display-is-asleep
	   #:display-is-builtin
	   #:display-is-in-hw-mirror-set
	   #:display-is-in-mirror-set
	   #:display-is-main
	   #:display-is-online
	   #:display-is-stereo
	   #:display-mirrors-display
	   #:display-model-number
	   #:display-primary-display
	   #:display-rotation
	   #:display-screen-size
	   #:display-serial-number
	   #:display-unit-number
	   #:display-uses-opengl-acceleration
	   #:display-vendor-number
	   #:display-bounds
	   #:display-pixels-high
	   #:display-pixels-wide
	   #:display-copy-display-mode
	   #:display-set-display-mode
	   #:retain-display-mode
	   #:release-display-mode
	   #:display-mode-width
	   #:display-mode-height
	   #:display-mode-refresh-rate
	   #:display-mode-io-flags
	   #:display-mode-io-display-mode-id
	   #:display-mode-is-usable-for-desktop-gui
	   #:display-mode-type-id
	   #:display-hide-cursor
	   #:display-show-cursor
	   #:display-move-cursor-to-point
	   #:associate-mouse-and-mouse-cursor-position
	   #:wrap-mouse-cursor-position
	   #:last-mouse-delta
	   
	   #:make-color-generic-rgb
	   
	   #:make-color-space
	   #:release-color-space
	   #:color-space-copy-name
	   
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
	   #:make-image-from-context
	   #:make-image-from-screen
	   #:retain-image
	   #:release-image
	   #:image-width
	   #:image-height
	   #:image-bits-per-pixel
	   #:image-bytes-per-row
	   #:image-data
	   

	   #:make-bitmap-context
	   #:context-data
	   #:context-width
	   #:context-height
	   #:release-context))


