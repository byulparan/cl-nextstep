(defpackage :cg
  (:use :cl)
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

	   #:color-create-generic-rgb
	   
	   #:color-space-create
	   #:color-space-release
	   
	   #:context-save-gstate
	   #:context-restore-gstate
	   #:context-scale-ctm
	   #:context-translate-ctm
	   #:context-rotate-ctm
	   #:context-set-line-width
	   #:context-set-line-cap
	   #:context-set-line-join
	   #:context-set-miter-limit
	   #:context-set-flatness
	   #:context-set-alpha
	   #:context-set-blend-mode
	   #:context-begin-path
	   #:context-move-to-path
	   #:context-add-line-to-point
	   #:context-add-curve-to-point
	   #:context-add-quad-curve-to-point
	   #:context-clsoe-path
	   #:context-add-arc
	   #:context-draw-path
	   #:context-fill-path
	   #:context-eo-fill-path
	   #:context-stroke-path
	   #:context-fill-rect
	   #:context-stroke-rect
	   #:context-stroke-rect-with-width
	   #:context-clear-rect
	   #:context-fill-ellipse-in-rect
	   #:context-stroke-ellipse-in-rect
	   #:context-set-fill-color-with-color
	   #:context-set-stroke-color-with-color
	   #:context-set-fill-color-space
	   #:context-set-stroke-color-space
	   #:context-set-gray-fill-color
	   #:context-set-gray-stroke-color
	   #:context-set-rgb-fill-color
	   #:context-set-rgb-stroke-color
	   #:context-set-cmyk-fill-color
	   #:context-set-cmyk-stroke-color
	   #:context-set-character-spacing
	   #:context-set-text-position
	   #:context-set-text-drawing-mode

	   #:create-font
	   #:context-set-font
	   #:context-set-font-size
	   #:context-select-font
	   #:context-show-text
	   #:context-show-text-at-point

	   #:load-image
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-bitmap-data
	   #:image-retain
	   #:image-release
	   #:make-image-from-screen))


