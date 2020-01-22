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
	   #:load-image
	   #:image-width
	   #:image-height
	   #:image-bpp
	   #:image-bitmap-data
	   #:retain-image
	   #:release-image
	   #:make-image-from-screen
	   
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
	   #:move-to-path
	   #:add-line-to-point
	   #:add-curve-to-point
	   #:add-quad-curve-to-point
	   #:clsoe-path

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

	   #:set-character-spacing
	   #:set-text-position
	   #:set-text-drawing-mode
	   #:select-font
	   #:show-text
	   #:show-text-at-point

	   #:color-create-generic-rgb))

(in-package :cg)

(cffi:defcstruct (point :class %point)
  (x :double)
  (y :double))

(defstruct (point
	    (:constructor make-point (x y)))
  x y)

(defmethod cffi:translate-from-foreign (p (type %point))
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (make-point x y)))

(defmethod cffi:translate-into-foreign-memory (point (type %point) p)
  (cffi:with-foreign-slots ((x y) p (:struct point))
    (setf x (coerce (point-x point) 'double-float)
	  y (coerce (point-y point) 'double-float))))


(cffi:defcstruct (size :class %size)
  (width :double)
  (height :double))

(defstruct (size
	    (:constructor make-size (width height)))
  width height)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (make-size width height)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height) p (:struct size))
    (setf width (coerce (size-width size) 'double-float)
	  height (coerce (size-height size) 'double-float))))



(cffi:defcstruct (rect :class %rect)
  (origin (:struct point))
  (size (:struct size)))

(defstruct (rect
	    (:constructor make-rect (x y width height)))
  x y width height)

(defmethod cffi:translate-from-foreign (p (type %rect))
  (cffi:with-foreign-slots ((origin size) p (:struct rect))
    (make-rect (point-x origin)
	       (point-y origin)
	       (size-width size)
	       (size-height size))))

(defmethod cffi:translate-into-foreign-memory (rect (type %rect) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct rect) 'origin))
	 (size (cffi:foreign-slot-pointer p '(:struct rect) 'size)))
    (cffi:with-foreign-slots ((x y) origin (:struct point))
      (cffi:with-foreign-slots ((width height) size (:struct size))
	(setf x (coerce (rect-x rect) 'double-float)
	      y (coerce (rect-y rect) 'double-float)
	  width (coerce (rect-width rect) 'double-float)
	  height (coerce (rect-height rect) 'double-float))))))

(cffi:defcfun ("cg_load_bitmap_image" %load-cg-image) :pointer
  (path :string))

(defun load-image (path)
  (let* ((full-path (uiop/driver:truename* path)))
    (assert full-path nil "~s : Can't find image file" path)
    (%load-cg-image (namestring full-path))))

(cffi:defcfun ("CGImageGetWidth" image-width) :sizet
  (image :pointer))

(cffi:defcfun ("CGImageGetHeight" image-height) :sizet
  (image :pointer))

(cffi:defcfun ("CGImageGetBitsPerPixel" image-bpp) :sizet
  (image :pointer))

(cffi:defcfun ("bitmap_data" image-bitmap-data) :pointer
  (image :pointer))

(cffi:defcfun ("CGImageRetain" retain-image) :pointer
  (image :pointer))

(cffi:defcfun ("CGImageRelease" release-image) :void
  (image :pointer))

(cffi:defcfun ("cg_get_cgimage_from_screen" make-image-from-screen) :pointer
  (rect (:struct rect)))


;; =======================================================
;; CoreGraphics frameworks 
;; =======================================================

(defmacro cgfloat (x)
  `(float ,x 1.0d0))

;; Graphics state
(defun save-gstate (context)
  (cffi:foreign-funcall "CGContextSaveGState" :pointer context))

(defun restore-gstate (context)
  (cffi:foreign-funcall "CGContextRestoreGState" :pointer context))


;; Coordinate space transformations
(defun scale-ctm (context sx sy)
  (cffi:foreign-funcall "CGContextScaleCTM" :pointer context :double (cgfloat sx) :double (cgfloat sy)))

(defun translate-ctm (context tx ty)
  (cffi:foreign-funcall "CGContextTranslateCTM" :pointer context :double (cgfloat tx) :double (cgfloat ty)))

(defun rotate-ctm (context radians)
  (cffi:foreign-funcall "CGContextRotateCTM" :pointer context :double (cgfloat radians)))


;; CGAffineTransform 구조체 필요..
;; (defun context-concat-ctm (context transform)
;;   (cffi:foreign-funcall "CGContextConcatCTM" :pointer context  transform))

;; (defun context-get-ctm (context)
;;   (let ((ctm (make-gcable-record #>CGAffineTransform)))
;;     (#_CGContextGetCTM ctm context)
;;     ctm))


;;; Drawing attribute functions
(defun set-line-width (context width)
  (cffi:foreign-funcall "CGContextSetLineWidth" :pointer context :double (cgfloat width)))

(defun set-line-cap (context cap)
  (let ((code (case cap
                (:butt 0) ;; kCGLineCapButt
                (:square 2) ;; kCGLineCapSquare
                (:round 1) ;;  kCGLineCapRound
                (otherwise cap))))
    (cffi:foreign-funcall "CGContextSetLineCap" :pointer context :int code)))

(defun set-line-join (context join)
  (let ((code (ecase join
                (:miter 0) ;; kCGLineJoinMiter
                (:round 1) ;; kCGLineJoinRound
                (:bevel 2) ;; kCGLineJoinBevel
                (otherwise join))))
    (cffi:foreign-funcall "CGContextSetLineJoin" :pointer context :int code)))

(defun set-miter-limit (context limit)
  (cffi:foreign-funcall "CGContextSetMiterLimit" :pointer context :double (cgfloat limit)))

;; (defun context-set-line-dash (context phase lengths)
;;   (let ((n (length lengths)))
;;     (%stack-block ((p (* n (record-length #>CGFloat))))
;;       (dotimes (i n)
;;         (setf (paref p (:array #>CGFloat) i) (cgfloat (elt lengths i))))
;;       (#_CGContextSetLineDash context (cgfloat phase) p n))))

(defun set-flatness (context flatness)
  (cffi:foreign-funcall "CGContextSetFlatness" :pointer context :double (cgfloat flatness)))

(defun set-alpha (context alpha)
  (cffi:foreign-funcall "CGContextSetAlpha" :pointer context :double (cgfloat alpha)))

(defparameter *blend-mode-alist*
  '((:normal . 0)
    (:multiply . 1)
    (:screen . 2)
    (:overlay . 3)
    (:darken . 4)
    (:lighten . 5)
    (:color-dodge . 6)
    (:color-burn . 7)
    (:soft-light . 8)
    (:hard-light . 9)
    (:difference . 10)
    (:exclusion . 11)
    (:hue . 12)
    (:saturation . 13)
    (:color . 14)
    (:luminosity . 15)
    (:clear . 16)
    (:copy . 17)
    (:source-in . 18)
    (:source-out . 19)
    (:source-atop . 20)
    (:destination-over . 21)
    (:destination-in . 22)
    (:destination-out . 23)
    (:destination-atop . 24)
    (:xor . 25)
    (:plus-darker . 26)
    (:plus-lighter . 27)))

(defun set-blend-mode (context mode)
  (let ((code (or (cdr (assoc mode *blend-mode-alist*))
                  mode)))
    (cffi:foreign-funcall "CGContextSetBlendMode" :pointer context :int code)))


;;; Path construction functions
(defun begin-path (context)
  (cffi:foreign-funcall "CGContextBeginPath" :pointer context))

(defun move-to-point (context x y)
  (cffi:foreign-funcall "CGContextMoveToPoint" :pointer context :double (cgfloat x) :double (cgfloat y)))

(defun add-line-to-point (context x y)
  (cffi:foreign-funcall "CGContextAddLineToPoint" :pointer context :double (cgfloat x) :double (cgfloat y)))

(defun add-curve-to-point (context cp1x cp1y cp2x cp2y x y)
  (cffi:foreign-funcall "CGContextAddCurveToPoint" :pointer context :double (cgfloat cp1x) :double (cgfloat cp1y)
			:double (cgfloat cp2x) :double (cgfloat cp2y) :double (cgfloat x) :double (cgfloat y)))

(defun add-quad-curve-to-point (context cpx cpy x y)
  (cffi:foreign-funcall "CGContextAddQuadCurveToPoint" :pointer context :double (cgfloat cpx) :double (cgfloat cpy)
			:double (cgfloat x) :double (cgfloat y)))

(defun close-path (context)
  (cffi:foreign-funcall "CGContextClosePath" :pointer context))

;;; Path construction convenience functions
#|
CGContextAddRect
CGContextAddRects
CGContextAddLines
CGContextAddEllipseInRect
|#

(defun add-arc (context x y radius start-angle end-angle clockwise)
  (cffi:foreign-funcall "CGContextAddArc" :pointer context :double (cgfloat x) :double (cgfloat y)
			:double (cgfloat radius) :double (cgfloat start-angle) :double (cgfloat end-angle)
			:int clockwise))
#|
CGContextAddArcToPoint
CGContextAddPath
|#


;;; Path stroking
#|
CGContextReplacePathWithStrokedPath
|#

;;; Path information functions
#|
CGContextIsPathEmpty
CGContextGetPathCurrentPoint
CGContextGetPathBoundingBox
CGContextCopyPath
CGContextPathContainsPoint
|#

;;; Path drawing functions
(defun draw-path (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGPathFill
                (:eofill 1) ;; kCGPathEOFill
                (:stroke 2) ;; kCGPathStroke
                (:fill-stroke 3) ;; kCGPathFillStroke
                (:eofill-stroke 4) ;; kCGPathEOFillStroke
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextDrawPath" :pointer context :int code)))


;;; Path drawing convenience functions
(defun fill-path (context)
  (cffi:foreign-funcall "CGContextFillPath" :pointer context))

(defun eo-fill-path (context)
  (cffi:foreign-funcall "CGContextEOFillPath" :pointer context))

(defun stroke-path (context)
  (cffi:foreign-funcall "CGContextStrokePath" :pointer context))

(defun fill-rect (context rect)
  (cffi:foreign-funcall "CGContextFillRect" :pointer context (:struct cg:rect) rect))

;; (defun fill-rects (context rects count)
;;   (cffi:foreign-funcall "CGContextFillRects" :pointer context (:struct cg:rect) rects :int count))

(defun stroke-rect (context rect)
  (cffi:foreign-funcall "CGContextStrokeRect" :pointer context (:struct cg:rect) rect))

(defun stroke-rect-with-width (context rect width)
  (cffi:foreign-funcall "CGContextStrokeRectWithWidth" :pointer context (:struct cg:rect) rect :double (cgfloat width)))

(defun clear-rect (context rect)
  (cffi:foreign-funcall "CGContextClearRect" :pointer context (:struct cg:rect) rect))

(defun fill-ellipse-in-rect (context rect)
  (cffi:foreign-funcall "CGContextFillEllipseInRect" :pointer context (:struct cg:rect) rect))

(defun stroke-ellipse-in-rect (context rect)
  (cffi:foreign-funcall "CGContextStrokeEllipseInRect" :pointer context (:struct cg:rect) rect))

;; (defun stroke-line-segments (context points count)
;;   (cffi:foreign-funcall "CGContextStrokeLineSegments" :pointer  context points count))


;;; Clipping functions
#|
CGContextClip
CGContextEOClip
CGContextClipToMask
CGContextGetClipBoundingBox
|#

;;; Clipping convenience functions
#|
CGContextClipToRect
CGContextClipToRects
|#

;;; Primitive color functions
(defun set-fill-color-with-color (context color)
  (cffi:foreign-funcall "CGContextSetFillColorWithColor" :pointer context :pointer color))

(defun set-stroke-color-with-color (context color)
  (cffi:foreign-funcall "CGContextSetStrokeColorWithColor" :pointer context :pointer color))

;;; Color space functions
(defun set-fill-color-space (context space)
  (cffi:foreign-funcall "CGContextSetFillColorSpace" :pointer context :pointer space))

(defun set-stroke-color-space (context space)
  (cffi:foreign-funcall "CGContextSetStrokeColorSpace" :pointer context :pointer space))

;;; Color functions
#|
CGContextSetFillColor
CGContextSetStrokeColor
|#

;;; Pattern functions
#|
CGContextSetFillPattern
CGContextSetStrokePattern
CGContextSetPatternPhase
|#

;;; Color convenience functions
(defun set-gray-fill-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayFillColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))

(defun set-gray-stroke-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayStrokeColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))


(defun set-rgb-fill-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun set-cmyk-fill-color (context cyan magenta yellow black &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKFillColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double  (cgfloat black) :double (cgfloat alpha)))

(defun set-cmyk-stroke-color (context cyan magenta yellow black &optional(alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKStrokeColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double (cgfloat black) :double (cgfloat alpha)))


;;; Rendering intent
#|
CGContextSetRenderingIntent
|#

;;; Image functions
#|
CGContextDrawImage
CGContextDrawTiledImage
CGInterpolationQuality
CGContextSetInterpolationQuality
|#

;;; Shadow support
#|
CGContextSetShadowWithColor
CGContextSetShadow
|#

;; (defun context-set-shadow (context dx dy blur)
;;   (rlet ((offset #>CGSize :width (cgfloat dx) :height (cgfloat dy)))
;;     (#_CGContextSetShadow context offset (cgfloat blur))))

;;; Gradient and shading functions
#|
CGContextDrawLinearGradient
CGContextDrawRadialGradient
CGContextDrawShading
|#

;;; Text functions
(defun set-character-spacing (context spacing)
  (cffi:foreign-funcall "CGContextSetCharacterSpacing" :pointer context :double (cgfloat spacing)))

(defun set-text-position (context x y)
  (cffi:foreign-funcall "CGContextSetTextPosition" :pointer context :double (cgfloat x) :double (cgfloat y)))

;; (defun context-get-text-position (context)
;;   (let ((pt (make-gcable-record #>CGPoint)))
;;     (#_CGContextGetTextPosition pt context)))

#|
CGContextSetTextMatrix
CGContextGetTextMatrix
|#
(defun set-text-drawing-mode (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGTextFill
                (:stroke 1) ;; kCGTextStroke
                (:fill-stroke 2) ;; kCGTextFillStroke
                (:invisible 3) ;; kCGTextInvisible
                (:fill-clip 4) ;; kCGTextFillClip
                (:stroke-clip 5) ;; kCGTextStrokeClip
                (:fill-stroke-clip 6) ;; kCGTextFillStrokeClip
                (:clip 7) ;; kCGTextClip
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextSetTextDrawingMode" :pointer context :int code)))

#|
CGContextSetFont
CGContextSetFontSize
|#

(defun select-font (context font-name size encoding)
  (let ((code (case encoding
		(:macroman 1) ;; #$kCGEncodingMacRoman
		(:font-specific 0) ;; #$kCGEncodingFontSpecific
		(otherwise encoding))))
    (cffi:foreign-funcall "CGContextSelectFont" :pointer context :string font-name :double (cgfloat size) :int code)))
#|
CGContextShowGlyphsAtPositions
|#

;;; Text convenience functions
(defun show-text (context string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowText" :pointer context :string string :int n)))

(defun show-text-at-point (context x y string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowTextAtPoint" :pointer context :double (cgfloat x) 
			  :double (cgfloat y) :string string :int n)))

#|
CGContextShowGlyphs
CGContextShowGlyphsAtPoint
CGContextShowGlyphsWithAdvances
|#

;;; PDF functions
#|
CGContextDrawPDFPage
|#

;;; Output page functions
#|
CGContextBeginPage
CGContextEndPage
|#

#|
CGContextFlush
CGContextSynchronize
|#

;;; Antialiasing functions
#|
CGContextSetShouldAntialias
CGContextSetAllowsAntialiasing
|#

;;; Font display functions
#|
CGContextSetShouldSmoothFonts
CGContextSetAllowsFontSmoothing
CGContextSetShouldSubpixelPositionFonts
CGContextSetAllowsFontSubpixelPositioning
CGContextSetShouldSubpixelQuantizeFonts
CGContextSetAllowsFontSubpixelQuantization
|#

;;; Transparency layer support
#|
CGContextBeginTransparencyLayer
CGContextBeginTransparencyLayerWithRect
CGContextEndTransparencyLayer
|#

;;; User space to device space transformations
#|
CGContextGetUserSpaceToDeviceSpaceTransform
CGContextConvertPointToDeviceSpace
CGContextConvertPointToUserSpace
CGContextConvertSizeToDeviceSpace
CGContextConvertSizeToUserSpace
CGContextConvertRectToDeviceSpace
CGContextConvertRectToUserSpace
|#


;;; CGPath

#|
CGPathCreateMutable
CGPathCreateCopy
CGPathCreateCopyByTransformingPath
CGPathCreateMutableCopy
CGPathCreateMutableCopyByTransformingPath
CGPathCreateWithRect
CGPathCreateWithEllipseInRect
CGPathCreateCopyByDashingPath
CGPathCreateCopyByStrokingPath
CGPathEqualToPath

|#

;; (defun path-move-to-point (path transform x y)
;;   (when (null transform) (setq transform +null-ptr+))
;;   (#_CGPathMoveToPoint path transform (cgfloat x) (cgfloat y)))

;; (defun path-add-line-to-point (path transform x y)
;;   (when (null transform) (setq transform +null-ptr+))
;;   (#_CGPathAddLineToPoint path transform (cgfloat x) (cgfloat y)))

#|
CGPathAddQuadCurveToPoint
CGPathAddCurveToPoint
CGPathCloseSubpath
CGPathAddRect
CGPathAddRects
CGPathAddLines
CGPathAddEllipseInRect
CGPathAddRelativeArc
CGPathAddArc
CGPathAddArcToPoint
CGPathAddPath
CGPathIsEmpty
CGPathIsRect
CGPathGetCurrentPoint
CGPathGetBoundingBox
CGPathGetPathBoundingBox
CGPathContainsPoint
|#

(defun color-create-generic-rgb (r g b a)
  (cffi:foreign-funcall "CGColorCreateGenericRGB" :double (cgfloat r) :double (cgfloat g) :double (cgfloat b)
			:double (cgfloat a) :pointer))




