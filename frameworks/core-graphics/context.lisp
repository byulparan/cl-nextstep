(in-package :core-graphics)

;; =======================================================
;; CoreGraphics frameworks 
;; =======================================================

(defmacro cgfloat (x)
  `(float ,x 1.0d0))

;; Graphics state
(defun context-save-gstate (context)
  (cffi:foreign-funcall "CGContextSaveGState" :pointer context))

(defun context-restore-gstate (context)
  (cffi:foreign-funcall "CGContextRestoreGState" :pointer context))


;; Coordinate space transformations
(defun context-scale-ctm (context sx sy)
  (cffi:foreign-funcall "CGContextScaleCTM" :pointer context :double (cgfloat sx) :double (cgfloat sy)))

(defun context-translate-ctm (context tx ty)
  (cffi:foreign-funcall "CGContextTranslateCTM" :pointer context :double (cgfloat tx) :double (cgfloat ty)))

(defun context-rotate-ctm (context radians)
  (cffi:foreign-funcall "CGContextRotateCTM" :pointer context :double (cgfloat radians)))


;; CGAffineTransform 구조체 필요..
;; (defun context-concat-ctm (context transform)
;;   (cffi:foreign-funcall "CGContextConcatCTM" :pointer context  transform))

;; (defun context-get-ctm (context)
;;   (let ((ctm (make-gcable-record #>CGAffineTransform)))
;;     (#_CGContextGetCTM ctm context)
;;     ctm))


;;; Drawing attribute functions
(defun context-set-line-width (context width)
  (cffi:foreign-funcall "CGContextSetLineWidth" :pointer context :double (cgfloat width)))

(defun context-set-line-cap (context cap)
  (let ((code (case cap
                (:butt 0) ;; kCGLineCapButt
                (:square 2) ;; kCGLineCapSquare
                (:round 1) ;;  kCGLineCapRound
                (otherwise cap))))
    (cffi:foreign-funcall "CGContextSetLineCap" :pointer context :int code)))

(defun context-set-line-join (context join)
  (let ((code (ecase join
                (:miter 0) ;; kCGLineJoinMiter
                (:round 1) ;; kCGLineJoinRound
                (:bevel 2) ;; kCGLineJoinBevel
                (otherwise join))))
    (cffi:foreign-funcall "CGContextSetLineJoin" :pointer context :int code)))

(defun context-set-miter-limit (context limit)
  (cffi:foreign-funcall "CGContextSetMiterLimit" :pointer context :double (cgfloat limit)))

;; (defun context-set-line-dash (context phase lengths)
;;   (let ((n (length lengths)))
;;     (%stack-block ((p (* n (record-length #>CGFloat))))
;;       (dotimes (i n)
;;         (setf (paref p (:array #>CGFloat) i) (cgfloat (elt lengths i))))
;;       (#_CGContextSetLineDash context (cgfloat phase) p n))))

(defun context-set-flatness (context flatness)
  (cffi:foreign-funcall "CGContextSetFlatness" :pointer context :double (cgfloat flatness)))

(defun context-set-alpha (context alpha)
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

(defun context-set-blend-mode (context mode)
  (let ((code (or (cdr (assoc mode *blend-mode-alist*))
                  mode)))
    (cffi:foreign-funcall "CGContextSetBlendMode" :pointer context :int code)))


;;; Path construction functions
(defun context-begin-path (context)
  (cffi:foreign-funcall "CGContextBeginPath" :pointer context))

(defun context-move-to-point (context x y)
  (cffi:foreign-funcall "CGContextMoveToPoint" :pointer context :double (cgfloat x) :double (cgfloat y)))

(defun context-add-line-to-point (context x y)
  (cffi:foreign-funcall "CGContextAddLineToPoint" :pointer context :double (cgfloat x) :double (cgfloat y)))

(defun context-add-curve-to-point (context cp1x cp1y cp2x cp2y x y)
  (cffi:foreign-funcall "CGContextAddCurveToPoint" :pointer context :double (cgfloat cp1x) :double (cgfloat cp1y)
			:double (cgfloat cp2x) :double (cgfloat cp2y) :double (cgfloat x) :double (cgfloat y)))

(defun context-add-quad-curve-to-point (context cpx cpy x y)
  (cffi:foreign-funcall "CGContextAddQuadCurveToPoint" :pointer context :double (cgfloat cpx) :double (cgfloat cpy)
			:double (cgfloat x) :double (cgfloat y)))

(defun context-close-path (context)
  (cffi:foreign-funcall "CGContextClosePath" :pointer context))

;;; Path construction convenience functions
#|
CGContextAddRect
CGContextAddRects
CGContextAddLines
CGContextAddEllipseInRect
|#

(defun context-add-arc (context x y radius start-angle end-angle clockwise)
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
(defun context-draw-path (context mode)
  (let ((code (case mode
                (:fill 0) ;; kCGPathFill
                (:eofill 1) ;; kCGPathEOFill
                (:stroke 2) ;; kCGPathStroke
                (:fill-stroke 3) ;; kCGPathFillStroke
                (:eofill-stroke 4) ;; kCGPathEOFillStroke
                (otherwise mode))))
    (cffi:foreign-funcall "CGContextDrawPath" :pointer context :int code)))


;;; Path drawing convenience functions
(defun context-fill-path (context)
  (cffi:foreign-funcall "CGContextFillPath" :pointer context))

(defun context-eo-fill-path (context)
  (cffi:foreign-funcall "CGContextEOFillPath" :pointer context))

(defun context-stroke-path (context)
  (cffi:foreign-funcall "CGContextStrokePath" :pointer context))

(defun context-fill-rect (context rect)
  (cffi:foreign-funcall "CGContextFillRect" :pointer context (:struct ns:rect) rect))

;; (defun fill-rects (context rects count)
;;   (cffi:foreign-funcall "CGContextFillRects" :pointer context (:struct cg:rect) rects :int count))

(defun context-stroke-rect (context rect)
  (cffi:foreign-funcall "CGContextStrokeRect" :pointer context (:struct ns:rect) rect))

(defun context-stroke-rect-with-width (context rect width)
  (cffi:foreign-funcall "CGContextStrokeRectWithWidth" :pointer context (:struct ns:rect) rect :double (cgfloat width)))

(defun context-clear-rect (context rect)
  (cffi:foreign-funcall "CGContextClearRect" :pointer context (:struct ns:rect) rect))

(defun context-fill-ellipse-in-rect (context rect)
  (cffi:foreign-funcall "CGContextFillEllipseInRect" :pointer context (:struct ns:rect) rect))

(defun context-stroke-ellipse-in-rect (context rect)
  (cffi:foreign-funcall "CGContextStrokeEllipseInRect" :pointer context (:struct ns:rect) rect))

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
(defun context-set-fill-color-with-color (context color)
  (cffi:foreign-funcall "CGContextSetFillColorWithColor" :pointer context :pointer color))

(defun context-set-stroke-color-with-color (context color)
  (cffi:foreign-funcall "CGContextSetStrokeColorWithColor" :pointer context :pointer color))

;;; Color space functions
(defun context-set-fill-color-space (context space)
  (cffi:foreign-funcall "CGContextSetFillColorSpace" :pointer context :pointer space))

(defun context-set-stroke-color-space (context space)
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
(defun context-set-gray-fill-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayFillColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))

(defun context-set-gray-stroke-color (context gray &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetGrayStrokeColor" :pointer context :double (cgfloat gray) :double (cgfloat alpha)))


(defun context-set-rgb-fill-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBFillColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun context-set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetRGBStrokeColor" :pointer context
						   :double (float red 1.0d0)
						   :double (float green 1.0d0)
						   :double (float blue 1.0d0)
						   :double (float alpha 1.0d0)))

(defun context-set-cmyk-fill-color (context cyan magenta yellow black &optional (alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKFillColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double  (cgfloat black) :double (cgfloat alpha)))

(defun context-set-cmyk-stroke-color (context cyan magenta yellow black &optional(alpha 1))
  (cffi:foreign-funcall "CGContextSetCMYKStrokeColor" :pointer context :double (cgfloat cyan) :double (cgfloat magenta)
			:double (cgfloat yellow) :double (cgfloat black) :double (cgfloat alpha)))


;;; Rendering intent
#|
CGContextSetRenderingIntent
|#

;;; Image functions
#|
CGContextDrawTiledImage
CGInterpolationQuality
CGContextSetInterpolationQuality
|#
(defun context-draw-image (context rect cg-image)
  (cffi:foreign-funcall "CGContextDrawImage" :pointer context
			(:struct ns:rect) rect
			:pointer cg-image))

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
(defun context-set-character-spacing (context spacing)
  (cffi:foreign-funcall "CGContextSetCharacterSpacing" :pointer context :double (cgfloat spacing)))

(defun context-set-text-position (context x y)
  (cffi:foreign-funcall "CGContextSetTextPosition" :pointer context :double (cgfloat x) :double (cgfloat y)))

;; (defun context-get-text-position (context)
;;   (let ((pt (make-gcable-record #>CGPoint)))
;;     (#_CGContextGetTextPosition pt context)))

#|
CGContextSetTextMatrix
CGContextGetTextMatrix
|#
(defun context-set-text-drawing-mode (context mode)
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

;; (defun create-font (name)
;;   (let* ((cf-name (ns:make-cf-string name)))
;;     (unwind-protect (cffi:foreign-funcall "CGFontCreateWithFontName"
;; 					  :pointer cf-name
;; 					  :pointer)
;;       (ns:cf-release cf-name))))

(defun context-set-font (context font)
  (cffi:foreign-funcall "CGContextSetFont" :pointer context :pointer font))

(defun context-set-font-size (context size)
  (cffi:foreign-funcall "CGContextSetFontSize" :pointer context :double (cgfloat size)))


(defun context-select-font (context font-name size encoding)
  (let ((code (case encoding
		(:macroman 1) ;; #$kCGEncodingMacRoman
		(:font-specific 0) ;; #$kCGEncodingFontSpecific
		(otherwise encoding))))
    (cffi:foreign-funcall "CGContextSelectFont" :pointer context :string font-name :double (cgfloat size) :int code)))
;; #|
;; CGContextShowGlyphsAtPositions
;; |#

;; ;;; Text convenience functions
(defun context-show-text (context string)
  (let ((n (length (babel:string-to-octets string))))
    (cffi:foreign-funcall "CGContextShowText" :pointer context :string string :int n)))

(defun context-show-text-at-point (context x y string)
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
