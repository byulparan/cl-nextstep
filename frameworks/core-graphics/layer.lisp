(in-package :cg)

(defun make-layer (context width height)
  (cffi:foreign-funcall "CGLayerCreateWithContext"
			:pointer context
			(:struct ns:size) (ns:size width height)
			:pointer (cffi:null-pointer)
			:pointer))


(cffi:defcfun (release-layer "CGLayerRelease") :void
  (cg-layer :pointer))

(cffi:defcfun (retain-layer "CGLayerRetain") :pointer
  (cg-layer :pointer))

(cffi:defcfun (draw-layer-in-rect "CGContextDrawLayerInRect") :void
  (context :pointer)
  (rect (:struct ns:rect))
  (layer :pointer))

(cffi:defcfun (draw-layer-at-point "CGContextDrawLayerAtPoint") :void
  (context :pointer)
  (point (:struct ns:point))
  (layer :pointer))


(cffi:defcfun (layer-size "CGLayerGetSize") (:struct ns:size)
  (cg-layer :pointer))

(cffi:defcfun (layer-context "CGLayerGetContext") :pointer
  (cg-layer :pointer))






