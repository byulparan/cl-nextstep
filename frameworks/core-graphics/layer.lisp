(in-package :cg)

(defun make-layer (context width height)
  (sb-alien:with-alien ((%size (sb-alien:struct ns:size)))
    (setf (sb-alien:slot %size 'ns:width) (float width 1.0d0)
	  (sb-alien:slot %size 'ns:height) (float height 1.0d0))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "CGLayerCreateWithContext" (sb-alien:function sb-alien:system-area-pointer
									  sb-alien:system-area-pointer
									  (sb-alien:struct ns:size)
									  sb-alien:system-area-pointer))
     context
     %size
     (cffi:null-pointer))))


(cffi:defcfun (release-layer "CGLayerRelease") :void
  (cg-layer :pointer))

(cffi:defcfun (retain-layer "CGLayerRetain") :pointer
  (cg-layer :pointer))

(defun draw-layer-in-rect (context rect layer)
  (ns::with-sb-alien-rect (rect rect)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "CGContextDrawLayerInRect" (sb-alien:function sb-alien:void
									  sb-alien:system-area-pointer
									  (sb-alien:struct ns:rect)
									  sb-alien:system-area-pointer))
     context
     rect
     layer)))

(defun draw-layer-at-point (context point layer)
  (sb-alien:with-alien ((%point (sb-alien:struct ns:point)))
    (setf (sb-alien:slot %point 'ns::x) (float (ns:point-x point) 1.0d0)
	  (sb-alien:slot %point 'ns::y) (float (ns:point-y point) 1.0d0))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "CGContextDrawLayerAtPoint" (sb-alien:function sb-alien:void
									   sb-alien:system-area-pointer
									   (sb-alien:struct ns:point)
									   sb-alien:system-area-pointer))
     context
     %point
     layer)))


(defun layer-size (cg-layer)
  (let* ((%size (sb-alien:alien-funcall
		 (sb-alien:extern-alien "CGLayerGetSize" (sb-alien:function (sb-alien:struct ns:size)
									    sb-alien:system-area-pointer))
		 cg-layer)))
    (ns:size (sb-alien:slot %size 'ns:width)
	     (sb-alien:slot %size 'ns:height))))


(cffi:defcfun (layer-context "CGLayerGetContext") :pointer
  (cg-layer :pointer))






