(defpackage :core-image
  (:nicknames :ci)
  (:use :cl)
  (:export #:make-context
	   #:make-context-from-cg-context
	   #:draw-image
	   #:render-to-bitmap
	   #:load-image
	   #:image-from-texture
	   #:image-from-cg-image
	   #:extent
	   #:draw-image-to-current-context
	   #:make-filter
	   #:active
	   #:set-filter-param
	   #:apply-filter))

(in-package :core-image)


(defun make-context (cgl-context pixel-format)
  "Creating a Context for GPU-Based Rendering with OpenGL. When end of lifetime you should be call `ns:release` explictly."
  (let* ((color-space (cg:make-color-space :color-space-srgb))
	 (ci-context (ns:objc "CIContext" "contextWithCGLContext:pixelFormat:colorSpace:options:"
			       :pointer cgl-context
			       :pointer pixel-format
			       :pointer color-space
			       :pointer (cffi:null-pointer)
			       :pointer)))
    (cg:release-color-space color-space)
    (ns:retain ci-context)))

(defun make-context-from-cg-context (cg-context &key (options (cffi:null-pointer)))
  "Creating a Context for CPU-Based Rendering. When end of lifetime you should be call `ns:release` explictly."
  (ns:retain
   (ns:objc "CIContext" "contextWithCGContext:options:"
	    :pointer cg-context
	    :pointer options
	    :pointer)))


(defun draw-image (ci-context ci-image in-rect from-rect)
  "Renders a region of an image to a rectangle in the context destination."
  (ns::with-sb-alien-rect (in-rect in-rect)
    (ns::with-sb-alien-rect (from-rect from-rect)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								(sb-alien:struct ns:rect)
								(sb-alien:struct ns:rect)))
       (ns::cocoa-ref ci-context)
       (ns::sel "drawImage:inRect:fromRect:")
       ci-image
       in-rect
       from-rect))))


(defun render-to-bitmap (ci-context ci-image data row-bytes bounds format color-space)
  (ns::with-sb-alien-rect (rect bounds)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer
							      sb-alien:int
							      (sb-alien:struct ns:rect)
							      sb-alien:int
							      sb-alien:system-area-pointer))
     (ns::cocoa-ref ci-context)
     (ns::sel "render:toBitmap:rowBytes:bounds:format:colorSpace:")
     ci-image
     data
     row-bytes
     rect
     (case format
       (:ci-format-argb8 265)
       (t format))
     color-space)))



;; ci-image
(defun load-image (path)
  (let* ((path (namestring (uiop:truenamize path))))
    (assert (probe-file path) nil "can't find file: ~s" path)
    (ns:with-event-loop (:waitp t)
      (let* ((path (ns:autorelease (ns:make-ns-string path)))
	     (url (ns:objc "NSURL" "fileURLWithPath:" :pointer path :pointer)))
	(ns:objc (ns:objc "CIImage" "alloc" :pointer)
		 "initWithContentsOfURL:" :pointer url :pointer)))))

(defun image-from-texture (texture size)
  (let* ((color-space (cg:make-color-space :color-space-srgb)))
    (sb-alien:with-alien ((%size (sb-alien:struct ns:size)))
      (setf (sb-alien:slot %size 'ns:width) (float (ns:size-width size) 1.0d0)
	    (sb-alien:slot %size 'ns:height) (float (ns:size-height size) 1.0d0))
      (unwind-protect (sb-alien:alien-funcall
		       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
										sb-alien:system-area-pointer
										sb-alien:system-area-pointer
										sb-alien:unsigned-int
										(sb-alien:struct ns:size)
										sb-alien:int
										sb-alien:system-area-pointer))
		       (ns:cls "CIImage")
		       (ns:sel "imageWithTexture:size:flipped:colorSpace:")
		       texture
		       %size
		       0
		       color-space)
	(cg:release-color-space color-space)))))


(defun image-from-cg-image (cg-image)
  (ns:objc "CIImage" "imageWithCGImage:" :pointer cg-image :pointer))

(defun extent (ci-image)
  (sb-alien:with-alien ((%rect (sb-alien:struct ns:rect)))
    (sb-alien:alien-funcall-into
     (sb-alien:extern-alien "objc_msgSend" (sb-alien:function (sb-alien:struct ns:rect)
							      sb-alien:system-area-pointer
							      sb-alien:system-area-pointer))
     (sb-alien:alien-sap %rect)
     (ns::cocoa-ref ci-image)
     (ns:sel "extent"))
    (let* ((origin (sb-alien:slot %rect 'ns::origin))
	   (size (sb-alien:slot %rect 'ns:size)))
      (ns:rect (sb-alien:slot origin 'ns::x)
	       (sb-alien:slot origin 'ns::y)
	       (sb-alien:slot size 'ns:width)
	       (sb-alien:slot size 'ns:height)))))


(defun draw-image-to-current-context (ci-image in-rect from-rect operation delta)
  "Draws all or part of the image in the specified rectangle in the current coordinate system."
  (ns::with-sb-alien-rect (in-rect in-rect)
    (ns::with-sb-alien-rect (from-rect from-rect)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:void
								sb-alien:system-area-pointer
								sb-alien:system-area-pointer
								(sb-alien:struct ns:rect)
								(sb-alien:struct ns:rect)
								sb-alien:int
								sb-alien:double))
       (ns::cocoa-ref ci-image)
       (ns:sel "drawInRect:fromRect:operation:fraction:")
       in-rect
       from-rect
       (case operation
	 (:clear 0)
	 (:copy 1)
	 (:source-over 2)
	 (:source-in 3)
	 (:source-out 4)
	 (:source-at-op 5)
	 (:destination-over 6)
	 (:destination-in 7)
	 (:destination-out 8)
	 (:Destination-at-op)
	 (t operation))
       (float delta 1.0d0)))))

;; ci-filter
(defvar *core-filter-db*)

(defclass filter ()
  ((name :initarg :name :reader name)
   (params :initarg :params :reader params)
   (cocoa-ref :initarg :cocoa-ref :reader ns::cocoa-ref)
   (active :initarg :active)))

(defmethod print-object ((object filter) stream)
  (format stream "#<CIFilter name: \"~a\" params: [~{\"~a\"~^ ~}]>" (name object) (params object)))

(defun make-filter (name &key (active t))
  (let* ((item (cdr (assoc name *core-filter-db*)))
	 (filter-name (car item))
	 (params (cdr item)))
    (assert filter-name nil "can't find ci-filter: ~a" name)
    (ns:with-event-loop (:waitp t)
      (let* ((name (ns:autorelease (ns:make-ns-string filter-name)))
	     (filter (ns:retain (ns:objc "CIFilter" "filterWithName:" :pointer name
								      :pointer))))
	(ns:objc filter "setDefaults")
	(make-instance 'filter :name filter-name
		       :params params
		       :active active
		       :cocoa-ref filter)))))

(defun active (filter value)
  (with-slots (active) filter
    (setf active value)))

(defmethod set-filter-param (filter param (value number))
  (let* ((parameter (find (string-upcase param) (params filter) :test #'equalp)))
    (assert parameter nil "can't find param ~a of ~a" param filter)
    (ns:with-event-loop nil
      (ns:objc filter "setValue:forKey:" :pointer (ns:objc "NSNumber" "numberWithDouble:"
							      :double (float value 1.0d0)
							      :pointer)
					    :pointer (ns:autorelease (ns:make-ns-string parameter))))))

(defmethod set-filter-param (filter param (value list))
  (let* ((parameter (find (string-upcase param) (params filter) :test #'equalp)))
    (assert parameter nil "can't find param ~a of ~a" param filter)
    (ns:with-event-loop nil
      (let* ((count (length value)))
	(cffi:with-foreign-objects ((value-ptr :double count))
	  (dotimes (i count)
	    (setf (cffi:mem-aref value-ptr :double i) (float (nth i value) 1.0d0)))
	  (ns:objc filter "setValue:forKey:" :pointer (ns:objc "CIVector" "vectorWithValues:count:"
							       :pointer value-ptr :int count :pointer)
					     :pointer (ns:autorelease (ns:make-ns-string parameter))))))))

(defun apply-filter (filter ci-image)
  (with-slots (active) filter
    (if active (ns:with-event-loop (:waitp t)
		   (ns:objc filter "setValue:forKey:" :pointer ci-image
						      :pointer (ns:autorelease (ns:make-ns-string "inputImage")))
		   (ns:objc filter "valueForKey:" :pointer (ns:autorelease (ns:make-ns-string "outputImage"))
						  :pointer))
      ci-image)))




