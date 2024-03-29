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
  (ns:objc ci-context "drawImage:inRect:fromRect:"
	   :pointer ci-image
	   (:struct ns:rect) in-rect
	   (:struct ns:rect) from-rect))

(defun render-to-bitmap (ci-context ci-image data row-bytes bounds format color-space)
  (ns:objc ci-context "render:toBitmap:rowBytes:bounds:format:colorSpace:"
	   :pointer ci-image
	   :pointer data
	   :int row-bytes
	   (:struct ns:rect) bounds
	   :int (case format
		  (:ci-format-argb8 265)
		  (t format))
	   :pointer color-space))

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
    (unwind-protect (ns:objc "CIImage" "imageWithTexture:size:flipped:colorSpace:"
			     :unsigned-int texture
			     (:struct ns:size) size
			     :int 0
			     :pointer color-space
			     :pointer)
      (cg:release-color-space color-space))))

(defun image-from-cg-image (cg-image)
  (ns:objc "CIImage" "imageWithCGImage:" :pointer cg-image :pointer))

(defun extent (ci-image)
  #+x86-64 (ns:objc-stret ns:rect ci-image "extent")
  #+arm64 (ns:objc ci-image "extent" (:struct ns:rect)))

(defun draw-image-to-current-context (ci-image in-rect from-rect operation delta)
  "Draws all or part of the image in the specified rectangle in the current coordinate system."
  (ns:objc ci-image "drawInRect:fromRect:operation:fraction:"
	   (:struct ns:rect) in-rect
	   (:struct ns:rect) from-rect
	   :int (case operation
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
	   :double (float delta 1.0d0)))

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




