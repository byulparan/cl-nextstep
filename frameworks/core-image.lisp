(defpackage :ci
  (:use :cl)
  (:export #:make-context
	   #:draw-image
	   #:render-to-bitmap
	   #:load-image
	   #:image-from-texture
	   #:image-from-cg-image
	   #:extent
	   #:draw-image-to-view
	   #:make-filter
	   #:set-filter-param
	   #:apply-filter))

(in-package :ci)

;; ci-context
(defun make-context (cgl-context pixel-format)
  (let* ((color-space (cg:color-space-create :color-space-srgb))
	 (ci-context (ns:objc
		      (ns:objc "CIContext" "contextWithCGLContext:pixelFormat:colorSpace:options:"
			       :pointer cgl-context
			       :pointer pixel-format
			       :pointer color-space
			       :pointer (cffi:null-pointer)
			       :pointer)
		      "retain" :pointer)))
    (cg:color-space-release color-space)
    ci-context))

(defun draw-image (ci-context ci-image in-rect from-rect)
  (ns:objc ci-context "drawImage:inRect:fromRect:"
	   :pointer ci-image
	   (:struct cg:rect) in-rect
	   (:struct cg:rect) from-rect))

(defun render-to-bitmap (ci-context ci-image data row-bytes bounds format color-space)
  (ns:objc ci-context "render:toBitmap:rowBytes:bounds:format:colorSpace:"
	   :pointer ci-image
	   :pointer data
	   :int row-bytes
	   (:struct cg:rect) bounds
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
  (let* ((color-space (cg:color-space-create :color-space-srgb)))
    (unwind-protect (ns:objc "CIImage" "imageWithTexture:size:flipped:colorSpace:"
			     :unsigned-int texture
			     (:struct cg:size) size
			     :bool nil
			     :pointer color-space
			     :pointer)
      (cg:color-space-release color-space))))

(defun image-from-cg-image (cg-image)
  (ns:objc "CIImage" "imageWithCGImage:" :pointer cg-image :pointer))

(defun extent (ci-image)
  (ns:objc ci-image "extent" (:struct cg:rect)))

(defun draw-image-to-view (ci-image rect from-rect operation delta)
  (ns:objc ci-image "drawInRect:fromRect:operation:fraction:"
	   (:struct cg:rect) rect
	   (:struct cg:rect) from-rect
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
   (cocoa-ref :initarg :cocoa-ref :reader ns::cocoa-ref)))

(defmethod print-object ((object filter) stream)
  (format stream "#<CIFilter name: \"~a\" params: [~{\"~a\"~^ ~}]>" (name object) (params object)))

(defun make-filter (name)
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
		       :cocoa-ref filter)))))

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
  (ns:with-event-loop (:waitp t)
    (ns:objc filter "setValue:forKey:" :pointer ci-image
				       :pointer (ns:autorelease (ns:make-ns-string "inputImage")))
    (ns:objc filter "valueForKey:" :pointer (ns:autorelease (ns:make-ns-string "outputImage"))
	     :pointer)))




