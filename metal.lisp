(in-package :mtl)


;; Type
(cffi:defcstruct (origin :class %origin)
  (x :unsigned-long)
  (y :unsigned-long)
  (z :unsigned-long))

(defstruct (origin 
	    (:constructor make-origin (x y z)))
  x y z)

(defmethod cffi:translate-from-foreign (p (type %origin))
  (cffi:with-foreign-slots ((x y z) p (:struct origin))
    (make-origin x y z)))

(defmethod cffi:translate-into-foreign-memory (origin (type %origin) p)
  (cffi:with-foreign-slots ((x y z) p (:struct origin))
    (setf x (floor (origin-x origin))
	  y (floor (origin-y origin))
	  z (floor (origin-z origin)))))


(cffi:defcstruct (size :class %size)
  (width :unsigned-long)
  (height :unsigned-long)
  (depth :unsigned-long))

(defstruct (size
	    (:constructor make-size (width height depth)))
  width height depth)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (make-size width height depth)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (setf width (floor (size-width size))
	  height (floor (size-height size))
	  depth (floor (msize-depth size)))))

(cffi:defcstruct (region :class %region)
  (origin (:struct origin))
  (size (:struct size)))

(defstruct (region
	    (:constructor make-region (x y z width height depth)))
  x y z width height depth)

(defmethod cffi:translate-from-foreign (p (type %region))
  (cffi:with-foreign-slots ((origin size) p (:struct region))
    (make-region (origin-x origin)
		 (origin-y origin)
		 (origin-z origin)
		 (size-width size)
		 (size-height size)
		 (size-depth size))))

(defmethod cffi:translate-into-foreign-memory (region (type %region) p)
  (let* ((origin (cffi:foreign-slot-pointer p '(:struct region) 'origin))
	 (size (cffi:foreign-slot-pointer p '(:struct region) 'size)))
    (cffi:with-foreign-slots ((x y z) origin (:struct origin))
      (cffi:with-foreign-slots ((width height depth) size (:struct size))
	(setf x (floor (region-x region))
	      y (floor (region-y region))
	      z (floor (region-z region))
	      width (floor (region-width region))
	      height (floor (region-height region))
	      depth (floor (region-depth region)))))))





;; CommandQueue
(defun make-command-queue (device)
  (ns:objc device "newCommandQueue" :pointer))

(defun get-command-buffer (command-queue)
  (ns:objc command-queue "commandBuffer" :pointer))

(defun get-render-command-encoder (command-buffer descriptor)
  (ns:objc command-buffer "renderCommandEncoderWithDescriptor:" :pointer descriptor :pointer))

;; CommandBuffer
(defun present-drawable (command-buffer drawable)
  (ns:objc command-buffer "presentDrawable:" :pointer drawable))

(defun commit (command-buffer)
  (ns:objc command-buffer "commit"))


;; CommandEncoder
(defun set-render-pipeline-state (command-encoder pipeline-state)
  (ns:objc command-encoder "setRenderPipelineState:" :pointer pipeline-state))

(defun set-vertex-buffer (command-encoder buffer &key (offset 0) index)
  (ns:objc command-encoder "setVertexBuffer:offset:atIndex:" :pointer buffer
							     :int offset
							     :int index))

(defun set-fragment-buffer (command-encoder buffer &key (offset 0) index)
  (ns:objc command-encoder "setFragmentBuffer:offset:atIndex:" :pointer buffer
							       :int offset
							       :int index))

(defun draw-primitives (command-encoder primitive start count &optional (instance-count 1))
  (ns:objc command-encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
	   :int primitive
	   :int start
	   :int count
	   :int instance-count))

(defun end-encoding (command-encoder)
  (ns:objc command-encoder "endEncoding"))

;; MTLBuffer
(defun make-buffer (device data length options)
  (ns:objc device "newBufferWithBytes:length:options:"
	   :pointer data
	   :int length
	   :int options
	   :pointer))


;; Pipeline
(defun make-library (device source &key (options (cffi:null-pointer)))
  (ns:objc device "newLibraryWithSource:options:error:"
	    :pointer (ns:autorelease (ns:make-ns-string source))
	    :pointer options
	    :pointer (cffi:null-pointer)
	    :pointer))

(defun make-function (library name)
  (ns:objc library "newFunctionWithName:" :pointer (ns:autorelease (ns:make-ns-string name)) :pointer))

(defun make-render-pipeline-descriptor ()
  (ns::new "MTLRenderPipelineDescriptor"))

(defun set-vertex-function (render-pipeline-descriptor function)
  (ns:objc render-pipeline-descriptor "setVertexFunction:" :pointer function))

(defun set-vertex-descriptor (render-pipeline-descriptor vertex-descriptor)
  (ns:objc render-pipeline-descriptor "setVertexDescriptor:" :pointer vertex-descriptor))

(defun set-fragment-function (render-pipeline-descriptor function)
  (ns:objc render-pipeline-descriptor "setFragmentFunction:" :pointer function))

(defun set-color-attachment-pixel-format (render-pipeline-descriptor index pixel-format)
  (let* ((color-attachment
	   (ns:objc (ns:objc render-pipeline-descriptor "colorAttachments" :pointer)
		    "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc color-attachment "setPixelFormat:" :int pixel-format)))


(defun make-vertex-descriptor ()
  (ns::new "MTLVertexDescriptor"))

(defun set-vertex-descriptor-attribute (vertex-descriptor index format offset buffer-index)
  (let* ((attribute (ns:objc (ns:objc vertex-descriptor "attributes" :pointer)
			     "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc attribute "setFormat:" :int format)
    (ns:objc attribute "setOffset:" :int offset)
    (ns:objc attribute "setBufferIndex:" :int buffer-index)))

(defun set-vertex-descriptor-layout (vertex-descriptor index stride step-rate step-function)
  (let* ((layout (ns:objc (ns:objc vertex-descriptor "layouts" :pointer)
			     "objectAtIndexedSubscript:" :int index :pointer)))
    (ns:objc layout "setStride:" :int stride)
    (ns:objc layout "setStepRate:" :int step-rate)
    (ns:objc layout "setStepFunction:" :int step-function)))

(defun make-render-pipeline-state (device render-pipeline-descriptor)
  (ns:objc device "newRenderPipelineStateWithDescriptor:error:"
	   :pointer render-pipeline-descriptor
	   :pointer (cffi:null-pointer)
	   :pointer))


;; MTLTexture
(defun get-texture2d-descriptor (pixel-format width height mipmap)
  (ns:objc "MTLTextureDescriptor" "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
	   :int pixel-format
	   :int width
	   :int height
	   :bool mipmap
	   :pointer))

(defun make-texture (device descriptor)
  (ns:objc device "newTextureWithDescriptor:" :pointer descriptor :pointer))

(defun replace-region (texture region mipmap-level data bpr)
  (ns:objc texture "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
	   (:struct region) region
	   :int mipmap-level
	   :pointer data
	   :int bpr))


;;  Function 
(cffi:defcstruct (clear-color :class %clear-color)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defstruct (clear-color
	    (:constructor make-clear-color (red green blue alpha)))
  red green blue alpha)

(defmethod cffi:translate-from-foreign (p (type %clear-color))
  (cffi:with-foreign-slots ((red green blue alpha) p (:struct clear-color))
    (make-clear-color red green blue alpha)))

(defmethod cffi:translate-into-foreign-memory (clear-color (type %clear-color) p)
  (cffi:with-foreign-slots ((red green blue alpha) p (:struct clear-color))
    (setf red (coerce (clear-color-red clear-color) 'double-float)
	  green (coerce (clear-color-green clear-color) 'double-float)
	  blue (coerce (clear-color-blue clear-color) 'double-float)
	  alpha (coerce (clear-color-alpha clear-color) 'double-float))))

(defun clear-color (mtk-view red green blue alpha)
  (ns:objc mtk-view "setClearColor:" (:struct clear-color) (make-clear-color red green blue alpha)))

