(in-package :mtl)


;; Type
(cffi:defcstruct (origin :class %origin)
  (x :unsigned-long)
  (y :unsigned-long)
  (z :unsigned-long))

(defstruct (origin 
	    (:constructor origin (x y z)))
  x y z)

(defmethod cffi:translate-from-foreign (p (type %origin))
  (cffi:with-foreign-slots ((x y z) p (:struct origin))
    (origin x y z)))

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
	    (:constructor size (width height depth)))
  width height depth)

(defmethod cffi:translate-from-foreign (p (type %size))
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (size width height depth)))

(defmethod cffi:translate-into-foreign-memory (size (type %size) p)
  (cffi:with-foreign-slots ((width height depth) p (:struct size))
    (setf width (floor (size-width size))
	  height (floor (size-height size))
	  depth (floor (msize-depth size)))))


(cffi:defcstruct (region :class %region)
  (origin (:struct origin))
  (size (:struct size)))

(defstruct (region
	    (:constructor region (x y z width height depth)))
  x y z width height depth)

(defmethod cffi:translate-from-foreign (p (type %region))
  (cffi:with-foreign-slots ((origin size) p (:struct region))
    (region (origin-x origin)
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




(cffi:defcstruct (viewport :class %viewport)
  (x :double)
  (y :double)
  (width :double)
  (height :double)
  (near :double)
  (far :double))

(defstruct (viewport
	    (:constructor viewport (x y width height near far)))
  x y width height near far)

(defmethod cffi:translate-from-foreign (p (type %viewport))
  (cffi:with-foreign-slots ((x y width height near far) p (:struct viewport))
    (viewport x y width height near far)))

(defmethod cffi:translate-into-foreign-memory (viewport (type %viewport) p)
  (cffi:with-foreign-slots ((x y width height near far) p (:struct viewport))
    (setf x (coerce (viewport-x viewport) 'double-float)
	  y (coerce (viewport-y viewport) 'double-float)
	  width (coerce (viewport-width viewport) 'double-float)
	  height (coerce (viewport-height viewport) 'double-float)
	  near (coerce (viewport-near viewport) 'double-float)
	  far (coerce (viewport-far viewport) 'double-float))))




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
(defun set-viewport (command-encoder viewport)
  (ns:objc command-encoder "setViewport:" (:struct viewport) viewport))

(defun set-render-pipeline-state (command-encoder pipeline-state)
  (ns:objc command-encoder "setRenderPipelineState:" :pointer pipeline-state))

(defun set-depth-stencil-state (command-encoder depth-stencil-state)
  (ns:objc command-encoder "setDepthStencilState:" :pointer depth-stencil-state))

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

(defun set-depth-attachment-pixel-format (render-pipeline-descriptor pixel-format)
  (ns:objc render-pipeline-descriptor "setDepthAttachmentPixelFormat:" :unsigned-int pixel-format))


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


(defun make-depth-stencil-descriptor ()
  (ns:new "MTLDepthStencilDescriptor"))

(defun set-depth-compare-function (depth-stencil-descriptor compare)
  (ns:objc depth-stencil-descriptor "setDepthCompareFunction:" :unsigned-int compare))

(defun set-depth-write-enabled (depth-stencil-descriptor enabled)
  (ns:objc depth-stencil-descriptor "setDepthWriteEnabled:" :bool enabled))

(defun make-depth-stencil-state (device depth-stencil-descriptor)
  (ns:objc device "newDepthStencilStateWithDescriptor:" :pointer depth-stencil-descriptor :pointer))


;; MTLBuffer
(defun make-buffer (device data length &optional (options +resource-cpu-cache-mode-default-cache+))
  (ns:objc device "newBufferWithBytes:length:options:"
	   :pointer data
	   :int length
	   :int options
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



;; ================================================================================
;; constants
;; ================================================================================

(defmacro define-constant (name value)
  `(progn
     (defconstant ,name ,value)
     (export ',name)))



;; primitive-type ================================================================================
(define-constant +primitive-type-point+ 0)
(define-constant +primitive-type-line+ 1)
(define-constant +primitive-type-line-strip+ 2)
(define-constant +primitive-type-triangle+ 3)
(define-constant +primitive-type-triangle-strip+ 4)

;; ResourceOptions ================================================================================
(define-constant +resource-cpu-cache-mode-default-cache+ 0)
(define-constant +resource-cpu-cache-mode-write-combined+ 1)
(define-constant +resource-storage-mode-shared+ 0)
(define-constant +resource-storage-mode-managed+ 16)
(define-constant +resource-storage-mode-private+ 32)
(define-constant +resource-storage-mode-memoryless+ 48)

;; vertex-format ================================================================================
(define-constant +vertex-format-invalid+ 0)

(define-constant +vertex-format-uchar+ 45)
(define-constant +vertex-format-uchar2+ 1)
(define-constant +vertex-format-uchar3+ 2)
(define-constant +vertex-format-uchar4+ 3)

(define-constant +vertex-format-char+ 46)
(define-constant +vertex-format-char2+ 4)
(define-constant +vertex-format-char3+ 5)
(define-constant +vertex-format-char4+ 6)

(define-constant +vertex-format-ushort+ 49)
(define-constant +vertex-format-ushort2+ 13)
(define-constant +vertex-format-ushort3+ 14)
(define-constant +vertex-format-ushort4+ 15)

(define-constant +vertex-format-short+ 50)
(define-constant +vertex-format-short2+ 16)
(define-constant +vertex-format-short3+ 17)
(define-constant +vertex-format-short4+ 18)

(define-constant +vertex-format-short+ 50)
(define-constant +vertex-format-short2+ 16)
(define-constant +vertex-format-short3+ 17)
(define-constant +vertex-format-short4+ 18)

(define-constant +vertex-format-half+ 53)
(define-constant +vertex-format-half2+ 25)
(define-constant +vertex-format-half3+ 26)
(define-constant +vertex-format-half4+ 27)

(define-constant +vertex-format-float+ 28)
(define-constant +vertex-format-float2+ 29)
(define-constant +vertex-format-float3+ 30)
(define-constant +vertex-format-float4+ 31)

(define-constant +vertex-format-uint+ 36)
(define-constant +vertex-format-uint2+ 37)
(define-constant +vertex-format-uint3+ 38)
(define-constant +vertex-format-uint4+ 39)

(define-constant +vertex-format-int+ 32)
(define-constant +vertex-format-int2+ 33)
(define-constant +vertex-format-int3+ 34)
(define-constant +vertex-format-int4+ 35)


;; vertex- ================================================================================
(define-constant +vertex-step-function-constant+ 0)
(define-constant +vertex-step-function-per-vertex+ 1)
(define-constant +vertex-step-function-per-instance+ 2)
(define-constant +vertex-step-function-per-patch+ 3)
(define-constant +vertex-step-function-per-patch-control-point+ 4)




;; compare-function ================================================================================
(define-constant +compare-function-never+ 0)
(define-constant +compare-function-less+ 1)
(define-constant +compare-function-equal+ 2)
(define-constant +compare-function-less-equal+ 3)
(define-constant +compare-function-greater+ 4)
(define-constant +compare-function-not-equal+ 5)
(define-constant +compare-function-greater-equal+ 6)


;; PixelFormat ================================================================================
;; Ordinary 8-Bit Pixel Formats
(define-constant +pixel-format-a8-unorm+ 1)
(define-constant +pixel-format-r8-unorm+ 10)
(define-constant +pixel-format-r8-unorm-srgb+ 11)
(define-constant +pixel-format-r8-snorm+ 12)
(define-constant +pixel-format-r8-uint+ 13)
(define-constant +pixel-format-r8-sint+ 14)

;; Ordinary 16-Bit Pixel Formats
(define-constant +pixel-format-r16-unorm+ 20)
(define-constant +pixel-format-r16-snorm+ 22)
(define-constant +pixel-format-r16-uint+ 23)
(define-constant +pixel-format-r16-sint+ 24)
(define-constant +pixel-format-r16-float+ 25)
(define-constant +pixel-format-rg8-unorm+ 30)
(define-constant +pixel-format-rg8-unorm-srgb+ 31)
(define-constant +pixel-format-rg8-snorm+ 32)
(define-constant +pixel-format-rg8-uint+ 33)
(define-constant +pixel-format-rg8-sint+ 34)

;; Packed 16-Bit Pixel Formats
(define-constant +pixel-format-b5g6r5-unorm+ 40)
(define-constant +pixel-format-a1bgr5-unorm+ 41)
(define-constant +pixel-format-abgr4-unorm+ 42)
(define-constant +pixel-format-bgr5a1-unorm+ 43)


;; Ordinary 32-Bit Pixel Formats
(define-constant +pixel-format-r32-uint+ 53)
(define-constant +pixel-format-r32-sint+ 54)
(define-constant +pixel-format-r32-float+ 55)
(define-constant +pixel-format-rg16-unorm+ 60)
(define-constant +pixel-format-rg16-snorm+ 62)
(define-constant +pixel-format-rg16-uint+ 63)
(define-constant +pixel-format-rg16-sint+ 64)
(define-constant +pixel-format-rg16-float+ 65)
(define-constant +pixel-format-rgba8-unorm+ 70)
(define-constant +pixel-format-rgba8-unorm-srgb+ 71)
(define-constant +pixel-format-rgba8-snorm+ 72)
(define-constant +pixel-format-rgba8-uint+ 73)
(define-constant +pixel-format-rgba8-sint+ 74)
(define-constant +pixel-format-bgra8-unorm+ 80)
(define-constant +pixel-format-bgra8-unorm-srgb+ 81)


;; Depth and Stencil Pixel Formats
(define-constant +pixel-format-depth16-unorm+ 250)
(define-constant +pixel-format-depth32-float+ 252)
(define-constant +pixel-format-stencil8+ 253)
(define-constant +pixel-format-depth24-unorm-stencil8+ 255)
(define-constant +pixel-format-depth32-float-stencil8+ 260)
(define-constant +pixel-format-x32-stencil8+ 261)
(define-constant +pixel-format-x24-stencil8+ 262)



