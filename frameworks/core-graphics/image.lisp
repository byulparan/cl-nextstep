(in-package :cg)


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

(cffi:defcfun ("CGImageRetain" image-retain) :pointer
  (image :pointer))

(cffi:defcfun ("CGImageRelease" image-release) :void
  (image :pointer))

(cffi:defcfun ("cg_get_cgimage_from_screen" make-image-from-screen) :pointer
  (rect (:struct rect)))
