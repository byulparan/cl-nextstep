
(in-package #:cl-nextstep)


(defclass wk-webview ()
  ((cocoa-ref :accessor cocoa-ref)))

(defmethod initialize-instance :after ((self wk-webview) &key (x 0) (y 0) (w 400) (h 200) (url ""))
  (let* ((config (ns:objc (ns:alloc "WKWebViewConfiguration") "init" :pointer))
	 (cocoa-ref (with-sb-alien-rect (rect (rect x y w h))
		      (sb-alien:alien-funcall
		       (sb-alien:extern-alien "objc_msgSend" (sb-alien:function sb-alien:system-area-pointer
										sb-alien:system-area-pointer
										sb-alien:system-area-pointer
										(sb-alien:struct rect)
										sb-alien:system-area-pointer))
		       (ns:alloc "WKWebView")
		       (sel "initWithFrame:configuration:")
		       rect
		       config))))
    (setf (cocoa-ref self) cocoa-ref)
    (setf (url self) url)))


(defun reload (wk-webview)
  (objc wk-webview "reload"))

(defun url (wk-webview)
  (ns-string-to-lisp 
   (objc (objc wk-webview "URL" :pointer)
	 "absoluteString" :pointer)))

(defun (setf url) (url wk-webview)
  (objc wk-webview "loadRequest:" :pointer
	(objc "NSURLRequest" "requestWithURL:"
	      :pointer (objc "NSURL" "URLWithString:" :pointer (make-ns-string url)
						      :pointer)
	      :pointer)))

