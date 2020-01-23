#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#import "view.h"
#import <Cocoa/Cocoa.h>
#import <pthread.h>

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now,
				    const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext);


@interface LispOpenGLView : NSOpenGLView {
  int mID;
  CGLContextObj mCGLContext;
  CGLPixelFormatObj mCGLPixelFormat;
  CVDisplayLinkRef mLink;
  pthread_t mDisplayLinkThread;
  bool mIsAnimate;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
}
@end

@implementation LispOpenGLView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
     pixelFormat: (NSOpenGLPixelFormat*) pixelFormat
       isAnimate: (bool) isAnimate
	  drawFn: (DrawFn) drawFn
	 mouseFn: (MouseFn) mouseFn {
  self = [super initWithFrame: frame pixelFormat: pixelFormat];
  mID = inID;
  mDisplayLinkThread = NULL;
  mIsAnimate = isAnimate;
  mDrawFn = drawFn;
  mMouseFn = mouseFn;
  return self;
}

-(void) prepareOpenGL {
  [super prepareOpenGL];
  mCGLContext =  [[self openGLContext] CGLContextObj];
  mCGLPixelFormat = [[self pixelFormat] CGLPixelFormatObj];

  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(mID, INIT, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];

  if(mIsAnimate) {
    GLint swapInt = 1;
    [[self openGLContext] setValues:&swapInt forParameter:  NSOpenGLContextParameterSwapInterval];
    CVDisplayLinkCreateWithActiveCGDisplays(&mLink);
    CVDisplayLinkSetOutputCallback(mLink, &DisplayLinkCallback ,self);
    CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(mLink, mCGLContext, mCGLPixelFormat);
    CVDisplayLinkStart(mLink);
  }
}

-(void) reshape {
  [super reshape];
  mCGLContext =  [[self openGLContext] CGLContextObj];
  mCGLPixelFormat = [[self pixelFormat] CGLPixelFormatObj];

  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(mID, RESHAPE, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];
}

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now, const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext) {

  LispOpenGLView* view = (LispOpenGLView*)displayLinkContext;
  view->mDisplayLinkThread = pthread_self();
  dispatch_async(dispatch_get_main_queue(),
		 ^{ [view setNeedsDisplayInRect: [view frame]]; });
  return kCVReturnSuccess;
}

-(void) drawRect: (NSRect) rect {
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  mDrawFn(mID, DRAW, mCGLContext, mCGLPixelFormat, rect.size.width, rect.size.height);
  [context flushBuffer];
}

-(void) dealloc {
  if (mIsAnimate) {
    CVDisplayLinkStop(mLink);
    pthread_join(mDisplayLinkThread, NULL);
    CVDisplayLinkRelease(mLink);
  }
  
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(mID, SHUTDOWN, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];
  [super dealloc];
}

-(void) mouseDown:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(mID, DOWN, event, point.x, point.y);
}

-(void) mouseDragged:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(mID, DRAGGED, event, point.x, point.y);
}

-(void) mouseUp:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(mID, UP, event, point.x, point.y);
}

-(void) mouseMoved: (NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(mID, MOVED, event, point.x, point.y);
}


-(void) scrollWheel:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(mID, SCROLLWHEEL, event, point.x, point.y);
}


@end


// ============================================================
// export C
// ============================================================

LispOpenGLView* make_opengl_view(int inID, CGLPixelFormatObj cglPixelFormat, bool isAnimate,int x, int y, int w, int h,
		       DrawFn drawFn, MouseFn mouseFn) {
  NSOpenGLPixelFormat *pixelFormat = [[NSOpenGLPixelFormat alloc]
				       initWithCGLPixelFormatObj: cglPixelFormat];
  LispOpenGLView  *view = [[LispOpenGLView alloc] initWithID: inID
						       frame: NSMakeRect(x,y,w,h)
						 pixelFormat: pixelFormat
						   isAnimate: isAnimate
						      drawFn: drawFn
						     mouseFn: mouseFn];
  return view;
}

// void setCglBestResolution(void* view, int option) {
//   [(GLView*)view setWantsBestResolutionOpenGLSurface: (option == 1) ? YES:NO];
// }

// void convertSizeToBacking(void* view, int w, int h, int* width, int* height) {
//   NSSize sz = [(GLView*)view convertSizeToBacking: NSMakeSize(w, h)];
//   *width = sz.width;
//   *height = sz.height;
// }
