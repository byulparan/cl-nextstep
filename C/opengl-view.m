#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#import <Cocoa/Cocoa.h>
#import <pthread.h>

typedef void(*DrawFn)(int,int,void*,void*,int,int);

enum {
      INIT = 0,
      DRAW,
      RESHAPE,
      SHUTDOWN
};

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now,
				    const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext);


@interface LispOpenGLView : NSOpenGLView {
  int mID;
  CGLContextObj mCGLContext;
  CGLPixelFormatObj mCGLPixelFormat;
  DrawFn mDrawFn;
  CVDisplayLinkRef mLink;
  pthread_t mDisplayLinkThread;
  bool mIsAnimate;
}

-(id) initWithID: (int) inID
	   Frame: (NSRect) rect
     pixelFormat: (NSOpenGLPixelFormat*) pixelFormat
       isAnimate: (bool) isAnimate
	  drawFn: (DrawFn) drawFn;
@end

@implementation LispOpenGLView

-(id) initWithID: (int) inID
	   Frame: (NSRect) rect
     pixelFormat: (NSOpenGLPixelFormat*) pixelFormat
       isAnimate: (bool) isAnimate
	  drawFn: (DrawFn) drawFn {
  self = [super initWithFrame: rect pixelFormat: pixelFormat];
  mID = inID;
  mDrawFn = drawFn;
  mDisplayLinkThread = NULL;
  mIsAnimate = isAnimate;
  return self;
}

// - (BOOL)acceptsFirstMouse:(NSEvent *)event {
//   return YES;
// }

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
  NSLog(@"Dealloc object");
}
@end


// ============================================================
// export C
// ============================================================

void* make_opengl_view(int inID, unsigned int* _attributes, bool isAnimate,int x, int y, int w, int h,
		       DrawFn drawFn) {
  NSOpenGLPixelFormat *pixelFormat = [[NSOpenGLPixelFormat alloc]
				       initWithAttributes:(NSOpenGLPixelFormatAttribute*)_attributes];
  LispOpenGLView  *view = [[LispOpenGLView alloc] initWithID: inID
						       Frame: NSMakeRect(x,y,w,h)
						 pixelFormat: pixelFormat
						   isAnimate: isAnimate
						      drawFn: drawFn];
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
