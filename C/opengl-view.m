#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#import "view.h"
#import <Cocoa/Cocoa.h>
#import <pthread.h>

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now,
				    const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext);


static NSMutableDictionary* sDrawingState = NULL;

@interface LispOpenGLView : NSOpenGLView {
  CGLContextObj mCGLContext;
  CGLPixelFormatObj mCGLPixelFormat;
  CVDisplayLinkRef mLink;
  pthread_t mDisplayLinkThread;
  bool mIsAnimate;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
}

@property(nonatomic) int mID;
@end

@implementation LispOpenGLView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
     pixelFormat: (CGLPixelFormatObj) pixelFormat
       isAnimate: (bool) isAnimate
	  drawFn: (DrawFn) drawFn
	 mouseFn: (MouseFn) mouseFn {
  self = [super initWithFrame: frame
		  pixelFormat: [[NSOpenGLPixelFormat alloc] initWithCGLPixelFormatObj: pixelFormat]];
  if (!sDrawingState) {
    sDrawingState = [[NSMutableDictionary alloc] init];
  }
  self.mID = inID;
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
  mDrawFn(self.mID, INIT, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];

  if(mIsAnimate) {
    sDrawingState[ [NSNumber numberWithInt: self.mID] ] = @1;
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
  mDrawFn(self.mID, RESHAPE, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];
}

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now, const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext) {

  LispOpenGLView* view = (LispOpenGLView*)displayLinkContext;
  view->mDisplayLinkThread = pthread_self();
  int mID = view.mID;
  dispatch_async(dispatch_get_main_queue(),
		 ^{
		   if ([sDrawingState[ [NSNumber numberWithInt: mID] ] intValue]) {
		     [view setNeedsDisplayInRect: [view frame]];
		   }
		 });
  return kCVReturnSuccess;
}

-(void) drawRect: (NSRect) rect {
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  mDrawFn(self.mID, DRAW, mCGLContext, mCGLPixelFormat, rect.size.width, rect.size.height);
  [context flushBuffer];
}

-(void) dealloc {
  if (mIsAnimate) {
    sDrawingState[ [NSNumber numberWithInt: self.mID] ] = @0;
    CVDisplayLinkStop(mLink);
    pthread_join(mDisplayLinkThread, NULL);
    CVDisplayLinkRelease(mLink);
  }
  
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(self.mID, SHUTDOWN, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [super dealloc];
}

-(void) mouseDown:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(self.mID, DOWN, event, point.x, point.y);
}

-(void) mouseDragged:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(self.mID, DRAGGED, event, point.x, point.y);
}

-(void) mouseUp:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(self.mID, UP, event, point.x, point.y);
}

-(void) mouseMoved: (NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(self.mID, MOVED, event, point.x, point.y);
}


-(void) scrollWheel:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mMouseFn(self.mID, SCROLLWHEEL, event, point.x, point.y);
}


@end

