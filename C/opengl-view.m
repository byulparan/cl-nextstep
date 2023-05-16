#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#import "view.h"
#import <Cocoa/Cocoa.h>
#import <pthread.h>
#import <OpenGL/gl.h>

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now,
				    const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext);

static NSMutableDictionary* sDrawingState = NULL;

@interface LispOpenGLView : NSOpenGLView {
  int mID;
  CGLContextObj mCGLContext;
  CGLPixelFormatObj mCGLPixelFormat;
  CVDisplayLinkRef mLink;
  pthread_t mDisplayLinkThread;
  bool mIsAnimate;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
  NSTrackingArea* mTrackingArea;
}

-(int) getID;
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
  mID = inID;
  mDisplayLinkThread = NULL;
  mIsAnimate = isAnimate;
  mDrawFn = drawFn;
  mMouseFn = mouseFn;
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  mTrackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
                                        options:opts
                                        owner:self
                                        userInfo:nil];
  [self addTrackingArea: mTrackingArea];
  
  return self;
}

-(int) getID {
  return mID;
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
    sDrawingState[ [NSNumber numberWithInt: mID] ] = @1;
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
  int mID = [view getID];
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
  mDrawFn(mID, DRAW, mCGLContext, mCGLPixelFormat, rect.size.width, rect.size.height);
  [context flushBuffer];
}

-(void) dealloc {
  if (mIsAnimate) {
    sDrawingState[ [NSNumber numberWithInt: mID] ] = @0;
    CVDisplayLinkStop(mLink);
    pthread_join(mDisplayLinkThread, NULL);
    CVDisplayLinkRelease(mLink);
  }
  
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(mID, SHUTDOWN, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  if(mTrackingArea != nil) {
    [self removeTrackingArea: mTrackingArea];
    [mTrackingArea release];
  }
  [super dealloc];
}


-(void) updateTrackingAreas {
  if(mTrackingArea != nil) {
    [self removeTrackingArea: mTrackingArea];
    [mTrackingArea release];
  }
    int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
    mTrackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
					       options:opts
						 owner:self
					      userInfo:nil];
  [self addTrackingArea: mTrackingArea];
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

