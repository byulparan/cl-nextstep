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
  CGLContextObj mCGLContext;
  CGLPixelFormatObj mCGLPixelFormat;
  pthread_t mDisplayLinkThread;
  bool mIsAnimate;
  DrawFn mDrawFn;
  EventFn mEventFn;
  NSTrackingArea* mTrackingArea;
}

@property(readonly, nonatomic) int id;
@property(readonly, nonatomic) CVDisplayLinkRef link;
@end

@implementation LispOpenGLView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
     pixelFormat: (CGLPixelFormatObj) pixelFormat
       isAnimate: (bool) isAnimate
	  drawFn: (DrawFn) drawFn
	 eventFn: (EventFn) eventFn {
  self = [super initWithFrame: frame
		  pixelFormat: [[NSOpenGLPixelFormat alloc] initWithCGLPixelFormatObj: pixelFormat]];
  if (!sDrawingState) {
    sDrawingState = [[NSMutableDictionary alloc] init];
  }
  _id = inID;
  mDisplayLinkThread = NULL;
  mIsAnimate = isAnimate;
  mDrawFn = drawFn;
  mEventFn = eventFn;
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  mTrackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
                                        options:opts
                                        owner:self
                                        userInfo:nil];
  [self addTrackingArea: mTrackingArea];
  
  return self;
}

-(void) prepareOpenGL {
  [super prepareOpenGL];
  mCGLContext =  [[self openGLContext] CGLContextObj];
  mCGLPixelFormat = [[self pixelFormat] CGLPixelFormatObj];

  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(self.id, INIT, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];

  if(mIsAnimate) {
    sDrawingState[ [NSNumber numberWithInt: self.id] ] = @1;
    GLint swapInt = 1;
    [[self openGLContext] setValues:&swapInt forParameter:  NSOpenGLContextParameterSwapInterval];
    CVDisplayLinkCreateWithActiveCGDisplays(&_link);
    CVDisplayLinkSetOutputCallback(self.link, &DisplayLinkCallback ,self);
    CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(self.link, mCGLContext, mCGLPixelFormat);
    CVDisplayLinkStart(self.link);
  }
}

-(void) reshape {
  [super reshape];
  mCGLContext =  [[self openGLContext] CGLContextObj];
  mCGLPixelFormat = [[self pixelFormat] CGLPixelFormatObj];

  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(self.id, RESHAPE, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
  [context flushBuffer];
}

static CVReturn DisplayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp* now, const CVTimeStamp* outputTime, CVOptionFlags flagsIn,
				    CVOptionFlags* flagsOut, void* displayLinkContext) {

  LispOpenGLView* view = (LispOpenGLView*)displayLinkContext;
  view->mDisplayLinkThread = pthread_self();
  int mID = view.id;
  dispatch_async(dispatch_get_main_queue(),
		 ^{
		   if ([sDrawingState[ [NSNumber numberWithInt: view.id] ] intValue]) {
		     [view setNeedsDisplayInRect: [view frame]];
		   }
		 });
  return kCVReturnSuccess;
}

-(void) drawRect: (NSRect) rect {
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect frame = [self bounds];
  mDrawFn(self.id, DRAW, mCGLContext, mCGLPixelFormat, frame.size.width, frame.size.height);
  [context flushBuffer];
}

-(void) dealloc {
  if (mIsAnimate) {
    sDrawingState[ [NSNumber numberWithInt: self.id] ] = @0;
    CVDisplayLinkStop(self.link);
    pthread_join(mDisplayLinkThread, NULL);
    CVDisplayLinkRelease(self.link);
  }
  
  NSOpenGLContext* context = [self openGLContext];
  [context makeCurrentContext];
  NSRect r = [self bounds];
  mDrawFn(self.id, SHUTDOWN, mCGLContext, mCGLPixelFormat, r.size.width, r.size.height);
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



-(BOOL) acceptsFirstResponder {
  return YES;
}

-(void) keyDown:(NSEvent*) event {
  mEventFn(self.id, KEY_DOWN, event, 0, 0);
}


-(void) mouseDown:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(self.id, DOWN, event, point.x, point.y);
}

-(void) mouseDragged:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(self.id, DRAGGED, event, point.x, point.y);
}

-(void) mouseUp:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(self.id, UP, event, point.x, point.y);
}

-(void) mouseMoved: (NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(self.id, MOVED, event, point.x, point.y);
}


-(void) scrollWheel:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(self.id, SCROLLWHEEL, event, point.x, point.y);
}


@end

