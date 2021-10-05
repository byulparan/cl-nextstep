#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#import "view.h"
#import <Cocoa/Cocoa.h>
#import <pthread.h>
#import <OpenGL/gl.h>


void init_fbo(int framebuffer, int colorbuffer,int depthbuffer, int target, int width, int height) {
  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
  glBindTexture(target, colorbuffer);
  glTexImage2D(target, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindTexture(target, 0);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, target, colorbuffer, 0);	
  glBindRenderbuffer(GL_RENDERBUFFER, depthbuffer);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthbuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void init_mssa_fbo(int framebuffer, int colorbuffer,int depthbuffer,  int width, int height) {
  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);

  glBindRenderbuffer(GL_RENDERBUFFER, colorbuffer);
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, 4, GL_RGB8, width, height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, colorbuffer);
  
  glBindRenderbuffer(GL_RENDERBUFFER, depthbuffer);
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, 4, GL_DEPTH_COMPONENT, width, height);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthbuffer);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void bind_framebuffer(int framebuffer) {
  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
}


void blit_framebuffer(int width, int height) {
  glBlitFramebuffer(0, 0, width, height, 0, 0, width, height, GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT, GL_NEAREST);
}


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
  NSTrackingArea* trackingArea;
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
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
                                        options:opts
                                        owner:self
                                        userInfo:nil];
  [self addTrackingArea: trackingArea];
  
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
  if(trackingArea != nil) {
    [self removeTrackingArea:trackingArea];
    [trackingArea release];
  }
  [super dealloc];
}


-(void) updateTrackingAreas {
  if(trackingArea != nil) {
    [self removeTrackingArea:trackingArea];
    [trackingArea release];
  }
    int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
					       options:opts
						 owner:self
					      userInfo:nil];
  [self addTrackingArea:trackingArea];
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

