#import "view.h"
#import <Cocoa/Cocoa.h>

@interface LispView : NSView {
  int mID;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
  NSTrackingArea* trackingArea;
}
-(int) getID;
@end

@implementation LispView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
	  drawFn: (DrawFn) drawFn
	 mouseFn: (MouseFn) mouseFn 
{
  self = [super initWithFrame: frame];
  mID = inID;
  mDrawFn = drawFn;
  mMouseFn = mouseFn;
  mDrawFn(mID, INIT, NULL, NULL, frame.size.width, frame.size.height);
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
                                        options:opts
                                        owner:self
                                        userInfo:nil];
  [self addTrackingArea: trackingArea];
  return self;
}

-(int) getID {
  return mID;
}

-(void) drawRect:(NSRect) frame {
  mDrawFn(mID, DRAW, NULL, NULL, frame.size.width, frame.size.height);
}

-(void) dealloc {
  NSRect frame = [self bounds];
  mDrawFn(mID, SHUTDOWN, NULL, NULL, frame.size.width, frame.size.height);
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
