#import "view.h"
#import <Cocoa/Cocoa.h>

@interface LispView : NSView {
  int mID;
  DrawFn mDrawFn;
  EventFn mEventFn;
  NSTrackingArea* trackingArea;
}
-(int) getID;
@end

@implementation LispView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
	  drawFn: (DrawFn) drawFn
	 eventFn: (EventFn) eventFn 
{
  self = [super initWithFrame: frame];
  mID = inID;
  mDrawFn = drawFn;
  mEventFn = eventFn;
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


-(BOOL) acceptsFirstResponder {
  return YES;
}

-(void) keyDown:(NSEvent*) event {
  mEventFn(mID, KEY_DOWN, event, 0, 0);
}


-(void) mouseDown:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(mID, DOWN, event, point.x, point.y);
}

-(void) mouseDragged:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(mID, DRAGGED, event, point.x, point.y);
}

-(void) mouseUp:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(mID, UP, event, point.x, point.y);
}

-(void) mouseMoved: (NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(mID, MOVED, event, point.x, point.y);
}


-(void) scrollWheel:(NSEvent*) event {
  NSPoint point = [self convertPoint: [event locationInWindow]
			    fromView: nil];
  mEventFn(mID, SCROLLWHEEL, event, point.x, point.y);
}

@end
