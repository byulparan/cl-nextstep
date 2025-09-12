#import "view.h"
#import <Cocoa/Cocoa.h>

@interface LispView : NSView {
  DrawFn mDrawFn;
  EventFn mEventFn;
  NSTrackingArea* trackingArea;
}

@property(readonly, nonatomic) int id;

@end

@implementation LispView

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
	  drawFn: (DrawFn) drawFn
	 eventFn: (EventFn) eventFn 
{
  self = [super initWithFrame: frame];
  _id = inID;
  mDrawFn = drawFn;
  mEventFn = eventFn;
  mDrawFn(self.id, INIT, NULL, NULL, frame.size.width, frame.size.height);
  int opts = (NSTrackingActiveAlways | NSTrackingMouseEnteredAndExited | NSTrackingMouseMoved | NSTrackingInVisibleRect);
  trackingArea = [ [NSTrackingArea alloc] initWithRect:[self bounds]
                                        options:opts
                                        owner:self
                                        userInfo:nil];
  [self addTrackingArea: trackingArea];
  return self;
}

-(void) drawRect:(NSRect) dirtyRect {
  NSRect frame = [self bounds];
  mDrawFn(self.id, DRAW, NULL, NULL, frame.size.width, frame.size.height);
}

-(void) dealloc {
  NSRect frame = [self bounds];
  mDrawFn(self.id, SHUTDOWN, NULL, NULL, frame.size.width, frame.size.height);
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
