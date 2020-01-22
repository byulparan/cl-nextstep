#import "view.h"
#import <Cocoa/Cocoa.h>

@interface LispView : NSView {
  int mID;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
}
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
  return self;
}

-(void) drawRect:(NSRect) frame {
  mDrawFn(mID, DRAW, NULL, NULL, frame.size.width, frame.size.height);
}

-(void) dealloc {
  NSRect frame = [self bounds];
  mDrawFn(mID, SHUTDOWN, NULL, NULL, frame.size.width, frame.size.height);
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

LispView* make_view(int inID, int x, int y, int w, int h,
		    DrawFn drawFn, MouseFn mouseFn) {
  LispView* view = [[LispView alloc] initWithID: inID
					  frame: NSMakeRect(x, y, w, h)
					 drawFn: drawFn
					mouseFn: mouseFn];
  return view;
}
