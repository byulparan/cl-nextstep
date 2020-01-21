#import <Cocoa/Cocoa.h>

@interface LispView : NSView {
  int mID;
  void (*mDrawFn) (int);
}
@end

@implementation LispView
-(id) initWithID: (int) inID X: (int) x Y: (int) y W: (int) w H: (int) h closeFn: (void(*)(int)) inDrawFn {
  NSRect frame = NSMakeRect(x, y, w, h);
  mID = inID;
  mDrawFn = inDrawFn;
  return self;
}

@end
