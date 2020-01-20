#import <Cocoa/Cocoa.h>

@interface LispWindow : NSWindow <NSWindowDelegate> {
  int mID;
  void (*mCloseFn) (int);
}

@end

@implementation LispWindow
-(id) initWithID: (int) inID X: (int) x Y: (int) y W: (int) w H: (int) h closeFn: (void(*)(int)) inCloseFn {
  NSRect frame = NSMakeRect(x, y, w, h);
  self = [super initWithContentRect: frame
			  styleMask: NSWindowStyleMaskTitled | NSWindowStyleMaskClosable
			    backing: NSBackingStoreBuffered
			      defer: NO];
  mID = inID;
  mCloseFn = inCloseFn;
  return self;
}

- (void)windowWillClose:(NSNotification *)notification {
  mCloseFn(mID);
}

@end


LispWindow* make_window(int inID, int x, int y, int w, int h, void(*inCloseFn)(int)) {
  LispWindow* window = [[LispWindow alloc] initWithID: inID
						    X: x
						    Y: y
						    W: w
						    H: h
					      closeFn: inCloseFn];
  [window setDelegate: window];
  return window;
}

void window_show(NSWindow* window) {
  [window makeKeyAndOrderFront: nil];
}


