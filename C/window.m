#import <Cocoa/Cocoa.h>

@interface LispWindow : NSWindow <NSWindowDelegate> {
  int mID;
  NSRect mMemFrame;
  NSWindowStyleMask mMemStyleMask;
  bool mIsFullscreen;
  void (*mCloseFn) (int);
}
@end

static NSMutableArray* gFullscreenWindows = NULL;

@implementation LispWindow

-(id) initWithID: (int) inID X: (int) x Y: (int) y W: (int) w H: (int) h closeFn: (void(*)(int)) inCloseFn {
  NSRect frame = NSMakeRect(x, y, w, h);
  self = [super initWithContentRect: frame
			  styleMask: NSWindowStyleMaskTitled | NSWindowStyleMaskClosable
			    backing: NSBackingStoreBuffered
			      defer: NO];
  if(!gFullscreenWindows) {
    gFullscreenWindows = [[NSMutableArray alloc] init];
  }
  mID = inID;
  mCloseFn = inCloseFn;
  mIsFullscreen = NO;
  return self;
}

-(void)windowWillClose:(NSNotification *)notification {
  if(mIsFullscreen) [self exitFullscreen];
  mCloseFn(mID);
}

-(void) enterFullscreen {
  mMemFrame = self.frame;
  mMemStyleMask = self.styleMask;
  self.styleMask = NSWindowStyleMaskBorderless;
  [self setFrame: self.screen.frame
	 display: YES];
  mIsFullscreen = YES;

  [gFullscreenWindows addObject: self];
  if([gFullscreenWindows count] == 1) {
    NSApp.presentationOptions =  NSApplicationPresentationAutoHideMenuBar;
  }
}

-(void) exitFullscreen {
  [self setFrame: mMemFrame display: YES];
  self.styleMask = mMemStyleMask;
  mIsFullscreen = NO;
  [gFullscreenWindows removeObject: self];
  if([gFullscreenWindows count] == 0) {
    NSApp.presentationOptions =  NSApplicationPresentationDefault;
  }
}

-(void) toggleFullscreen {
  if(mIsFullscreen) {
    [self exitFullscreen];
  } else {
    [self enterFullscreen];
  }
}

@end

// ============================================================
// export C
// ============================================================

LispWindow* make_window(int inID, char* title, int x, int y, int w, int h, void(*inCloseFn)(int)) {
  LispWindow* window = [[LispWindow alloc] initWithID: inID
						    X: x
						    Y: y
						    W: w
						    H: h
					      closeFn: inCloseFn];
  window.title = [NSString stringWithUTF8String: title];
  [window setDelegate: window];
  return window;
}

void window_show(NSWindow* window) {
  [window makeKeyAndOrderFront: nil];
}


