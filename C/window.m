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

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
	 closeFn: (void(*)(int)) closeFn {
  self = [super initWithContentRect: frame
			  styleMask: NSWindowStyleMaskTitled | NSWindowStyleMaskClosable
			    backing: NSBackingStoreBuffered
			      defer: NO];
  if(!gFullscreenWindows) {
    gFullscreenWindows = [[NSMutableArray alloc] init];
  }
  mID = inID;
  mCloseFn = closeFn;
  mIsFullscreen = NO;
  return self;
}

-(void)windowWillClose:(NSNotification *)notification {
  if(mIsFullscreen) [self exitFullscreen];
  mCloseFn(mID);
}


-(void) detectChangedViewSize: (NSNotification*) notification {
  [[NSNotificationCenter defaultCenter] removeObserver: self
					    name: NSViewFrameDidChangeNotification
					  object: [notification object]];
  NSRect frame = [self frame];
  NSRect contentRect = [self frameRectForContentRect: [[notification object] frame]];
  
  NSRect newRect = NSMakeRect(frame.origin.x, frame.origin.y - (contentRect.size.height - frame.size.height),
			      contentRect.size.width, contentRect.size.height);
  
  [self setFrame: newRect display: YES];

  [[NSNotificationCenter defaultCenter] addObserver: self
				     selector: @selector(detectChangedViewSize:)
					 name: NSViewFrameDidChangeNotification
				       object: [notification object]];
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

