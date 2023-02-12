#import <Cocoa/Cocoa.h>

@interface LispWindow : NSWindow <NSWindowDelegate> {
  int mID;
  NSString* mMemTitle;
  NSRect mMemFrame;
  NSWindowStyleMask mMemStyleMask;
  void (*mCloseFn) (int);
}
@property(assign, nonatomic) bool isFullscreen;
@end

static NSMutableArray* gFullscreenWindows = NULL;

@implementation LispWindow

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
       styleMask: (int) styleMask
	 closeFn: (void(*)(int)) closeFn {
  self = [super initWithContentRect: frame
			  styleMask: styleMask
			    backing: NSBackingStoreBuffered
			      defer: NO];
  if(!gFullscreenWindows) {
    gFullscreenWindows = [[NSMutableArray alloc] init];
  }
  mID = inID;
  mCloseFn = closeFn;
  self.isFullscreen = NO;
  return self;
}

-(void)windowWillClose:(NSNotification *)notification {
  if(self.isFullscreen) [self exitFullscreen];
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
  mMemTitle = self.title;
  mMemFrame = self.frame;
  mMemStyleMask = self.styleMask;
  self.styleMask = NSWindowStyleMaskBorderless;
  [self setFrame: self.screen.frame
	 display: YES];
  self.isFullscreen = YES;
  
  [gFullscreenWindows addObject: self];
  if([gFullscreenWindows count] == 1) {
    [NSMenu setMenuBarVisible: NO];
    // NSApp.presentationOptions =  NSApplicationPresentationAutoHideMenuBar;
  }
}

-(void) exitFullscreen {
  self.styleMask = mMemStyleMask;
  [self setFrame: mMemFrame display: YES];
  self.title = mMemTitle;
  self.isFullscreen = NO;
  [gFullscreenWindows removeObject: self];
  if([gFullscreenWindows count] == 0) {
    [NSMenu setMenuBarVisible: YES];
    // NSApp.presentationOptions =  NSApplicationPresentationDefault;
  }
}

-(void) toggleFullscreen {
  if(self.isFullscreen) {
    [self exitFullscreen];
  } else {
    [self enterFullscreen];
  }
}

@end

