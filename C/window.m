#import <Cocoa/Cocoa.h>

enum {
  WINDOW_CLOSE = 0
};


@interface LispWindow : NSWindow <NSWindowDelegate> {
  int mID;
  NSString* mMemTitle;
  NSRect mMemFrame;
  bool mMemHasShadow;
  NSWindowStyleMask mMemStyleMask;
  void (*mHandleFn) (int, int);
}
@property(assign, nonatomic) bool isFullscreen;
@end

static NSMutableArray* gFullscreenWindows = NULL;

@implementation LispWindow

-(id) initWithID: (int) inID
	   frame: (NSRect) frame
       styleMask: (int) styleMask
	handleFn: (void(*)(int,int)) handleFn {
  self = [super initWithContentRect: frame
			  styleMask: styleMask
			    backing: NSBackingStoreBuffered
			      defer: NO];
  if(!gFullscreenWindows) {
    gFullscreenWindows = [[NSMutableArray alloc] init];
  }
  mID = inID;
  mHandleFn = handleFn;
  self.isFullscreen = NO;
  return self;
}

-(void)windowWillClose:(NSNotification *)notification {
  if(self.isFullscreen) [self exitFullscreen];
  mHandleFn(mID, WINDOW_CLOSE);
}


// for AudioUnit view resize
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
  
  mMemHasShadow = self.hasShadow;
  self.hasShadow = NO;
  
  [gFullscreenWindows addObject: self];
  if([gFullscreenWindows count] == 1) {
    // [NSMenu setMenuBarVisible: NO];
    NSApp.presentationOptions =  NSApplicationPresentationAutoHideMenuBar;
  }
}

-(void) exitFullscreen {
  self.styleMask = mMemStyleMask;
  [self setFrame: mMemFrame display: YES];
  self.title = mMemTitle;
  self.isFullscreen = NO;
  self.hasShadow = mMemHasShadow;
  [gFullscreenWindows removeObject: self];
  if([gFullscreenWindows count] == 0) {
    // [NSMenu setMenuBarVisible: YES];
    NSApp.presentationOptions =  NSApplicationPresentationDefault;
  }
}

-(void) toggleFullscreen {
  if(self.isFullscreen) {
    NSResponder* view =  [self firstResponder];
    [self exitFullscreen];
    [self makeFirstResponder: view];
  } else {
    NSResponder* view =  [self firstResponder];
    [self enterFullscreen];
    [self makeFirstResponder: view];
  }
}

@end

