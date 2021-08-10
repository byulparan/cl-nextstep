
#import <Cocoa/Cocoa.h>

int main(int argc, char** argv) {

  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSApp = [NSApplication sharedApplication];

  NSRect frame = NSMakeRect(0, 800, 800, 600);
  NSWindow* window  = [[[NSWindow alloc] initWithContentRect:frame
						   styleMask:NSWindowStyleMaskTitled |
					 NSWindowStyleMaskClosable
						     backing:NSBackingStoreBuffered
						       defer:NO] autorelease];
  
  NSMenu* menubar = [[NSMenu alloc] initWithTitle: @"MainMenu"];
  NSMenuItem* appMenuItem = [[NSMenuItem new] autorelease];
  [NSApp setMainMenu: menubar];
  [menubar addItem: appMenuItem];
   
  NSMenu* appMenu = [[NSMenu new] autorelease];
  NSMenuItem* quitMenuItem = [[NSMenuItem alloc] initWithTitle: @"sbcl"
							action: @selector(terminate:)
						 keyEquivalent: @"q"];
  [appMenu addItem: quitMenuItem];
  [appMenuItem setSubmenu: appMenu];
  [window makeKeyAndOrderFront:nil];
  [NSApp run];
  [pool release];
  
  return 0;
}


