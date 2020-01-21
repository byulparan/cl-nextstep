#import <Cocoa/Cocoa.h>

static void (*gLispCallback)(int task_id);

void execute_in_event_loop_async(int task_id) {
  dispatch_async(dispatch_get_main_queue(),
		 ^{ gLispCallback(task_id); });
}

void execute_in_event_loop_sync(int task_id) {
  dispatch_sync(dispatch_get_main_queue(),
		^{ gLispCallback(task_id); });
}


void start_event_loop(void(*callback)(int)) {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSApp = [NSApplication sharedApplication];

  id menubar = [[NSMenu new] autorelease];
  [NSApp setMainMenu:menubar];
  
  id appMenuItem = [[NSMenuItem new] autorelease];
  [menubar addItem:appMenuItem];
  id appMenu = [[NSMenu new] autorelease];
  id quitTitle = [@"Quit " stringByAppendingString: [[NSProcessInfo processInfo] processName]];
  id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
						action:@selector(terminate:)
					 keyEquivalent:@"q"] autorelease];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];


  id editMenuItem = [[NSMenuItem new] autorelease];
  [menubar addItem:editMenuItem];
  id editMenu = [[[NSMenu alloc] initWithTitle: @"Edit"] autorelease];
  id closeMenuItem = [[[NSMenuItem alloc] initWithTitle:@"Close"
						 action:@selector(performClose:)
					 keyEquivalent:@"w"] autorelease];
  [editMenu addItem: closeMenuItem];
  id fullscreenMenuItem = [[[NSMenuItem alloc] initWithTitle:@"ToggleFullscreen"
						 action:@selector(toggleFullscreen)
					 keyEquivalent:@"f"] autorelease];
  [editMenu addItem: fullscreenMenuItem];
  
  [editMenuItem setSubmenu: editMenu];

  
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular]; // Launching App on MenuBar
  [NSApp activateIgnoringOtherApps:YES]; // Enable Foreground

  unsigned long long activityOptions =
    NSActivityIdleDisplaySleepDisabled |
    NSActivityIdleSystemSleepDisabled |
    NSActivitySuddenTerminationDisabled |
    NSActivityAutomaticTerminationDisabled |
    NSActivityUserInitiated |
    NSActivityUserInitiatedAllowingIdleSystemSleep |
    NSActivityBackground |
    NSActivityLatencyCritical;
  [[NSProcessInfo processInfo] beginActivityWithOptions: activityOptions
						 reason: @"NONE REASON"];
  gLispCallback = callback;
  
  [NSApp run];
  [pool release];
}
