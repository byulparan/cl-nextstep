#import <Cocoa/Cocoa.h>

static void (*gLispCallback)(int task_id);

void execute_in_event_loop(int task_id) {
   dispatch_async(dispatch_get_main_queue(),
		  ^{ gLispCallback(task_id); });
}

void start_event_loop(void(*callback)(int)) {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSApp = [NSApplication sharedApplication];

  id menubar = [[NSMenu new] autorelease];
  id appMenuItem = [[NSMenuItem new] autorelease];
  [menubar addItem:appMenuItem];
  [NSApp setMainMenu:menubar];
  id appMenu = [[NSMenu new] autorelease];
  id appName = [[NSProcessInfo processInfo] processName];
  id quitTitle = [@"Quit " stringByAppendingString:appName];
  id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
						action:@selector(terminate:)
					 keyEquivalent:@"q"] autorelease];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];
  
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
