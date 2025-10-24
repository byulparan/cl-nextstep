#import <Cocoa/Cocoa.h>

@interface LispApplication : NSApplication<NSApplicationDelegate> {
  void(*mLispDelegateCallback)(int);
  void(*mLispUserActionCallback)(id);
}
@end

@implementation LispApplication

-(void) setLispDelegateCallback: (void(*)(int)) delegateCallback {
  mLispDelegateCallback = delegateCallback;
}

-(void) setLispUserActionCallback: (void(*)(id)) userActionCallback {
  mLispUserActionCallback = userActionCallback;
}

-(void) terminate: (id) sender {
  dispatch_async(dispatch_get_main_queue(), ^{
      [super terminate: sender];
    });
}

-(void) applicationDidFinishLaunching:(NSNotification *)notification {
  mLispDelegateCallback(0);
}

-(void)applicationWillTerminate:(NSNotification *)notification {
  mLispDelegateCallback(2);
}

-(BOOL)applicationSupportsSecureRestorableState:(NSApplication *)app {
  return YES;
}

-(IBAction) lispUserAction:(id)sender {
  mLispUserActionCallback(sender);
}


@end
