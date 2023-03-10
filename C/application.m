#import <Cocoa/Cocoa.h>

@interface LispApplication : NSApplication<NSApplicationDelegate> {
  void(*mLispDelegateCallback)(int);
}
@end

@implementation LispApplication

-(void) setLispDelegateCallback: (void(*)(int)) delegateCallback {
  mLispDelegateCallback = delegateCallback;
}

-(void) terminate: (id) sender {
  dispatch_async(dispatch_get_main_queue(), ^{
      [super terminate: sender];
    });
}

-(void) applicationDidFinishLaunching:(NSNotification *)notification {
  mLispDelegateCallback(0);
}

- (void)applicationWillTerminate:(NSNotification *)notification {
  mLispDelegateCallback(2);
}

@end
