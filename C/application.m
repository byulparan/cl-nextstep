#import <Cocoa/Cocoa.h>

@interface LispApplication : NSApplication<NSApplicationDelegate> {
  void(*mLispDispatch)(int);
}
@end

@implementation LispApplication

-(void) setLispApplicationDispatch: (void(*)(int)) dispatch {
  mLispDispatch = dispatch;
}

-(void) terminate: (id) sender {
  mLispDispatch(1);
  dispatch_async(dispatch_get_main_queue(), ^{
      [super terminate: sender];
    });
}

-(void) applicationDidFinishLaunching:(NSNotification *)notification {
  mLispDispatch(0);
}

- (void)applicationWillTerminate:(NSNotification *)notification {
  mLispDispatch(2);
}

@end
