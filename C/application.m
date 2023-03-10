#import <Cocoa/Cocoa.h>

@interface LispApplication : NSApplication<NSApplicationDelegate> {
  void(*mLispDelegateCallback)(int);
  void(*mLispWidgetCallback)(id);
}
@end

@implementation LispApplication

-(void) setLispDelegateCallback: (void(*)(int)) delegateCallback {
  mLispDelegateCallback = delegateCallback;
}

-(void) setLispWidgetCallback: (void(*)(id)) widgetCallback {
  mLispWidgetCallback = widgetCallback;
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

-(IBAction) lispWidgetHandle:(id)sender {
  mLispWidgetCallback(sender);
}

@end
