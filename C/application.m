#import <Cocoa/Cocoa.h>

static void (*gLispDispatch)(int task_id);


@interface LispDelegate : NSObject <NSApplicationDelegate> {
  void(*mLispDispatch)(int);
}
@end

@implementation LispDelegate

-(id) initWithDispatch: (void(*)(int)) dispatch {
  self = [super init];
  mLispDispatch = dispatch;
  return self;
}

-(void) applicationDidFinishLaunching:(NSNotification *)notification {
  mLispDispatch(0);
}

-(void) applicationWillTerminate:(NSNotification *)notification {
  mLispDispatch(1);
}

@end
