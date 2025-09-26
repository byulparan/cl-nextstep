#import <Cocoa/Cocoa.h>

typedef void(*TimerFn)(int);

@interface LispTimer : NSObject {
  int mID;
  TimerFn mTimerFn;
  NSTimer* mTimer;
}
@end

@implementation LispTimer

-(id) initWithID: (int) inID
	 timerFn: (TimerFn) timerFn
   timerInterval: (double) interval
	 repeats: (bool) repeats {
  self = [super init];
  mID = inID;
  mTimerFn = timerFn;
  
  mTimer = [NSTimer timerWithTimeInterval: interval
				   target: self
				 selector: @selector(timerHandle:)
				 userInfo: nil
				  repeats: repeats];
  [[NSRunLoop mainRunLoop] addTimer: mTimer
			    forMode: NSRunLoopCommonModes];

  
  return self;
}


-(void) timerHandle: (NSTimer*)timer {
  mTimerFn(mID);
}

-(void) invalidate {
  [mTimer invalidate];
}



@end


