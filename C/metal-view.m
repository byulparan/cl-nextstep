#import "view.h"
#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>


@interface LispMTKView<MTKViewDelegate> : MTKView {
  DrawFn mDrawFn;
  EventFn mEventFn;
}
@property(readonly, nonatomic) int id;
@end

@implementation LispMTKView

-(id) initWithFrame:(CGRect) inFrameRect
	     device:(id<MTLDevice>) inDevice
		 id:(int)inID
	     drawFn:(DrawFn) drawFn
	    eventFn:(EventFn) eventFn {
  self = [super initWithFrame: inFrameRect
		       device: inDevice];
  _id = inID;
  mDrawFn = drawFn;
  mEventFn = eventFn;
  return self;
}

-(void) mtkView:(MTKView *)view drawableSizeWillChange:(CGSize)size {
  mDrawFn(self.id, RESHAPE, NULL, NULL, size.width, size.height);
}

-(void) drawInMTKView:(MTKView *)view {
  mDrawFn(self.id, DRAW, NULL, NULL, view.bounds.size.width, view.bounds.size.height);
}

-(void) dealloc {
  mDrawFn(self.id, SHUTDOWN, NULL, NULL, self.bounds.size.width, self.bounds.size.height);
  [super dealloc];
}

@end

