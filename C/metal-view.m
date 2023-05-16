#import "view.h"
#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>


@interface LispMTKView<MTKViewDelegate> : MTKView {
  int mID;
  DrawFn mDrawFn;
  MouseFn mMouseFn;
}

@end

@implementation LispMTKView

-(id) initWithFrame:(CGRect) inFrameRect
	     device:(id<MTLDevice>) inDevice
		 id:(int)inID
	     drawFn:(DrawFn) drawFn
	    mouseFn:(MouseFn) mouseFn {
  self = [super initWithFrame: inFrameRect
		       device: inDevice];
  mID = inID;
  mDrawFn = drawFn;
  mMouseFn = mouseFn;
  return self;
}

-(void) mtkView:(MTKView *)view drawableSizeWillChange:(CGSize)size {
  mDrawFn(mID, RESHAPE, NULL, NULL, size.width, size.height);
}

-(void) drawInMTKView:(MTKView *)view {
  mDrawFn(mID, DRAW, NULL, NULL, view.bounds.size.width, view.bounds.size.height);
}

-(void) dealloc {
  mDrawFn(mID, SHUTDOWN, NULL, NULL, self.bounds.size.width, self.bounds.size.height);
  [super dealloc];
}

@end

