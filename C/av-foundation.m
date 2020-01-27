#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <Cocoa/Cocoa.h>

// =========================================================================
// AVFoundation Capture 


@interface CaptureVideoDataOutputDelegate : NSObject <AVCaptureVideoDataOutputSampleBufferDelegate>  {
  CVImageBufferRef mHead;
}

-(CVImageBufferRef) getImageBuffer;

-(void) captureOutput: (AVCaptureOutput*) output
didOutputSampleBuffer: (CMSampleBufferRef) buffer
       fromConnection: (AVCaptureConnection*) connection;

@end

@implementation CaptureVideoDataOutputDelegate

-(CVImageBufferRef) getImageBuffer {
  return mHead;
}

-(void) captureOutput: (AVCaptureOutput*) output
didOutputSampleBuffer: (CMSampleBufferRef) buffer
       fromConnection: (AVCaptureConnection*) connection {
  CVImageBufferRef frame = CMSampleBufferGetImageBuffer(buffer);
  CVImageBufferRef prev;
  CVBufferRetain(frame);
  prev = mHead;
  mHead = frame;
  CVBufferRelease(prev);
}

@end

// =========================================================================
// AVFoundation Player 

@interface PlayerItemDelegate : NSObject {
  int mID;
  void(*mEndFn)(int);
  AVPlayerItemVideoOutput* mOutput;
}

-(void)observeValueForKeyPath: (NSString*) keyPath
		     ofObject:(AVPlayerItem*) object
		       change:(NSDictionary*) change
		      context:(void*) context;

@end

@implementation PlayerItemDelegate

-(id) initWithID: (int) inID
	   endFn: (void(*)(int)) endFn {
  self = [super init];
  mID = inID;
  mEndFn = endFn;
  return self;
}

-(void) playerItemDidReachEnd: (NSNotification*) notification {
  mEndFn(mID);
} 

-(void)observeValueForKeyPath: (NSString*) keyPath
		     ofObject:(AVPlayerItem*) object
		       change:(NSDictionary*) change
		      context:(void*) context {
  if (object.status == AVPlayerStatusReadyToPlay) {
    [object removeObserver: self forKeyPath: @"status"];
    NSDictionary* options = @{
			      (NSString*) kCVPixelBufferPixelFormatTypeKey: @(kCVPixelFormatType_32ARGB),
			      (NSString*) kCVPixelBufferOpenGLCompatibilityKey: @YES
    };
    
    AVPlayerItemVideoOutput* output = [[AVPlayerItemVideoOutput alloc] initWithPixelBufferAttributes: options];
    [output autorelease];
    mOutput = output;
    [object addOutput: output];
    [[NSNotificationCenter defaultCenter] addObserver: self
					     selector: @selector(playerItemDidReachEnd:)
						 name: AVPlayerItemDidPlayToEndTimeNotification
					       object: object];
  }
}

@end
