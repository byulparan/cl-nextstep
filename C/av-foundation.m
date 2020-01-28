#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <Cocoa/Cocoa.h>

// =========================================================================
// AVFoundation Capture 


@interface CaptureDelegate : NSObject <AVCaptureVideoDataOutputSampleBufferDelegate>  {
  CVImageBufferRef mHead;
}

-(CVImageBufferRef) getImageBuffer;

-(void) captureOutput: (AVCaptureOutput*) output
didOutputSampleBuffer: (CMSampleBufferRef) buffer
       fromConnection: (AVCaptureConnection*) connection;

@end

@implementation CaptureDelegate

-(id) init {
  self = [super init];
  mHead = NULL;
  return self;
}

-(CVImageBufferRef) getImageBuffer {
  return mHead;
}

-(void) captureOutput: (AVCaptureOutput*) output
didOutputSampleBuffer: (CMSampleBufferRef) buffer
       fromConnection: (AVCaptureConnection*) connection {
  CVImageBufferRef frame = CMSampleBufferGetImageBuffer(buffer);
  CVImageBufferRef prev;
  if(frame) {
    CVBufferRetain(frame);
    prev = mHead;
    mHead = frame;
    CVBufferRelease(prev);
  }
}

-(void) dealloc {
  CVBufferRelease(mHead);
  [super dealloc];
}

@end

// =========================================================================
// AVFoundation Player 

@interface PlayerDelegate : NSObject {
  int mID;
  void(*mEndFn)(int);
  CVImageBufferRef mHead;
  AVPlayerItemVideoOutput* mOutput;
}

-(void)observeValueForKeyPath: (NSString*) keyPath
		     ofObject:(AVPlayerItem*) object
		       change:(NSDictionary*) change
		      context:(void*) context;

@end

@implementation PlayerDelegate

-(id) initWithID: (int) inID
	   endFn: (void(*)(int)) endFn {
  self = [super init];
  mID = inID;
  mEndFn = endFn;
  mHead = NULL;
  mOutput = NULL;
  return self;
}

-(void) playerItemDidReachEnd: (NSNotification*) notification {
  mEndFn(mID);
} 


-(CVImageBufferRef) getImageBuffer {
  CMTime presentTime = [mOutput itemTimeForHostTime: CACurrentMediaTime()];
  if([mOutput hasNewPixelBufferForItemTime: presentTime]) {
    CVPixelBufferRef current;
    CVPixelBufferRef prev;
    current = [mOutput copyPixelBufferForItemTime: presentTime
			       itemTimeForDisplay: nil];
    prev = mHead;
    mHead = current;
    CVBufferRelease(prev);
  }
  return mHead;
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

-(void) dealloc {
  CVBufferRelease(mHead);
  [super dealloc];
}

@end
