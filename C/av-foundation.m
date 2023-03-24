#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <Cocoa/Cocoa.h>

// =========================================================================
// AVFoundation Capture 


@interface CaptureDelegate : NSObject <AVCaptureVideoDataOutputSampleBufferDelegate>  {
  CVPixelBufferRef mHead;
}

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

-(CVPixelBufferRef) getPixelBuffer {
  return mHead;
}

-(void) captureOutput: (AVCaptureOutput*) output
didOutputSampleBuffer: (CMSampleBufferRef) buffer
       fromConnection: (AVCaptureConnection*) connection {
  CVPixelBufferRef frame = CMSampleBufferGetImageBuffer(buffer);
  CVPixelBufferRef prev;
  if(frame) {
    CVPixelBufferRetain(frame);
    prev = mHead;
    mHead = frame;
    CVPixelBufferRelease(prev);
  }
}

-(void) dealloc {
  CVPixelBufferRelease(mHead);
  [super dealloc];
}

@end

// =========================================================================
// AVFoundation Player 

enum {
      READY_FN = 0,
      END_FN = 1
};

@interface PlayerManager : NSObject {
  int mID;
  void(*mHandlerFn)(int,int);
}

@property(assign, nonatomic) int ready;
@property(assign, nonatomic) CVPixelBufferRef head;
@property(assign, nonatomic) AVPlayer* player;
@property(assign, nonatomic) AVPlayerItem* playerItem;
@property(assign, nonatomic) AVPlayerItemVideoOutput* output;


-(void)observeValueForKeyPath: (NSString*) keyPath
		     ofObject:(AVPlayerItem*) object
		       change:(NSDictionary*) change
		      context:(void*) context;

@end

@implementation PlayerManager

-(id) initWithID: (int) inID
	    path: (NSString*) path
     requestSize: (NSSize) size
       handlerFn: (void(*)(int,int)) handlerFn {
  self = [super init];
  mID = inID;
  mHandlerFn = handlerFn;
  self.ready = NO;
  self.head = NULL;
  self.player = [[AVPlayer alloc] initWithURL: [NSURL fileURLWithPath: path]];
  self.playerItem = [self.player currentItem];
  NSMutableDictionary* options = [NSMutableDictionary dictionaryWithCapacity: (NSUInteger)10];

  [options setValue: @(kCVPixelFormatType_32ARGB) forKey: (NSString*)kCVPixelBufferPixelFormatTypeKey];
  [options setValue: @(YES) forKey: (NSString*)kCVPixelBufferOpenGLCompatibilityKey];

  if(size.width > 0 && size.height > 0) {
    [options setValue: [NSNumber numberWithDouble: size.width] forKey: (NSString*)kCVPixelBufferWidthKey];
    [options setValue: [NSNumber numberWithDouble: size.height] forKey: (NSString*)kCVPixelBufferHeightKey];
  }
  
  self.output = [[AVPlayerItemVideoOutput alloc] initWithPixelBufferAttributes: options];
  [self.playerItem addObserver: self
		    forKeyPath: @"status"
		       options: 0
		       context: NULL];
  return self;
}


-(void)observeValueForKeyPath: (NSString*) keyPath
		     ofObject:(AVPlayerItem*) object
		       change:(NSDictionary*) change
		      context:(void*) context {
  if (object.status == AVPlayerStatusReadyToPlay) {
    [object removeObserver: self forKeyPath: @"status"];
    [object addOutput: self.output];
    [[NSNotificationCenter defaultCenter] addObserver: self
					     selector: @selector(playerItemDidReachEnd:)
						 name: AVPlayerItemDidPlayToEndTimeNotification
					       object: object];
    self.ready = YES;
    mHandlerFn(mID, READY_FN);
  }
}

-(void) playerItemDidReachEnd: (NSNotification*) notification {
  mHandlerFn(mID, END_FN);
} 


-(CVPixelBufferRef) getPixelBuffer {
  CMTime presentTime = [self.output itemTimeForHostTime: CACurrentMediaTime()];
  if([self.output hasNewPixelBufferForItemTime: presentTime]) {
    CVPixelBufferRef current;
    CVPixelBufferRef prev;
    current = [self.output copyPixelBufferForItemTime: presentTime
				   itemTimeForDisplay: nil];
    prev = self.head;
    self.head = current;
    CVPixelBufferRelease(prev);
  }
  return self.head;
}

-(void) dealloc {
  CVPixelBufferRelease(self.head);
  [super dealloc];
}

@end
