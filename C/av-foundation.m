#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <Cocoa/Cocoa.h>

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


