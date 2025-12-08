// ------------------------------------------------------------------------------
// 2025.12.08 byulparan@gmail.com
// 
// 

#import <ScreenCaptureKit/ScreenCaptureKit.h>


@interface ScreenCapturer : NSObject <SCStreamOutput> {
  CVPixelBufferRef mHead;
  SCStream* stream;
}
-(void) startCaptureWindow: (NSString*) windowTitle;
-(void) startCaptureDisplay: (unsigned int) displayID;
-(void) startCaptureFn: (void (*) (SCShareableContent*)) fn;
-(void) processFilter: (SCContentFilter*)filter withConfiguration: (SCStreamConfiguration*) configuration;
-(CVPixelBufferRef) getPixelBuffer;
@end



@implementation ScreenCapturer

// ================================================================================
// getPixelBuffer
// ================================================================================
-(CVPixelBufferRef) getPixelBuffer {
  return mHead;
}


// ================================================================================
// startCaptureFn: user-defined function 
// ================================================================================
-(void) startCaptureFn: (void(*) (SCShareableContent*)) fn {
  [SCShareableContent getShareableContentExcludingDesktopWindows: YES
					     onScreenWindowsOnly:YES
					       completionHandler:^(SCShareableContent * _Nullable shareableContent, NSError * _Nullable error) {
      fn(shareableContent);
    }];
}

// ================================================================================
// capture to window by windowTitle
// ================================================================================
-(void) startCaptureWindow: (NSString*) windowTitle {

  [SCShareableContent getShareableContentExcludingDesktopWindows: YES
					     onScreenWindowsOnly:YES
					       completionHandler:^(SCShareableContent * _Nullable shareableContent, NSError * _Nullable error) {

      SCWindow* targetWindow = NULL;

      for (SCWindow *window in shareableContent.windows) {
	if ([window.title isEqualToString: windowTitle]) {
	  targetWindow = window;
	  break;
	}
      }

      if(!targetWindow) return;

      SCContentFilter *filter = [[SCContentFilter alloc] initWithDesktopIndependentWindow:targetWindow];
      SCStreamConfiguration *configuration = [[SCStreamConfiguration alloc] init];
      configuration.pixelFormat = kCVPixelFormatType_32BGRA;
      configuration.width = targetWindow.frame.size.width;
      configuration.height = targetWindow.frame.size.height;
      configuration.minimumFrameInterval = CMTimeMake(1, 60); 

      [self processFilter: filter withConfiguration: configuration];
      
    }];

}

// ================================================================================
// capture to display by displayID
// ================================================================================
-(void) startCaptureDisplay: (unsigned int)displayID {

  [SCShareableContent getShareableContentExcludingDesktopWindows: YES
					     onScreenWindowsOnly:YES
					       completionHandler:^(SCShareableContent * _Nullable shareableContent, NSError * _Nullable error) {

      SCDisplay* targetDisplay = NULL;

      for (SCDisplay *display in shareableContent.displays) {
	if (display.displayID == displayID) {
	  targetDisplay = display;
	  break;
	}
      }

      if(!targetDisplay) return;

      SCContentFilter *filter = [[SCContentFilter alloc] initWithDisplay: targetDisplay
							excludingWindows: @[]];
      SCStreamConfiguration *configuration = [[SCStreamConfiguration alloc] init];
      configuration.pixelFormat = kCVPixelFormatType_32BGRA;
      configuration.width = targetDisplay.frame.size.width;
      configuration.height = targetDisplay.frame.size.height;
      configuration.minimumFrameInterval = CMTimeMake(1, 60); 

      [self processFilter: filter withConfiguration: configuration];
      
    }];

}


// ================================================================================
//
// ================================================================================
-(void) processFilter: (SCContentFilter*)filter
    withConfiguration: (SCStreamConfiguration*) configuration {
  stream =  [[SCStream alloc] initWithFilter:filter
			       configuration:configuration
				    delegate: nil];
  NSError * addOutputError;
  [stream addStreamOutput: self
		     type: SCStreamOutputTypeScreen
       sampleHandlerQueue: dispatch_get_main_queue()
		    error: &addOutputError];

  [stream startCaptureWithCompletionHandler:^(NSError * _Nullable error) {   }];

  [filter release];
  [configuration release];
}


// ================================================================================
//
// ================================================================================
-(void) dealloc {
  if(stream) {
    [stream stopCaptureWithCompletionHandler: ^(NSError* error) {  }];
    CVPixelBufferRelease(mHead);
    [stream release];
  }
  [super dealloc];
}


// ================================================================================
//
// ================================================================================
-(void)stream: (SCStream *)stream
didOutputSampleBuffer:(CMSampleBufferRef) buffer
       ofType:(SCStreamOutputType)type {

  if (type != SCStreamOutputTypeScreen) {
    return;
  }

  CVImageBufferRef imageBuffer = CMSampleBufferGetImageBuffer(buffer);
  if(imageBuffer && CFGetTypeID(imageBuffer) == CVPixelBufferGetTypeID()) {
    CVPixelBufferRef frame = (CVPixelBufferRef) imageBuffer;
    CVPixelBufferRef prev;
    CVPixelBufferRetain(frame);
    prev = mHead;
    mHead = frame;
    CVPixelBufferRelease(prev);
  }
}


@end


