#import <Cocoa/Cocoa.h>

CGImageRef cg_load_bitmap_image(char* path) {
  NSImage *image = [[NSImage alloc] initWithContentsOfFile: [NSString stringWithUTF8String: path]];
  NSRect imageRect = NSMakeRect(0, 0, image.size.width, image.size.height);
  CGImageRef cgImage = [image CGImageForProposedRect:&imageRect context:NULL hints:nil];
  return cgImage;
}

unsigned char* cg_bitmap_data(CGImageRef ref) {
  NSBitmapImageRep* nsbitmap = [[[NSBitmapImageRep alloc] initWithCGImage: ref] autorelease];
  return nsbitmap.bitmapData;
}

CGImageRef cg_get_cgimage_from_screen(CGRect rect) {
  CGImageRef screen =  CGWindowListCreateImage(rect,
					       kCGWindowListOptionIncludingWindow | kCGWindowListOptionOnScreenBelowWindow,
					       kCGNullWindowID, kCGWindowImageNominalResolution);
  return screen;
}
