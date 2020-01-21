#import <Cocoa/Cocoa.h>

typedef void(*DrawFn)(int,int,void*,void*,int,int);
typedef void(*MouseFn)(int inID, int type, NSEvent*, double locationX, double locationY);
  
enum {
      INIT = 0,
      DRAW,
      RESHAPE,
      SHUTDOWN
};

enum {
      DOWN = 0,
      DRAGGED,
      UP,
      MOVED,
      SCROLLWHEEL
};

