cmake_minimum_required(VERSION 3.6)

project(cl-nextstep)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/../")

set(CMAKE_INSTALL_NAME_DIR @rpath)
set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)

set(SRC_FILES
  application.m
  timer.m
  window.m
  view.m
  opengl-view.m
  av-foundation.m
  metal-view.m
)

set(CMAKE_OSX_ARCHITECTURES arm64 x86_64)
add_library(cl-nextstep SHARED ${SRC_FILES})
target_link_libraries(cl-nextstep "-framework Cocoa" "-framework QuartzCore" "-framework AVFoundation" "-framework CoreMedia" "-framework OpenGL -framework Metal -framework MetalKit")
