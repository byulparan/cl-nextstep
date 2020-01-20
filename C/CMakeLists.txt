cmake_minimum_required(VERSION 2.6)

project(cl_nextstep)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "~/quicklisp/local-projects/libs/cffi-shared-libs")

set(CMAKE_INSTALL_NAME_DIR @loader_path)
set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)

set(SRC_FILES
  application.m
  window.m)

add_library(cl_nextstep SHARED ${SRC_FILES})
target_link_libraries(cl_nextstep "-framework Cocoa")
