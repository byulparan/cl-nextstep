# CL-NEXTSTEP

Cocoa binding for CommonLisp on macOS.  
It used CFFI.

## DEPENDENCIES
  * Quicklisp
  * [trivial-main-thread](https://github.com/Shinmera/trivial-main-thread) 
	You should be use recent version. Quicklisp version is outdated.

## USAGE

1. build C shared library.
   ```
   $ cd /PATH/TO/cl-nextstep/
   $ cd C
   $ cmake -B build
   $ make -C build
   ```

