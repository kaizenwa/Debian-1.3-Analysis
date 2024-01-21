#ifdef NeXT
#include "objc-gnu2next.h"
#endif

#include "Tcl.h"
 
 int main (int argc, char *argv[])
 {
   id tcl = [[Tcl alloc] initWithArgc:argc argv:argv];
   [tcl promptAndEval];
   exit(0);
 }
