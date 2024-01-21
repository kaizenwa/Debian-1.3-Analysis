#include "Tk.h"
 
 int main (int argc, char *argv[])
 {
   id tk = [[Tk alloc] initWithArgc:argc argv:argv];
   [tk promptAndEval];
   exit(0);
 }
