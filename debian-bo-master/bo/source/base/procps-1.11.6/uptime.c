#include "proc/whattime.h"
#include "proc/version.h"
#include <stdio.h>

void usage () {    
    fprintf (stderr, "usage:\tuptime [-V]\n");
}


void main(int argc, char *argv[]) {
  set_linux_version();
  if(argc == 1)
    print_uptime();
  else if((argc == 2) && (!strcmp(argv[1], "-V")))
    display_version();
  else
     usage ();
  if (argc >2 ) usage ();
  exit(0);
}
