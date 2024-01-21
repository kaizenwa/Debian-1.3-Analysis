/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/26/1993
*/
#include <stdio.h>

extern int parse_error;

int
main(int argc, char **argv)
{
     int error;
     int stat;
     int i;

     if(!argv[1]){
	  fprintf(stderr,
		  "Usage: %s autodir mapname [autodir mapname [...]]\n",
		  argv[0]);
	  exit(1);
     }

     initglobals();

     error = 0;
     for(i=1;argv[i];i+=2){
	  if(argv[i+1]){
	       stat = addfile(argv[i+1]);
#ifdef NISMAPS
	       if(stat != 0)
		    stat = addnismap(argv[i+1], 0, 1);
#endif
	       if(!stat){
		    if(!error){
			 resetparser();
			 parse();
			 if(!parse_error)
			      pushmap(argv[i], argv[i+1]);
			 else
			      error = 1;
		    }
	       }
	       else {
		    fprintf(stderr, "Can't locate map called `%s'.\n",
			    argv[i+1]);
		    error = 1;
	       }
	  }
	  else {
	       fprintf(stderr, "Missing map file for %s\n",
		       argv[i]);
	       error = 1;
	       break;
	  }
     }
     if(!error){
	  stat = scan();
	  putchar('\n');
	  exit(stat);
     }
     else
	  exit(-1);

     return 0;
}
