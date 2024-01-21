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

01/30/1993
*/
#include <ctype.h>

char *
findchar(char *s, char *cset)
{
     int quote;
     int i;

     quote = 0;
     for(;*s;s++){
	  if(quote){
	       if(*s == '"')
		    quote = 0;
	  }
	  else if(*s == '"')
	       quote = 1;
	  else for(i=0;cset[i];i++)
	       if(*s == cset[i])
		    return s;
     }

     return (char *)0;
}
