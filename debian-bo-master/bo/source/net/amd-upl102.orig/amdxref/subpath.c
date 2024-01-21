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

int
subpath(register char *p1, register char *p2)
{
     for(;*p1 && *p2 && *p1==*p2;p1++,p2++)
	  ;
     if(!*p1){
	  switch(*p2){
	  case 0:
	       return 3;
	       break;
	  case '/':
	       return 1;
	       break;
	  default:
	       return 0;
	       break;
	  }
     }
     else if(!*p2){
	  switch(*p1){
	  case '/':
	       return 2;
	       break;
	  default:
	       return 0;
	       break;
	  }
     }
     return 0;
}
