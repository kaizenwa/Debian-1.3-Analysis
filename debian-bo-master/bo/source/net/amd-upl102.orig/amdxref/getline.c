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
#include <sys/types.h>
#include <stdio.h>
#include <malloc.h>

#include "chario.h"

char *
getline(void)
{
     char *buffer;
     int bufsize;
     int bufptr;
     int c;
     int quote;
     int escape;
     int comment;

     bufsize = 128;
     buffer = (char *)malloc(bufsize);
     bufptr = 0;

     do {
	  comment = quote = escape = 0;
	  while((c=fetchar()) != EOF){
	       if(escape){
		    if(c == '\n'){
			 escape = 0;
			 continue;
		    }
		    if(bufptr == bufsize){
			 bufsize *= 2;
			 buffer = (char *)realloc(buffer, bufsize);
		    }
		    buffer[bufptr] = c;
		    escape = 0;
		    continue;
	       }
	       else if(quote){
		    if(c == '"'){
			 quote = 0;
			 continue;
		    }
	       }
	       else if(comment){
		    if(c == '\n'){
			 comment = 0;
		    }
		    continue;
	       }
	       else if(c == '\n')
		    break;
	  
	       switch(c){
	       case '"':
		    quote = 1;
		    break;
	       case '\\':
		    escape = 1;
		    break;
	       case '#':
		    comment = 1;
		    break;
	       default:
		    if(bufptr == bufsize){
			 bufsize *= 2;
			 buffer = (char *)realloc(buffer, bufsize);
		    }
		    buffer[bufptr++] = c;
	       }
	  }
     } while(c != EOF && !bufptr);

     if(bufptr == bufsize){
	  bufsize += 1;
	  buffer = (char *)realloc(buffer, bufsize);
     }
     if(bufptr)
	  buffer[bufptr] = 0;
     else {
	  free(buffer);
	  buffer = (char *)0;
     }
     return buffer;
}
