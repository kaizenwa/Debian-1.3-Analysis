#include "includes.h"
#include "debug.h"

extern char * term_server;
				/* This file is intended to be the */
				/* replacement for checkline, and */
				/* linerem. This should be able to */
				/* work out which character */
				/* combinations are getting eaten, and */
				/* set up a termrc file accordingly. */

int meta_state(int i) 
{
  static int state = 0;

/*  static char control[10];*/
  switch (state) {
  case 0:
    /* We wait for start of packet processes. */
    if (i != 'E') return 1;
    state = 1;
    break;
  case 1:			/* We got an E, now receive a number */
    
    return 0;			/* return to normal processing. */
  }
  return 0;
}
