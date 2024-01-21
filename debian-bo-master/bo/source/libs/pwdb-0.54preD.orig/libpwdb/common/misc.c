/*
 * $Id$
 *
 * $Log$
 */

/*
 * this is the enevitable file that contains all the utility things that
 * are needed by the library
 */

#include "../_pwdb_internal.h"

char *__pwdb_strdup(const char *x)
{
     if (x != NULL) {
	  char *s = (char *) malloc(strlen(x)+1);
	  if (s) {
	       strcpy(s,x);
	  }
	  return s;
     } else
	  return NULL;
}
