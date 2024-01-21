#include "portable.h"
#include "common.h"
#include "hp4.h"

char *
strdup(const char *s)
{
	  char	*copy;

	  copy = (char *)malloc(strlen(s) + 1);
	  if (copy != NULL) {
			strcpy(copy, s);
	  }
	  return copy;
}
