/*
 * Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.
 * 
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the above
 * copyright notice of Xerox Corporation and this paragraph.  Any
 * distribution of this software must comply with all applicable
 * United States export control laws.  This software is made
 * available AS IS, and XEROX CORPORATION DISCLAIMS ALL WARRANTIES,
 * EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED
 * HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR
 * ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT,
 * TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX
 * CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 */
/* $Id: decoderr.c,v 1.3 1996/02/05 15:33:07 spreitze Exp $ */
/* Last edited by Mike Spreitzer February 5, 1996 7:32 am PST */

#include <stdio.h>

typedef struct {
  long unsigned   hash;
  const char     *filename;
}               hashnote;

static hashnote hashnotes[] = {
#include "errhashs.c"
	{0, NULL}
};

int
main(int argc, char **argv)
{
  int             i, j, ans = 0;
  long unsigned   code, hash, line;
  for (i = 1; i < argc; i++) {
    if (sscanf(argv[i], "%lu", &code) != 1) {
      fprintf(stderr,
	      "Failed to parse \"%s\" as a long unsigned int!\n",
	      argv[i]);
      ans = -1;
    } else if (code < 1000)
      fprintf(stderr,
	      "%lu isn't an internal check failure minor code!\n",
	      code);
    else {
      int             found = 0;
      code -= 1000;
      hash = code / 10000;
      line = code - hash * 10000;
      fprintf(stderr, "%lu = line %lu in", code + 1000, line);
      for (j = 0; hashnotes[j].filename != NULL; j++)
	if (hashnotes[j].hash == hash) {
	  printf(" %s $ILUSRC/runtime/kernel/%s",
		 (found ? "or" : "file"),
		 hashnotes[j].filename);
	  found = 1;
	}
      if (found)
	printf("\n");
      else {
	printf(" unknown file (that hashes to %lu)\n", hash);
	ans = -1;
      }
  next:
      0;
    }
  }
  return ans;
}
