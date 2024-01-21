/* "mgt" Copyright (c) 1991 Shodan  */

#include "mgt.h"
#include <string.h>
#include <ctype.h>


#define NEWBLOCK 20

typedef char clinetype[MAXCOMMENTWIDTH];

static int nlines = 0;
int clines = 0;
clinetype *cbuf = 0;
char *empty = "";

FUNCTION char *commentGet(line)	/* int */
int line;
{
   if (cbuf && line < clines)
      return cbuf[line];
   else
      return empty;
}


FUNCTION int commentLines()
{
   return clines;
}


FUNCTION void formatComment(comment, width)
char *comment;
int width;
{
   char *partstart, *dest, c;
   int len, partlen, wordlen;

   clines = 0;
   width = MIN(width, MAXCOMMENTWIDTH - 1);
   wordlen = partlen = len = 0;
   dest = *cbuf;
   if (*comment == '\n')
      comment++;
   partstart = comment;
   do {
      if (clines >= nlines) {
	 nlines += NEWBLOCK;
	 cbuf = (clinetype *) (cbuf ? realloc(cbuf, nlines * sizeof(clinetype)) :
			       malloc(nlines * sizeof(clinetype)));
	 if (!cbuf)
	    barf("Memory allocation failure (formatComment)");
	 dest = cbuf[clines];
      }
      c = *(comment++);
      len++;
      partlen++;
      wordlen++;
      if (c == ' ')
	 wordlen = 0;
      else if (c == '\n' && *(comment - 2) == ' ' &&
	       *comment != '\n') {
	 partlen--;
	 len--;
	 strncpy(dest, partstart, partlen);
	 dest += partlen;
	 partstart = comment;
	 wordlen = partlen = 0;
      } else if (c == '\n' || !c) {
	 strncpy(dest, partstart, partlen - 1);
	 *(dest + partlen - 1) = 0;
	 len = partlen = wordlen = 0;
	 dest = cbuf[++clines];
	 partstart = comment;
      }
      if (len == width) {

	 if (len == wordlen)
	    wordlen = 0;

	 else
	    len = partlen - wordlen;

	 strncpy(dest, partstart, len);
	 partstart += len;
	 *(dest + len) = 0;
	 len = partlen = wordlen;

	 dest = cbuf[++clines];
      }
   } while (c);
}
