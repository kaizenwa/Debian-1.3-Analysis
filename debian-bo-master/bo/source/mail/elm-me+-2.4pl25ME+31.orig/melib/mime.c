
#include "headers.h"
#include "melib.h"

#ifdef MIME

#include <errno.h>
#include <sys/stat.h>

/* Notice that ENCODING_ILLEGAL is -1 
 */

char *mime_encode_names[] = {
  "none", /* Not used. */
  "7bit",
  "8bit",
  "binary",
  "quoted-printable",
  "base64",
  "X-???"         /* ENCODING_EXPERIMENTAL */
};


void mime_panic(f,ln,pr,ms) 
     char * f;
     int ln;
     char *pr;
     char *ms;
{
  int do_cursor = RawState();

  dprint(1,(debugfile,"\nMIME PANIC in %s:%d:%s\n",f,ln,pr));
  dprint(1,(debugfile,">>%s\n",ms));

  /* softkeys_off(); */

  if (do_cursor) {
    error1("MIME PANIC: %s",ms);
    sleep(1);
  }

  if (do_cursor) {
    MoveCursor(elm_LINES, 0);
    Raw(OFF);
  }

  fprintf(stderr,"\nMIME PANIC in %s:%d:%s\n",f,ln,pr);
  fprintf(stderr,">>%s\n",ms);

#if DEBUG
  if (debug > 10) {
    fprintf(stderr,"ABORTING...\n");
    fflush(stderr);
    abort();
  }
#endif
  emergency_exit();
}


int check_encoding(Encoding)
     char *Encoding;
{
  char *c, *d, tmp[VERY_LONG_STRING];
  int result =  ENCODING_ILLEGAL;

  dprint (9, (debugfile, "check_encoding(): Encoding=\"%s\"\n", Encoding));

  /* Don't harm "str" */
  strfcpy (tmp, Encoding, sizeof(tmp));
  
  rfc822_reap_comments (tmp, NULL, 0);
  c = tmp;
  d = tmp + strlen(tmp);

  while (d > tmp && whitespace(*(d-1)))
    d--;
  *d = '\0';
  while (*c && whitespace(*c))
    c++;

  if ('\0' == *c)
    result = ENCODING_7BIT;
  else if (strincmp(c, "x-", 2) == 0) 
    result = ENCODING_EXPERIMENTAL;
  else {
    int i;
    for (i = ENCODING_7BIT; i < ENCODING_EXPERIMENTAL; i++) {
      if (istrcmp(c, mime_encode_names[i]) == 0) {
	result = i;
	break;
      }
    }
  }

  dprint (9, (debugfile, "check_encoding()=%s (%d)\n",
	      ENCODING(result),result));
  return(result);
}

int charset_ok(s)
     char *s;
{
    /* Return true if configured charset could display us-ascii too */
    char buf[SLEN];	/* assumes sizeof(charset_compatlist) <= SLEN */
    char *bp, *chset;

    /* the "charset_compatlist[]" format is: */
    /*   charset charset charset ... */
    bp = strfcpy(buf, charset_compatlist, sizeof buf);
    while ((chset = strtok(bp, " \t\n")) != NULL) {
	bp = NULL;
	if (istrcmp(chset, s) == 0)
	    break;
    }

    /* see if we reached the end of the list without a match */
    if (chset == NULL) {
	return(FALSE);
    }
    return(TRUE);
}


#endif /* MIME */
