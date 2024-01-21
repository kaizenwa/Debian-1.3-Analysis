/*
 * getopt.c -- get command-line options.
 */

#include "../h/gsupport.h"

#ifndef SysOpt
extern char* progname;

/*
 * Based on a public domain implementation of System V
 *  getopt(3) by Keith Bostic (keith@seismo), Aug 24, 1984.
 */

#define BadCh	(int)'?'
#define EMSG	""
#define tell(m)	fprintf(stderr,"%s: %s -- %c\n",progname,m,optopt);return BadCh;

int optindex = 1;		/* index into parent argv vector */
int optopt;		/* character checked for validity */
char *optarg;		/* argument associated with option */

int getopt(nargc,nargv,ostr)
int nargc;
char **nargv, *ostr;
   {
   static char *place = EMSG;		/* option letter processing */
   register char *oli;			/* option letter list index */

   if(!*place) {			/* update scanning pointer */
      if(optindex >= nargc || *(place = nargv[optindex]) != '-' || !*++place)
         return(EOF);
      if (*place == '-') {		/* found "--" */
         ++optindex;
         return(EOF);
         }
      }					/* option letter okay? */

#if SCCX_MX
   /* Avoids compiler warning */
   if( ((optopt = (int)*place++) == (int)':')  ||
        (oli = index(ostr,optopt)) == 0) {
#else
   if ((optopt = (int)*place++) == (int)':' || !(oli = index(ostr,optopt))) {
#endif

      if(!*place) ++optindex;
      tell("illegal option");
      }
   if (*++oli != ':') {			/* don't need argument */
      optarg = NULL;
      if (!*place) ++optindex;
      }
   else {				/* need an argument */
      if (*place) optarg = place;	/* no white space */
      else if (nargc <= ++optindex) {	/* no arg */
         place = EMSG;
         tell("option requires an argument");
         }
      else optarg = nargv[optindex];	/* white space */
      place = EMSG;
      ++optindex;
      }
   return(optopt);			/* dump back option letter */
   }

#else					/* SysOpt */
static char junk; 			/* prevent empty file */
#endif					/* SysOpt */
