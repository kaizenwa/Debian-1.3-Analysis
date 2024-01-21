/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistributions and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *     This product includes software developed by the University of
 *     California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getopt.c   4.13 (Berkeley) 2/23/91";
#endif /* LIBC_SCCS and not lint */

#define I_STRING
#include "includes.h"

/* When redefining a system routine, we have to make sure to exactly match
   any prototypes.  Linux prototypes getopt() with const arguments; other
   systems probably do too.  Hopefully __STDC__ is close enough; if not,
   add system-specific preprocessor changes. */

#if __STDC__
#define CONST const
#else
#define CONST
#endif

/*
 * get option letter from argument vector
 */
int    term_opterr = 1,             /* if error message should be printed */
       term_optind = 1,             /* index into parent argv vector */
       term_optopt;                 /* character checked for validity */
char   *term_optarg;                /* argument associated with option */

#define        BADCH   (int)'?'
#define        EMSG    ""

int term_getopt(nargc, nargv, ostr)
       int nargc;
       char * CONST *nargv;
       CONST char *ostr;
{
       static char *place = EMSG;              /* option letter processing */
       register char *oli;                     /* option letter list index */
       char *p;

       if (!*place) {                          /* update scanning pointer */
               if (term_optind >= nargc || *(place = nargv[term_optind]) != '-') {
                       place = EMSG;
                       return(EOF);
               }
               if (place[1] && *++place == '-') {      /* found "--" */
                       ++term_optind;
                       place = EMSG;
                       return(EOF);
               }
       }                                       /* option letter okay? */
       if ((term_optopt = (int)*place++) == (int)':' ||
           !(oli = strchr(ostr, term_optopt))) {
               /*
                * if the user didn't specify '-' as an option,
                * assume it means EOF.
                */
               if (term_optopt == (int)'-')
                       return(EOF);
               if (!*place)
                       ++term_optind;
               if (term_opterr) {
                       if (!(p = strrchr(*nargv, '/')))
                               p = *nargv;
                       else
                               ++p;
                       (void)fprintf(stderr, "%s: illegal option -- %c\n",
                           p, term_optopt);
               }
               return(BADCH);
       }
       if (*++oli != ':') {                    /* don't need argument */
               term_optarg = NULL;
               if (!*place)
                       ++term_optind;
       }
       else {                                  /* need an argument */
               if (*place)                     /* no white space */
                       term_optarg = place;
               else if (nargc <= ++term_optind) {   /* no arg */
                       place = EMSG;
                       if (!(p = strrchr(*nargv, '/')))
                               p = *nargv;
                       else
                               ++p;
                       if (term_opterr)
                               (void)fprintf(stderr,
                                   "%s: option requires an argument -- %c\n",
                                   p, term_optopt);
                       return(BADCH);
               }
               else                            /* white space */
                       term_optarg = nargv[term_optind];
               place = EMSG;
               ++term_optind;
       }
       return(term_optopt);                         /* dump back option letter */
}

