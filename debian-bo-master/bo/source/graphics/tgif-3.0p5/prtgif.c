/*
 * Author:      William Chia-Wei Cheng (william@cs.ucla.edu)
 *
 * Copyright (C) 1990-1996, William Chia-Wei Cheng.
 *
 * Permission limited to the use, copy, display, distribute without
 * charging for a fee, and produce derivative works of "tgif" and
 * its documentation for not-for-profit purpose is hereby granted by
 * the Author, provided that the above copyright notice appears in
 * all copies made of "tgif" and that both the copyright notice
 * and this permission notice appear in supporting documentation,
 * and that the name of the Author not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  The Author makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.  All other rights (including, but not limited to, the
 * right to sell "tgif", the right to sell derivative works of
 * "tgif", and the right to distribute "tgif" for a fee) are
 * reserved by the Author.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef lint
static char RCSid[] =
      "@(#)$Header: /n/opus/u/guest/william/src/tgif/v3/RCS/prtgif.c,v 3.0 1996/05/06 16:06:53 william Exp $";
#endif

#include <stdio.h>

#define PRTGIF_NO_TGIF_DBG 1

#include "const.h"

#ifndef _NO_EXTERN
#include "prtgif.e"
#endif

extern char	* getenv ARGS_DECL((char *));
#ifdef SYSV
extern unsigned	sleep ARGS_DECL((unsigned));
#else
extern int	sleep ARGS_DECL((unsigned));
#endif

int main (argc, argv)
   int	argc;
   char	* argv[];
{
   register int	i;
   int		total_len;
   char		tgif_path[MAXSTRING+1], print_cmd[MAXSTRING+1], * cmd, * c_ptr;
   char		tmp_str[MAXSTRING+1], **s_ptr;
   FILE		* fp;

   if ((c_ptr = getenv ("TGIFPATH")) == NULL)
      strcpy (tgif_path, TGIF_PATH);
   else
      if (strlen (c_ptr) >= MAXSTRING)
         /* must be an error */
         strcpy (tgif_path, TGIF_PATH);
      else
         strcpy (tgif_path, c_ptr);

#ifdef PRINT_CMD
   strcpy (print_cmd, PRINT_CMD);
#else
#ifdef VMS
   strcpy (print_cmd, "print");
#else
#ifdef SYSV
   strcpy (print_cmd, "lp -dpostscript");
#else
   strcpy (print_cmd, "lpr");
#endif /* SYSV */
#endif /* VMS */
#endif /* PRINT_CMD */

   total_len = strlen("tgif ")+strlen("-prtgif ");
   total_len += strlen(tgif_path); total_len += strlen("-tgif_path")+4;
   total_len += strlen(print_cmd); total_len += strlen("-print_cmd")+4;
   for (i=argc-1, s_ptr=(&argv[1]); i > 0; i--, s_ptr++) {
      total_len += strlen(*s_ptr)+1;
   }
   cmd = (char*)malloc((total_len+2)*sizeof(char));
   if (cmd == NULL) {
      fprintf(stderr, "Out of virtual memory and can not malloc().\n");
      fflush(stderr);
   }
   c_ptr = cmd;
   sprintf(c_ptr, "tgif "); c_ptr += strlen("tgif")+1;
   sprintf(c_ptr, "-prtgif "); c_ptr += strlen("-prtgif")+1;
   sprintf(c_ptr, "-tgif_path \"%s\" ", tgif_path);
   c_ptr += strlen(tgif_path)+strlen("-tgif_path")+4;
   sprintf(c_ptr, "-print_cmd \"%s\" ", print_cmd);
   c_ptr += strlen(print_cmd)+strlen("-print_cmd")+4;
   for (argc--, argv++; argc > 0; argc--, argv++)
   {
      sprintf(c_ptr, "%s ", *argv);
      c_ptr += strlen(*argv)+1;
   }
   *(--c_ptr) = '\0';

   if ((fp = (FILE*) popen (cmd, "r")) == NULL)
   {
      fprintf(stderr, "Can not popen() to tgif.  Abort!\n");
      exit (-1);
   }

   while (fgets (tmp_str, MAXSTRING, fp) != NULL)
   {
      fprintf (stderr, "%s", tmp_str);
      sleep (5);
   }
   pclose (fp);
   free(cmd);
   return (0);
}
