#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "group.h"
#include "art.h"
#include "startup.h"
#include "misc.h"
#include "score.h"
#include "util.h"

int slrn_edit_score (Slrn_Header_Type *h, char *newsgroup)
{
   char ch = 'e';
   int ich;
   char file[256];
   char qregexp[512];
   unsigned int mm = 0, dd = 0, yy = 0;
   int use_expired = 0;
   unsigned int linenum = 0;
   char *q, *ng = newsgroup;
   int score;
   FILE *fp;
   time_t myclock;
   
   
   if (Slrn_Score_File == NULL)
     {
	slrn_error ("A Score file has not been specified.");
	return -1;
     }
   
   if (Slrn_Prefix_Arg_Ptr == NULL)
     {
	ch = slrn_get_response ("SsFfRrEeCc\007", "Pick Score type: S-ubject, F-rom, R-eferences, E-dit, C-ancel");
	if (ch == 7) return -1;
	ch |= 0x20;
	if (ch == 'c') return -1;
   
	score = -9999;
	if (-1 == slrn_read_integer ("Score", &score, &score))
	  return -1;
	if (score == 0) return -1;
	
	if ('a' == (0x20 | slrn_get_response ("TtaA", "Which newsgroups: T-his group, A-ll groups")))
	  ng = "*";

	while (1)
	  {
	     *qregexp = 0;
	     if (-1 == slrn_read_input ("Expires (MM/DD/YYYY or DD-MM-YYYY or leave blank)", NULL, qregexp, 1))
	       return -1;
	     if (*qregexp)
	       {
		  if (((3 != sscanf (qregexp, "%u/%u/%u", &mm, &dd, &yy))
		       && (3 != sscanf (qregexp, "%u-%u-%u", &dd, &mm, &yy)))
		      || (dd > 31)
		      || (mm > 12)
		      || (yy < 1900))
		    continue;
		  use_expired = 1;
		  break;
	       }
	     else
	       {
		  use_expired = 0;
		  break;
	       }
	  }
     }
   
   if ((NULL == (fp = slrn_open_home_file (Slrn_Score_File, "r+", file, 1)))
       && (NULL == (fp = slrn_open_home_file (Slrn_Score_File, "w+", file, 1))))
     {
	slrn_error ("Unable to open %s", file);
	return -1;
     }
   
   if (Slrn_Prefix_Arg_Ptr == NULL)
     {
	linenum = 1;
	while (EOF != (ich = getc (fp)))
	  {
	     if (ich == '\n')	linenum++;
	  }
	
	myclock = time((time_t *) 0);
	fprintf (fp, "\n%%Score created by slrn on %s\n[%s]\nScore: %d\n",
		 (char *) ctime(&myclock), ng, score);
	
	if (use_expired)
	  fprintf (fp, "Expires: %u/%u/%u\n", mm, dd, yy);
	else fprintf (fp, "%%Expires: \n");
	
	q = SLregexp_quote_string (h->subject, qregexp, sizeof (qregexp));
	if (q == NULL) q = h->subject;
	fprintf (fp, "%c\tSubject: %s\n",
		 ((ch == 's') ? ' ' : '%'), q);
	
	q = SLregexp_quote_string (h->from, qregexp, sizeof (qregexp));
	if (q == NULL) q = h->from;
	fprintf (fp, "%c\tFrom: %s\n",
		 ((ch == 'f') ? ' ' : '%'), q);
	
	q = SLregexp_quote_string (h->msgid, qregexp, sizeof (qregexp));
	if (q == NULL) q = h->msgid;
	fprintf (fp, "%c\tReferences: %s\n",
		 ((ch == 'r') ? ' ' : '%'), q);
	
	
	q = SLregexp_quote_string (h->xref, qregexp, sizeof(qregexp));
	if (q != NULL)
	  {
	     /* The way slrn handles Xref, we only need this: */
	     fprintf (fp, "%%\t%s\n", q);
	  }
	
	q = SLregexp_quote_string (newsgroup, qregexp, sizeof(qregexp));
	if (q == NULL) q = newsgroup;
	fprintf (fp, "%%\tNewsgroup: %s\n", q);
     }
   
   Slrn_Prefix_Arg_Ptr = NULL;

   if (-1 == slrn_fclose (fp))
     return -1;

   if (ch == 'e')
     {
	if (-1 == slrn_edit_file (Slrn_Editor_Score, file, linenum + 1))
	  {
	     slrn_error ("Error calling editor.");
	     return -1;
	  }
     }
   slrn_message_now ("Reloading score file ...");
   (void) slrn_read_score_file (file);
   slrn_message ("Exit group for changes to take effect.");
   return 0;
}
