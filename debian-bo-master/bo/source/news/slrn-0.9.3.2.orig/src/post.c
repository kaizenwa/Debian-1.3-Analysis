/*  Copyright (c) 1995 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
/* post an article */
#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>
#include <string.h>


#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <sys/types.h>
#include <time.h>
#include <slang.h>
#include "jdmacros.h"


#ifdef VMS
# include "vms.h"
#endif

#include "slrn.h"
#include "server.h"
#include "misc.h"
#include "post.h"
#include "group.h"
#include "art.h"
#include "uudecode.h"
#include "util.h"
#include "chmap.h"

#if SLRN_HAS_MIME
# include "mime.h"
#endif

char *Slrn_Courtesy_CC_Message = NULL;
char *Slrn_Save_Posts_File;
char *Slrn_Last_Message_Id;
char *Slrn_Post_Custom_Headers;
int Slrn_Reject_Long_Lines = 1;

#ifdef VMS
# define SYSTEM_OS_NAME		"VMS"
#else
# ifdef __os2__
#  define SYSTEM_OS_NAME	"OS/2"
# else
#  ifdef __unix__
#   define SYSTEM_OS_NAME       "UNIX"
#  else
#   define SYSTEM_OS_NAME	"UNKNOWN"
#  endif
# endif
#endif


#if SLRN_HAS_GEN_MSGID
static char *slrn_create_message_id (void)
{
   unsigned long pid, now;
   static unsigned char baseid[64];
   unsigned char *b, *t, tmp[32];
   char *chars32 = "0123456789abcdefghijklmnopqrstuv";
   static unsigned long last_now;
   
   while (1)
     {
	if ((Slrn_User_Info.posting_host == NULL)
	    || ((time_t) -1 == time ((time_t *)&now)))
	  return NULL;
	
	if (now != last_now) break;
	sleep (1);
     }
   last_now = now;
   
   pid = (unsigned long) getpid ();
   now -= 0x28000000;
   
   b = baseid;
   t = tmp;
   while (now)
     {
	*t++ = chars32[now & 0x1F];
	now = now >> 5;
     }
   while (t > tmp)
     {
	t--;
	*b++ = *t;
     }
   *b++ = '.';
   
   t = tmp;
   while (pid)
     {
	*t++ = chars32[pid & 0x1F];
	pid = pid >> 5;
     }
   while (t > tmp)
     {
	t--;
	*b++ = *t;
     }
   
   *b++ = '.';
   t = (unsigned char *) Slrn_User_Info.username;
   if (t != NULL)
     while ((*t != 0) && (b < baseid + sizeof(baseid) - 1)) *b++ = *t++;
   *b = 0;
   
   return (char *) baseid;
}
#endif
void slrn_add_signature (FILE *fp)
{
   FILE *sfp;
   char buf[256];
   
   if ((sfp = slrn_open_home_file (Slrn_User_Info.signature, "r", buf, 0)) != NULL)
     {
	/* Apparantly some RFC suggests the -- \n. */
        fputs ("\n\n-- \n", fp);
	
	/* If signature file already has -- \n, do not add it. */
	if ((NULL != fgets (buf, sizeof (buf), sfp))
	    && (0 != strcmp (buf, "-- \n")))
	  fputs (buf, fp);
	  
        while (NULL != fgets (buf, sizeof(buf), sfp))
	  {
	     fputs (buf, fp);
	  }
        slrn_fclose(sfp);
     }
}

static int is_empty_header (char *line)
{
   char *b;
   
   if ((*line == ' ') || (*line == '\t')) return 0;
   
   b = slrn_strchr (line, ':');
   if (b == NULL) return 0;
   
   b = slrn_skip_whitespace (b + 1);
   return (*b == 0);
}


static int slrn_cc_file (char *file, char *to, char *msgid)
{
#if defined(VMS) || !SLRN_HAS_PIPING
   return -1;
#else
   FILE *pp, *fp;
   char line[1024];
   unsigned char *b;
   unsigned int cc_line = 0;
   unsigned int linenum;
   int num_ccd;
   
   if (NULL == (fp = fopen (file, "r")))
     {
	slrn_error ("Unable to open %s.");
	return -1;
     }
   
   /* Look for CC line */
   linenum = 0;
   while ((NULL != fgets (line, sizeof (line) - 1, fp)) && (*line != '\n'))
     {
	linenum++;
	if (0 == slrn_case_strncmp ((unsigned char *)line,
				    (unsigned char *) "Cc: ", 4))
	  {
	     b = (unsigned char *) slrn_skip_whitespace (line + 4);
	     if (*b && (*b != ',')) cc_line = linenum;
	     break;
	  }
     }
   
   /* At this point, if all has gone well line contains the cc information */
   
   if (cc_line == 0)
     {
	slrn_fclose (fp);
	return -1;
     }
   
   pp = slrn_popen (Slrn_SendMail_Command, "w");
   if (pp == NULL)
     {
	slrn_fclose (fp);
	return -1;
     }
   
   fputs ("To: ", pp);
   num_ccd = 0;
   b = (unsigned char *) line + 4;
   while (*b != 0)
     {
	unsigned char *bmin, ch;
	/* skip to first non-whitespace, non-comma char */
        if ((*b <= ' ') || (*b == ','))
	  {
	     b++;
	     continue;
	  }
	
	bmin = b;
	/* Now skip past to white space or comma */
	while (((ch = *b) != ',') && (ch > ' ')) b++;
	
	if ((b == bmin + 6)
	    && (0 == slrn_case_strncmp (bmin, (unsigned char *) "poster", 6)))
	  {
	     if (to != NULL)
	       {
		  if (num_ccd) putc (',', pp);
		  fputs (to, pp);
		  num_ccd++;
		  to = NULL;
	       }
	  }
	else
	  {
	     if (num_ccd) putc (',', pp);
	     fwrite ((char *)bmin, 1, (unsigned int) (b - bmin), pp);
	     num_ccd++;
	  }
     }
   putc ('\n', pp);
   
   rewind (fp);
   linenum = 0;
   
#ifdef __os2__
   fprintf (pp, "From: %s@%s (%s)\n",
            Slrn_User_Info.username, Slrn_User_Info.host,
            Slrn_User_Info.realname);
#endif
   
   if (msgid != NULL)
     fprintf (pp, "Message-Id: <slrn%s@%s>\n", 
	      msgid, Slrn_User_Info.posting_host);
   
   
   while ((NULL != fgets (line, sizeof (line) - 1, fp)) && (*line != '\n'))
     {
	linenum++;
	if (linenum == cc_line) continue;
	if (is_empty_header (line)) continue;
	if (0 == slrn_case_strncmp ((unsigned char *)line,
				    (unsigned char *) "To: ", 4))
	  continue;
	
	/* There is some discussion of this extension to mail headers.  For
	 * now, assume that this extension will be adopted.
	 */
	if (0 == slrn_case_strncmp ((unsigned char *)line,
				    (unsigned char *) "Newsgroups: ", 12))
	  {
	     fputs ("Posted-To: ", pp);
	     fputs (line + 12, pp);
	  }
	else
	  fputs (line, pp);
     }

#ifdef __os2__
   /* Add the correct From: line under OS/2 Warp */
   fprintf (pp, "From: %s@%s (%s)\n",
	    Slrn_User_Info.username, Slrn_User_Info.host,
	    ((Slrn_User_Info.realname == NULL) ? "" : Slrn_User_Info.realname));
#endif
   
# if SLRN_HAS_MIME
   if (Slrn_Use_Mime) slrn_mime_add_headers (pp);
# endif
   
   fputs ("\n", pp);
   
   if ((NULL != Slrn_Courtesy_CC_Message) && (*Slrn_Courtesy_CC_Message))
     fprintf (pp, "%s\n", Slrn_Courtesy_CC_Message);
   
# if SLRN_HAS_MIME
   if (Slrn_Use_Mime) fp = slrn_mime_encode (fp);
# endif
   
   while (NULL != fgets (line, sizeof (line) - 1, fp))
     {
	fputs (line, pp);
     }
   slrn_fclose (fp);
   slrn_pclose (pp);
   return 0;
#endif
}

static int check_file_for_posting (char *file)
{
   char line[1024], *the_line;
   FILE *fp;
   unsigned int num;
   char *err;
   int rsp;
   int newsgroups_found, subject_found;
   char ch;
   char *colon;
   
   try_again:
   newsgroups_found = subject_found = 0;
   err = NULL;
   fp = fopen (file, "r");
   
   if (fp == NULL)
     {
	slrn_error ("Unable to open %s", file);
	return -1;
     }
   
   the_line = line;
   
   /* scan the header */
   num = 0;
   while (NULL != fgets (line, sizeof (line), fp))
     {
	ch = *line;	
	num++;
	if ((ch == ' ') || (ch == '\t') || (ch == '\n'))
	  {
	     if (num == 1) 
	       {
		  err = "The first line must begin with a header.";
		  break;
	       }
	     if (ch == '\n') break;
	     
	     continue;
	  }
	
	if (NULL == (colon = slrn_strchr (line, ':')))
	  {
	     err = "Expecting a header.  This is not a header line.";
	     break;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *) line, (unsigned char *) "Subject:", 8))
	  {
	     if (is_empty_header (line))
	       {
		  err = "The subject header is not allowed be to empty.";
		  break;
	       }
	     subject_found = 1;
	     continue;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *) line, (unsigned char *) "Newsgroups:", 11))
	  {
	     if (is_empty_header (line)) 
	       {
		  err = "The Newsgroups header is not allowed be to empty.";
		  break;
	       }
	     newsgroups_found = 1;
	     continue;
	  }
	
	/* slrn will remove it later if it is empty */
	if (is_empty_header (line)) continue;
	
	if (*(colon + 1) != ' ') 
	  {
	     err = "A space must follow the ':' in a header";
	     break;
	  }
	
	if (!slrn_case_strncmp ((unsigned char *) line, (unsigned char *) "From:", 5))
	  {
	     err = "This news reader will not accept user generated From lines.";
	     break;
	  }
     }

   if (err == NULL) 
     {
	if (subject_found == 0) 
	  {
	     err = "Subject header is required.";
	     num = 0;
	  }
	else if (newsgroups_found == 0)
	  {
	     err = "Newsgroups header is required.";
	     num = 0;
	  }
     }
   
   /* Now body.  Check for non-quoted lines. */
   if (err == NULL) 
     {
	char *qs = Slrn_Quote_String;
	unsigned int qlen;
	
	if (qs == NULL) qs = ">";
	qlen = strlen (qs);
	
	err = "Your message does not appear to have any unquoted text.";
	the_line = NULL;
	while (NULL != fgets (line, sizeof (line), fp))
	  {
	     num++;
	     if (!strncmp (line, qs, qlen))
	       continue;
	     
	     colon = slrn_skip_whitespace (line);
	     if (*colon == 0) continue;
	     
	     err = NULL;
	     if (Slrn_Reject_Long_Lines == 0)
	       break;
	     
	     if (strlen (line) > 81)   /* allow \n to slip */
	       {
		  err = "Line contains more than 80 characters.  You need to wrap it.";
		  the_line = line;
		  break;
	       }
	  }
     }
   
   fclose (fp);

   if (err == NULL) return 0;
   
   if (Slrn_Batch)
     {
	slrn_error ("Message is not acceptable.");
	if (err != NULL) slrn_error ("Reason: %s", err);
	return -1;
     }
	  
   Slrn_Full_Screen_Update = 1;

   slrn_set_color (0);
   
   SLsmg_cls ();
   SLsmg_gotorc (2,0);
   slrn_set_color (SUBJECT_COLOR);
   SLsmg_write_string ("Your message is not acceptable for the following reason:");
   slrn_set_color (ERROR_COLOR);
   SLsmg_gotorc (4,8); SLsmg_write_string (err);
   if (num && (the_line != NULL))
     {
	SLsmg_gotorc (6,0);
	slrn_set_color (SUBJECT_COLOR);
	SLsmg_printf ("This message was generated while looking at line %d:", num);
	SLsmg_gotorc (8,0);
	slrn_set_color (QUOTE_COLOR);
	SLsmg_printf (the_line);
	SLsmg_gotorc (12, 0);
	slrn_set_color (ARTICLE_COLOR);
	SLsmg_write_string ("Perhaps this error was generated because you did not separate the header");
	SLsmg_gotorc (13, 0);
	SLsmg_write_string ("section from the body by a BLANK line.");
     }
   
   rsp = slrn_get_response ("yYnNcC\007", "Press 'Y' to re-edit or 'C' to cancel");
   rsp |= 0x20;
   if (rsp == 'y')
     {
	if (slrn_edit_file (Slrn_Editor_Post, file, num) < 0) return -1;
	goto try_again;
     }
   return -1;
}

int slrn_post_file (char *file, char *to)
{
   char line[1024], *linep;
   int len, header;
   FILE *fp;
   int rsp;
   int perform_cc;
   int status;
   char *msgid = NULL;
#if SLRN_HAS_GEN_MSGID
   int has_messageid = 0;
#endif
   
   try_again:
   perform_cc = 0;
   if (Slrn_Batch == 0) while (1)
     {
	rsp = slrn_get_response ("yYnNeE", "Post the message? Y-es, N-o, E-dit");
	rsp |= 0x20;
	if (rsp == 'n') return -1;
	if (rsp == 'y') break;
	if (slrn_edit_file (Slrn_Editor_Post, file, 1) < 0) 
	  {
	     slrn_error ("User abort.");
	     return -1;
	  }
     }
   
   if (-1 == check_file_for_posting (file)) return -1;
   
   slrn_message_now ("Posting ...");
   
   if ((Slrn_Save_Posts_File != NULL) && (*Slrn_Save_Posts_File))
     {
	FILE *infp, *outfp;
	time_t now;
	char save_post_file[256];
	
	if (NULL == (infp = fopen (file, "r")))
	  {
	     slrn_error ("File not found: %s--- message not posted.", file);
	     return -1;
	  }
	
	if (NULL == (outfp = slrn_open_home_file (Slrn_Save_Posts_File, "at",
						  save_post_file, 1)))
	  {
	     slrn_error ("Error saving to %s", save_post_file);
	     slrn_fclose (infp);
	     return -1;
	  }
	
	time (&now);
	fprintf (outfp, "From %s@%s %s", Slrn_User_Info.username, Slrn_User_Info.host, ctime(&now));
	fprintf (outfp, "From: %s@%s (%s)\n", Slrn_User_Info.username, Slrn_User_Info.host, Slrn_User_Info.realname);
	
	while (fgets (line, sizeof(line) - 1, infp) != NULL)
	  {
	     if ((*line == 'F')
		 && !strncmp ("From", line, 4)
		 && ((unsigned char)line[4] <= ' '))
	       {
		  putc ('>', outfp);
	       }
	     
	     fputs (line, outfp);
	  }
	fputs ("\n\n", outfp);	       /* separator */
	slrn_fclose (infp);
	slrn_fclose (outfp);
     }
   
   slrn_chmap_fix_file (file);
   
   if ((fp = fopen (file, "r")) == NULL)
     {
	slrn_error ("File not found: %s--- message not posted.", file);
	return -1;
     }
   
#if SLRN_HAS_MIME
   if (Slrn_Use_Mime)
     slrn_mime_scan_file (fp);
#endif

   /* slrn_set_suspension (1); */
   status = Slrn_Post_Obj->po_start ();
   if (status != CONT_POST)
     {
	if (status != -1)
	  slrn_error ("Posting not allowed.");
	return -1;
     }
   
   if (Slrn_User_Info.username != NULL)
     Slrn_Post_Obj->po_printf ("Path: %s\n", Slrn_User_Info.username);
   
   Slrn_Post_Obj->po_printf ("From: %s@%s (%s)\n", Slrn_User_Info.username, Slrn_User_Info.host, Slrn_User_Info.realname);
   /* if (Slrn_User_Info.posting_host != NULL)
     * Slrn_Post_Obj->po_printf ("X-Posting-Host: %s\n", Slrn_User_Info.posting_host); */

   linep = line + 1;
   header = 1;
   while (fgets (linep, sizeof(line) - 1, fp) != NULL)
     {
	len = strlen (linep);
	if (len == 0) continue;
	if (header)
	  {
	     unsigned char *b;
	     char *white = linep;
	     
	     while ((*white == ' ') || (*white == '\t')) white++;
	     if (*white == '\n')
	       {
#if SLRN_HAS_GEN_MSGID
		  if (has_messageid == 0)
		    {
		       Slrn_Last_Message_Id = msgid = slrn_create_message_id ();
		       if (msgid != NULL)
			 {
			    Slrn_Post_Obj->po_printf ("Message-Id: <slrn%s@%s>\n", msgid, Slrn_User_Info.posting_host);
			 }
		    }
#endif
#if SLRN_HAS_MIME
		  if (Slrn_Use_Mime)
		    slrn_mime_add_headers (0);   /* 0 --> Slrn_Post_Obj->po_puts */
#endif
		  Slrn_Post_Obj->po_printf ("X-Newsreader: slrn (%s %s)\n\n", Slrn_Version, 
			       SYSTEM_OS_NAME);
		  header = 0;
#if SLRN_HAS_MIME
		  if (Slrn_Use_Mime) fp = slrn_mime_encode (fp);
#endif
		  
		  continue;
	       }
	     
	     if (!slrn_case_strncmp ((unsigned char *)"Cc: ", 
				     (unsigned char *)linep, 4))
	       {
		  b = (unsigned char *) linep + 4;
		  b = (unsigned char *) slrn_skip_whitespace ((char *) b);
		  if (*b && (*b != ',')) perform_cc = 1;
		  continue;
	       }
	     if (is_empty_header (linep)) continue;
#if SLRN_HAS_GEN_MSGID
	     if (!slrn_case_strncmp ((unsigned char *)"Message-Id: ",
				     (unsigned char *)linep, 12))
	       has_messageid = 1;
#endif	     
	     linep[len - 1] = 0;
#if SLRN_HAS_MIME
	     if (Slrn_Use_Mime)
	       slrn_mime_header_encode (linep, sizeof (line) - 1);
#endif
	  }

	/* Since the header may not have ended in a \n, make sure the 
	 * other lines do not either.  Later, the \n will be added.
	 */
	if (linep[len - 1] == '\n')
	  {
	     len--;
	     linep[len] = 0;
	  }
	
	if (*linep == '.')
	  {
	     linep--;
	     *linep = '.';
	  }
	
	Slrn_Post_Obj->po_puts (linep);
	Slrn_Post_Obj->po_puts ("\n");
	
	linep = line + 1;
     }
   slrn_fclose (fp);
   
   if (0 == Slrn_Post_Obj->po_end ())
     slrn_message ("Posting...done.");
   else
     {
	if (Slrn_Batch) return -1;

	/* slrn_set_suspension (0); */
	slrn_smg_refresh ();
	sleep (2);
	slrn_clear_message (); SLang_Error = 0;
	rsp = slrn_get_response ("RrEeCc\007", "Select one: R-epost, E-dit, C-ancel");
	if (rsp == 7) return -1;
	rsp |= 0x20;
	if (rsp == 'c') return -1;
	if ((rsp == 'e') && (slrn_edit_file (Slrn_Editor_Post, file, 1) < 0)) return -1;
	goto try_again;
     }
   
   if (perform_cc)
     {
	slrn_cc_file (file, to, msgid);
     }
   return 0;
}




int slrn_post (char *newsgroup, char *subj, char *dist)
{
   FILE *fp;
   char file[256];
   unsigned int header_lines;
   int ret;
   
   if (Slrn_Use_Tmpdir)
     {
	fp = slrn_open_tmpfile (file, "w");
     }
   else fp = slrn_open_home_file (SLRN_ARTICLE_FILENAME, "w", file, 0);
   
   if (fp == NULL)
     {
	slrn_error ("Unable to create %s.", file);
	return -1;
     }
   
   header_lines = 9;
   fprintf (fp, "Newsgroups: %s\nSubject: %s\n", newsgroup, subj);
   
   if (Slrn_User_Info.org != NULL)
     {
	header_lines++;
	fprintf (fp, "Organization: %s\n", Slrn_User_Info.org);
     }
   
   fprintf (fp, "Reply-To: %s\nFollowup-To: \n", Slrn_User_Info.replyto);
   fprintf (fp, "Keywords: \nSummary: \n");
   
   if (dist != NULL) fprintf (fp, "Distribution: %s\n", dist);
   
   header_lines += slrn_add_custom_headers (fp, Slrn_Post_Custom_Headers, NULL);

   fputs ("\n", fp);
   
   slrn_add_signature (fp);
   slrn_fclose (fp);
   
   if (slrn_edit_file (Slrn_Editor_Post, file, header_lines) >= 0)
     {
	ret = slrn_post_file (file, NULL);
     }
   else ret = -1;
   
   if (Slrn_Use_Tmpdir) (void) slrn_delete_file (file);
   return ret;
}



int slrn_add_custom_headers (FILE *fp, char *headers, int (*write_fun)(char *, FILE *))
{
   int n;
   char *s, *s1, ch, last_ch;
   
   if (headers == NULL) return 0;
   
   s = slrn_skip_whitespace (headers);
   if (*s == 0) return 0;
   
   s1 = s;
   n = 0;
   last_ch = 0;
   while ((ch = *s1) != 0)
     {
	if (ch == '\n') n++;
	last_ch = ch;
	s1++;
     }

   if (write_fun != NULL)
     (*write_fun)(s, fp);
   else fputs (s, fp);
   
   if (last_ch != '\n')
     {
	fputc ('\n', fp);
	n++;
     }
   
   return n;
}

   



