/* -*- mode: C; mode: fold -*- */

#include "config.h"

#define SLRNPULL_CODE
#include "slrnfeat.h"

/*{{{ System Includes */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <time.h>

#include <sys/stat.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <ctype.h>

/* This should be ported to older systems. */
#include <dirent.h>

#include <slang.h>
#include "jdmacros.h"

/*}}}*/

/*{{{ Local Includes */

#include "ttymsg.h"
#include "util.h"
#include "sltcp.h"
#include "nntplib.h"
#include "nntpcodes.h"

#include "score.c"
#include "xover.c"

#undef SLRN_HAS_MSGID_CACHE
#define SLRN_HAS_MSGID_CACHE 1

#include "hash.c"
/*}}}*/

/*{{{ slrnpull global variables and structures */

#ifndef SLRNPULL_ROOT_DIR
# define SLRNPULL_ROOT_DIR	"/var/spool/news/slrn"
#endif

#ifndef SLRNPULL_CONF
# define SLRNPULL_CONF		"slrnpull.conf"
#endif

#ifndef SLRNPULL_OUTGOING_DIR
# define SLRNPULL_OUTGOING_DIR	"out.going"
#endif

#ifndef SLRNPULL_SCORE_FILE
# define SLRNPULL_SCORE_FILE	"score"
#endif

#ifndef SLRNPULL_NEWS_DIR
# define SLRNPULL_NEWS_DIR	"news"
#endif

#ifndef SLRNPULL_LOGFILE
# define SLRNPULL_LOGFILE	"log"
#endif

#ifndef SLRNPULL_OUTGOING_BAD_DIR
# define SLRNPULL_OUTGOING_BAD_DIR	"rejects"
#endif

char *SlrnPull_Dir = SLRNPULL_ROOT_DIR;
char *SlrnPull_Spool_News_Dir;
char *Group_Min_Max_File;	       /* relative to group dir */
char *Overview_File;	       /* relative to group dir */
char *Outgoing_Dir;
char *Outgoing_Bad_Dir;
char *New_Groups_File = "new.groups";
char *New_Groups_Time_File = "new.groups-time";
char *Data_Dir = "data";
static char *Active_File = "active";

static int Stdout_Is_TTY;
static char *Active_Groups_File;
static time_t Start_Time;

#define CREATE_OVERVIEW 1


static int handle_interrupts (void);

typedef struct _Active_Group_Type /*{{{*/
{
   unsigned int flags;
   unsigned int min, max;
   unsigned int max_to_get;	       /* if non-zero, get only this many */
   unsigned int expire_days;	       /* if zero, no expiration */
#define MAX_GROUP_NAME_LEN 80
   char name [MAX_GROUP_NAME_LEN + 1];
   char dirname [MAX_GROUP_NAME_LEN + 1];
   struct _Active_Group_Type *next;
}

/*}}}*/
Active_Group_Type;

static char *Current_Newsgroup;

static Active_Group_Type *Active_Groups;
static Active_Group_Type *Active_Groups_Tail;

/*}}}*/

static FILE *MLog_Fp = stdout;
static FILE *ELog_Fp = stderr;

static void write_timestamp (FILE *fp) /*{{{*/
{
   struct tm *tms;
   time_t tloc;
   
   time (&tloc);
   tms = localtime (&tloc);
   
   fprintf (fp, "%02d/%02d/%04d %02d:%02d:%02d ",
	    tms->tm_mon + 1, tms->tm_mday, 1900 + tms->tm_year,
	    tms->tm_hour, tms->tm_min, tms->tm_sec);
   
}

/*}}}*/

static void va_log (FILE *fp, char *pre, char *fmt, va_list ap) /*{{{*/
{
   write_timestamp (fp);
   if (pre != NULL) fputs (pre, fp);
   vfprintf (fp, fmt, ap);
   fputc ('\n', fp);
   
   fflush (fp);
   
   if (Stdout_Is_TTY && ((fp != stdout) && (fp != stderr)))
     {
	if (fp == MLog_Fp) fp = stdout; else fp = stderr;
	va_log (fp, pre, fmt, ap);
     }
}

/*}}}*/

static void log_message (char *fmt, ...) /*{{{*/
{
   va_list ap;
   
   va_start (ap, fmt);
   va_log (MLog_Fp, NULL, fmt, ap);
   va_end (ap);
}

/*}}}*/

static void log_error (char *fmt, ...) /*{{{*/
{
   va_list ap;
   
   va_start (ap, fmt);
   va_log (ELog_Fp, "***", fmt, ap);
   va_end (ap);
}

/*}}}*/

static void va_log_error (char *fmt, va_list ap) /*{{{*/
{
   va_log (ELog_Fp, "***", fmt, ap);
}

/*}}}*/

static void va_log_message (char *fmt, va_list ap) /*{{{*/
{
   va_log (MLog_Fp, NULL, fmt, ap);
}

/*}}}*/

static Active_Group_Type *find_group_type (char *name) /*{{{*/
{
   Active_Group_Type *g;
   
   g = Active_Groups;
   while (g != NULL)
     {
	if (!strcmp (name, g->name))
	  break;
	
	g = g->next;
     }
   
   return g;
}

/*}}}*/

static Active_Group_Type *add_group_type (char *name) /*{{{*/
{
   Active_Group_Type *g;
   
   g = (Active_Group_Type *) slrn_malloc (sizeof (Active_Group_Type), 1, 1);
   
   if (g == NULL)
     return NULL;
   
   strncpy (g->name, name, MAX_GROUP_NAME_LEN);   /* null terminated 
						   * by construction */
   
   if (Active_Groups_Tail != NULL)
     Active_Groups_Tail->next = g;
   else 
     Active_Groups = g;
   
   Active_Groups_Tail = g;
   return g;
}

/*}}}*/

static int do_mkdir (char *dir, int err) /*{{{*/
{
   if (0 == mkdir (dir, 0777))
     {
	log_message ("Created dir %s.", dir);
	return 0;
     }
   
   if (errno == EEXIST)
     return 0;
	
   if (err)
     log_error ("Unable to create directory %s. (errno = %d)", dir, errno);

   return -1;
}

/*}}}*/

static FILE *open_group_min_max_file (Active_Group_Type *g, char *mode, /*{{{*/
				      char *file)
{
   if (-1 == slrn_dircat (SlrnPull_Spool_News_Dir, g->dirname, file))
     return NULL;
   if (-1 == slrn_dircat (file, Group_Min_Max_File, file))
     return NULL;
   
   return fopen (file, mode);
}

/*}}}*/
   
/* This function returns 1 upon success, -1 up parse error, and 0
 * if file could not be opened.
 */
static int read_group_min_max_file (Active_Group_Type *g) /*{{{*/
{
   char file[SLRN_MAX_PATH_LEN + 1];
   char line[256];
   unsigned int min, max;
   FILE *fp;

   g->min = 1;
   g->max = 0;

   fp = open_group_min_max_file (g, "r", file);
   
   if (fp == NULL)
     return 0;
   
   if (NULL == fgets (line, sizeof(line), fp))
     {
	fclose (fp);
	log_error ("Error reading %s.", file);
	return -1;
     }

   fclose (fp);
   
   if (2 != sscanf (line, "%u %u", &min, &max))
     {
	log_error ("Error parsing %s.", file);
	return -1;
     }
   
   g->min = min;
   g->max = max;
   
   return 1;
}

/*}}}*/

static int write_group_min_max_file (Active_Group_Type *g) /*{{{*/
{
   char file[SLRN_MAX_PATH_LEN + 1];
   FILE *fp;
   
   fp = open_group_min_max_file (g, "w", file);
   if (fp == NULL)
     {
	log_error ("Unable to open %s for writing.", file);
	return -1;
     }
   if (EOF == fprintf (fp, "%u %u", g->min, g->max))
     {
	log_error ("Write to %s failed.", file);
	(void) fclose (fp);
	return -1;
     }
   if (-1 == slrn_fclose (fp))
     {
	log_error ("Error closing %s.", file);
	return -1;
     }
   return 0;
}

/*}}}*/

static int create_group_directory (Active_Group_Type *g) /*{{{*/
{
   char dirbuf [SLRN_MAX_PATH_LEN + 1];
   char *dir, *d, ch;
   unsigned int len;
   int status;
      
   strcpy (g->dirname, g->name);
   
   d = g->dirname;
   while (*d != 0)
     {
	if (*d == '.') *d = '/';
	d++;
     }
   
   /* If the min-max file is available, we know the directory exists.  
    * Check it now.  Also, this provides a convenient check on the length
    * of the filename.
    */
   status = read_group_min_max_file (g);

   if (status == -1)
     return -1;
   
   if (status != 0)
     return 0;
   
   /* Does not exist so we will have to create the directory. */

   len = strlen (SlrnPull_Spool_News_Dir);
   
   strcpy (dirbuf, SlrnPull_Spool_News_Dir);
   dirbuf [len] = '/';
   len++;
   
   dir = dirbuf + len;
   strcpy (dir, g->dirname);
   
   d = dir + strlen (dir);

   if (0 == do_mkdir (dirbuf, 0))
     return 0;
     
   /* Go back and create it piece by piece. */
   d = dir;
   do
     {
	ch = *d;
	if ((ch == '/') || (ch == 0))
	  {
	     *d = 0;
	     if (-1 == do_mkdir (dirbuf, 1))
	       return -1;

	     *d = ch;
	  }
	d++;
     }
   while (ch != 0);
   
   return 0;
}

/*}}}*/

/* The argv list is NOT NULL terminated.
 */
static int chop_string (char *str, char **argv, int *argc_p, int max_args) /*{{{*/
{
   char *s;
   int argc;
   
   argc = 0;
   while (argc < max_args)
     {
	str = slrn_skip_whitespace (str);
	if (*str == 0)
	  break;
	
	s = slrn_strbrk (str, " \t\n");
	if (s != NULL)
	  *s = 0;

	argv[argc] = str;
	argc++;
	
	if (s == NULL) break;
	
	str = s + 1;
     }
   
   *argc_p = argc;
   return argc;
}

/*}}}*/

static int read_active_groups (void) /*{{{*/
{
   FILE *fp;
   char buf[256];
   unsigned int num;
   int default_max_to_get, default_expire_days;
   
   fp = fopen (Active_Groups_File, "r");
   if (fp == NULL)
     {
	log_error ("Unable to open active groups file %s", Active_Groups_File);
	return -1;
     }
   
   log_message ("Reading %s", Active_Groups_File);

   default_max_to_get = 50;
   default_expire_days = 10;

   num = 0;
   while (NULL != fgets (buf, sizeof(buf), fp))
     {
#define MAX_ARGS 10
	char *argv[MAX_ARGS];
	int argc;
	char *name, *arg;
	Active_Group_Type *g;
	int max_to_get;
	int expire_days;
	
	num++;
	
	name = slrn_skip_whitespace (buf);
	if ((*name == '#') || (*name == 0))
	  continue;
	
	slrn_trim_string (name);
	
	chop_string (name, argv, &argc, MAX_ARGS);
	
	name = argv[0]; argc--;
	if (strlen (name) > MAX_GROUP_NAME_LEN)
	  {
	     log_error ("%s: line %u: group name too long.",
			 Active_Groups_File, num);
	     fclose (fp);
	     return -1;
	  }
	/* Make sure name is a valid name.  Here I just check for whitespace 
	 * in name which means it is invalid.
	 */
	arg = name;
	while (*arg != 0)
	  {
	     unsigned char uch;
	     
	     uch = (unsigned char) *arg;
	     
	     if (uch <= 32)
	       {
		  log_error ("%s: line %u: Group name contains whitespace.",
			      Active_Groups_File, num);
		  fclose (fp);
		  return -1;
	       }
	     arg++;
	  }
	
	max_to_get = default_max_to_get;
	expire_days = default_expire_days;
	
	arg = argv[1];
	
	if (argc && (*arg != '*'))
	  {
	     if ((1 != sscanf (arg, "%d", &max_to_get))
		 || (max_to_get < 0))
	       {
		  log_error ("%s: line %u: expecting positive integer in second field.",
			      Active_Groups_File, num);
		  fclose (fp);
		  return -1;
	       }
	     argc--;
	  }
	
	arg = argv[2];
	if (argc && (*arg != '*'))
	  {
	     if ((1 != sscanf (arg, "%d", &expire_days))
		 || (expire_days < 0))
	       {
		  log_error ("%s: line %u: expecting positive integer in third field.",
			      Active_Groups_File, num);
		  fclose (fp);
		  return -1;
	       }
	     argc--;
	  }
	
	if (0 == strcmp (name, "default"))
	  {
	     default_expire_days = expire_days;
	     default_max_to_get = max_to_get;
	     continue;
	  }

	if (NULL != find_group_type (name))
	  {
	     log_error ("%s: line %u: group duplicated.", 
			 Active_Groups_File, num);
	     fclose (fp);
	     return -1;
	  }
	
	if (NULL == (g = add_group_type (name)))
	  {
	     log_error ("%s: line %u: failed to add %s.",
			 Active_Groups_File, num, name);
	     fclose (fp);
	     return -1;
	  }
	
	g->max_to_get = (unsigned int) max_to_get;
	g->expire_days = (unsigned int) expire_days;

	if (-1 == create_group_directory (g))
	  {
	     fclose (fp);
	     return -1;
	  }
     }
   
   fclose (fp);
   return 0;
}

/*}}}*/

static char *root_dircat (char *name) /*{{{*/
{
   char *f;

   if (*name == '/')
     f = slrn_strmalloc (name, 0);
   else
     f = slrn_spool_dircat (SlrnPull_Dir, name, 0);

   if (f == NULL)
     slrn_exit_error ("Out of memory.");

   return f;
}

/*}}}*/

static char *data_dircat (char *name) /*{{{*/
{
   char *f;

   if (*name == '/')
     f = slrn_strmalloc (name, 0);
   else
     f = slrn_spool_dircat (Data_Dir, name, 0);

   if (f == NULL)
     slrn_exit_error ("Out of memory.");

   return f;
}

/*}}}*/

static int make_filenames (void) /*{{{*/
{
   Data_Dir = root_dircat (Data_Dir);
   Outgoing_Dir = root_dircat (SLRNPULL_OUTGOING_DIR);
   
   Active_Groups_File = root_dircat (SLRNPULL_CONF);
   SlrnPull_Spool_News_Dir = root_dircat (SLRNPULL_NEWS_DIR);
   
   New_Groups_Time_File = data_dircat (New_Groups_Time_File);
   Active_File = data_dircat (Active_File);
   New_Groups_File = data_dircat (New_Groups_File);
   
   Outgoing_Bad_Dir = slrn_spool_dircat (Outgoing_Dir, SLRNPULL_OUTGOING_BAD_DIR, 0);
   if (Outgoing_Bad_Dir == NULL)
     slrn_exit_error ("Out of memory.");
	
   if (-1 == do_mkdir (SlrnPull_Spool_News_Dir, 1))
     return -1;
   
   if (-1 == do_mkdir (Data_Dir, 1))
     return -1;
   
   Overview_File = SLRN_SPOOL_NOV_FILE;
   Group_Min_Max_File = ".minmax";

   return 0;
}

/*}}}*/

static int *listgroup_numbers (NNTP_Type *s, char *name, unsigned int *nump) /*{{{*/
{
   int *numbers;
   unsigned int max, num;
   char buf[256];
   int status;
   
   status = nntp_listgroup (s, name);
   if (status != OK_GROUP)
     {
	if (status == -1) log_error ("Read failed.");
	log_error ("listgroup %s failed.", name);
	return NULL;
     }
   
   max = 0;
   num = 0;
   numbers = NULL;
     
   while (1 == (status = nntp_read_line (s, buf, sizeof (buf))))
     {
	if (num == max)
	  {
	     int *newnums;
	     
	     max += 1000;
	     newnums = (int *) slrn_realloc ((char *) numbers, max * sizeof (int), 1);
	     if (newnums == NULL)
	       {
		  slrn_free ((char *)numbers);
		  nntp_discard_output (s);
		  return NULL;
	       }
	     numbers = newnums;
	  }
	numbers[num] =  atoi (buf);
	num++;
     }
   
   if (status == -1)
     {
	slrn_free ((char *) numbers);
	return NULL;
     }
   
   *nump = num;
   return numbers;
}

/*}}}*/

static unsigned int Num_Duplicates;

static int *list_server_numbers (NNTP_Type *s, Active_Group_Type *g, unsigned int *nump) /*{{{*/
{
   char *name; 
   unsigned int min, max;
   int status;
   char buf [512];
   int *numbers;
   unsigned int num_numbers, max_num_numbers;
   
   *nump = 0;
   
   name = g->name;
   if (1 != nntp_has_cmd (s, "XHDR"))
     return listgroup_numbers (s, name, nump);
   
   /* Server has XHDR.  Good. */
   min = g->min;
   max = g->max + 1;
#if 0   
   if (max >= min)
     max++;
#else
   if (max < min) max = min;
#endif

   status = nntp_server_vcmd (s, "XHDR Message-Id %d-", max);
   if (status == -1)
     return NULL;			       /* server closed? */
   
   if (status == 224) status = OK_HEAD;/* Micro$soft broken server */
   if (status != OK_HEAD)
     {
	log_error ("Server failed XHDR command: %s", s->rspbuf);
	return NULL;
     }
   
   num_numbers = 0;
   max_num_numbers = 0;
   numbers = NULL;
   
   min = max;
   while (1 == (status = nntp_read_line (s, buf, sizeof (buf))))
     {
	int num;
	char *b1, *b2;
	
	num = (int) atoi (buf);
	b1 = slrn_strchr (buf, '<');
	if (b1 == NULL)
	  {
	     /* defective server?? */
	     continue;
	  }
	b2 = slrn_strchr (b1, '>');
	if (b2 == NULL) continue;
	
	b2++;
	*b2 = 0;
	
	if (num > (int) max) max = (unsigned int) num;

	if (NULL != is_msgid_cached (b1, name, num, 0))
	  {
	     if (g->min > g->max) g->min = num;
	     g->max = num;
	
	     Num_Duplicates++;
	     continue;
	  }
	
	if (num_numbers == max_num_numbers)
	  {
	     int *newnums;
	     
	     max_num_numbers += 100;
	     newnums = (int *) slrn_realloc ((char *) numbers, max_num_numbers * sizeof (int), 1);
	     if (newnums == NULL)
	       {
		  slrn_free ((char *)numbers);
		  nntp_discard_output (s);
		  return NULL;
	       }
	     numbers = newnums;
	  }
	numbers [num_numbers] = num;
	num_numbers++;
     }

   log_message ("%s: Retrieving articles %d-%d.", g->name, min, max);

   if (status == -1)
     {
	slrn_free ((char *) numbers);
	return NULL;
     }
   
   *nump = num_numbers;
   return numbers;
}

/*}}}*/

static unsigned int Num_Killed;
static unsigned int Num_Articles_Received;
static unsigned int Num_Articles_To_Receive;

static void print_time_stats (NNTP_Type *s, int do_log) /*{{{*/
{
   char buf[512];
   time_t now;
   unsigned int in;
   unsigned long elapsed_time;
   unsigned int hour, min, sec;
   
   if ((Stdout_Is_TTY == 0) && (do_log == 0))
     return;
   
   if ((s == NULL) || (s->tcp == NULL))
     return;
   
   time (&now);
   
   elapsed_time = (unsigned long) now - (unsigned long) Start_Time;
   
   if (elapsed_time == 0)
     {
	if (do_log == 0) return;
	elapsed_time = 1;
     }
   
   in = s->tcp->bytes_in;

   hour = elapsed_time / 3600;
   min = (elapsed_time - 3600 * hour) / 60;
   sec = elapsed_time % 60;

   if (Stdout_Is_TTY)
     {
	
	sprintf (buf, "%u/%u (%u killed), Time: %02u:%02u:%02u, BPS: %lu      ",
		 Num_Articles_Received, Num_Articles_To_Receive, Num_Killed,
		 hour, min, sec,
		 (unsigned long) (in / elapsed_time));
	
	fputs (buf, stdout);
	fputc ('\r', stdout);
	fflush (stdout);
     }
   
   if (do_log)
     {
	log_message ("%s: %u/%u (%u killed), Time: %02u:%02u:%02u, BPS: %lu",
		     Current_Newsgroup, 
		     Num_Articles_Received, Num_Articles_To_Receive, Num_Killed,
		     hour, min, sec,
		     in / elapsed_time);
     }
}

/*}}}*/

static int write_xover_line (FILE *fp, Slrn_XOver_Type *xov) /*{{{*/
{
   if (fp == NULL)
     return 0;

#if CREATE_OVERVIEW
   if ((EOF == fprintf (fp,
			"%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d",
			xov->id, xov->subject_malloced, 
			xov->from, xov->date, xov->message_id,
			xov->references, xov->bytes, xov->lines))
       || ((xov->xref != NULL) && (xov->xref[0] != 0)
	   && (EOF == fprintf (fp, "\tXref: %s", xov->xref)))
       || (EOF == fputc ('\n', fp)))
     {
	log_error ("Error writing to overview database: %s:%d.", Current_Newsgroup, xov->id);
	return -1;
     }
#else
   (void) xov;
#endif
   return 0;
}

/*}}}*/

static int write_head_and_body (Active_Group_Type *g, int n, /*{{{*/
				char *head, char *body, 
				Slrn_XOver_Type *xov, FILE *xov_fp)
{
   char file [SLRN_MAX_PATH_LEN + 1];
   char buf[128];
   FILE *fp;
   
   if ((head == NULL) || (body == NULL))
     {
	if (g->min > g->max) g->min = n;
	g->max = n;
	
	return 0;
     }
   
   sprintf (buf, "%d", n);
   
   if ((-1 == slrn_dircat (SlrnPull_Spool_News_Dir, g->dirname, file))
       || (-1 == slrn_dircat (file, buf, file)))
     return -1;
   
   fp = fopen (file, "w");
   if (fp == NULL)
     {
	log_error ("Unable to open %s for writing.", file);
	return -1;
     }
   
   if ((EOF == fputs (head, fp))
       || (EOF == fputc ('\n', fp))
       || (EOF == fputs (body, fp)))
     {
	log_error ("Error writing to %s.", file);
	fclose (fp);
	slrn_delete_file (file);
	return -1;
     }
   
   if (-1 == slrn_fclose (fp))
     {
	log_error ("Error writing to %s.", file);
	slrn_delete_file (file);
	return -1;
     }

   if (-1 == write_xover_line (xov_fp, xov))
     return -1;

   if (g->min > g->max) g->min = n;
   g->max = n;
   
   
   return 0;
}

/*}}}*/

static int fetch_body (NNTP_Type *s, char **body) /*{{{*/
{
   int status;
   
   *body = NULL;

   print_time_stats (s, 0);

   status = nntp_get_server_response (s);
   if (status == -1)
     return -1;
   
   if (status != OK_BODY)
     return 0;
   
   if (NULL == (*body = nntp_read_and_malloc (s)))
     return -1;
   
   return 0;
}

/*}}}*/

static int get_bodies (NNTP_Type *s, int *numbers, /*{{{*/
		       char **heads, char **bodies, unsigned int num)
{
   unsigned int i;
   char buf[256], *b;
   char *crlf;
   
   crlf = "";
   b = buf;
   
   for (i = 0; i < num; i++)
     {
	bodies [i] = NULL;
	
	if (heads[i] == NULL)
	  continue;
	
	sprintf (b, "%sbody %d", crlf, numbers[i]);
	b += strlen (b);
	
	crlf = "\r\n";
     }
   
   if (b == buf)
     return 0;
   
   if (-1 == nntp_start_server_cmd (s, buf))
     return -1;
   
   for (i = 0; i < num; i++)
     {	
	if (heads [i] == NULL)
	  continue;

	if (-1 == fetch_body (s, bodies + i))
	  return -1;
     }
   
   return 0;
}

/*}}}*/

static int fetch_head (NNTP_Type *s, int n, char **headers, Slrn_XOver_Type *xov) /*{{{*/
{
   int status;
   Slrn_Header_Type h;
   int score;
   
   *headers = NULL;

   print_time_stats (s, 0);

   status = nntp_get_server_response (s);
   if (status == -1)
     return -1;
   
   if (status != OK_HEAD)
     return 0;
   
   if (NULL == (*headers = nntp_read_and_malloc (s)))
     return -1;
   
   /* Now score this header. */
   if (-1 == xover_parse_head (n, *headers, xov))
     {
	slrn_free (*headers);
	*headers = NULL;
	return 0;
     }
   
   slrn_map_xover_to_header (xov, &h);

#if 1
   (void) is_msgid_cached (h.msgid, Current_Newsgroup, (unsigned int) n, 1);
#endif

   score = slrn_score_header (&h, Current_Newsgroup);
   if (score < 0)
     {
	Num_Killed++;
	slrn_free (*headers);
	*headers = NULL;
	return 0;
     }
#if 0
   /* This next call should add the message id to the cache. */
   (void) is_msgid_cached (h.msgid, Current_Newsgroup, (unsigned int) n, 1);
#endif
   return 0;
}

/*}}}*/


static int get_heads (NNTP_Type *s, int *numbers, char **heads, /*{{{*/
		      Slrn_XOver_Type *xovs, unsigned int num)
{
   unsigned int i;
   char buf[256];
   char *b;
   char *crlf;
   
   b = buf;
   crlf = "";
   
   /* Final crlf added by nntp_start_server_cmd. */
   for (i = 0; i < num; i++)
     {
	sprintf (b, "%shead %d", crlf, numbers[i]);
	crlf = "\r\n";
	b += strlen (b);
	
	heads [i] = NULL;
	memset ((char *) (xovs + i), 0, sizeof (Slrn_XOver_Type));
     }

   if (-1 == nntp_start_server_cmd (s, buf))
     return -1;

   for (i = 0; i < num; i++)
     {
	if (-1 == fetch_head (s, numbers[i], heads + i, xovs + i))
	  return -1;
     }
   
   return 0;
}

/*}}}*/

static FILE *open_xover_file (Active_Group_Type *g, char *mode) /*{{{*/
{
#if CREATE_OVERVIEW
   char ov_file [SLRN_MAX_PATH_LEN + 1];
   FILE *fp;
   
   fp = NULL;
   if ((-1 != slrn_dircat (SlrnPull_Spool_News_Dir, g->dirname, ov_file))
       && (-1 != slrn_dircat (ov_file, Overview_File, ov_file)))
     {
	fp = fopen (ov_file, mode);
	if (fp == NULL)
	  log_error ("Unable to open overview file %s.\n", ov_file);
     }
   
   return fp;
#else
   (void) g;
   (void) mode;
   return NULL;
#endif
}

/*}}}*/

static int get_articles (NNTP_Type *s, Active_Group_Type *g, int *numbers, unsigned int num) /*{{{*/
{
   unsigned int i;
#define MAX_QUEUED 10
   char *heads[MAX_QUEUED];
   char *bodies[MAX_QUEUED];
   Slrn_XOver_Type xovs [MAX_QUEUED];
   int ret;
   FILE *fp;
   
   if (-1 == get_heads (s, numbers, heads, xovs, num))
     return -1;

   ret = 0;
   
   if (-1 != get_bodies (s, numbers, heads, bodies, num))
     {
	fp = open_xover_file (g, "a");

	for (i = 0; i < num; i++)
	  {
	     if (-1 == write_head_and_body (g, numbers[i], heads[i], bodies[i],
					    xovs + i, fp))
	       {
		  ret = -1;
		  break;
	       }
	  }
	
	if (fp != NULL)
	  {
	     if (-1 == slrn_fclose (fp))
	       {
		  log_error ("Error closing overview file for %s.", g->name);
		  ret = -1;
	       }
	  }
     }
   
   for (i = 0; i < num; i++) 
     {
	slrn_free (bodies[i]);
	slrn_free (heads[i]);
	slrn_free (xovs[i].subject_malloced);
     }
   return ret;
}

/*}}}*/

static int get_group_articles (NNTP_Type *s, Active_Group_Type *g, int min, int max) /*{{{*/
{
   unsigned int gmin, gmax;
   int *numbers;
   unsigned int num_numbers, i, imin;
   
   Num_Articles_Received = 0;
   Num_Killed = 0;
   Num_Articles_To_Receive = 0;

   gmin = g->min;
   gmax = g->max;
   
   if (((min > max) || (max < 0))
       || (((unsigned int)max <= gmax) && (gmin <= gmax)))
     {
	log_message ("%s: no new articles available.", g->name);
	return 0;
     }
   
   Num_Duplicates = 0;
   numbers = list_server_numbers (s, g, &num_numbers);
   if (Num_Duplicates)
     log_message ("%u duplicates removed leaving %u/%u.", 
		  Num_Duplicates, num_numbers, num_numbers + Num_Duplicates);
	
   if (numbers == NULL) return -1;
   
   i = 0;
   while ((i < num_numbers) && (numbers[i] <= (int) gmax))
     i++;
   
   if (i == num_numbers)
     {
	log_message ("%s: No articles available.", g->name);
	slrn_free ((char *) numbers);
	return 0;
     }
   
   log_message ("%s: %u articles available.", g->name, num_numbers - i);
   
   /* Hmmm...  How shall g->max_to_get be defined?  Here I assume that it 
    * means to attempt to retrieve the last max_to_get articles.
    */
   if ((g->max_to_get != 0) && (g->max_to_get + i < num_numbers))
     {
	log_message ("%s: Only retrieving last %u articles.", g->name, g->max_to_get);
	i = num_numbers - g->max_to_get;
     }
	
   imin = i;

   (void) slrn_open_score (g->name);
   
   Num_Articles_To_Receive = num_numbers - i;
   
   while (i < num_numbers)
     {
	int ns[MAX_QUEUED];
	unsigned int j;
	
	j = 0;
	while ((i < num_numbers) && (j < MAX_QUEUED))
	  {
	     ns[j] = numbers[i];
	     i++;
	     j++;
	  }
	
	print_time_stats (s, 0);
	(void) get_articles (s, g, ns, j);
	
	Num_Articles_Received += j;
     }
   
   (void) slrn_close_score ();

   slrn_free ((char *) numbers);
   return 0;
}

/*}}}*/

static int pull_news (NNTP_Type *s) /*{{{*/
{
   int status;
   Active_Group_Type *g;
   
   g = Active_Groups;
   while (g != NULL)
     {
	int min, max;
	
	log_message ("Fetching articles for %s.", g->name);
	
	status = nntp_select_group (s, g->name, &min, &max);
	if (status != OK_GROUP)
	  {
	     log_error ("Error selecting group %s.", g->name);
	     g = g->next;
	     continue;
	  }

	Current_Newsgroup = g->name;
	
	(void) get_group_articles (s, g, min, max);
	
	print_time_stats (s, 1);

	g = g->next;
     }	
	     
   return 0;
}

/*}}}*/

static int post_file (NNTP_Type *s, char *file) /*{{{*/
{
   FILE *fp;
   int status;
   char buf[8 * 1024];
   
   log_message ("Attempting to post %s...", file);

   fp = fopen (file, "r");
   if (fp == NULL)
     {
	log_error ("Unable to open file %s for posting.", file);
	return -1;
     }
   
   status = nntp_post_cmd (s);
   if (status != CONT_POST)
     {
	log_error ("Server failed post cmd.  status = %d.", status);
	fclose (fp);
	return -1;
     }
   
   while (NULL != fgets (buf, sizeof (buf), fp))
     {
	char *b;
	
	/* Kill possible \r and \r\n.  We will add it later */

	b = buf + strlen (buf);
	if ((b != buf) && (*(b - 1) == '\n'))
	  b--;
	if ((b != buf) && (*(b - 1) == '\r'))
	  b--;
	*b = 0;
	
	if ((-1 == nntp_fputs_server (s, buf))
	    || (-1 == nntp_fputs_server (s, "\r\n")))
	  {
	     log_error ("Write to server failed while posting %s.", file);
	     fclose (fp);
	     return -1;
	  }
     }
   
   fclose (fp);

   status = nntp_end_post (s);
   if (status == -1)
     {
	log_error ("Write to server failed while posting %s.", file);
	return -1;
     }
   
   if (status != OK_POSTED)
     {
	char *name;
	char bad_file [SLRN_MAX_PATH_LEN + 1];
	
	log_error ("Article %s rejected. status = %d: %s.", file, status, s->rspbuf);
	
	name = slrn_basename (file);
	if (-1 == slrn_dircat (Outgoing_Bad_Dir, name, bad_file))
	  return -1;
	
	log_error ("Saving article in %s...", Outgoing_Bad_Dir);
	if (-1 == rename (file, bad_file))
	  log_error ("Failed to rename %s to %s.", file, bad_file);
	
	return -1;
     }
   
   if (-1 == slrn_delete_file (file))
     log_error ("Unable to delete %s after posting.", file);
   
   return 0;
}

/*}}}*/

static int make_outgoing_dir (char *dir) /*{{{*/
{
   log_error ("%s directory does not exist.  Creating it...", dir);
   if (-1 == mkdir (dir, 0700))
     {
	log_error ("Unable to create %s.", dir);
	return -1;
     }
	
   if (-1 == chmod (dir, 0777 | 01000))
     log_error ("chmod 01777 failed on %s.", dir);
   
   return 0;
}

/*}}}*/

static int post_outgoing (NNTP_Type *s) /*{{{*/
{
   DIR *dp;
   struct dirent *df;
   int n;
   char file [SLRN_MAX_PATH_LEN + 1];
   
   dp = opendir (Outgoing_Dir);
   if (dp == NULL)
     return make_outgoing_dir (Outgoing_Dir);


   if (2 != slrn_file_exists (Outgoing_Bad_Dir))
     (void) make_outgoing_dir (Outgoing_Bad_Dir);
	
   if (s->can_post == 0)
     {
	log_error ("Server does not permit posting at this time.");
	return 0;
     }

   n = 0;
   while (NULL != (df = readdir (dp)))
     {
	char *name;
	
	name = df->d_name;

	if (*name != 'X')
	  continue;
	
	if (-1 == slrn_dircat (Outgoing_Dir, name, file))
	  break;
	
	if (1 != slrn_file_exists (file))
	  continue;
	
	if (-1 == post_file (s, file))
	  log_error ("Posting of %s failed.", file);
	else 
	  {
	     log_message ("%s posted.", file);
	     n++;
	  }
     }
   
   closedir (dp);
   return n;
}

/*}}}*/

static int write_active (void) /*{{{*/
{
   Active_Group_Type *g = Active_Groups;
   FILE *fp;
   char file [SLRN_MAX_PATH_LEN + 5];
   
   sprintf (file, "%s.tmp", Active_File);
   
   fp = fopen (file, "w");
   if (fp == NULL)
     {
	log_error ("Unable to create tmp active file (%s).", file);
	return -1;
     }
   
   while (g != NULL)
     {
	if (EOF == fprintf (fp, "%s %u %u y\n", g->name, g->max, g->min))
	  {
	     fclose (fp);
	     goto write_error;
	  }
	
	(void) write_group_min_max_file (g);
	  
	g = g->next;
     }
   
   if (0 == slrn_fclose (fp))
     {
	(void) slrn_delete_file (Active_File);
	if (-1 == rename (file, Active_File))
	  {
	     log_error ("Failed to rename %s to %s.", file, Active_File);
	     return -1;
	  }
	return 0;
     }
   
   write_error:
   log_error ("Write failed to tmp active file (%s).", file);
   (void) slrn_delete_file (file);
   return -1;
}

/*}}}*/

static NNTP_Type *Pull_Server;
static time_t Actual_Start_Time;

static int open_servers (char *host) /*{{{*/
{
   time (&Actual_Start_Time);
   Pull_Server = nntp_open_server (host, -1);
   if (Pull_Server == NULL)
     return -1;
   
   Pull_Server->tcp->bytes_in = Pull_Server->tcp->bytes_out = 0;
   time (&Start_Time);
   
   /* Probe for XHDR now because DNEWS does not handle probing later properly. */
   if (-1 == nntp_has_cmd (Pull_Server, "XHDR"))
     return -1;

   return 0;
}

/*}}}*/

static void print_stats (unsigned long bytes_in, unsigned long bytes_out) /*{{{*/
{
   time_t done;
   
   if (Actual_Start_Time == 0)
    done = 0;
   else
     time (&done);

   log_message ("A total of %lu bytes received, %lu bytes sent in %ld seconds.",
		bytes_in, bytes_out, (long) done - (long) Actual_Start_Time);
}

/*}}}*/

static void close_servers (void) /*{{{*/
{
   unsigned long bytes_in = 0;
   unsigned long bytes_out = 0;

   if (Pull_Server != NULL)
     {
	if (Pull_Server->tcp != NULL) 
	  {
	     bytes_in = Pull_Server->tcp->bytes_in;
	     bytes_out = Pull_Server->tcp->bytes_out;
	  }
	nntp_close_server (Pull_Server);
     }
   Pull_Server = NULL;
   print_stats (bytes_in, bytes_out);
}

/*}}}*/

static void init_signals (void);

static void open_log_files (void) /*{{{*/
{
   char file [SLRN_MAX_PATH_LEN + 1];
   
   ELog_Fp = stderr;
   MLog_Fp = stdout;
   
   if (-1 == slrn_dircat (SlrnPull_Dir, SLRNPULL_LOGFILE, file))
     return;
   
   MLog_Fp = fopen (file, "a");
   if (MLog_Fp == NULL)
     {
	MLog_Fp = stdout;
	log_error ("Unable to open %s for logging.", file);
	return;
     }
   
   ELog_Fp = MLog_Fp;
}

/*}}}*/

static void close_log_files (void) /*{{{*/
{
   if ((MLog_Fp != NULL) && (MLog_Fp != stdout))
     fclose (MLog_Fp);

   if ((ELog_Fp != NULL) && (ELog_Fp != stderr) && (ELog_Fp != MLog_Fp))
     fclose (ELog_Fp);
   
   ELog_Fp = stderr;
   MLog_Fp = stdout;
}

/*}}}*/

static void usage (char *pgm) /*{{{*/
{
   log_error ("%s usage: %s [-h HOSTNAME] [-d SPOOLDIR] [--expire] [--post] [--new-groups]\n", pgm, pgm);
   close_log_files ();
   exit (1);
}

/*}}}*/

static int read_score_file (void) /*{{{*/
{
   char file [SLRN_MAX_PATH_LEN + 1];
   
   if (-1 == slrn_dircat (SlrnPull_Dir, SLRNPULL_SCORE_FILE, file))
     return -1;
   
   return slrn_read_score_file (file);
}

/*}}}*/

static int do_expire (void);
static int get_new_groups (NNTP_Type *s);
static int read_authinfo (void);

int main (int argc, char **argv) /*{{{*/
{
   char *host = NULL;
   char *pgm;
   int expire_mode;
   int post_mode;
   int check_new_groups = 0;
   char *dir;
   
   pgm = argv[0];
   argv++; argc--;
   
   Stdout_Is_TTY = isatty (fileno(stdout));
   expire_mode = 0;
   post_mode = 0;
   
   dir = getenv ("SLRNPULL_ROOT");
   
   while (argc > 0)
     {
	char *arg;
	
	arg = *argv++; argc--;
	
	if (!strcmp (arg, "--help")) usage (pgm);

	if (!strcmp (arg, "-h") && (argc > 0))
	  {
	     host = *argv;
	     argv++; argc--;
	  }
	else if (!strcmp (arg, "-d") && (argc > 0))
	  {
	     dir = *argv;
	     argv++; argc--;
	  }
	else if (!strcmp (arg, "--expire"))
	  expire_mode = 1;
	else if (!strcmp (arg, "--post"))
	  post_mode = 1;
	else if (!strcmp (arg, "--new-groups"))
	  check_new_groups = 1;
	else usage (pgm);
     }

   if (dir != NULL)
     SlrnPull_Dir = dir;
   
   if (SlrnPull_Dir == NULL)
     {
	fprintf (stderr, "The slrnpull spool directory has not been defined.");
	return 1;
     }
   
   open_log_files ();

   if (expire_mode)
     log_message ("slrnpull started in expire mode.");
   else
     log_message ("slrnpull started.");
   
   SLang_init_case_tables ();

   if (-1 == make_filenames ())
     slrn_exit_error (NULL);

   if (-1 == read_active_groups ())
     slrn_exit_error (NULL);
   
   if (-1 == read_authinfo ())
     slrn_exit_error (NULL);
   
   if (expire_mode)
     {
	if (-1 == do_expire ())
	  slrn_exit_error (NULL);
	
	close_log_files ();
	return 0;
     }

   if (-1 == read_score_file ())
     slrn_exit_error (NULL);
     
   if (-1 == open_servers (host))
     slrn_exit_error ("Unable to initialize server.");

   SLTCP_Interrupt_Hook = handle_interrupts;
   
   post_outgoing (Pull_Server);
   
   if (check_new_groups)
     (void) get_new_groups (Pull_Server);
       
   if (post_mode == 0)
     {
	init_signals ();
	pull_news (Pull_Server);
	if (-1 == write_active ())
	  slrn_exit_error (NULL);
     }

   close_servers ();

   close_log_files ();
   
   return 0;
}

/*}}}*/

static int get_new_groups (NNTP_Type *s)
{
   FILE *fp_time;
   FILE *fp_ng;
   time_t tloc;
   struct tm *tm_struct;
   char line [1024];
   int num;
   char *p;
   
   log_message ("Checking for new groups.");
   
   if (NULL == (fp_ng = fopen (New_Groups_File, "a")))
     {
	log_message ("Unable to open new groups file %s.\n", New_Groups_File);
	return -1;
     }
   
   time (&tloc);

   if (NULL != (fp_time = fopen (New_Groups_Time_File, "r")))
     {
	char ch;
	int i;
	int parse_error;
	
	*line = 0;
	(void) fgets (line, sizeof (line), fp_time);
	(void) fclose (fp_time);
	
	parse_error = 1;
	
	/* parse this line to make sure it is ok.  If it is bad, issue a warning
	 * and go on.
	 */
	if (strncmp ("NEWGROUPS ", line, 10)) goto parse_error_label;
	p = line + 10;
	
	p = slrn_skip_whitespace (p);
	
	/* parse yymmdd */
	for (i = 0; i < 6; i++)
	  {
	     ch = p[i];
	     if ((ch < '0') || (ch > '9')) goto parse_error_label;
	  }
	if (p[6] != ' ') goto parse_error_label;
	
	ch = p[2];
	if (ch > '1') goto parse_error_label;
	if ((ch == '1') && (p[3] > '2')) goto parse_error_label;
	ch = p[4];
	if (ch > '3') goto parse_error_label;
	if ((ch == '3') && (p[5] > '1')) goto parse_error_label;
	
	/* Now the hour: hhmmss */
	p = slrn_skip_whitespace (p + 6);

	for (i = 0; i < 6; i++)
	  {
	     ch = p[i];
	     if ((ch < '0') || (ch > '9')) goto parse_error_label;
	  }
	ch = p[0];
	if (ch > '2') goto parse_error_label;
	if ((ch == '2') && (p[1] > '3')) goto parse_error_label;
	if ((p[2] > '5') || (p[4] > '5')) goto parse_error_label;
	
	p = slrn_skip_whitespace (p + 6);
	
	if ((p[0] == 'G') && (p[1] == 'M') && (p[2] == 'T'))
	  p += 3;
	*p = 0;
	
	parse_error = 0;
	
	switch (nntp_server_cmd (s, line))
	  {
	   case OK_NEWGROUPS:
	     break;
	     
	   case ERR_FAULT:
	     return 0;
	     
	   case ERR_COMMAND:
	     log_message ("Server does not implement NEWGROUPS command.");
	     return 0;

	   default:
	     slrn_message ("Server failed to return proper response to NEWGROUPS:\n%s\n",
			   s->rspbuf);

	     goto parse_error_label;
	  }
      
	num = 0;
	while (1 == nntp_read_line (s, line, sizeof (line)))
	  {
	     if ((EOF == fputs (line, fp_ng))
		 || (EOF == fputc ('\n', fp_ng)))
	       {
		  log_error ("Write to %s failed.", New_Groups_File);
		  (void) fclose (fp_ng);
		  return -1;
	       }
	     num++;
	  }
	
	if (-1 == slrn_fclose (fp_ng))
	  return -1;
	
	log_message ("%d new groups found.", num);
	
	
	parse_error_label:

	if (parse_error)
	  {
	     log_message ("%s appears corrupt, expected to see see: NEWGROUPS yymmdd hhmmss GMT, I will patch the file up for you.",
			  New_Groups_File);
	  }
     }
      
   if (NULL == (fp_time = fopen (New_Groups_Time_File, "w")))
     return -1;
   
#if defined(VMS) || defined(__BEOS__)
   /* gmtime is broken on BEOS */
   tm_struct = localtime (&tloc);
   fprintf (fp_time, "NEWGROUPS %02d%02d%02d %02d%02d%02d",
            tm_struct->tm_year, 1 + tm_struct->tm_mon,
            tm_struct->tm_mday, tm_struct->tm_hour,
            tm_struct->tm_min, tm_struct->tm_sec);
#else
   tm_struct = gmtime (&tloc);
   fprintf (fp_time, "NEWGROUPS %02d%02d%02d %02d%02d%02d GMT",
	    tm_struct->tm_year, 1 + tm_struct->tm_mon,
	    tm_struct->tm_mday, tm_struct->tm_hour,
	    tm_struct->tm_min, tm_struct->tm_sec);
#endif

   return slrn_fclose (fp_time);
}
   
/*{{{ Compatibility functions (slrn_error, etc... ) */
void slrn_exit_error (char *fmt, ...) /*{{{*/
{
   va_list ap;

   if (fmt != NULL)
     {
	va_start (ap, fmt);
	va_log_error (fmt, ap);
	va_end (ap);
     }

   close_servers ();
   close_log_files ();
   
   exit (1);
}

/*}}}*/


static Terminate_Slrn_Pull_Requested;
static void sigint_handler (int sig) /*{{{*/
{
   (void) sig;

   SLKeyBoard_Quit = 1;
   Terminate_Slrn_Pull_Requested = 1;
}

/*}}}*/

static void init_signals (void) /*{{{*/
{
   SLsignal_intr (SIGINT, sigint_handler);
   SLsignal_intr (SIGHUP, sigint_handler);
   SLsignal_intr (SIGTERM, sigint_handler);
   SLsignal_intr (SIGPIPE, SIG_IGN);
}

/*}}}*/

static int handle_interrupts (void) /*{{{*/
{
   if (Terminate_Slrn_Pull_Requested)
     {
	log_error ("Performing shutdown.");
	write_active ();
	slrn_exit_error ("Slrn exiting on signal.");
     }
   
   return 0;
}

/*}}}*/


void slrn_error_now (char *fmt, ...) /*{{{*/
{
   va_list ap;
   va_start(ap, fmt);
   va_log_error (fmt, ap);
   va_end (ap);
}

/*}}}*/

void slrn_error (char *fmt, ...) /*{{{*/
{
   va_list ap;
   
   va_start(ap, fmt);
   va_log_error (fmt, ap);
   va_end (ap);
}

/*}}}*/

int slrn_message_now (char *fmt, ...) /*{{{*/
{
   va_list ap;
   va_start(ap, fmt);
   va_log_message (fmt, ap);
   va_end (ap);
   
   return 0;
}

/*}}}*/

int slrn_message (char *fmt, ...) /*{{{*/
{
   va_list ap;
   va_start(ap, fmt);
   va_log_message (fmt, ap);
   va_end (ap);
   
   return 0;
}

/*}}}*/


/*}}}*/

static char *read_header_from_file (char *file)
{
   FILE *fp;
   char line [NNTP_BUFFER_SIZE];
   char *mbuf;
   unsigned int buffer_len, buffer_len_max;
   
   if (NULL == (fp = fopen (file, "r")))
     return NULL;

   mbuf = NULL;
   buffer_len_max = buffer_len = 0;
   
   while (NULL != fgets (line, sizeof(line), fp))
     {
	unsigned int len;
	
	if (*line == '\n')
	  break;

	len = strlen (line);
	
	if (len + buffer_len + 4 > buffer_len_max)
	  {
	     char *new_mbuf;
	     
	     buffer_len_max += 4096 + len;
	     new_mbuf = slrn_realloc (mbuf, buffer_len_max, 0);
	     
	     if (new_mbuf == NULL)
	       {
		  slrn_free (mbuf);
		  mbuf = NULL;
		  break;
	       }
	     mbuf = new_mbuf;
	  }
   
	strcpy (mbuf + buffer_len, line);
	buffer_len += len;
     }
   
   fclose (fp);
   return mbuf;
}

static int sort_int_cmp (unsigned int *a, unsigned int *b)
{
   if (*a > *b) return 1;
   if (*a == *b) return 0;
   return -1;
}

static int create_overview_for_dir (Active_Group_Type *g, unsigned int *nums, unsigned int n_nums)
{
   char file [SLRN_MAX_PATH_LEN + 1];
   char dir [SLRN_MAX_PATH_LEN + 1];
   FILE *xov_fp;
   unsigned i;
   void (*qsort_fun) (char *, unsigned int, int, int (*)(unsigned int *, unsigned int *));

   log_message ("Creating Overview file for %s...", g->name);
   
   xov_fp = open_xover_file (g, "w");
   
   if (xov_fp == NULL)
     return -1;
   
   if (-1 == slrn_dircat (SlrnPull_Spool_News_Dir, g->dirname, dir))
     return -1;
   
   if ((nums != NULL) && (n_nums != 0))
     {
	qsort_fun = (void (*)(char *, unsigned int, int, 
			      int (*)(unsigned int *, unsigned int *))) 
	  qsort;
	
	(*qsort_fun) ((char *) nums, n_nums, sizeof (unsigned int), sort_int_cmp);
	
	g->min = nums[0];
     }
   else g->min = g->max + 1;

   for (i = 0; i < n_nums; i++)
     {
	char *header;
	Slrn_XOver_Type xov;
	char buf[32];
	int id;
	struct stat st;
	
	id = (int) nums [i];
	
	sprintf (buf, "%d", id);
	
	if (-1 == slrn_dircat (dir, buf, file))
	  continue;
	
	if (-1 == stat (file, &st))
	  {
	     log_error ("Unable to stat %s.", file);
	     continue;
	  }
	
	if (0 == S_ISREG(st.st_mode))
	  continue;

	header = read_header_from_file (file);
	if (header == NULL)
	  continue;
	
	if (-1 == xover_parse_head (id, header, &xov))
	  {
	     slrn_free (header);
	     continue;
	  }
	
	if (-1 == write_xover_line (xov_fp, &xov))
	  {
	     slrn_free (header);
	     slrn_free (xov.subject_malloced);
	     fclose (xov_fp);
	     return -1;
	  }
	
	slrn_free (header);
	slrn_free (xov.subject_malloced);
     }
   
   return slrn_fclose (xov_fp);
}

static int expire_group (Active_Group_Type *g) /*{{{*/
{
   DIR *dp;
   struct dirent *df;
   char dir [SLRN_MAX_PATH_LEN + 1];
   char file [SLRN_MAX_PATH_LEN + 1];
   unsigned int *ok_names;
   unsigned int num_ok_names, max_num_ok_names;
   unsigned int min_not_expired, num_expired;
   unsigned int i, n, new_num_ok_names;
   time_t expire_time;
   int perform_expire = 1;
   
   if (g->expire_days == 0)
     perform_expire = 0;

   if (-1 == slrn_dircat (SlrnPull_Spool_News_Dir, g->dirname, dir))
     return -1;

   dp = opendir (dir);
   if (dp == NULL)
     {
	log_error ("opendir %s failed.", dir);
	return -1;
     }
   
   ok_names = NULL;
   max_num_ok_names = num_ok_names = 0;
   
   while (NULL != (df = readdir (dp)))
     {
	char *name, *p;
	
	name = df->d_name;
	
	/* Look for names composed of digits.  Skip others. */
	p = name;
	while (*p && isdigit (*p)) p++;
	if (*p != 0) continue;
	
	if (1 != sscanf (name, "%u", &n))
	  continue;		       /* hmm... I'm paranoid. */

	if (num_ok_names == max_num_ok_names)
	  {
	     max_num_ok_names += 500;
	     ok_names = (unsigned int *) slrn_realloc ((char *) ok_names, max_num_ok_names * sizeof (unsigned int), 1);
	     if (ok_names == NULL)
	       {
		  log_error ("malloc error. Unable to expire group %s.", g->name);
		  closedir (dp);
		  return -1;
	       }
	  }
	
	ok_names [num_ok_names] = n;
	num_ok_names++;
     }
   
   closedir (dp);

   min_not_expired = 0xFFFFFFFF;
   time (&expire_time);
   expire_time -= g->expire_days * (24 * 60 * 60);
   num_expired = 0;
   
   new_num_ok_names = 0;
   
   /* In this loop, articles are expired and the ok_names list is pruned 
    * to only consist of non-expired articles.  The resulting list will
    * be used to create the overview database.
    */

   for (i = 0; i < num_ok_names; i++)
     {
	char buf[32];
	struct stat st;
	
	n = ok_names[i];
	ok_names [new_num_ok_names] = n;
	
	if ((perform_expire == 0)
	    || (n > min_not_expired))
	  {
	     new_num_ok_names++;
	     continue;
	  }
	
	sprintf (buf, "%d", n);
	
	if (-1 == slrn_dircat (dir, buf, file))
	  continue;
	
	if (-1 == stat (file, &st))
	  {
	     log_error ("Unable to stat %s.", file);
	     continue;
	  }
	
	if (0 == S_ISREG(st.st_mode))
	  continue;
	
	if (st.st_mtime > expire_time)
	  {
	     if (n < min_not_expired)
	       min_not_expired = n;
	     
	     new_num_ok_names++;
	     continue;
	  }

	if (-1 == slrn_delete_file (file))
	  log_error ("Unable to expire %s.", file);
	else
	  num_expired++;
     }
   
   if (num_expired) log_message ("%u articles expired in %s.", num_expired, g->name);
   
   (void) create_overview_for_dir (g, ok_names, new_num_ok_names);
   
   slrn_free ((char *) ok_names);
   
   return 0;
}

/*}}}*/

static int do_expire (void) /*{{{*/
{
   Active_Group_Type *g;
   
   g = Active_Groups;
   
   init_signals ();

   while (g != NULL)
     {
	(void) expire_group (g);
	g = g->next;
     }
   
   return write_active ();
}

/*}}}*/


static char *Auth_User_Name;
static char *Auth_Password;

static int get_authorization (char *host, char **name, char **pass)
{
   (void) host;

   *name = Auth_User_Name;
   *pass = Auth_Password;

   return 0;
}

static int read_authinfo (void)
{
   FILE *fp;
   char file [SLRN_MAX_PATH_LEN + 1];
   static char pass[256];
   static char name[256];
   
   Auth_Password = NULL;
   Auth_User_Name = NULL;
   
   if (-1 == slrn_dircat (SlrnPull_Dir, "authinfo", file))
     return -1;
   
   if (NULL == (fp = fopen (file, "r")))
     return 0;
   
   if ((NULL == fgets (name, sizeof (name), fp))
       || (NULL == fgets (pass, sizeof (pass), fp)))
     {
	log_error ("Error reading name and password from %s.");
	fclose (fp);
	
	return -1;
     }
   
   fclose (fp);

   slrn_trim_string (pass);
   slrn_trim_string (name);
   
   if ((0 == strlen (pass)) || (0 == strlen (name)))
     {
	log_error ("Invalid name or password.");
	return -1;
     }
   
   Auth_User_Name = name;
   Auth_Password = pass;

   NNTP_Authorization_Hook = get_authorization;
   
   return 0;
}

	
     
     
   
   
