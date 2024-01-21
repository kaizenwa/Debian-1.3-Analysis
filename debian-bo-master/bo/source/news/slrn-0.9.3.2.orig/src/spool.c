/* Local spool support for slrn added by Olly Betts <olly@mantis.co.uk> */
#include "config.h"
#include "slrnfeat.h"

#define DEBUG_SPOOL 0
#define DEBUG_SPOOL_FILENAME "SLRNDEBUG"

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <string.h>
#include <errno.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include <time.h>

#include "misc.h"
#include "slrn.h"

#ifndef SLRN_SPOOL_ROOT
# define SLRN_SPOOL_ROOT "/var/spool/news" /* a common place for the newsspool */
#endif

#ifndef SLRN_SPOOL_NOV_ROOT
# define SLRN_SPOOL_NOV_ROOT SLRN_SPOOL_ROOT
#endif

#ifndef SLRN_SPOOL_NOV_FILE
# define SLRN_SPOOL_NOV_FILE ".overview"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# define NEED_D_NAMLEN
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#include <limits.h>

static int spool_put_server_cmd (char *, char *, unsigned int );
static int spool_select_group (char *, int *, int *);
static void spool_close_server (void);
static int spool_has_cmd (char *);
static int spool_initialize_server (void);
static char *spool_read_line (char *, unsigned int );
static int spool_xpat_cmd (char *, int, int, char *);
static int spool_xgtitle_cmd (char *);
static int spool_select_article (int, char *);
static int spool_xhdr_command (char *, int, char *, unsigned int);
static int spool_list_newsgroups (void);
static int spool_list_active (void);
static char *spool_read_xover (char *, unsigned int);
static char *spool_read_xpat (char *, unsigned int);

static Slrn_Server_Obj_Type Spool_Server_Obj;

/* some state that the NNTP server would take care of if we were using one */
static FILE *Spool_fh_local=NULL;
static char *Spool_Group=NULL;
static char *Spool_Group_Name;

static FILE *Spool_fh_nov=NULL; /* we use the overview file lots, so keep it open */
static int Spool_cur_artnum = 0;

/* These are set when the group is selected. */
static int Spool_Max_Artnum = 0;
static int Spool_Min_Artnum = 0;

static int Spool_Doing_XOver;	       /* if non-zero, reading from ,overview */
static int Spool_Doing_XPat;	       /* reading xpat */
static int Spool_fhead=0; /* if non-0 we're emulating "HEAD" so stop on blank line */
static int Spool_fFakingActive=0; /* if non-0 we're doing funky stuff with MH folders */

static int spool_fake_active( char *);
static char *spool_fakeactive_read_line(char *, int);
static int Spool_fakeactive_newsgroups=0;

#if DEBUG_SPOOL
# include <stdarg.h>
static void spool_debug (char *fmt, ...)
{
   FILE *fp = NULL;
   va_list ap;

   if (fp == NULL)
     {
	fp = fopen (DEBUG_SPOOL_FILENAME, "w");
	if (fp == NULL)
	  return;
     }

   fprintf (fp, "%lu: ", (unsigned long) clock ());

   if (fmt == NULL)
     {
	fputs ("(NULL)", fp);
     }
   else
     {
	va_start(ap, fmt);
	vfprintf(fp, fmt, ap);
	va_end (ap);
     }

   putc ('\n', fp);
   fflush (fp);
}
#endif

/* close any current file (unless it's the overview file) and NULL the FILE* */
static int spool_fclose_local (void)
{
   int res = 0;

   Spool_fhead=0;
   Spool_fFakingActive=0;

   if (Spool_fh_local != NULL)
     {
	if (Spool_fh_local != Spool_fh_nov)
	  res = fclose(Spool_fh_local);
	Spool_fh_local=NULL;
     }
   return res;
}

static FILE *spool_open_nov_file (void)
{
   char *p, *q;
   FILE *fp;
   
   /* The spool_dircat function will exit if it fails to malloc. */
   p = slrn_spool_dircat (Slrn_Nov_Root, Spool_Group_Name, 1);
   q = slrn_spool_dircat (p, Slrn_Nov_File, 0);
   
   fp = fopen (q,"rb");
   SLFREE(q);
   SLFREE(p);
   
   return fp;
}

static int Spool_XOver_Next;
static int Spool_XOver_Max;
static int Spool_XOver_Min;

static int spool_nntp_xover (int min, int max)
{
   int i, ch;
   long fp;

#if DEBUG_SPOOL
   spool_debug ("spool_nntp_xover(%d,%d)", min, max);
#endif
   
   Spool_Doing_XOver = 0;

   spool_fclose_local ();

   if (max > Spool_Max_Artnum)
     max = Spool_Max_Artnum;
   
   if (min < Spool_Min_Artnum)
     min = Spool_Min_Artnum;
   
   if (Spool_Server_Obj.sv_has_xover)
     {
	Spool_fh_local = Spool_fh_nov;
	if (Spool_fh_local == NULL)
	  return -1;

	/* find first record in range in overview file */
	/* first look at the current position and see where we are */
	/* this is worth trying as slrn will often read a series of ranges */
	fp = ftell (Spool_fh_local);

	if ((1 != fscanf (Spool_fh_local,"%d", &i))
	    || (i > min))
	  {
	     /* looks like we're after the start of the range */
	     /* therefore we'll have to rescan the file from the start */
	     rewind (Spool_fh_local);
	     i = -1;
	     /* this might be improved by doing some binary-chop style searching */
	  }
	else
	  {
	     while (((ch = getc(Spool_fh_local)) != '\n')
		    && (ch != EOF))
	       ; /* do nothing */
	     
	     if (ch == EOF)
	       {
		  rewind (Spool_fh_local);
		  i = -1;
	       }
	  }

#if DEBUG_SPOOL
	spool_debug ("Starting with i=%d",i);
#endif

	while (i < min)
	  {
	     fp = ftell( Spool_fh_local );
	     if (1 != fscanf(Spool_fh_local,"%d", &i))
	       return -1;

	     while (((ch = getc (Spool_fh_local)) != '\n')
		    && (ch != EOF))
	       ; /* do nothing */
	  }

	fseek (Spool_fh_local, fp, SEEK_SET); /* reset to start of line */
     }

   Spool_XOver_Next = Spool_XOver_Min = min;
   Spool_XOver_Max = max;
   Spool_Doing_XOver = 1;
   
   return OK_XOVER;
}

static char *spool_read_xover (char *the_buf, unsigned int len)
{
   char *p;
   long pos;
   
#if DEBUG_SPOOL
   spool_debug ("spool_read_xover");
#endif

   if (Spool_Doing_XOver == 0)
     return NULL;

   if (Spool_XOver_Next > Spool_XOver_Max) 
     {
	Spool_Doing_XOver = 0;
	return NULL;
     }

   pos = ftell (Spool_fh_nov);
   
   p = fgets (the_buf, len, Spool_fh_nov);
   if (p != NULL)
     {
	unsigned int buflen;

	buflen = strlen (the_buf);
	if (buflen && (the_buf[buflen - 1] == '\n'))
	  the_buf [buflen - 1] = 0;
     }
   
   /* check if we've reached the end of the requested range */
   if (p != NULL)
     {
	Spool_XOver_Next = atoi (p);
	if (Spool_XOver_Next > Spool_XOver_Max)
	  {
	     fseek (Spool_fh_nov, pos, SEEK_SET);
	     p = NULL;
	  }
	else Spool_XOver_Next++;
     }

#if DEBUG_SPOOL
   spool_debug (p);
#endif
   
   if (p == NULL) 
     Spool_Doing_XOver = 0;
   
   return p;
}

static int spool_find_artnum_from_msgid (char *msgid)
{
   char buf [4096];
   char *p;
   int n;
   
#if DEBUG_SPOOL
   spool_debug ("spool_find_artnum_from_msgid('%s')", msgid);
#endif

   if (Slrn_Server_Obj->sv_has_xover == 0)
     {
	unsigned int len = strlen (msgid);
   
	for (n = Spool_Min_Artnum; n <= Spool_Max_Artnum; n++)
	  {
	     if (-1 == spool_xhdr_command ("Message-Id", n, buf, sizeof (buf)))
	       continue;
	     
	     p = slrn_skip_whitespace (buf);
	     if (0 == strncmp (p, msgid, len))
	       return n;
	  }
   
	return -1;
     }
   
   if (OK_XOVER != spool_nntp_xover (1, INT_MAX))
     return -1;

   while (NULL != spool_read_xover (buf, sizeof(buf)))
     {
	char *q;

	/* 5th field is message id. */

	if (NULL == (p = slrn_strchr(buf, '\t'))) continue;
	if (NULL == (p = slrn_strchr(p + 1, '\t'))) continue;
	if (NULL == (p = slrn_strchr(p + 1, '\t'))) continue;
	if (NULL == (p = slrn_strchr(p + 1, '\t'))) continue;

	p++; /* skip tab */
	q = slrn_strchr(p,'\t');
	if (q != NULL) *q='\0';

	if (0 == strcmp(msgid, p))
	  {
#if DEBUG_SPOOL
	     spool_debug ("spool_find_artnum_from_msgid() returns %d",atoi(buf));
#endif
	     Spool_Doing_XOver = 0;
	     return atoi(buf);
	  }
     }
   
#if DEBUG_SPOOL
   spool_debug ("spool_find_artnum_from_msgid() found no match");
#endif
   
   Spool_Doing_XOver = 0;
   return -1;
}


static FILE *spool_open_article_num (int num)
{
   char buf [SLRN_MAX_PATH_LEN];
   
   sprintf (buf, "%s/%d", Spool_Group, num);
   
   return fopen (buf,"r");
}

static int spool_article_num_exists (int num)
{
   char buf [SLRN_MAX_PATH_LEN];
   
   sprintf (buf, "%s/%d", Spool_Group, num);

   if (1 == slrn_file_exists (buf))
     return 0;
   
   return -1;
}

static int spool_is_name_all_digits (char *p)
{
   char *pmax;

   pmax = p + strlen (p);
   while (p < pmax)
     {
	if (!isdigit (*p))
	  return 0;
	p++;
     }
   return 1;
}

/*{{{ The routines in this fold implement the sv_put_server_cmd */

static int spool_nntp_head (int id, char *msgid, int *real_id)
{
   spool_fclose_local();

   if (id == -1)
     {
	if (msgid == NULL) id = Spool_cur_artnum;
	else id = spool_find_artnum_from_msgid (msgid);
     }

   if (real_id != NULL) *real_id = id;
   
   if ((id == -1) 
       || (NULL == (Spool_fh_local = spool_open_article_num (id))))
     return ERR_NOARTIG; /* No such article in this group */
   
   Spool_cur_artnum = id;
   Spool_fhead = 1; /* set flag to stop after headers */
   
   return OK_HEAD; /* Head follows */
}

static int spool_nntp_next (int *id)
{
   int i;
   
   spool_fclose_local();

   /* !HACK! better to find value from overview file or active file in case the group grows while we're in it? */
   for (i = Spool_cur_artnum + 1; i <= Spool_Max_Artnum; i++)
     {
	if (-1 != spool_article_num_exists (i))
	  {	
	     Spool_cur_artnum = i;
	     if (id != NULL) *id = i;
	     
#if DEBUG_SPOOL
	     spool_debug ("NEXT found article %d",Spool_cur_artnum);
#endif

	     return OK_NOTEXT; /* No text sent -- stat, next, last */
	  }
     }

#if DEBUG_SPOOL
   spool_debug ("No NEXT -- %d > %d", Spool_cur_artnum, Spool_Max_Artnum);
#endif
   
   return ERR_NONEXT; /* No next article in this group */
}

static int spool_nntp_newgroups (char *line, char *buf, unsigned int len)
{
   /* expect something like "NEWGROUPS 960328 170939 GMT" GMT is optional */
   char *p;
   int Y,M,D,h,m,s;
   int c;
   int fGMT;
   int n;
   time_t threshold;
   struct tm t;
   long fpos;
   char buf1[512];
   int ch;
   int found;
   int first;

   (void) buf; (void) len;

   /* The %n format specification returns the number of charcters parsed up
    * to that point.  I do not know how portable this is.
    */
   c = sscanf(line+9,"%02d%02d%02d %02d%02d%02d%n",&Y,&M,&D,&h,&m,&s,&n);
   assert(c==6); /* group.c should have sanity checked the line */

   p = slrn_skip_whitespace(line + 9 + n);
   fGMT = (0 == strncmp (p, "GMT", 3));

   /* this !HACK! is good until 2051 -- round at 50 cos the RFC says to */
   Y += ( Y>50 ? 1900 : 2000 );
   /* for full version, use this instead:
    * if (Y<=50) Y+=100;
    * yr = this_year();
    * Y += ((yr-51)/100*100);
    */
#if DEBUG_SPOOL
   spool_debug ("%d read, %04d-%02d-%02d %d:%02d:%02d %s",
		c,Y,M,D,h,m,s,fGMT?"GMT":"(local)");
#endif
   t.tm_year = Y - 1900; /* tm_year is years since 1900 */
   t.tm_mon = M - 1; /* tm_mon has January as 0 */
   t.tm_mday = D;
   t.tm_hour = h;
   t.tm_min = m;
   t.tm_sec = s;
   
   /* This used to be 0, but the man page indicates that -1 should be used
    * if the information is not available.
    */
   t.tm_isdst = -1; /* say it's not DST (? !HACK!) */
   threshold = mktime( &t );

#if DEBUG_SPOOL
   spool_debug ("threshold (local) = %d",threshold);
#endif

   if (fGMT)
     {
	/* !HACK! is this correct? */
	threshold -= mktime (gmtime (&threshold)) - threshold;

#if DEBUG_SPOOL
	spool_debug ("threshold (GMT) = %d",threshold);
#endif
     }

   spool_fclose_local();
   Spool_fh_local = fopen (Slrn_ActiveTimes_File, "r");
   if (Spool_fh_local == NULL)
     {
	/* !HACK! at this point, it would be nice to be able to check for
	 * recently created directories with readdir() and to return those
	 * which would be useful for reading MH folders, etc.  This would
	 * be more than a little slow for a true newsspool though.
	 *
	 * Hmm, looked into this and it seems that you can't use the mtime
	 * or ctime to decide when a directory was created.  Hmmm.
	 */
#if DEBUG_SPOOL
	spool_debug ("Couldn't open active.times");
#endif
#if 0
	slrn_message ("Couldn't open active.times");
	return OK_NEWGROUPS;
#else
	return ERR_FAULT; /* Program fault, command not performed */
#endif
     }

   /* chunk size to step back through active.times by
    * when checking for new groups */
   /* 128 should be enough to find the last line in the probably most
    * common case of no new groups */
#define SLRN_SPOOL_ACTIVETIMES_STEP 128

   /* find start of a line */
   fseek (Spool_fh_local, 0, SEEK_END ); /* !HACK! check return value from this and all the rest... */
   fpos = ftell(Spool_fh_local);
#if DEBUG_SPOOL
   spool_debug ("ftell=%ld errno=%d (%s)",fpos,
		errno, strerror(errno));
   errno = 0;
#endif

   found=0;
   first=1;

   while (!found)
     {
	int i, len1;

	len1 = SLRN_SPOOL_ACTIVETIMES_STEP;

	if (fpos < (long)len1) len1=fpos; /* don't run of the start of the file */
	fpos -= len1;
	fseek (Spool_fh_local, fpos, SEEK_SET );
	if (fpos == 0) break;

#if DEBUG_SPOOL
	spool_debug ("ftell=%ld errno=%d (%s)", ftell (Spool_fh_local),
		     errno, strerror(errno));
#endif

	if (first)
	  {
	     /* on the first pass, we want to ignore the last byte \n at eof */
	     --len1;
	     first=0;
	  }

	for (i = 0; i < len1; i++)
	  {
	     ch = getc(Spool_fh_local);

	     assert(ch!=EOF); /* shouldn't happen */
	     if (ch != '\n') continue;

	     while ((i < len1)
		    && (NULL != fgets (buf1, sizeof(buf1), Spool_fh_local)))
	       {
		  i -= strlen(buf1);
		  p = buf1;
		  while (*p && (0 == isspace (*p)))
		    p++;

		  if (atol(p) < threshold)/* or <= ? !HACK! */
		    {
		       found = 1;
		       break;
		    }
	       }
	     break;
	  }
     }

   fpos=ftell(Spool_fh_local);
#if DEBUG_SPOOL
   spool_debug ("ftell=%ld errno=%d (%s)",fpos, errno, strerror(errno));
#endif

   while (NULL != fgets( buf1, sizeof(buf1), Spool_fh_local ))
     {
	p = buf1;
	while (*p && (0 == isspace (*p))) p++;

	if (atol(p) >= threshold) /* or just > ? !HACK! */
	  {
	     fseek( Spool_fh_local, fpos, SEEK_SET );
	     break;
	  }
	fpos = ftell(Spool_fh_local);
     }

   return OK_NEWGROUPS; /* New newsgroups follow */
}

typedef struct
{
   char *name;
   unsigned int len;
   int (*f) (char *, char *, unsigned int);
}
Spool_NNTP_Map_Type;

static Spool_NNTP_Map_Type Spool_NNTP_Maps [] =
{
     {"NEWGROUPS", 9, spool_nntp_newgroups},
     {NULL, 0, NULL}
};

static int spool_put_server_cmd (char *line, char *buf, unsigned int len)
{
   Spool_NNTP_Map_Type *nntpmap;

#if DEBUG_SPOOL
   spool_debug ("spool_put_server_cmd('%s')", line);
#endif

   nntpmap = Spool_NNTP_Maps;
   while (nntpmap->name != NULL)
     {
	if (!slrn_case_strncmp ((unsigned char *)nntpmap->name,
				(unsigned char *) line, nntpmap->len))
	  return (*nntpmap->f)(line, buf, len);

	nntpmap++;
     }

#if DEBUG_SPOOL
   spool_debug ("Hmmm, didn't know about that command");
#endif
   return ERR_COMMAND;
}

/*}}}*/


static int spool_read_minmax_from_dp (DIR *dp, int *min, int *max)
{
   struct dirent *ep;
   char *p;
   long l;
   long hi = 0;
   long lo = LONG_MAX;

   /* Scan through all the files, checking the ones with numbers for names */
   while ((ep = readdir(dp)) != NULL)
     {
	p = ep->d_name;
#ifdef NEED_D_NAMLEN
	p[ep->d_namlen] = 0;
#endif
	if (!isdigit(*p)) continue;

	if (0 == spool_is_name_all_digits (p))
	  continue;

	if (0 == (l = atol (p)))
	  continue;

	if (l < lo)
	  lo = l;
	if (l > hi)
	  hi = l;
     }

   if ((lo == LONG_MAX)
       && (hi == 0))
     return -1;

   *min=lo;
   *max=hi;

   return 0;
}

/* Get the lowest and highest article numbers by the simple method
 * or looking at the files in the directory.
 * Returns 1 on success, 0 on failure
 */
static int spool_read_minmax_from_dir( int *min, int *max, char *dir )
{
   /* This is adapted from some code in INN's ng.c */
   DIR *dp;

   if (dir == NULL) dir = ".";
   
   /* I suspect this is very unlikely to fail */
   if ((dp = opendir(dir)) == NULL)
     return 0;

   if (-1 == spool_read_minmax_from_dp (dp, min, max))
     {
	*min = 1;
	*max = 0;
     }

   (void) closedir(dp);
   return 1;
}

#if SPOOL_ACTIVE_FOR_ART_RANGE
/* Get the lowest and highest article numbers from the active file
 * Returns 1 on success, 0 on failure
 * (failure => active file didn't open, or the group wasn't in it)
 */
static int spool_read_minmax_from_active( char *name, int *min, int *max )
{
   char buf[512];
   unsigned int len;

   spool_fclose_local();
   Spool_fh_local = fopen(Slrn_Active_File,"r");
   if (Spool_fh_local == NULL) return 0;

   len = strlen(name);
   buf[len] = 0;		       /* init this for test below */

   while (NULL != fgets (buf, sizeof(buf), Spool_fh_local))
     {
	/* quick, crude test first to see if it could possibly be a match */
	if ((buf[len] == ' ')
	    && (0 == memcmp (buf, name, len)))
	  {
	     spool_fclose_local ();
	     if (2 != sscanf (buf + len + 1, "%d%d", max, min))
	       return 0;

	     Spool_Max_Artnum = *max;
# if DEBUG_SPOOL
	     spool_debug ("from active:%s %d %d",
			  name,*min,*max);
# endif
	     return 1;
	  }
	buf[len] = 0;
     }
   spool_fclose_local();

   return 0;
}
#endif

/* Get the lowest and highest article numbers from the overview file
 * Returns 1 on success, 0 on failure
 */
static int spool_read_minmax_from_overview (char *name, int *min, int *max)
{
   /* chunk size to step back through .overview files by
    * when trying to find start of last line */
#define SPOOL_NOV_STEP 1024
   /* If there's no .overview file, get min/max info from the active file */
   /* ditto if .overview file is empty */
   int ch;
   long fpos;
   int found;
   int first;

   (void) name;
   
   /* !HACK! this assumes the overview file is rewound */
   Spool_Server_Obj.sv_has_xover = ((Spool_fh_nov != NULL)
				    && (1 == fscanf (Spool_fh_nov,"%d", min)));

   if (0 == Spool_Server_Obj.sv_has_xover)
     return 0;

   /* find start of last line */
   fseek (Spool_fh_nov, 0, SEEK_END); /* !HACK! check return value from this and all the rest... */

   fpos = ftell (Spool_fh_nov);

#if DEBUG_SPOOL
   spool_debug("ftell=%ld errno=%d (%s)",
	       fpos, errno, strerror(errno));
   errno = 0;
#endif

   found=0;
   first=1;

   while (!found && (fpos > 0))
     {
	int i, len;

	len = SPOOL_NOV_STEP;

	/* don't run of the start of the file */
	if (fpos < (long)len) len = fpos;

	fpos -= len;
	fseek(Spool_fh_nov, fpos, SEEK_SET);

#if DEBUG_SPOOL
	spool_debug("ftell=%ld errno=%d (%s)",
		    ftell (Spool_fh_nov), errno, strerror(errno));
	errno = 0;
#endif

	if (first)
	  {
	     /* on the first pass, we want to ignore the last byte \n at eof */
	     --len;
	     first = 0;
	  }

	for(i = 0; i < len; i++ )
	  {
	     ch = getc(Spool_fh_nov);

	     assert(ch!=EOF); /* shouldn't happen */
	     if (ch =='\n')
	       found = i + 1; /* and keep going in case there's another */
	  }
     }

   fseek (Spool_fh_nov, fpos + found, SEEK_SET);

#if DEBUG_SPOOL
   spool_debug("ftell=%ld errno=%d (%s)",
	       ftell (Spool_fh_nov), errno, strerror(errno));
   errno = 0;
#endif

   fscanf (Spool_fh_nov, "%d", max);

   rewind (Spool_fh_nov);

#if DEBUG_SPOOL
   spool_debug ("%s %d %d",name,*min,*max);
#endif
   return 1;
}

static int spool_select_group (char *name, int *min, int *max)
{
   /* close any open files */
   spool_fclose_local();
   
   if (Spool_fh_nov != NULL)
     {
	fclose (Spool_fh_nov);
	Spool_fh_nov = NULL;
     }
   
   slrn_free (Spool_Group);
   slrn_free (Spool_Group_Name);
   
   Spool_Group = slrn_spool_dircat (Slrn_Spool_Root, name, 1);
   Spool_Group_Name = slrn_safe_strmalloc (name);
   
#if DEBUG_SPOOL
   spool_debug ("spool_select_group(%s) spool_group dir = %s", name, Spool_Group);
#endif

   Spool_fh_nov = spool_open_nov_file ();

   if (!spool_read_minmax_from_overview (name, min, max)
#if SPOOL_ACTIVE_FOR_ART_RANGE
       && !spool_read_minmax_from_active (name, min, max)
#endif
       )
     {
	/* change directory to the spool directory.  The read_minmax_from_dir
	 * function assumes it.
	 */
#if 0
	if (chdir (Spool_Group))
	  return ERR_NOGROUP;
#endif
	if (!spool_read_minmax_from_dir (min, max, Spool_Group))
	  return -1;
	
     }

   Spool_Max_Artnum = *max;
   Spool_Min_Artnum = *min;

#if DEBUG_SPOOL
   spool_debug ("Group: %s %d - %d", name, *min, *max);
#endif
   return OK_GROUP;
}

static int Spool_Server_Inited = 0;

static void spool_close_server (void)
{
#if DEBUG_SPOOL
   spool_debug ("spool_close_server()");
#endif

   slrn_free (Spool_Group);
   Spool_Group = NULL;

   spool_fclose_local();

   if (NULL != Spool_fh_nov)
     {
	fclose (Spool_fh_nov);
	Spool_fh_nov = NULL;
     }
   Spool_Server_Inited = 0;
}

static int spool_has_cmd (char *cmd)
{
   (void) cmd;
   return 0; /* deny everything */
}

static int spool_initialize_server (void)
{
   if (Spool_Server_Inited) spool_close_server ();

#if DEBUG_SPOOL
   spool_debug ("spool_initialize_server(%s)", host);
#endif

   if (2 != slrn_file_exists (Slrn_Spool_Root))
     {
	slrn_message_now ("Local spool directory '%s' doesn't exist.", Slrn_Spool_Root);
	return -1;
     }

   /* I think it's better to think that the *server* has XOVER, but
    * some (or all) groups may not.
    * So set this to 1 here, and then to 0 or 1 in spool_select_group if we
    * find an overview file
    */
   Spool_Server_Obj.sv_has_xover = 1;
   Spool_Server_Inited = 1;
   return 0;
}

static char *spool_read_line (char *line, unsigned int len)
{
   if (Spool_Doing_XPat) return spool_read_xpat (line, len);
   
   if (Spool_Doing_XOver) return spool_read_xover (line, len);
   
   if (Spool_fFakingActive) return spool_fakeactive_read_line (line, len);
   
   if ((NULL == Spool_fh_local)
       || (NULL == fgets (line, len, Spool_fh_local))
       || (Spool_fhead && (line[0]=='\n')))
     {
	spool_fclose_local();
	return NULL;
     }

   len = strlen(line);
   if (len && (line [len - 1] == '\n'))
     line [len-1] = '\0';
   
   return line;
}

typedef struct
{
   int xover_field;
   unsigned int rmin, rmax;
   char header[80];
   char pat[256];
}
Spool_XPat_Type;

static Spool_XPat_Type Spool_XPat_Struct;

static int spool_xpat_match (char *str, char *pat)
{
   /* HACK.  This needs fixed for more general patterns. */
   if (NULL == strstr (str, pat))
     return -1;
   
   return 0;
}

static char *spool_read_xpat (char *buf, unsigned int len)
{
   char tmpbuf [8129];

   if (Spool_XPat_Struct.xover_field == -1)
     {
	unsigned int num;
	
	Spool_Doing_XPat = 0;
	for (num = Spool_XPat_Struct.rmin; num <= Spool_XPat_Struct.rmax; num++)
	  {
	     if (-1 == spool_xhdr_command (Spool_XPat_Struct.header, (int) num,
					   tmpbuf, sizeof (tmpbuf)))
	       continue;
	     
	     if (-1 != spool_xpat_match (tmpbuf, Spool_XPat_Struct.pat))
	       {
		  unsigned int blen;
		  
		  Spool_Doing_XPat = 1;
		  Spool_XPat_Struct.rmin = num + 1;
		  
		  sprintf (buf, "%d ", num);
		  blen = strlen (buf);
		  
		  strncpy (buf + blen, tmpbuf, len - blen);
		  buf[len - 1] = 0;

		  return buf;
	       }
	  }
	
	Spool_XPat_Struct.rmin = Spool_XPat_Struct.rmax + 1;
	return NULL;
     }
   
   /* Not implemented Yet. */
   Spool_Doing_XPat = 0;
   spool_fclose_local ();
   return NULL;
}

   
static int spool_xpat_cmd (char *hdr, int rmin, int rmax, char *pat)
{
   static char *overview_headers [] = 
     {
	"Subject", "From", "Date", "Message-ID", 
	"References", "Bytes", "Lines",
	NULL
     };

   spool_fclose_local ();
   Spool_Doing_XPat = 0;
   memset ((char *) &Spool_XPat_Struct, 0, sizeof (Spool_XPat_Struct));
   
   if (rmin < Spool_Min_Artnum)
     rmin = Spool_Min_Artnum;
   if (rmax > Spool_Max_Artnum)
     rmax = Spool_Max_Artnum;
   
   Spool_XPat_Struct.rmin = rmin;
   Spool_XPat_Struct.rmax = rmax;
   
   /* The memset will guarantee that these are NULL terminated. */
   strncpy (Spool_XPat_Struct.header, hdr, sizeof (Spool_XPat_Struct.header) - 1);
   strncpy (Spool_XPat_Struct.pat, pat, sizeof (Spool_XPat_Struct.pat) - 1);
   
   Spool_XPat_Struct.xover_field = -1;
   
   if (Slrn_Server_Obj->sv_has_xover)
     {
	int field = 0;
	
	while (1)
	  {
	     char *h = overview_headers [field];
	     
	     if (h == NULL) break;
	     if (0 == slrn_case_strcmp ((unsigned char *) h, (unsigned char *) hdr))
	       {
		  Spool_XPat_Struct.xover_field = field;
		  break;
	       }
	     field++;
	  }
     }
   
   Spool_XPat_Struct.xover_field = -1;
   
   if (Spool_XPat_Struct.xover_field != -1)
     {
	Spool_fh_local = spool_open_nov_file ();
	if (Spool_fh_local == NULL)
	  return ERR_COMMAND;
     }
   
   Spool_Doing_XPat = 1;

   return OK_HEAD;
}

static int spool_xgtitle_cmd (char *pattern)
{
   (void) pattern;
   return ERR_COMMAND;
}

static int spool_select_article (int n, char *msgid)
{
   /*    printf("spool_select_article(%d,%s)\n",n,msgid); */

   if (n == -1)
     {
	if ((msgid == NULL) || (*msgid == 0))
	  return -1;
	
	if (-1 == (n = spool_find_artnum_from_msgid (msgid)))
	  return ERR_NOARTIG;
     }

   spool_fclose_local();
   
   if (NULL == (Spool_fh_local = spool_open_article_num (n)))
     return ERR_NOARTIG;
   
   Spool_cur_artnum = n;
   return OK_ARTICLE;
}


/* The hdr string should NOT include the ':' */
static int spool_xhdr_command (char *hdr, int num, char *buf, unsigned int buflen)
{
   char tmpbuf [1024];
   unsigned int colon;

   spool_fclose_local ();
   
   if (NULL == (Spool_fh_local = spool_open_article_num (num)))
     return -1;
   
   Spool_fhead = 1;		       /* stop after headers */
   
   colon = strlen (hdr);

   while (NULL != spool_read_line (tmpbuf, sizeof (tmpbuf)))
     {
	char *b;
	if (slrn_case_strncmp ((unsigned char *) tmpbuf, (unsigned char *) hdr, colon)
	    || (tmpbuf[colon] != ':'))
	  continue;

	/* HACK!!  This needs fixed for folded lines. */
	if (buflen > sizeof (tmpbuf))
	  buflen = sizeof (tmpbuf);
	
	b = tmpbuf + (colon + 1);
	if (*b == ' ') b++;
	strncpy (buf, b, buflen - 1);
	buf[buflen - 1] = 0;
	return 0;
     }
   
   return -1;
}

static int spool_list_newsgroups (void)
{
   spool_fclose_local();
   Spool_fh_local=fopen(Slrn_Newsgroups_File,"r");
   if (!Spool_fh_local)
     {
	/* Use readdir() to return a list of newsgroups read from the
	 * "newsspool" so we can read MH folders, etc.  This would be more
	 * than a little slow for a true newsspool.
	 */
	spool_fake_active(Slrn_Spool_Root);
	Spool_fFakingActive=1;
	Spool_fakeactive_newsgroups=1;
/*	slrn_exit_error("Couldn't open newsgroups file '%s'", NEWSGROUPS); */
     }
   return OK_GROUPS;
}

static int spool_list_active (void)
{
   spool_fclose_local();
   Spool_fh_local=fopen (Slrn_Active_File,"r");
   if (!Spool_fh_local)
     {
	spool_fake_active(Slrn_Spool_Root);
	Spool_fFakingActive=1;
	Spool_fakeactive_newsgroups=0;
	return OK_GROUPS;
	/* Use readdir() to return a list of newsgroups and article ranges read
	 * from the "newsspool" so we can read MH folders, etc.  This would
	 * be more than a little slow for a true newsspool.
	 */
/*	slrn_exit_error("Couldn't open active file '%s'", ACTIVE);*/
     }
   return OK_GROUPS;
}

typedef struct _Spool_DirTree_Type
{
   struct _Spool_DirTree_Type *parent;
   DIR *dp;
   int len;
   long lo, hi;
}
Spool_DirTree_Type;

static Spool_DirTree_Type *Spool_Head;

static char Spool_Buf[256];
static char Spool_nBuf[256];
static int Spool_Is_LeafDir;

static void spool_fake_active_in (char *dir)
{
   char *p;
   DIR *dp;
   Spool_DirTree_Type *tmp;

   p = Spool_Buf + strlen(Spool_Buf);

   if (dir != NULL)
     {
	*p = '/';
	strcpy (p + 1, dir);
     }

   if ((2 != slrn_file_exists (Spool_Buf))
       || (NULL == (dp = opendir (Spool_Buf))))
     {
	*p = 0;
	return;
     }

   Spool_Is_LeafDir = 1;
   tmp = (Spool_DirTree_Type *) slrn_safe_malloc (sizeof(Spool_DirTree_Type));

   tmp->dp = dp;
   tmp->parent = Spool_Head;
   tmp->hi = 0;
   tmp->lo = LONG_MAX;

   if (dir == NULL)
     tmp->len = 1;
   else
     {
	tmp->len = strlen (dir);

	p = Spool_nBuf + strlen(Spool_nBuf);
	if (p != Spool_nBuf) *p++ = '.';
	strcpy (p, dir);
     }

   Spool_Head = tmp;
}

static void spool_fake_active_out (void)
{
   Spool_DirTree_Type *tmp;
   int i;

   (void)closedir (Spool_Head->dp);
   Spool_Is_LeafDir = 0;

   Spool_Buf [strlen(Spool_Buf) - Spool_Head->len - 1] = '\0';

   i = strlen(Spool_nBuf) - Spool_Head->len - 1;

   if (i < 0) i = 0;
   Spool_nBuf[i]='\0';

   tmp = Spool_Head;
   Spool_Head = Spool_Head->parent;
   SLFREE(tmp);
}

static int spool_fake_active (char *path)
{
   strcpy(Spool_Buf, path);
   *Spool_nBuf='\0';
   Spool_Head=NULL;
   spool_fake_active_in (NULL);
   return 0;
}

static char *spool_fakeactive_read_line(char *line, int len)
{
   struct dirent *ep;
   char *p;
   long l;

   (void) len;
   
   emptydir:

   if (!Spool_Head)
     {
      /* we've reached the end of the road */
	Spool_fFakingActive = 0;
	return NULL;
     }

   /* Scan through all the files, checking the ones with numbers for names */
   while ((ep = readdir(Spool_Head->dp)) != NULL)
     {
	p = ep->d_name;
#ifdef NEED_D_NAMLEN
	p[ep->d_namlen] = 0;
#endif
	if ((0 == spool_is_name_all_digits (p))
	    || ((l = atol (p)) == 0))
	  {
	     if (!(p[0]=='.' && (p[1]=='\0' || (p[1]=='.' && p[2]=='\0'))))
	       {
		  spool_fake_active_in(p);
	       }
	     continue;
	  }
	if (l < Spool_Head->lo)
	  Spool_Head->lo = l;
	if (l > Spool_Head->hi)
	  Spool_Head->hi = l;
     }

   if (Spool_Head->lo == LONG_MAX && Spool_Head->hi==0)
     {
	/* assume all leaf directories are valid groups */
	/* non-leaf directories aren't groups unless they have articles in */
	if (!Spool_Is_LeafDir)
	  {
	     spool_fake_active_out();
	     goto emptydir; /* skip empty "groups" */
	  }
	Spool_Head->lo = 1;
     }

   if (Spool_fakeactive_newsgroups)
     {
      /* newsgroups: alt.foo A group about foo */
	sprintf(line,"%s ?\n",Spool_nBuf); /* !HACK! check len */
     }
   else
     {
      /* active: alt.guitar 0000055382 0000055345 y */
	sprintf(line,"%s %ld %ld y\n",Spool_nBuf,Spool_Head->hi,Spool_Head->lo); /* !HACK! check len */
     }
   spool_fake_active_out();
   return line;
}


char *Slrn_Inn_Root;
char *Slrn_Spool_Root;
char *Slrn_Nov_Root;
char *Slrn_Nov_File;
char *Slrn_Active_File;
char *Slrn_ActiveTimes_File;
char *Slrn_Newsgroups_File;

static int spool_init_objects (void)
{
   Spool_Server_Obj.sv_select_group = spool_select_group;
   Spool_Server_Obj.sv_read_line = spool_read_line;
   Spool_Server_Obj.sv_close = spool_close_server;
   Spool_Server_Obj.sv_initialize = spool_initialize_server;
   Spool_Server_Obj.sv_select_article = spool_select_article;
   Spool_Server_Obj.sv_put_server_cmd = spool_put_server_cmd;
   Spool_Server_Obj.sv_xpat_cmd = spool_xpat_cmd;
   Spool_Server_Obj.sv_xhdr_command = spool_xhdr_command;
   Spool_Server_Obj.sv_xgtitle_cmd = spool_xgtitle_cmd;
   Spool_Server_Obj.sv_has_cmd = spool_has_cmd;
   Spool_Server_Obj.sv_list_newsgroups = spool_list_newsgroups;
   Spool_Server_Obj.sv_list_active = spool_list_active;

   Spool_Server_Obj.sv_has_xover = 0;
   Spool_Server_Obj.sv_nntp_xover = spool_nntp_xover;
   Spool_Server_Obj.sv_nntp_head = spool_nntp_head;
   Spool_Server_Obj.sv_nntp_next = spool_nntp_next;

   Slrn_Inn_Root = slrn_safe_strmalloc (SLRN_SPOOL_INNROOT);
   Slrn_Spool_Root = slrn_safe_strmalloc (SLRN_SPOOL_ROOT);
   Slrn_Nov_Root = slrn_safe_strmalloc (SLRN_SPOOL_NOV_ROOT);
   Slrn_Nov_File = slrn_safe_strmalloc (SLRN_SPOOL_NOV_FILE);
   Slrn_Active_File = slrn_safe_strmalloc (SLRN_SPOOL_ACTIVE);
   Slrn_ActiveTimes_File = slrn_safe_strmalloc (SLRN_SPOOL_ACTIVETIMES);
   Slrn_Newsgroups_File = slrn_safe_strmalloc (SLRN_SPOOL_NEWSGROUPS);
   
   return 0;
}

/* This function is used below.  It has a very specific purpose. */
static char *spool_root_dircat (char *file)
{
   char *f;

   if (*file == '/') return file;
   f = slrn_spool_dircat (Slrn_Inn_Root, file, 0);
   SLFREE (file);
   return f;
}

static int spool_select_server_object (void)
{
   Slrn_Server_Obj = &Spool_Server_Obj;
   Slrn_Active_File = spool_root_dircat (Slrn_Active_File);
   Slrn_ActiveTimes_File = spool_root_dircat (Slrn_ActiveTimes_File);
   Slrn_Newsgroups_File = spool_root_dircat (Slrn_Newsgroups_File);
   
   slrn_free (Spool_Server_Obj.sv_name);
   Spool_Server_Obj.sv_name = slrn_safe_strmalloc (Slrn_Spool_Root);
   
   return 0;
}


static void spool_usage (void)
{
   fputs ("--spool options:\n\
",
	  stdout);
   exit (0);
}

static int spool_parse_args (char **argv, int argc)
{
   int i;
   
   for (i = 0; i < argc; i++)
     {
	if (!strcmp (argv[i], "--help"))
	  spool_usage ();
	else break;
     }
   
   return i;
}
