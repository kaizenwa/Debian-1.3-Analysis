/*  Copyright (c) 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#include "slrn.h"
#include "server.h"
#include "misc.h"
#include "util.h"
#include "startup.h"
#if SLRN_USE_SLTCP 
# if SLRN_HAS_NNTP_SUPPORT || SLRN_HAS_GROUP_LENS
#  include "sltcp.h"
#  include "sltcp.c"
# endif
#endif

#if SLRN_HAS_NNTP_SUPPORT
# if !SLRN_USE_SLTCP
#  include "clientlib.c"
# endif
# include "nntplib.c"
# include "nntp.c"
#endif

#if SLRN_HAS_SPOOL_SUPPORT
# include "spool.c"
#endif

int Slrn_Server_Id;
int Slrn_Post_Id;

Slrn_Server_Obj_Type *Slrn_Server_Obj;
Slrn_Post_Obj_Type *Slrn_Post_Obj;

#if SLRN_HAS_PULL_SUPPORT
static int pull_init_objects (void);
static int pull_select_post_object (void);
static int pull_parse_args (char **, int);
int Slrn_Use_Pull_Post;
#endif

#if SLRN_HAS_INEWS_SUPPORT
static FILE *Fp_Inews;
char *Slrn_Inews_Pgm;

static int inews_start_post (void)
{
   /* pipe a message to inews. Its done this way because inews expects the
    * article on stdin. Inews errors WILL mess up the screen.
    */
   /* !HACK! should we use slrn_popen() and slrn_pclose()?
    * They stop the screen getting messed up, but the error messages aren't
    * very appropriate
    */
   if (NULL == (Fp_Inews = popen (Slrn_Inews_Pgm, "w")))
     {
	slrn_error ("Couldn't open pipe to inews! -- Article not posted.");
	return -1;
     }
   return CONT_POST;
}

static int inews_end_post (void)
{
   int res = 0;
   
   if (-1 == pclose (Fp_Inews))
     {
	slrn_error ("pclose() failed -- check if article was posted"); /* !HACK! can we do better? */
	res = -1;
     }
   Fp_Inews=NULL;
   
   return res;
}

static int inews_puts (char *s)
{
   fputs (s, Fp_Inews );
   return 0;
}

static int inews_printf (char *fmt, ...)
{
   va_list ap;

   char buf [1024];
   
   va_start (ap, fmt);
   vsprintf (buf, fmt, ap);
   va_end (ap);
   
   return inews_puts(buf);
}


static Slrn_Post_Obj_Type Inews_Post_Obj;

static int inews_init_objects (void)
{
   Inews_Post_Obj.po_start = inews_start_post;
   Inews_Post_Obj.po_end = inews_end_post;
   Inews_Post_Obj.po_printf = inews_printf;
   Inews_Post_Obj.po_puts = inews_puts;
   Inews_Post_Obj.po_can_post = 1;
   Slrn_Inews_Pgm = slrn_safe_strmalloc (SLRN_INEWS_PROGRAM);
   return 0;
}

static int inews_select_post_object (void)
{
   char inews [SLRN_MAX_PATH_LEN + 1];
   char *p;
   
   strncpy (inews, Slrn_Inews_Pgm, SLRN_MAX_PATH_LEN);
   inews[SLRN_MAX_PATH_LEN - 1] = 0;
   
   p = inews;
   while (*p && (*p != ' ')) p++;
   *p = 0;
   
   if (1 != slrn_file_exists (inews))
     {
	slrn_error ("Unable to locate inews program \"%s\"", inews);
	return -1;
     }
   
   Slrn_Post_Obj = &Inews_Post_Obj;
   return 0;
}

static void inews_usage (void)
{
   fputs ("--inews options:\n\
",
	  stdout);
   exit (0);
}

static int inews_parse_args (char **argv, int argc)
{
   int i;
   
   for (i = 0; i < argc; i++)
     {
	if (!strcmp (argv[i], "--help"))
	  inews_usage ();
	else break;
     }
   
   return i;
}

#endif				       /* HAS_INEWS_SUPPORT */


int slrn_init_objects (void)
{
#if SLRN_HAS_NNTP_SUPPORT
   if (-1 == nntp_init_objects ())
     return -1;
#endif
#if SLRN_HAS_SPOOL_SUPPORT
   if (-1 == spool_init_objects ())
     return -1;
#endif
#if SLRN_HAS_INEWS_SUPPORT
   if (-1 == inews_init_objects ())
     return -1;
#endif
#if SLRN_HAS_PULL_SUPPORT
   if (-1 == pull_init_objects ())
     return -1;
#endif
   return 0;
}

#if !SLRN_HAS_INEWS_SUPPORT
# undef SLRN_FORCE_INEWS
# define SLRN_FORCE_INEWS 0
#endif

int slrn_select_post_object (int id)
{
   switch (id)
     {
#if SLRN_HAS_INEWS_SUPPORT
      case SLRN_POST_ID_INEWS:
# if SLRN_HAS_PULL_SUPPORT
	if (Slrn_Use_Pull_Post
	    && (Slrn_Server_Id == SLRN_SERVER_ID_SPOOL))
	  {
	     slrn_error ("inews cannot be used with an slrnpull spool.");
	     return -1;
	  }
# endif
	return inews_select_post_object ();
#endif
   
#if !SLRN_FORCE_INEWS
# if SLRN_HAS_NNTP_SUPPORT
      case SLRN_POST_ID_NNTP:
	return nntp_select_post_object ();
# endif
#endif
	
#if SLRN_HAS_PULL_SUPPORT
      case SLRN_POST_ID_PULL:
	return pull_select_post_object ();
#endif

      default:
	slrn_error ("Object %d is not a supported posting agent.", id);
     }
   
   return -1;
}

int slrn_select_server_object (int id)
{
   int ret;
   
   switch (id)
     {
#if SLRN_HAS_NNTP_SUPPORT
      case SLRN_SERVER_ID_NNTP:
	return nntp_select_server_object ();
#endif
	
#if SLRN_HAS_SPOOL_SUPPORT
      case SLRN_SERVER_ID_SPOOL:
	ret = spool_select_server_object ();

# if SLRN_HAS_PULL_SUPPORT
	if (ret == -1)
	  return ret;

	if (Slrn_Use_Pull_Post) 
	  Slrn_Post_Id = SLRN_POST_ID_PULL;
# endif
#endif
	return ret;
   
      default:
	slrn_error ("server(%d) is not a supported server object.", id);
     }
   return -1;
}

int slrn_parse_object_args (char *name, char **argv, int argc)
{
   int num_parsed = -1;

   if (name == NULL) return -1;
   
   if (!strcmp (name, "nntp"))
     {
#if SLRN_HAS_NNTP_SUPPORT
	num_parsed = nntp_parse_args (argv, argc);
	if (Slrn_Server_Id == 0)
	  Slrn_Server_Id = SLRN_SERVER_ID_NNTP;
# if !SLRN_FORCE_INEWS
	if (Slrn_Post_Id == 0)
	  Slrn_Post_Id = SLRN_POST_ID_NNTP;
# endif
	return num_parsed;
#else
	return -2;
#endif
     }
   
   if (!strcmp (name, "spool"))
     {
#if SLRN_HAS_SPOOL_SUPPORT
	num_parsed = spool_parse_args (argv, argc);
	Slrn_Server_Id = SLRN_SERVER_ID_SPOOL;
	return num_parsed;
#else
	return -2;
#endif
     }
   
   if (!strcmp (name, "inews"))
     {
#if SLRN_HAS_INEWS_SUPPORT
	num_parsed = inews_parse_args (argv, argc);
	Slrn_Post_Id = SLRN_POST_ID_INEWS;
	return num_parsed;
#else 
	return -2;
#endif
     }
   
   if (!strcmp (name, "pull"))
     {
#if SLRN_HAS_PULL_SUPPORT
	num_parsed = pull_parse_args (argv, argc);
	Slrn_Post_Id = SLRN_POST_ID_PULL;
	return num_parsed;
#else
	return -2;
#endif
     }

   return num_parsed;
}

#if SLRN_HAS_PULL_SUPPORT
static Slrn_Post_Obj_Type Pull_Post_Obj;
FILE *Pull_Fp;
unsigned int Pull_Num_Posted;
char Pull_Post_Filename [SLRN_MAX_PATH_LEN + 1];
char Pull_Post_Dir [SLRN_MAX_PATH_LEN + 1];

static int pull_make_tempname (char *file, char *prefix)
{   
   int pid;
   unsigned int max_tries;
   time_t now;
   char name[256];
   static unsigned int num;
   char *login_name;
   
   pid = getpid ();
   max_tries = 0;
   time (&now);
   
   login_name = Slrn_User_Info.login_name;
   if ((login_name == NULL) || (*login_name == 0))
     login_name = "unknown";

   do
     {
	sprintf (name, "%s%lu-%d-%u.%s", prefix,
		 (unsigned long) now, pid, num, login_name);
	
	if (-1 == slrn_dircat (Pull_Post_Dir, name, file))
	  return ERR_FAULT;
	
	num++;
	
	max_tries++;
	if (max_tries == 100)
	  {
	     slrn_error ("Unable to create temporary file in %s.", Pull_Post_Dir);
	     return ERR_FAULT;
	  }
     }
   while (0 != slrn_file_exists (file));
   
   return 0;
}


static int pull_start_post (void)
{

   if (-1 == pull_make_tempname (Pull_Post_Filename, "_"))
     return ERR_FAULT;

   /* There is a race condition here but the chances of another file with this name
    * being created is practically nil.
    */
   
   if (NULL == (Pull_Fp = fopen (Pull_Post_Filename, "w")))
     {
	slrn_error ("Unable to open file in %s.", Pull_Post_Dir);
	return ERR_FAULT;
     }

   return CONT_POST;
}

static int pull_end_post (void)
{
   char file [SLRN_MAX_PATH_LEN + 1];
   
   if (Pull_Fp == NULL)
     return ERR_FAULT;
   
   if (-1 == slrn_fclose (Pull_Fp))
     {
	Pull_Fp = NULL;
	return ERR_FAULT;
     }
   Pull_Fp = NULL;
   
   while (1)
     {
	if (-1 == pull_make_tempname (file, "X"))
	  return ERR_FAULT;
   
	if (0 == rename (Pull_Post_Filename, file))
	  break;
	
# ifdef EEXIST
	if (errno == EEXIST) continue;
# endif
       
	slrn_error ("Unable to rename file. errno = %d.", errno);
	return ERR_FAULT;
     }
   
   return 0;
}

static int pull_puts (char *s)
{
   unsigned int len;

   len = strlen (s);
	
   if (len != fwrite (s, 1, len, Pull_Fp))
     {
	slrn_error ("Write error.  File system full?");
	return -1;
     }

   return 0;
}

static int pull_printf (char *fmt, ...)
{
   int ret;
   
   va_list ap;
   
   va_start (ap, fmt);
   ret = vfprintf (Pull_Fp, fmt, ap);
   va_end (ap);
   
   if (EOF == ret)
     {
	slrn_error ("Write failed.  File system full?");
	return -1;
     }
   
   return 0;
}


static Slrn_Post_Obj_Type Pull_Post_Obj;

static int pull_init_objects (void)
{
   Pull_Post_Obj.po_start = pull_start_post;
   Pull_Post_Obj.po_end = pull_end_post;
   Pull_Post_Obj.po_printf = pull_printf;
   Pull_Post_Obj.po_puts = pull_puts;
   Pull_Post_Obj.po_can_post = 1;

   return 0;
}

static int pull_select_post_object (void)
{
   if (-1 == slrn_dircat (Slrn_Inn_Root, SLRNPULL_OUTGOING_DIR, Pull_Post_Dir))
     return -1;
   
   if (2 != slrn_file_exists (Pull_Post_Dir))
     {
	slrn_error ("Posting directory %s does not exist.", Pull_Post_Filename);
	return -1;
     }
   
   Slrn_Post_Obj = &Pull_Post_Obj;
   
   return 0;
}

static int pull_parse_args (char **argv, int argc)
{
   (void) argv;
   (void) argc;

   return 0;
}

#endif
