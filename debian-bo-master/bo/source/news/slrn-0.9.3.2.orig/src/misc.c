/* -*- mode: C; mode: fold; -*- */
/*  Copyright (c) 1995, 1996 John E. Davis (davis@space.mit.edu)
 *  All rights reserved.
 */
#include "config.h"
#include "slrnfeat.h"

/*{{{ Include Files */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef VMS
#  include <pwd.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#else
# include "vms.h"
#endif

#if defined(VMS) && defined(MULTINET)
# include "multinet_root:[multinet.include]netdb.h"
#else
# include <netdb.h>
# ifndef h_errno
extern int h_errno;
# endif
#endif 

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

#ifdef NeXT
# undef WIFEXITED
# undef WEXITSTATUS
#endif

#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#include <slang.h>
#include "jdmacros.h"

#include "misc.h"
#include "group.h"
#include "slrn.h"
#include "post.h"
#include "server.h"
#include "util.h"
#include "ttymsg.h"
#include "chmap.h"

#if SLRN_HAS_MIME
#include "mime.h"
#endif

#ifdef VMS
/* valid filname chars for unix equiv of vms filename */
# define VALID_FILENAME_CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_-/"
# include "vms.h"
#endif
/*}}}*/

/*{{{ Global Variables */
int Slrn_Full_Screen_Update = 1; 
int Slrn_User_Wants_Confirmation = 1;
int Slrn_Message_Present = 0;

#ifndef VMS
char *Slrn_SendMail_Command;
#endif

char *Slrn_Editor;
char *Slrn_Editor_Post;
char *Slrn_Editor_Score;
char *Slrn_Editor_Mail;

Slrn_User_Info_Type Slrn_User_Info;
SLKeyMap_List_Type *Slrn_RLine_Keymap;

/*}}}*/
/*{{{ Static Variables */

static int Error_Present;
static SLang_RLine_Info_Type *Slrn_Keymap_RLI;
static char *Input_String;
static char *Input_String_Ptr;
static int Beep_Pending;
/*}}}*/

static void redraw_message (void);
static void redraw_mini_buffer (void);

/*{{{ Screen Update Functions */

void slrn_smg_refresh (void)
{
   if (Slrn_TT_Initialized & SLRN_SMG_INIT)
     {
	slrn_push_suspension (0);
	if (Beep_Pending)
	  SLtt_beep ();
	Beep_Pending = 0;
	SLsmg_refresh ();
	slrn_pop_suspension ();
     }
}

void slrn_update_screen (int rf)
{
   if (Slrn_Batch) return;

   if ((Slrn_Current_Mode != NULL)
       && (Slrn_Current_Mode->redraw_fun != NULL))
     {
	slrn_push_suspension (0);
	(*Slrn_Current_Mode->redraw_fun) ();
	if (rf) slrn_smg_refresh ();
	slrn_pop_suspension ();
     }
}

void slrn_set_color (int color) /*{{{*/
{
   SLsmg_set_color (color);
}

/*}}}*/

void slrn_redraw (void) /*{{{*/
{
   if (Slrn_Batch) return;
   
   slrn_push_suspension (0);
   
   SLsmg_cls ();
   Slrn_Full_Screen_Update = 1;
   
   redraw_message ();

   if ((Slrn_Current_Mode != NULL)
       && (Slrn_Current_Mode->redraw_fun != NULL))
     (*Slrn_Current_Mode->redraw_fun) ();
   
   redraw_mini_buffer ();

   slrn_smg_refresh ();
   slrn_pop_suspension ();
}

/*}}}*/

void slrn_print_percent (int row, int col, SLscroll_Window_Type *w) /*{{{*/
{
   int bot_showing;
   unsigned int bot_number;
   
   SLsmg_erase_eol ();
   SLsmg_gotorc (row, col);
   SLsmg_printf ("-- %d/%d", w->line_num, w->num_lines);

   bot_number = w->line_num + (w->nrows - w->window_row) - 1;
   
   bot_showing = ((w->bot_window_line == NULL)
		  || (w->num_lines == bot_number));

   if (w->line_num == w->window_row + 1)
     {
	SLsmg_write_string (bot_showing ? "  (All)" : "  (Top)" );
     }
   else if (bot_showing) SLsmg_write_string("  (Bot)");
   else SLsmg_printf("  (%d%%)", (100 * bot_number) / w->num_lines);
   SLsmg_erase_eol ();
}

/*}}}*/

void slrn_update_top_status_line (void) /*{{{*/
{
   if (Slrn_Full_Screen_Update == 0) return;
   SLsmg_gotorc (0, 0);
   slrn_set_color (MENU_COLOR);
   SLsmg_printf ("\
slrn %s ** Press '?' for help, 'q' to quit. ** Server: %s",
		 Slrn_Version,
		 Slrn_Server_Obj->sv_name);
   SLsmg_erase_eol ();
   slrn_set_color (0);
}

/*}}}*/

/*}}}*/
/*{{{ Message/Error Functions */

/* The first character is the color */
static char Message_Buffer[1024];

static void redraw_message (void)
{
   if (Slrn_Batch) return;

   if (Slrn_Message_Present == 0)
     return;
   
   slrn_push_suspension (0);

   SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
   slrn_set_color (Message_Buffer[0]);
   SLsmg_write_string (Message_Buffer + 1);
   SLsmg_erase_eol ();
   slrn_set_color (0);
   
   slrn_pop_suspension ();
}

     
static void vmessage_1 (int color, char *fmt, va_list ap)
{
   vsprintf (Message_Buffer + 1, fmt, ap);
   Message_Buffer[0] = (char) color;
   Slrn_Message_Present = 1;
   redraw_message ();
}

static void vmessage (FILE *fp, char *fmt, va_list ap)
{
   if (Slrn_TT_Initialized & SLRN_SMG_INIT)
     vmessage_1 (0, fmt, ap);
   else
     slrn_tty_vmessage (fp, fmt, ap);
}

static void verror (char *fmt, va_list ap)
{
   if ((Slrn_TT_Initialized & SLRN_SMG_INIT) == 0)
     {
	slrn_tty_vmessage (stderr, fmt, ap);
     }
   else
     {
	if (Error_Present) return;

	slrn_clear_message ();
	Error_Present = 1;
	Beep_Pending = 1;
	vmessage_1 (ERROR_COLOR, fmt, ap);
	SLang_flush_input ();
     }
   
   if (SLang_Error == 0) SLang_Error = INTRINSIC_ERROR;
}

/*}}}*/
void slrn_clear_message (void) /*{{{*/
{
   Slrn_Message_Present = Error_Present = 0;
   /* SLang_Error = 0; */
   Beep_Pending = 0;
   SLKeyBoard_Quit = 0;
   
   if ((Slrn_TT_Initialized & SLRN_SMG_INIT) == 0)
     return;
   
   slrn_push_suspension (0);
   SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
   SLsmg_erase_eol ();
   *Message_Buffer = 0;
   slrn_pop_suspension ();
}

/*}}}*/
int slrn_message (char *fmt, ...) /*{{{*/
{
   va_list ap;
   
   if (Error_Present) return -1;
   va_start(ap, fmt);
   vmessage (stdout, fmt, ap);
   va_end (ap);
   return 0;
}

/*}}}*/

int slrn_message_now (char *fmt, ...) /*{{{*/
{
   va_list ap;
   
   if (Error_Present) return -1;
   va_start(ap, fmt);
   vmessage (stdout, fmt, ap);
   va_end (ap);
   slrn_smg_refresh ();
   Slrn_Message_Present = 0;
   return 0;
}

/*}}}*/

void slrn_error (char *fmt, ...) /*{{{*/
{
   va_list ap;

   va_start(ap, fmt);
   verror (fmt, ap);
   va_end (ap);
}

/*}}}*/

void slrn_error_now (char *fmt, ...) /*{{{*/
{
   va_list ap;

   va_start(ap, fmt);
   verror (fmt, ap);
   va_end (ap);
   slrn_smg_refresh ();
   Slrn_Message_Present = 0;
}

/*}}}*/

int slrn_check_batch (void)
{
   if (Slrn_Batch == 0) return 0;
   slrn_error ("This function is not available in batch mode.");
   return -1;
}

/*}}}*/

/*{{{ File Related Functions */
   

#ifdef VMS
/*{{{ VMS Filename fixup functions */

static void vms_fix_name(char *name)
{
   int idx, pos;
   
   pos = strspn(name, VALID_FILENAME_CHARS);
   if (pos == strlen(name))
     return;
   for(idx=pos;idx<strlen(name);idx++)
     if (!(isdigit(name[idx]) || isalpha(name[idx]) || (name[idx] == '$') || (name[idx] == '_') || (name[idx] == '-')
	   || (name[idx] == '/')))
       name[idx] = '-';
}

static char Copystr[256];
static int vms_copyname1 (char *name)
{
   strcpy(Copystr, name);
   return(1);
}

static int vms_copyname2 (char *name, int type)
{
   strcpy(Copystr, name);
   return(1);
}

/*}}}*/
#endif

void slrn_make_home_filename (char *name, char *file) /*{{{*/
{
   char *home;
#ifndef VMS
   if (
#ifdef __os2__
       (*name == '/') || (*name == '\\') || (*(name + 1) == ':') 
       || ((*name == '.') && (*(name + 1) == '/')) 
       || ((*name == '.') && (*(name + 1) == '\\'))
#else
       (*name == '/') || ((*name == '.') && (*(name + 1) == '/'))
#endif
       )
     {
	strcpy (file, name);
	return;
     }
   
   if (NULL == (home = getenv ("SLRNHOME")))
     home = getenv ("HOME");
   
   *file = 0;
   slrn_dircat (home, name, file);
#else
   char *cp, *cp1;
   static char fname[256];
   char fn[256], fn1[256];
   int rc, idx;
   
   strcpy (fn1, name);
   if (NULL != slrn_strchr (name, ':'))
     {
	strcpy (file, name);
	return;
     }
   
   if (NULL == (home = getenv ("SLRNHOME")))
     home = getenv ("HOME");
   
   *file = 0;
   if (NULL != (cp = slrn_strchr (fn1, '/')))
     {
# ifdef __DECC
	*cp = '\0'; cp++;
	cp1 = decc$translate_vms(home);
	if (cp1 == 0 || (int)cp1 == -1)
	  { /* error translating */ }
	else
	  {
	     strcpy(fname, cp1);
	     strcat(cp1, "/");
 	  }
	strcat (cp1, fn1);
	
	vms_fix_name (cp1);
 	
	rc = decc$to_vms(cp1, vms_copyname2, 0, 2);
 	if (rc > 0)
 	  {
 	     strcpy(fname, Copystr);
 	     rc = mkdir(fname, 0755);
 	  }
	strcat(fname, cp);
# else
	*cp = '\0'; cp++;
	cp1 = shell$translate_vms(home);
	if (cp1 == 0 || (int)cp1 == -1)
	  { /* error translating */ }
	else
 	  {
	     strcpy(fname, cp1);
	     strcat(cp1, "/");
	  }
	strcat (cp1, fn1);
	
	vms_fix_name (cp1);
 	
	rc = shell$to_vms(cp1, vms_copyname2, 0, 2);
	if (rc > 0)
	  {
	     strcpy(fname, Copystr);
	     rc = mkdir(fname, 0755);
	  }
	strcat(fname, cp);
# endif
	strcpy(file,fname);
     }
   else
     {
	if (home != NULL) strcpy(file, home);
	strcat(file, name);
     }
#endif /* VMS */
}

/*}}}*/

int slrn_make_home_dirname (char *name, char *dir) /*{{{*/
{
   /* This needs modified to deal with VMS directory syntax */
#ifndef VMS
   slrn_make_home_filename (name, dir);
#else
   char *home, *cp;
   char fn[256];
   static char fname[256];
   int rc, idx, len;
   
   if (NULL != slrn_strchr (name, ':'))
     {
	strcpy (dir, name);
	return;
     }
   home = getenv ("HOME");
   *dir = 0;
   if (cp = strchr(name,'/'))
     {
#ifdef __DECC
	cp = decc$translate_vms(home);
	if (cp == 0 || (int)cp == -1)
	  { /* error translating */ }
	else
	  {
	     strcpy(fname, cp);
	     strcat(cp, "/");
	  }
	strcat (cp, name);
	vms_fix_name (cp);
	
	rc = decc$to_vms(cp, vms_copyname2, 0, 2);
	if (rc > 0)
	  {
	     strcpy(fname, Copystr);
	     rc = mkdir(fname, 0755);
	  }
#else
	if (shell$from_vms(home, vms_copyname1, 0))
	  {
	     if (Copystr != NULL) strcpy (fn, Copystr);
	     strcat(fn, "/");
	  }
	strcat (fn, name);
	vms_fix_name(fn);
	if (shell$to_vms(fn, vms_copyname1, 0))
	  strcpy(fname, Copystr);
#endif
	strcpy(dir,fname);
     }
   else
     {
	if (home != NULL) 
	 {
	  strcpy(dir, home);
	  len = strlen(dir) - 1;
	  if (dir[len] == ']')
	   {
	    dir[len] = '.';
	    strcat(dir, name);
	    strcat(dir, "]");
	   }
	  else
	    strcat(dir, name);
	 }
	else
	  strcat(dir, name);
     }
#endif /* VMS */
   
   return 0;
}

/*}}}*/

   
/* Note: This function should not create a file that is deleted when it
 * is closed.
 */

FILE *slrn_open_tmpfile_in_dir (char *dir, char *file, char *mode)
{
   FILE *fp;
   unsigned int len;
   unsigned int i;
   char buf[80];
   
   if (2 != slrn_file_exists (dir))
     return NULL;
   
#ifdef __os2__
   sprintf (buf, "SLRN%04u", (unsigned int) getpid ());
#else
   sprintf (buf, "SLRN%uX", (unsigned int) getpid ());
#endif
   
   if (-1 == slrn_dircat (dir, buf, file))
     return NULL;
   
#ifdef __os2__
# define MAX_TMP_FILE_NUMBER 999
#else
# define MAX_TMP_FILE_NUMBER 1024
#endif
   
   len = strlen (file);
   for (i = 0; i < MAX_TMP_FILE_NUMBER; i++)
     {
	sprintf (file + len, ".%u", i);
	if ((0 == slrn_file_exists (file))	    
	    && (NULL != (fp = fopen (file, mode))))
	  {
	     return fp;
	  }
     }
   return NULL;
}

	  

FILE *slrn_open_tmpfile (char *file, char *mode) /*{{{*/
{
   char *dir;
   
   dir = getenv ("TMP");
   if ((dir == NULL) || (2 != slrn_file_exists (dir)))
     dir = getenv ("TMPDIR");

   if ((dir == NULL) || (2 != slrn_file_exists (dir)))
     {
#ifdef VMS
	dir = "SYS$LOGIN:";
#else
# ifdef __os2__
	dir = ".";
# else
	dir = "/tmp";
# endif
#endif
     }
   
   return slrn_open_tmpfile_in_dir (dir, file, mode);
}

/*}}}*/

FILE *slrn_open_home_file (char *name, char *mode, char *file, int create_flag) /*{{{*/
{
   slrn_make_home_filename (name, file);
   
#ifdef VMS
   if (create_flag)
     {
	FILE *fp = fopen (file, mode, "fop=cif");
	if (fp == NULL) perror ("fopen");
	return fp;
     }
#else
   (void) create_flag;

   return fopen (file, mode);
#endif	
}

/*}}}*/

int slrn_mail_file (char *file, int edit, unsigned int editline, char *to, char *subject) /*{{{*/
{
   char buf [256 + 2 * SLRN_MAX_PATH_LEN];
#ifdef __os2__
   char outfile [SLRN_MAX_PATH_LEN];
#endif
   
   if (edit && (Slrn_Batch == 0))
     {
	if (slrn_edit_file (Slrn_Editor_Mail, file, editline) < 0) return -1;
	
	while (1)
	  {
	     char rsp;
	     
	     rsp = slrn_get_response ("yYnNeE", "Mail the message? y/n/e(dit)");
	     rsp |= 0x20;
	     if (rsp == 'n') return -1;
	     if (rsp == 'y') break;
	     if (slrn_edit_file (Slrn_Editor_Mail, file, 1) < 0) return -1;
	  }
     }
   slrn_message_now ("Sending ...");
   
   slrn_chmap_fix_file (file);

#ifdef VMS
   sprintf(buf, "%s\"%s\"", MAIL_PROTOCOL, to);
   vms_send_mail( buf, subject, file );
#else
   (void) to; (void) subject;

   /* What I need to do is to open the file and feed it line by line to the
    * sendmail program.  This way I can strip out blank headers.
    */
   if (Slrn_Use_Mime)
     {
	FILE *fp, *pp;
	int header = 1;
	char line[1024];
	
	fp = fopen (file, "r");
	if (fp == NULL) return (-1);
	
	slrn_mime_scan_file (fp);
# ifdef __os2__
	pp = slrn_open_tmpfile (outfile, "w");
# else
	pp = slrn_popen (Slrn_SendMail_Command, "w");
# endif
	if (pp == NULL)
	  {
	     slrn_fclose (fp);
	     return (-1);
	  }
	
	while (fgets (line, sizeof(line), fp) != NULL)
	  {
	     unsigned int len = strlen (line);
	     
	     if (len == 0) continue;
	     len--;
	     
	     if (line [len] == '\n') line [len] = 0;
	     
	     if (header)
	       {
		  if (line[0] == 0)
		    {
		       header = 0;
		       
		       slrn_mime_add_headers (pp);
		       fp = slrn_mime_encode (fp);
		    }
		  slrn_mime_header_encode (line, sizeof(line));
	       }
	     fputs (line, pp);
	     putc('\n', pp);
	  }
	slrn_fclose (fp);
#ifdef __os2__
	slrn_fclose (pp);
	sprintf (buf, "%s %s", Slrn_SendMail_Command, outfile);
	slrn_posix_system (buf, 0);
#else
	slrn_pclose (pp);
#endif
     }
   else
     {
#ifdef __os2__
	sprintf (buf, "%s %s", Slrn_SendMail_Command, file);
#else
	sprintf (buf, "%s < %s", Slrn_SendMail_Command, file);
#endif
	slrn_posix_system (buf, 0);
     }
#endif /* NOT VMS */
   slrn_message ("Sending...done");
   return 0;
}

/*}}}*/

int slrn_pclose (FILE *fp) /*{{{*/
{
#if SLRN_HAS_PIPING
   int ret;
   if (fp == NULL) return -1;
   ret = pclose (fp);
   if (ret)
     {
	char buf[256];
#if defined(WIFEXITED) && defined(WEXITSTATUS)
	if ((ret != -1) && WIFEXITED(ret))
	  {
	     ret = WEXITSTATUS(ret);
	  }
#endif
	if (ret)
	  {
	     fprintf (stderr, "Command returned exit status %d.  Press RETURN.\n", ret);
	     fgets (buf, 255, stdin);
	  }
     }
   
   slrn_set_display_state (SLRN_TTY_INIT | SLRN_SMG_INIT);
   return 0;
#else
   return -1;
#endif
}

/*}}}*/

FILE *slrn_popen (char *cmd, char *mode) /*{{{*/
{
#if SLRN_HAS_PIPING
   FILE *fp;
   
   slrn_set_display_state (0);
   fp = popen (cmd, mode);
   
   if (fp == NULL)
     {
	char buf[256];
	fprintf (stderr, "Command %s failed to run.  Press RETURN.\n", cmd);
	fgets (buf, 255, stdin);
	slrn_set_display_state (SLRN_TTY_INIT | SLRN_SMG_INIT);
     }
   return fp;
#else
   return NULL;
#endif
}

/*}}}*/


/*}}}*/

int slrn_posix_system (char *cmd, int reset) /*{{{*/
{
   int init_mode = Slrn_TT_Initialized;
#ifndef VMS
   void (*sint)(int);
   void (*squit)(int);
   int ret;
   
   sint = signal (SIGINT, SIG_IGN);
   squit = signal (SIGQUIT, SIG_IGN);
   if (reset) slrn_set_display_state (0);
   ret = system (cmd);
   signal (SIGINT, sint);
# ifdef SIGQUIT
   signal (SIGQUIT, squit);
# endif
#else
   int ret;
   if (reset) slrn_set_display_state (0);
   ret = system (cmd);
#endif
   if (reset) slrn_set_display_state (init_mode);
   Slrn_Full_Screen_Update = 1;
   return ret;
}

/*}}}*/
static int create_edit_command (char *edit, char *cmd, char *file, unsigned int line) /*{{{*/
{
   int d, s;
   char ch, *p = edit;
   /* Look for %d and %s */
   
   d = s = 0;
   
   while (0 != (ch = *p++))
     {
	if (ch != '%') continue;
	ch = *p;
	if (!d && (ch == 'd'))
	  {
	     *p = 'u';		       /* map %d to %u (unsigned) */
	     if (s == 0) d = 1; else d = 2;
	  }
	else if (!s && (ch == 's'))
	  {
	     if (d == 0) s = 1; else s = 2;
	  }
	else
	  {
	     slrn_error ("Invalid Editor definition.");
	     return 0;
	  }
	p++;
     }
   
   
   /* No %d, %s */
   
   if ((d == 0) && (s == 0))
     {
	sprintf (cmd, "%s %s", edit, file);
     }
   else if (d == 0)
     {
	sprintf (cmd, edit, file);
     }
   else if (s == 0)
     {
	sprintf (cmd, edit, (int) line);
	strcat (edit, " ");
	strcat (edit, file);
     }
   else /* d and s */
     {
	if (d == 1)
	  sprintf (cmd, edit, line, file);
	else sprintf (cmd, edit, file, line);
     }
   return 1;
}

/*}}}*/
int slrn_edit_file (char *editor, char *file, unsigned int line) /*{{{*/
{
   char buf[512];
   char editbuf[512];
   int ret;
   
   if (editor == NULL)
     editor = Slrn_Editor;

   if ((editor == NULL)
       && (NULL == (editor = getenv("SLRN_EDITOR")))
       && (NULL == (editor = getenv("SLANG_EDITOR")))
       && (NULL == (editor = getenv("EDITOR")))
       && (NULL == (editor = getenv("VISUAL"))))
     {
#ifdef VMS
	editor = "edit";
#else
# ifdef __os2__
	editor = "e";
# else
#  ifdef __unix__
	editor = "vi";
#  endif
# endif
#endif
     }
   
   strcpy (editbuf, editor);
   if (0 == create_edit_command(editbuf, buf, file, line)) return -1;
   
   ret = slrn_posix_system (buf, 1);
   
   /* Am I the only one who thinks this is a good idea?? */
   if (Slrn_TT_Initialized) while (SLang_input_pending (5))
     {
	SLang_flush_input ();
     }
   
   /* slrn_redraw (); */
   return ret;
}

/*}}}*/

/*{{{ Get Input From User Related Functions */

static void rline_update (unsigned char *buf, int len, int col) /*{{{*/
{
   slrn_push_suspension (0);
   SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
   SLsmg_write_nchars ((char *) buf, len);
   SLsmg_erase_eol ();
   SLsmg_gotorc (SLtt_Screen_Rows - 1, col);
   slrn_smg_refresh ();
   slrn_pop_suspension ();
}

/*}}}*/

/* If 1, redraw read_line.  If 2, redraw message */
static int Reading_Input;

static void redraw_mini_buffer (void) /*{{{*/
{
   if (Reading_Input == 1)
     SLrline_redraw (Slrn_Keymap_RLI);
   else if (Reading_Input == 2)
     redraw_message ();
}

/*}}}*/

static SLang_RLine_Info_Type *init_readline (void) /*{{{*/
{
   unsigned char *buf;
   SLang_RLine_Info_Type *rli;
   
   rli = (SLang_RLine_Info_Type *) slrn_malloc (sizeof(SLang_RLine_Info_Type),
						1, 1);
   if (rli == NULL)
     return NULL;
   
   if (NULL == (buf = (unsigned char *) slrn_malloc (256, 0, 1)))
     {
	SLFREE (rli);
	return NULL;
     }

   rli->buf = buf;
   rli->buf_len = 255;
   rli->tab = 8;
   rli->dhscroll = 20;
   rli->getkey = SLang_getkey;
   rli->tt_goto_column = NULL;
   rli->update_hook = rline_update;
   
   if (SLang_init_readline (rli) < 0)
     {
	SLFREE (rli);
	SLFREE (buf);
	rli = NULL;
     }
   
   return rli;
}

/*}}}*/

int slrn_init_readline (void) /*{{{*/
{
   if ((Slrn_Keymap_RLI == NULL) 
       && (NULL == (Slrn_Keymap_RLI = init_readline ())))
     return -1;
   
   Slrn_RLine_Keymap = Slrn_Keymap_RLI->keymap;

   return 0;
}   

/*}}}*/

static int read_from_input_string (char *str)
{
   char *s;
   
   if (Input_String == NULL) return -1;
   
   s = slrn_strchr (Input_String_Ptr, '\n');
   if (s != NULL) 
     *s = 0;
   
   strncpy (str, Input_String_Ptr, 255);
   str[255] = 0;
   
   if (s == NULL)
     {
	SLFREE (Input_String);
	Input_String_Ptr = Input_String = NULL;
     }
   else Input_String_Ptr = s + 1;
   
   return strlen (str);
}

/* s could be NULL.  If so, input string is cleared. */
void slrn_set_input_string (char *s)
{
   if (Input_String != NULL)
     {
	SLFREE (Input_String);
	Input_String = NULL;
     }
   
   Input_String = s;
   Input_String_Ptr = s;
}

static int generic_read_input (char *prompt, char *dfl, char *str, int trim_flag, int no_echo) /*{{{*/
{
   int i;
   int tt_init_state;
   char prompt_buf[256];
   unsigned int len;
   int save_slang_error;
   
   strncpy (prompt_buf, prompt, sizeof (prompt_buf));
   prompt_buf[sizeof(prompt_buf) - 1] = 0;
   len = strlen (prompt);
   if (len + 3 < sizeof (prompt_buf))
     {
	if (len && (prompt[len - 1] != '?'))
	  {
	     strcpy (prompt_buf + len, ": ");
	     len += 2;
	  }
	else 
	  {
	     strcpy (prompt_buf + len, " ");
	     len++;
	  }
     }
   
   if ((dfl != NULL) && *dfl)
     {
	if (len + 13 + strlen (dfl) < sizeof (prompt_buf))
	  {
	     sprintf (prompt_buf + len,
		      "(default: %s) ", dfl);
	  }
	else dfl = NULL;
     }
   prompt = prompt_buf;

   if ((str == NULL) && (dfl == NULL)) return -1;
   
   Slrn_Keymap_RLI->edit_width = SLtt_Screen_Cols - 1;
   Slrn_Keymap_RLI->prompt = prompt;
   *Slrn_Keymap_RLI->buf = 0;

   /* slrn_set_suspension (1); */   

   if ((str != NULL) && *str)
     {
	strcpy ((char *) Slrn_Keymap_RLI->buf, str);
	Slrn_Keymap_RLI->point = 0;   /* strlen (str); */
	*str = 0;
     }
   if (str == NULL) str = dfl;

   i = read_from_input_string (str);
   if (i >= 0) return i;
   
   tt_init_state = Slrn_TT_Initialized;
   
   slrn_set_display_state (Slrn_TT_Initialized | SLRN_TTY_INIT);
   
   if (no_echo)
     {
#ifdef SL_RLINE_NO_ECHO
	Slrn_Keymap_RLI->flags |= SL_RLINE_NO_ECHO;
#endif
     }
   else
     {
#ifdef SL_RLINE_NO_ECHO
	Slrn_Keymap_RLI->flags &= ~SL_RLINE_NO_ECHO;
#endif
     }

   if (tt_init_state & SLRN_SMG_INIT)
     Slrn_Keymap_RLI->update_hook = rline_update;
   else
     Slrn_Keymap_RLI->update_hook = NULL;

   slrn_enable_mouse (0);
   
   
   save_slang_error = SLang_Error;
   SLang_Error = 0;

   Reading_Input = 1;
   i = SLang_read_line (Slrn_Keymap_RLI);
   Reading_Input = 0;
   
   slrn_enable_mouse (1);
   
   if ((i >= 0) && !SLang_Error && !SLKeyBoard_Quit)
     {
	char *b = (char *) Slrn_Keymap_RLI->buf;
	
	if (*b) 
	  {
	     SLang_rline_save_line (Slrn_Keymap_RLI);
	     if (trim_flag) 
	       {
		  slrn_trim_string (b);
		  b = slrn_skip_whitespace (b);
	       }
	  }
	else if (dfl != NULL) b = dfl;

	/* b could be equal to dfl and dfl could be equal to str.  If this is
	 * the case, there is no need to perform the strcpy */
	if (b != str) strcpy (str, b);
	i = strlen (str);
     }
   
   if (SLKeyBoard_Quit) i = -1;
   SLKeyBoard_Quit = 0;
   SLang_Error = save_slang_error;
   
   slrn_set_display_state (tt_init_state);

   if (tt_init_state & SLRN_SMG_INIT)
     {
	/* put cursor at edge of screen to comfort user */
	SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
	slrn_smg_refresh ();
     }
   else
     {
	putc ('\n', stdout);
	fflush (stdout);
     }

   /* slrn_set_suspension (0); */
   return i;
}

/*}}}*/

int slrn_read_input (char *prompt, char *dfl, char *str, int trim_flag)
{
   return generic_read_input (prompt, dfl, str, trim_flag, 0);
}

int slrn_read_input_no_echo (char *prompt, char *dfl, char *str, int trim_flag)
{
   return generic_read_input (prompt, dfl, str, trim_flag, 1);
}

int slrn_read_integer (char *prompt, int *dflt, int *np) /*{{{*/
{
   char sdfl_buf[32];
   char *sdfl = NULL;
   char str[256];
   int n;
   
   if (dflt != NULL)
     {
	sprintf (sdfl_buf, "%d", *dflt);
	sdfl = sdfl_buf;
     }
   
   *str = 0;
   if (-1 == (n = slrn_read_input (prompt, sdfl, str, 1)))
     {
	slrn_error ("Abort!");
	return -1;
     }
   
   if (1 != sscanf(str, "%d", &n))
     {
	slrn_error ("Integer expected.");
	return -1;
     }
   *np = n;
   return 0;
}

/*}}}*/

char slrn_get_response (char *valid_chars, char *str, ...) /*{{{*/
{
   char ch;
   va_list ap;
   char *v;
   
   /* if (SLang_Error) return -1; */
   
   while (1)
     {
	if (Slrn_TT_Initialized == 0)
	  {
	     char buf[256];
	     
	     va_start(ap, str);
	     slrn_tty_vmessage (stdout, str, ap);
	     va_end(ap);

	     *buf = 0;
	     (void) fgets (buf, sizeof(buf), stdin);
	     ch = *buf;
	  }
	else
	  {
	     SLang_flush_input ();
	     slrn_clear_message ();

	     va_start(ap, str);
	     vmessage_1 (0, str, ap);
	     va_end(ap);
	     
	     slrn_smg_refresh ();
	     
	     Reading_Input = 2;
	     ch = SLang_getkey ();
	     Reading_Input = 0;
	     
	     slrn_clear_message ();
	     SLang_Error = SLKeyBoard_Quit = 0;
	     
	  }
	
	v = valid_chars;
	while (*v)
	  {
	     if (*v == ch) return ch;
	     v++;
	  }
	
	slrn_error_now ("Invalid response! Try again.");
	if (Slrn_TT_Initialized & SLRN_TTY_INIT)
	  {
	     (void) SLang_input_pending (15);
	  }
     }
}

/*}}}*/
int slrn_get_yesno (int dflt, char *str, ...) /*{{{*/
{
   va_list ap;
   char buf[512];
   char ch, rsp;
   char *fmt;
   
   /* if (SLang_Error) return -1; */
   
   va_start(ap, str);
   (void) vsprintf(buf, str, ap);
   va_end(ap);
   
   if (dflt)
     {
	ch = 'y';
	fmt = "? ([y]/n)";
     }
   else
     {
	ch = 'n';
	fmt = "? (y/[n])";
     }
   
   strcat (buf, fmt);
   rsp = slrn_get_response ("yYnN\r", buf);
   if (rsp == '\r') rsp = ch;
   else rsp |= 0x20;
   
   if (rsp == 'n') return 0;
   return 1;
}

/*}}}*/
int slrn_get_yesno_cancel (char *str, ...) /*{{{*/
{
   va_list ap;
   char buf[512];
   
   if (SLang_Error) return -1;
   
   if (strlen (str) >= sizeof (buf) + 25)
     return -1;
   
   va_start(ap, str);
   (void) vsprintf(buf, str, ap);
   va_end(ap);
   
   strcat (buf, "? [Y]-es, N-o, C-ancel");
   
   switch (slrn_get_response ("\007yYnNcC\r", buf))
     {
      case '\r':
      case 'Y':
      case 'y':
	return 1;
	
      case 'n':
      case 'N':
	return 0;
	
      default:
	return -1;
     }
}

/*}}}*/

void slrn_get_mouse_rc (int *rp, int *cp) /*{{{*/
{
   int r, c;
   
   c = (unsigned char) SLang_getkey () - 32;
   r = (unsigned char) SLang_getkey () - 32;
   if (cp != NULL) *cp = c;
   if (rp != NULL) *rp = r;
}

/*}}}*/

/*}}}*/

/*{{{ Misc Regexp Utility Functions */

SLRegexp_Type *slrn_compile_regexp_pattern (char *pat) /*{{{*/
{
   static unsigned char compiled_pattern_buf [512];
   static SLRegexp_Type re;
   
   re.pat = (unsigned char *) pat;
   re.buf = compiled_pattern_buf;
   re.buf_len = sizeof (compiled_pattern_buf);
   re.case_sensitive = 0;
   
   if (0 != SLang_regexp_compile (&re))
     {
	slrn_error ("Invalid regular expression or expression too long.");
	return NULL;
     }
   return &re;
}

/*}}}*/

unsigned char *slrn_regexp_match (SLRegexp_Type *re, char *str) /*{{{*/
{
   unsigned int len;
   
   if ((str == NULL)
       || (re->min_length > (len = strlen (str))))
     return NULL;
   
   return SLang_regexp_match ((unsigned char *)str, len, re);
}

/*}}}*/

/*}}}*/

int slrn_is_fqdn (char *h) /*{{{*/
{
   char *p;
   
   /* Believe it or not, I have come across one system with a '(' character
    * as part of the hostname!!!  I suppose that I should also check for
    * other strange characters as well.  This is an issue since a message
    * id will be composed from the fqdn.  For that reason, such names will
    * be rejected.  Sigh.
    */
   if (NULL != slrn_strbrk (h, "~`!@#$%^&*()=+|\\[]{}/?;"))
     return 0;
   
   p = slrn_strchr (h, '.');
   return ((p != NULL) && (p != h));
}

/*}}}*/
void slrn_get_user_info (void) /*{{{*/
{
   struct hostent *host_entry;
   char *name, *host;
#ifndef VMS
   struct passwd *pw;
   
   pw = getpwuid (getuid ());
#endif
   
   /* Fill in what is assumed to be non-NULL by rest of program. */
   Slrn_User_Info.followup_string = slrn_safe_strmalloc ("In article %m, %r wrote:");
   Slrn_Courtesy_CC_Message = slrn_safe_strmalloc ("[This message has also been posted.]");
   
   /* Now get default values for rest. */
   
   host = Slrn_User_Info.host;
   
   /* gethostname may not provide the full name so use gethostbyname
    * to get more information.  Why isn't there a simplified interface to
    * get the FQDN!!!!
    */
   host_entry = NULL;
   if (-1 != gethostname (host, MAX_HOST_NAME_LEN))
     host_entry = gethostbyname (host);
#if defined(TRY_AGAIN) && !defined(MULTINET)
   if ((host_entry == NULL) && (h_errno == TRY_AGAIN))
     {
	sleep (2);
	host_entry = gethostbyname (host);
     }
#endif
   
   if ((host_entry == NULL) || (host_entry->h_name == NULL))
     *host = 0;
   else
     {
	if (slrn_is_fqdn ((char *)host_entry->h_name))
	  {
	     Slrn_User_Info.posting_host = slrn_safe_strmalloc ((char *)host_entry->h_name);
	  }
	else
	  {
	     char **aliases;
	     aliases = host_entry->h_aliases;
	     if (aliases != NULL) while (*aliases != NULL)
	       {
		  if (slrn_is_fqdn (*aliases))
		    {
		       Slrn_User_Info.posting_host = slrn_safe_strmalloc (*aliases);
		       break;
		    }
		  aliases++;
	       }
	  }
	
	if (Slrn_User_Info.posting_host == NULL)
	  {
	     strcpy (host, (char *) host_entry->h_name);
	  }
	else strcpy (host, Slrn_User_Info.posting_host);
     }
   
#if defined(USE_DOMAIN_NAME) && defined(MY_DOMAIN_NAME)
   if (*host && (0 == slrn_is_fqdn (host)))
     {
	char *my_domain_name;
	unsigned int len;
	
	len = strlen (host);
	my_domain_name = MY_DOMAIN_NAME;
	if (*my_domain_name != '.')
	  {
	     host[len++] = '.';
	  }
	strcpy (host + len, my_domain_name);
     }
#endif
   /* If a value was compiled in, use it. */
#ifdef OUR_HOSTNAME
# ifdef __unix__
   /* If the value is a file, read the file for the FQDN */
   if (OUR_HOSTNAME[0] == '/')
     {
	FILE *fp;
	
	if (NULL != (fp = fopen(OUR_HOSTNAME, "r")))
	  {
	   char tmphost[MAX_HOST_NAME_LEN];
	     
	     if (fgets(tmphost, MAX_HOST_NAME_LEN , fp) != NULL)
	       {
		  char *p;
		  
		  if ((p = slrn_strchr (tmphost, '\n')) != NULL)
		    *p = '\0';
	       /* If the entry is valid, we're done */
		  if (slrn_is_fqdn(tmphost))
		    strcpy(host, tmphost);
	       }
	     fclose(fp);
	  }
     }
   else
# endif
     if (slrn_is_fqdn (OUR_HOSTNAME)
	 && strlen(OUR_HOSTNAME) < MAX_HOST_NAME_LEN)
       strcpy (host, OUR_HOSTNAME);
#endif

   /* Allow user chance to over ride this.  Until now, host was a pointer
    * to Slrn_User_Info.host.
    */
   if ((NULL != (host = getenv ("HOSTNAME")))
       && slrn_is_fqdn (host))
     {
	strncpy (Slrn_User_Info.host, host, MAX_HOST_NAME_LEN);
	Slrn_User_Info.host[MAX_HOST_NAME_LEN - 1] = 0;
     }
   
   if (
#ifdef VMS
       ((name = slrn_vms_getlogin()) == NULL)
#else 
       /* I cannot use getlogin under Unix because some implementations 
	* truncate the username to 8 characters.  Besides, I suspect that
	* it is equivalent to the following line.
	*/
       ((pw == NULL) 
	|| (NULL == (name = pw->pw_name))
	|| (*name == 0))
#endif
       && ((name = getenv("USER")) == NULL)
       && ((name = getenv("LOGNAME")) == NULL))
     name = "";


   Slrn_User_Info.username = slrn_safe_strmalloc (name);
   Slrn_User_Info.login_name = slrn_safe_strmalloc (name);

   if ((Slrn_User_Info.replyto = getenv ("REPLYTO")) == NULL)
     Slrn_User_Info.replyto = "";
   Slrn_User_Info.replyto = slrn_safe_strmalloc (Slrn_User_Info.replyto);
   
#ifdef VMS
   Slrn_User_Info.realname = slrn_vms_fix_fullname(slrn_vms_get_uaf_fullname());
#else
   if (((Slrn_User_Info.realname = getenv ("NAME")) == NULL)
# ifndef __BEOS__
       && ((pw == NULL) || ((Slrn_User_Info.realname = pw->pw_gecos) == NULL))
# endif
       )
     {
	Slrn_User_Info.realname = "";
     }
#endif

   Slrn_User_Info.realname = slrn_safe_strmalloc (Slrn_User_Info.realname);
   
   /* truncate at character used to delineate extra gecos fields */
   name = Slrn_User_Info.realname;
   while (*name && (*name != ',')) name++;
   *name = 0;
   
   Slrn_User_Info.org = getenv ("ORGANIZATION");
#ifdef OUR_ORGANIZATION
   if (Slrn_User_Info.org == NULL) Slrn_User_Info.org = OUR_ORGANIZATION;
#endif
   if (Slrn_User_Info.org != NULL)
     {
	/* Check to see if this is an organization file. */
	char orgbuf[512];
	if (*Slrn_User_Info.org == '/')
	  {
	     FILE *fporg;
	     if (NULL != (fporg = fopen (Slrn_User_Info.org, "r")))
	       {
		  if (NULL != fgets (orgbuf, sizeof (orgbuf) - 1, fporg))
		    {
		       unsigned int orglen = strlen (orgbuf);
		       if (orglen && (orgbuf[orglen - 1] == '\n'))
			 orgbuf[orglen - 1] = 0;
		       Slrn_User_Info.org = orgbuf;
		    }
		  slrn_fclose (fporg);
	       }
	  }
	Slrn_User_Info.org = slrn_safe_strmalloc (Slrn_User_Info.org);
     }
   
   Slrn_User_Info.signature = slrn_safe_strmalloc (SLRN_SIGNATURE_FILE);
   
#if SLRN_HAS_MIME
   Slrn_Mime_Display_Charset = slrn_safe_strmalloc ("iso-8859-1");
#endif
   
#ifdef SLRN_SENDMAIL_COMMAND
   Slrn_SendMail_Command = slrn_safe_strmalloc (SLRN_SENDMAIL_COMMAND);
#endif
}

/*}}}*/
