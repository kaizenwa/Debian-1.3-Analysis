/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ Include Files */

#ifdef MSWINDOWS
# ifndef __WIN32__
#  include <toolhelp.h>
# else
#  include <windows.h>
# endif
#endif

#include <stdio.h>

#ifdef HAVE_SYS_WAIT_H
# include <sys/types.h>
# include <sys/wait.h>
#endif

#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#include <slang.h>
#include "jdmacros.h"

#include <string.h>

#ifdef pc_system
# include <fcntl.h>
#endif

#include "buffer.h"
#include "keymap.h"
#include "file.h"
#include "ins.h"
#include "ledit.h"
#include "screen.h"
#include "window.h"
#include "display.h"
#include "search.h"
#include "misc.h"
#include "replace.h"
#include "paste.h"
#include "sysdep.h"
#include "cmds.h"
#include "text.h"
#include "hooks.h"
#include "undo.h"

#define JED_PROMPT "S-Lang>"

#if JED_HAS_SUBPROCESSES
# include "jprocess.h"
#endif


/*}}}*/

Buffer *MiniBuffer;
Buffer *The_MiniBuffer;     /* this never gets deleted since we may need it */
Window_Type *The_MiniWindow;

static Buffer *Last_Buffer;

extern char *get_cwd(void);
int Ignore_Beep = 0;
int MiniBuffer_Active = 0;

#ifdef pc_system
char Jed_Library[JED_MAX_PATH_LEN];
#else
char Jed_Library[1024];
#endif

int (*complete_open)(char *);
int (*complete_next)(char *);
static int Buf_List_Len;
Buffer *Buf_Context;

static char *Expect_File_Error = "Expecting filename.";
static char *Keymap_Error = "Unknown_Keymap";


/* read doc file */
int get_doc_string(char *f, char *file) /*{{{*/
{
   char *buf, *b, *f1;
   VFILE *fp;
   int ret = 0;
   unsigned int n;
   char nline = 1;		       /* ascii value of line term (^A) */
   
   if (*f == 0) return(0);
   
   if ((fp = vopen(file, 1024, VFILE_TEXT)) == NULL) return(0);
   
   while ((buf = vgets(fp, &n)) != NULL)
     {
	f1 = f; b = buf;
	while (*f1 && (*f1 == *b)) f1++, b++;
	if (*f1) continue;
	
	b = buf;
	b[n - 1] = 0;
	
	while (*b)
	  {
	     if (*b == nline) *b = '\n';
	     b++;
	  }
	
	SLang_push_string(buf + 1);   /* lose the prefix */
	ret = 1;
	break;
     }
   vclose(fp);
   return(ret);
}

/*}}}*/

static int next_bufflist(char *buf) /*{{{*/
{
   Buffer *tthis;
   while (1)
     {
	tthis = Buf_Context;
	if (tthis == MiniBuffer) return(0);
	Buf_Context = Buf_Context->next;
#ifdef __os2__
 	if (!Buf_List_Len || !strncmpi(buf, tthis->name, Buf_List_Len))
#else
	  if (!Buf_List_Len || !strncmp(buf, tthis->name, Buf_List_Len))
#endif
	  {
	     if (*tthis->name == ' ') continue;   /* internal buffers */
	     strcpy(buf, tthis->name);
	     return(1);
	  }
     }
}

/*}}}*/

static int open_bufflist(char *buf) /*{{{*/
{
   if ((Buf_Context = MiniBuffer) == NULL) return(0);
   Buf_Context = Buf_Context->next;
   Buf_List_Len = strlen(buf);
   return next_bufflist(buf);
}

/*}}}*/

char *what_buffer() /*{{{*/
{
   return (CBuf->name);
}

/*}}}*/

int bufferp(char *name) /*{{{*/
{
   if (NULL == find_buffer(name)) return(0); else return(1);
}

/*}}}*/

int insert_buffer_name(char *name) /*{{{*/
{
   Buffer *buf;
   
   if (NULL != (buf = find_buffer(name)))
     {
	insert_buffer(buf);
	return(1);
     }
   else msg_error("Unable to find buffer.");
   return(0);
}

/*}}}*/

int replace_cmd(char *old, char *neew) /*{{{*/
{
   int n = 0;
   
   CHECK_READ_ONLY
     push_spot ();
   if (search(old, 1, 0)) while(replace_next(old, neew)) n++;
   pop_spot();
   return n;
}

/*}}}*/

static Buffer *find_scratch_buffer (void) /*{{{*/
{
   char *sname = "*scratch*";
   Buffer *scratch;
   
   scratch = find_buffer (sname);
   if (NULL == scratch)
     {
	scratch = make_buffer();
	strcpy(scratch->name, sname);
	strcpy(scratch->dir, CBuf->dir);
	*scratch->file = 0;
     }
   return scratch;
}

/*}}}*/


/* Try to find any other buffer but kill_buf such that the buffer is not
 * visible, not scratch and not buried.
 */
static Buffer *find_non_visible_buffer (Buffer *kill_buf) /*{{{*/
{
   Buffer *buf, *scratch;
   
   buf = kill_buf->next;
   scratch = find_scratch_buffer ();
   
   while ((buf != kill_buf) &&
	  ((*buf->name == ' ') || (buf->flags & BURIED_BUFFER)
	   || (buf == scratch) || (buffer_visible(buf))))
     {
	buf = buf->next;
     }
   if (buf == kill_buf) buf = scratch;
   return buf;
}

/*}}}*/



int kill_buffer_cmd(char *name) /*{{{*/
{
   Buffer *this_buf, *kill_buf, *scratch, *buf;
   Window_Type *w;
   int kill;
   
   if (NULL == (kill_buf = find_buffer(name)))
     {
	msg_error("Buffer does not exist.");
	return(0);
     }
   
   this_buf = CBuf;
   switch_to_buffer(kill_buf);
   
#if JED_HAS_SUBPROCESSES
   if (kill_buf->locked)
     {
	erase_buffer ();
	switch_to_buffer (this_buf);
	return 0;
     }
#endif
   
   kill = 1;
   if ((*name != ' ') && (kill_buf->flags & BUFFER_TRASHED))
     {
	char msgbuf[JED_MAX_PATH_LEN];
	sprintf (msgbuf,"Buffer %s modified. Kill, Save First, Abort: [KSA]", name);
	flush_message(msgbuf);
	/* This does not go through keyboard macro routines
	   on purpose! */
	switch (my_getkey())
	  {
	   case 'k': case 'K': kill = 1; break;
	   case 's': case 'S': kill = 2; break;
	   default: msg_error("Aborted."); return(0);
	  }
	clear_message ();
     }
   
   if (kill == 2)
     {
	write_buffer();
	if (SLang_Error)
	  {
	     switch_to_buffer(this_buf);
	     return(0);
	  }
     }
   
   /* if it is the scratch buffer, just erase it since we are going to 
      recreate it anyway. */
   
   scratch = find_scratch_buffer ();
   
   if (kill_buf == scratch)
     {
	erase_buffer();
#if JED_HAS_SUBPROCESSES
	if (kill_buf->subprocess) jed_kill_process (kill_buf->subprocess - 1);
#endif
	switch_to_buffer(this_buf);
	return 1;
     }
   
   
   /* find a buffer to replace this one with if it is in a window */
   if (buffer_visible(kill_buf))
     {
	buf = find_non_visible_buffer (kill_buf);
	
	/* search through windows looking for the buffer and replace it */
	w = JWindow;
	do
	  {
	     if (kill_buf == JWindow->buffer)
	       {
		  touch_window_hard (JWindow, 0);
		  window_buffer(buf);  /* I should choose a different one
					every time */
	       }
	     JWindow = JWindow->next;
	  }
	while (w != JWindow);
     }
   else buf = scratch;
   if (kill_buf == Last_Buffer) Last_Buffer = NULL;
   if (kill_buf == this_buf) this_buf = buf;
   switch_to_buffer(this_buf);
   delete_buffer(kill_buf);
   return(1);
}

/*}}}*/

int write_buffer_cmd(char *filestr) /*{{{*/
{
#ifdef VMS
   char *ff;
#endif
   char dir[JED_MAX_PATH_LEN], *f, file[JED_MAX_PATH_LEN];
   char msgstr[JED_MAX_PATH_LEN + 80];
   int n;
   
#ifdef REAL_UNIX_SYSTEM
   f = expand_link(filestr);
#else
   f = expand_filename(filestr);
#endif
   
   strncpy (file, f, JED_MAX_PATH_LEN); file[JED_MAX_PATH_LEN - 1] = 0;
   f = extract_file(file);
   
   if ((*f == 0) && (*CBuf->file == 0))
     {
	msg_error("Filename Required.");
	return(0);
     }
   
   n = (int) (f - file);
   SLMEMCPY((char *) dir, (char *) file, n);
   dir[n] = 0;
   
   if (*f == 0) f = CBuf->file;
   
   n = write_file_with_backup(dir, f);
   if (n >= 0)
     {
#ifdef VMS
	ff = f; while (*ff) if (*ff == ';') *ff = 0; else ff++;
#endif
	if (Batch != 2)
	  {
	     sprintf(msgstr,"Wrote %d lines to %s%s", n, dir, f);
	     message(msgstr);
	  }
	CBuf->flags &= ~BUFFER_TRASHED;
	CBuf->flags |= AUTO_SAVE_BUFFER;
	CBuf->hits = 0;
#ifdef UNDO_HAS_REDO
	update_undo_unchanged ();
#endif
	visit_file(dir, f);
	return(1);
     }
   else
     {
	sprintf(msgstr,"Error writing file %s%s", dir, f);
	msg_error(msgstr);
	return(0);
     }
}

/*}}}*/

#if defined(msdos) && !defined(__WATCOMC__) && !defined(MSWINDOWS)
int show_memory() /*{{{*/
{
   struct farheapinfo hi;
   char buf[132];
   char *c;
   unsigned long total = 0, core, used = 0;
   unsigned long max = 0;
   int i = 0;
   
   hi.ptr = NULL;
   if (farheapcheck() == _HEAPCORRUPT) c = "corrupt"; else c = "ok";
   while (farheapwalk(&hi) == _HEAPOK)
     {
	if (hi.in_use)
	  used += hi.size;
	else
	  {
	     total += hi.size;
	     if (hi.size > max) max = hi.size;
	     i++;
	  }
     }
   core = farcoreleft();
   sprintf(buf, "used:%lu, core:%lu, free:%lu, grand:%lu, max:%lu, frag:%d (%s)",
	   used, core, total, core + total, max, i, c);
   message(buf);
   return (0);
}

/*}}}*/

#endif

#ifdef MSWINDOWS
int show_memory (void) /*{{{*/
{
   char buf[132];
#ifndef __WIN32__
   MEMMANINFO mmi;
   if (MemManInfo(&mmi))
     {
 	sprintf(buf, "tot pages: %lu, free pages: %lu",
 		mmi.dwTotalLinearSpace, mmi.dwFreeLinearSpace);
 	message(buf);
     }
#else
   MEMORYSTATUS mst;
   GlobalMemoryStatus(&mst);
   sprintf(buf, "avail space: %lu, tot phys space: %lu, avail phys space: %lu",
 	   mst.dwAvailPhys + mst.dwAvailPageFile, mst.dwTotalPhys, mst.dwAvailPhys);
   message(buf);
#endif
   
   return 0;
}

/*}}}*/

#endif

#ifdef __WATCOMC__
#include <malloc.h>
int show_memory() /*{{{*/
{
   char buf[132];
   
   sprintf(buf, "avail mem: %lu, tot phys pgs: ???, free lin space: %lu",
	   _memavl (), _memmax ());
   
   message(buf);
   return (0);
}

/*}}}*/

#endif

#ifdef __GO32__
#include <dpmi.h>

int show_memory() /*{{{*/
{
   char buf[132];
   unsigned long mem;
   _go32_dpmi_meminfo info;
   
   _go32_dpmi_get_free_memory_information(&info);
   
   if (info.available_physical_pages != -1)
     mem = info.available_physical_pages * 4096;
   else mem = info.available_memory;
   
   sprintf(buf, "avail mem: %lu, tot phys pgs: %lu, free lin space: %lu",
	   mem, info.total_physical_pages, info.free_linear_space);
   
   message(buf);
   return (0);
}

/*}}}*/

#endif

int set_buffer(char *name) /*{{{*/
{
   Buffer *buf;
   
   if ((name == NULL) || (*name == 0))
     {
	msg_error("set_buffer: Buffer name is NULL");
	return (0);
     }
   
    /* Last_Buffer = CBuf; */
   
   if (NULL == (buf = find_buffer(name)))
     {
	buf = make_buffer();
	strcpy(buf->dir, CBuf->dir);
	*buf->file = 0;
	switch_to_buffer(buf);
	uniquely_name_buffer(name);
     }
   else switch_to_buffer(buf);
   return(1);
}

/*}}}*/

int get_yes_no(char *question) /*{{{*/
{
   char buf[JED_MAX_PATH_LEN], *tmp;
   int n;
   
   strncpy(buf, question, 240); buf[240] = 0;
   strcat(buf, "? (yes or no)");
   
   while(1)
     {
	if (NULL == (tmp = read_from_minibuffer(buf, 0, NULL, &n))) return(-1);
	
	if (!strcmp(tmp, "yes"))
	  {
	     SLFREE(tmp);
	     return(1);
	  }
	
	if (!strcmp(tmp, "no"))
	  {
	     SLFREE(tmp);
	     return(0);
	  }
	msg_error("yes or no!");
	SLFREE(tmp);
     }
}

/*}}}*/

int find_file_cmd (char *filestr)       /* filestr is const ! */ /*{{{*/
{
   char *f, *file, filebuf[JED_MAX_PATH_LEN];
   char msgstr[132];
   Buffer *buf;
   int n;
   
#ifdef REAL_UNIX_SYSTEM
   file = expand_link(filestr);
#else
   file = expand_filename(filestr);
#endif
   
   safe_strcpy(filebuf, file, sizeof (filebuf));
   file = filebuf;
   f = extract_file(file);
   if ((*file == 0) || (*f == 0))
     {
	msg_error(Expect_File_Error);
	return(0);
     }
   
   check_buffers ();
   
   /* search for the file in current buffer list */
   
   if (NULL != (buf = find_file_buffer(file)))
     {
	if (file_changed_on_disk(file))
	  {
	     if (get_yes_no("File changed on disk.  Read From Disk"))
	       {
		  if (*Error_Buffer) return(1);
		  n = (int) buf->linenum;
		  buf->flags  &= ~BUFFER_TRASHED;
		  kill_buffer_cmd(buf->name);
		  find_file_cmd(file);
		  goto_line(&n);
		  return(1);
	       }
	  }
	
	switch_to_buffer (buf);
	return(1);
     }
   
   buf = make_buffer();
   switch_to_buffer(buf);
   buffer_filename(file, NULL);
   
   n = read_file(file);
   CLine = CBuf->beg;
   Point = 0;
   LineNum = 1;
   if (CLine == NULL) make_line(25);
   set_file_modes();
   CBuf->flags |= UNDO_ENABLED;
   
   switch(n)
     {
      case -2: msg_error("File not readable."); break;
      default:
	if (Batch == 2) break;
	if (*Message_Buffer) break;   /* autosave warning? */
	if (n == -1) message("New file.");
	else
	  {
	     sprintf(msgstr,"%d lines read", n);
	     message(msgstr);
	  }
     }
   
   SLang_run_hooks("find_file_hook", file, NULL);
   return(1);
}

/*}}}*/

int find_file_in_window(char *file) /*{{{*/
{
   int ret;
   Buffer *b = CBuf;
   
   ret = find_file_cmd (file);
   if ((b != CBuf) && (*CBuf->name != ' ')) Last_Buffer = CBuf;
   window_buffer(CBuf);
   return(ret);
}

/*}}}*/

void set_mode_cmd(char *mode, int *flags) /*{{{*/
{
   char *m = CBuf->mode_str;
   CBuf->modes = *flags;
   strncpy((char *) m, (char *) mode, 12);
   m[12] = 0;
}

/*}}}*/



/* create a minibuffer with window and switch to it. */
static void create_minibuffer(void) /*{{{*/
{
   Window_Type *w;
   MiniBuffer = The_MiniBuffer;
   
   /* I want to make Mini_Window->next = Current Window so that scroll other
      window routines will scroll it. */
   
   w = JWindow;
   do other_window(); while (JWindow->next != w);
   JWindow->next = The_MiniWindow;
   The_MiniWindow->next = w;
   The_MiniWindow->column = 1;
   Mini_Info.action_window = w;
   other_window();    /* now in minibuffer window */
   window_buffer(MiniBuffer);
   switch_to_buffer(MiniBuffer);
   MiniBuffer_Active = 1;
   erase_buffer ();
   /* allow kill region to kill to beginning of minibuffer. 
    * Suggested by stefen@uni-paderborn.de */
   push_mark ();
}

/*}}}*/

char *Completion_Buffer = "*Completions*";

static char *Last_Completion_Buffer;
static int Last_Completion_Windows;

/* evaluate command in minibuffer and leave */
int exit_minibuffer() /*{{{*/
{
   if (IS_MINIBUFFER)
     {
	if (Last_Completion_Buffer != NULL)
	  {
	     pop_to_buffer (Completion_Buffer);
	     CBuf->flags &= ~BUFFER_TRASHED;
	     switch_to_buffer_cmd (Last_Completion_Buffer);
	     kill_buffer_cmd (Completion_Buffer);
	     touch_window_hard (JWindow, 0);
	     if (Last_Completion_Windows == 1) one_window ();
	  }
        select_minibuffer ();
	Exit_From_MiniBuffer = 1;
     }
   Last_Completion_Buffer = NULL;
   return(0);
}

/*}}}*/

/* return 1 if minibuffer already exists otherwise returns 0 */
int select_minibuffer() /*{{{*/
{
   Window_Type *w;
   
    /* Try to find active minibuffer and go there */
   w = JWindow;
   while (MiniBuffer != NULL)
     {
	if (JWindow->top == *tt_Screen_Rows) return(1);
	other_window();
	if (w == JWindow) exit_error("Internal Error:  no window!", 1);
     }
   
    /* switchs to minibuffer too */
   create_minibuffer();
   return(0);
}

/*}}}*/

/* if cmd != NULL, insert it into minibuffer and then send the result to
   the appropriate routine. */

static int ledit(void) /*{{{*/
{
   int n;
   char *tmp;
   
   if (MiniBuffer == NULL) complete_open = NULL;
   if (NULL == (tmp = read_from_minibuffer(JED_PROMPT, 0, NULL, &n))) return(0);
   SLang_Error = 0;
   Suspend_Screen_Update = 1;
   
   SLang_load_string(tmp);
   SLFREE(tmp);
   
   if ((SLang_Error == -1) && SLKeyBoard_Quit)
     {
	msg_error("Quit!");
     }
   SLang_Error = 0;
   
   return(1);
}

/*}}}*/

static char *read_file_from_minibuffer(char *prompt, char *def) /*{{{*/
{
   int n;
   char buf[JED_MAX_PATH_LEN];
   
   complete_open = sys_findfirst;
   complete_next = sys_findnext;
   
   if (*CBuf->dir == 0) strcpy (CBuf->dir, get_cwd());
   strcpy (buf, CBuf->dir);
   return read_from_minibuffer (prompt, def, buf, &n);
}

/*}}}*/

static char *String_Completion_Str;
static char *String_Completion_Str_Next;
static int String_Completion_Str_Len;


static int next_string_list (char *buf) /*{{{*/
{
   register char *s = String_Completion_Str_Next;
   int len;
   while (*s)
     {
	while (*s && (*s != ',')) s++;
	len = (int) (s - String_Completion_Str_Next);
	
	if (*s == ',') s++;
	
	if (!len
	    || strncmp (buf, String_Completion_Str_Next, String_Completion_Str_Len))
	  {
	     String_Completion_Str_Next = s;
	     continue;
	  }
	if (len >= JED_MAX_PATH_LEN) len = JED_MAX_PATH_LEN - 1;
	strncpy (buf, String_Completion_Str_Next, len);
	buf[len] = 0;
	String_Completion_Str_Next = s;
	return 1;
     }
   String_Completion_Str_Next = s;
   return 0;
}

/*}}}*/

static int open_string_list (char *buf) /*{{{*/
{
   String_Completion_Str_Next = String_Completion_Str;
   String_Completion_Str_Len = strlen (buf);
   return next_string_list (buf);
}

/*}}}*/


void read_object_with_completion(char *prompt, char *dflt, char *stuff, int *typep) /*{{{*/
{
   int type = *typep, n;
   char buf[JED_MAX_PATH_LEN], *tmp;
   char *str = NULL;
   
   *buf = 0;
   if (type == 'f')		       /* file */
     {
	complete_open = sys_findfirst;
	complete_next = sys_findnext;
	if (*CBuf->dir == 0) strcpy(CBuf->dir, get_cwd());
	strcpy(buf, CBuf->dir);
     }
   else if (type == 'b')	       /* buffer */
     {
	complete_open = open_bufflist;
	complete_next = next_bufflist;
     }
   else if (type == 'F')	       /* function */
     {
	complete_open = open_function_list;
	complete_next = next_function_list;
     }
   else if (type == 's')
     {
	complete_open = open_string_list;
	complete_next = next_string_list;
	if (SLpop_string (&str)) return;
	String_Completion_Str = str;
     }
   else
     {
	complete_open = NULL;
     }
   
   strcat (buf, stuff);
   
   if (NULL != (tmp = read_from_minibuffer(prompt, dflt, buf, &n)))
     {
	if (type == 'f') SLang_push_string(expand_filename(tmp));
	else SLang_push_string(tmp);
	
	SLFREE(tmp);
	if (str != NULL) SLFREE (str);
     }
}

/*}}}*/

int insert_file_cmd() /*{{{*/
{
   char *filebuf, *f, *file;
   
   CHECK_READ_ONLY
     
     if (NULL == (filebuf = read_file_from_minibuffer("Insert file:", NULL))) return(0);
   file = expand_filename(filebuf);
   SLFREE (filebuf);
   f = extract_file(file);
   if ((*file == 0) || (*f == 0))
     {
	msg_error(Expect_File_Error);
	return(1);
     }
   
   if (insert_file(file) < 0) msg_error("Error inserting file.");
   return(1);
}

/*}}}*/

int find_file (void) /*{{{*/
{
   char *tmp, *f;
   char file [JED_MAX_PATH_LEN];
   
   if (NULL == (tmp = read_file_from_minibuffer("Find file:", (char *) NULL))) 
     return 0;
   
   safe_strcpy (file, tmp, sizeof (file));
   SLFREE(tmp);
   
   f = extract_file (file);
   if (*f == 0)
     {
	/* Use buffer filename as default. */
	safe_strcpy (f, CBuf->file, 
		     sizeof(file) - (unsigned int) (f - file));
     }
   find_file_in_window (file);

   return 0;
}

/*}}}*/

int write_buffer (void) /*{{{*/
{
   char *tmp;
   
   if (NULL == (tmp = read_file_from_minibuffer("Write to file:", (char *) NULL))) return(0);
   write_buffer_cmd(tmp);
   SLFREE(tmp);
   return(1);
}

/*}}}*/

void switch_to_buffer_cmd (char *name) /*{{{*/
{
   Buffer *tthis = CBuf;
   
   set_buffer(name);
   window_buffer(CBuf);
   if ((CBuf != tthis) && (*CBuf->name != ' ')) Last_Buffer = tthis;
}

/*}}}*/

static void get_last_buffer(void) /*{{{*/
{
   if ((Last_Buffer == CBuf) || (Last_Buffer == NULL)
       || (*Last_Buffer->name == ' ')
       || (Last_Buffer->flags & BURIED_BUFFER))
     {
	Last_Buffer = find_non_visible_buffer (CBuf);
     }
}

/*}}}*/

int get_buffer() /*{{{*/
{
   char *tmp;
   int n;
   complete_open = open_bufflist;
   complete_next = next_bufflist;
   
   get_last_buffer ();
   
   if (NULL == (tmp = read_from_minibuffer("Switch to buffer:", Last_Buffer->name, NULL, &n))) return(0);
   switch_to_buffer_cmd(tmp);
   SLFREE(tmp);
   return(1);
}

/*}}}*/

int kill_buffer (void) /*{{{*/
{
   char *tmp;
   int n;
   
   complete_open = open_bufflist;
   complete_next = next_bufflist;
   tmp = read_from_minibuffer("Kill buffer:", (char *) CBuf->name, NULL, &n);
   if (tmp != NULL)
     {
#if JED_HAS_SUBPROCESSES
	Buffer *b = find_buffer(tmp);
	if ((b != NULL) && (b->subprocess))
	  {
	     if (0 == get_yes_no("Buffer has a subprocess attached.  Delete anyway"))
	       return 0;
	  }
#endif
	kill_buffer_cmd(tmp);
	SLFREE(tmp);
	return(1);
     }
   return 0;
}

/*}}}*/

int evaluate_cmd() /*{{{*/
{
   return(!ledit());
}

/*}}}*/

void insert_string(char *s) /*{{{*/
{
   CHECK_READ_ONLY_VOID
     ins_chars((unsigned char *) s, strlen(s));
}

/*}}}*/

/* This is weird, Ultrix cc will not compile if set_key comes before unset_key */
void unset_key(char *key) /*{{{*/
{
   if (*key)
     SLang_undefine_key(key, Global_Map);
}

/*}}}*/

void set_key(char *function, char *key) /*{{{*/
{
   if (*key)
     SLang_define_key(key, function, Global_Map);
}

/*}}}*/

void unset_key_in_keymap(char *key, char *map) /*{{{*/
{
   SLKeyMap_List_Type *kmap;
   
   if (NULL == (kmap = SLang_find_keymap(map)))
     {
	msg_error(Keymap_Error);
	return;
     }
   
   if (*key) SLang_undefine_key(key, kmap);
}

/*}}}*/

int keymap_p(char *name) /*{{{*/
{
   return ! (NULL == SLang_find_keymap(name));
}

/*}}}*/

void set_key_in_keymap(char *f, char *key, char *map) /*{{{*/
{
   SLKeyMap_List_Type *kmap;
   
   if (NULL == (kmap = SLang_find_keymap(map)))
     {
	msg_error(Keymap_Error);
	return;
     }
   
   if (*key) SLang_define_key(key, f, kmap);
}

/*}}}*/

char *pop_to_buffer(char *name) /*{{{*/
{
   Window_Type *w, *action, *use_this;
   char *bname;
   Line *line, *oldline;
   int p, oldp, lnum, oldlnum;
   Buffer *b, *oldb;
   
   if (!strcmp(name, " <mini>"))
     {
	select_minibuffer ();
	return CBuf->name;
     }
   
   /* save position so we can pop back to it if buffer already exists in 
      window */
   oldb = CBuf; oldline = CLine; oldp = Point; oldlnum = LineNum;
   
   set_buffer(name);
   line = CLine; p = Point; lnum = LineNum;
   
   use_this = NULL;
   if (MiniBuffer != NULL)
     {
	action = Mini_Info.action_window;
     }
   else action = NULL;

   if (Batch) return CBuf->name;

   w = JWindow;
   /* find a window to use */
   do
     {
	if (w->top != *tt_Screen_Rows)
	  {
	     if (action != NULL)
	       {
		  if (w != action) use_this = w;
	       }
	     else if (w != JWindow) use_this = w;
	     
	     if (w->buffer == CBuf)
	       {
		  use_this = w;
		  break;
	       }
	  }
	w = w->next;
     }
   while (w != JWindow);
   
   b = CBuf;
   if (use_this != NULL)
     {
	while(JWindow != use_this) other_window();
	/* This is a good reason for haveing otherwindow avoid switching buffers */
	if (CBuf == oldb)
	  {
	     CLine = oldline; Point = oldp; LineNum = oldlnum;
	  }
     }
   else
     {
	if (action != NULL) while(JWindow != action) other_window();
	split_window();
	/* 
	 * doing it this way makes screen update look better
	 */
	w = JWindow;
	do
	  {
	     other_window();
	  }
	while (JWindow->buffer != w->buffer);
	JWindow->column = 1;
     }
   
   bname = CBuf->name;
   switch_to_buffer(b);
   b->line = CLine = line;
   b->point = Point = p;
   b->linenum = LineNum = lnum;
   if (b != JWindow->buffer) window_buffer(b);
   return bname;
}

/*}}}*/

/* return number of windows */
int num_windows() /*{{{*/
{
   Window_Type *w;
   int i = 0;
   
   w = JWindow->next;
   while (i++, w != JWindow) w = w->next;
   return(i);
}

/*}}}*/

/* I need to make this take another parameter which indicates what to do 
 * with the cursor rather than sticking it at the end.  Call the parameter p.
 * Then try:
 * if (p <= 0) p = strlen(Message_Buffer) + 1;
 * tt_goto_rc(Screen_Height, p); */

void flush_message(char *m) /*{{{*/
{
   message(m);
   if ((JScreen[0].old == NULL) || Batch) return;
   do_dialog(Message_Buffer);
   tt_goto_rc(*tt_Screen_Rows - 1, strlen(Message_Buffer));
   *Message_Buffer = 0;
   JWindow->trashed = 1;
   flush_output ();
}

/*}}}*/

#if defined (REAL_UNIX_SYSTEM) || defined (__os2__)

# if defined (__BORLANDC__) || defined (_MSC_VER)
#   define popen _popen
#   define pclose _pclose
# endif

# if !JED_HAS_SUBPROCESSES
#  define jed_popen popen
#  define jed_pclose pclose
# endif

static char *Process_Error = "Unable to open process.";
int shell_command(char *cmd) /*{{{*/
{
   FILE *pp;
   VFILE *vp;
   int status;
   
   if (Jed_Secure_Mode)
     {
	msg_error ("Access denied.");
	return -1;
     }
   
   if (NULL == (pp = jed_popen(cmd, "r")))
     {
	msg_error(Process_Error);
	return -1;
     }
   
   if (NULL != (vp = vstream(fileno(pp), 0, VFILE_BINARY)))
     {
	(void) insert_file_pointer(vp);
	if (vp->buf != NULL) SLFREE(vp->buf);
	SLFREE(vp);
     }
   else	msg_error("Malloc Error.");
   
   status = jed_pclose (pp);
   
#if defined(WIFEXITED) && defined(WEXITSTATUS)
   if ((status != -1) && WIFEXITED(status))
     {
	status = WEXITSTATUS(status);
     }
#endif
   return status;
}

/*}}}*/
int pipe_region(char *cmd) /*{{{*/
{
   FILE *pp;
   int n;
   
   if (Jed_Secure_Mode)
     {
	msg_error ("Access denied.");
	return -1;
     }
   
   if (NULL == (pp = jed_popen (cmd, "w")))
     {
	msg_error(Process_Error);
	return(-1);
     }
   
   n = write_region_to_fp(fileno(pp));
   if (n == -1)
     msg_error ("pipe_region: write failed");
   
   return jed_pclose (pp);
}

/*}}}*/
#endif

/* called by SLang_load_file
 * Try to open a .slc then a .sl
 */
#ifdef msdos
#define VFILE_BUF_SIZE 1024
#else
#define VFILE_BUF_SIZE 4096
#endif

static VFILE *jed_open_lib_file(char *file) /*{{{*/
{
   char *lib = Jed_Library, libfsl[JED_MAX_PATH_LEN], libfslc[JED_MAX_PATH_LEN], *libp, buff[JED_MAX_PATH_LEN],
     *type, *libf;
   int n, comma = ',';
   VFILE *vp;
#ifdef VMS
   int vmsn;
#endif
   
   if (*file == '.') lib = "";
   else if (*lib == 0)
     {
	exit_error("JED_ROOT environment variable needs set.", 0);
     }
   
   n = 0;
   type = file_type(file);
   while (1)
     {
	libp = SLang_extract_list_element(lib, &n, &comma);
	n++;
	safe_strcpy(libfsl, libp, sizeof (libfsl));
	fixup_dir(libfsl);
	safe_strcat(libfsl, file, sizeof (libfsl));
	strcpy(libfsl, expand_filename(libfsl));
	if ((type == NULL) || (*type == 0))
	  {
#ifdef VMS
	     /* remove trailing '.' */
	     if (0 != (vmsn = strlen(libfsl)))
	       {
		  vmsn--;
		  if (libfsl[vmsn] == '.') libfsl[vmsn] = 0;
	       }
#endif
	     
	     strcat(libfsl, ".sl");
	     strcpy(libfslc, libfsl);
	     strcat(libfslc, "c");
	     if (file_time_cmp(libfslc, libfsl) > 0) libf = libfslc; else libf = libfsl;
	  }
	else libf = libfsl;
	vp = vopen(libf, VFILE_BUF_SIZE, VFILE_TEXT);
	if ((vp != NULL) || (*libp == 0)) break;
     }
   
   if (Batch != 2)
     {
	sprintf(buff, "loading %s", libf);
	flush_message(buff);
     }
   
   return (vp);
}

/*}}}*/

static int jed_close_object(SLang_Load_Type *x) /*{{{*/
{
   switch (x->type)
     {
      case 'F':  /* File */
	
	vclose((VFILE *) x->handle);
	return (1);
	
      default: return SL_OBJ_UNKNOWN;
     }
}

/*}}}*/

static char *jed_read_from_file(SLang_Load_Type *x) /*{{{*/
{
   char *s;
   unsigned int n;
   
   if ((s = vgets((VFILE *) x->handle, &n)) != NULL)
     {
	if (s[n - 1] != '\n') s[n] = 0;
     }
   
   return (x->buf = s);
}

/*}}}*/

static char *jed_read_from_buffer(SLang_Load_Type *x) /*{{{*/
{
   int n;
   Line *l = (Line *) x->handle;
   char *buf = x->buf;
   
   if (l == NULL) return (NULL);
   
   n = l->len;
   if (n > 255) n = 255;
   SLMEMCPY(buf, (char *) l->data, n);
   buf[n] = 0;
   x->handle = (long) l->next;
   return(buf);
}

/*}}}*/

/* returns 0 if successful */
static int jed_open_object(SLang_Load_Type *x) /*{{{*/
{
   
   switch (x->type)
     {
      case 'F':  /* File */
	
	x->read = jed_read_from_file;
	
	if (0 == (x->handle = (long) jed_open_lib_file((char *) x->name)))
	  {
	     return (SL_OBJ_NOPEN);
	  }
	
	x->buf = (long) NULL;
	x->n = 0;
	return (0);
	
      case 'B': /* Buffer */
	x->read = jed_read_from_buffer;
	x->handle = (long) (((Buffer *) (x->name))->beg);
	return (0);
	
      default: return SL_OBJ_UNKNOWN;
     }
}

/*}}}*/

void load_buffer() /*{{{*/
{
   SLang_Load_Type x;
   Buffer *cbuf = CBuf;
   int flags = CBuf->flags;
   char buf[JED_MAX_PATH_LEN];
   Line *l, *lwant;
   
   cbuf->flags |= READ_ONLY;
   
   x.name = (long) cbuf;
   x.type = 'B';
   x.buf = buf;
   
   SLang_load_object(&x);
   
   if (SLang_Error || *Error_Buffer)
     {
	SLang_doerror(NULL);
	pop_to_buffer(cbuf->name);
	/* if (x.ptr != NULL)
	   { */
	lwant = (Line *) x.handle;
	if (lwant != NULL)
	  {
	     bob();
	     while (1)
	       {
		  l = CLine->next;
		  if ((l == NULL) || (l == lwant)) break;
		  CLine = l; LineNum++;
	       }
	  }
	else eob();
	Point = x.ofs;
	/* Point = (int) (x.ptr - buf); */
	if ((Point >= 0) && (Point < CLine->len))
	  {
	     (void) skip_whitespace();
	  }
	else Point = 0;
     /* } */
     }
   cbuf->flags = flags;
}

/*}}}*/


void get_key_function() /*{{{*/
{
   char *s;
   int kind;
   
   s = find_key(&kind);
   if (s != NULL)
     {
	if (SLKeyBoard_Quit && (SLang_Error == USER_BREAK))
	  {
	     SLang_Error = 0;
	     SLKeyBoard_Quit = 0;
	     /* s = "kbd_quit"; */
	  }
	SLang_push_integer(kind);
     }
   else s = "";
   SLang_push_string(s);
}

/*}}}*/

static SLang_Name_Type *Expand_File_Hook;
void set_expansion_hook (char *s) /*{{{*/
{
   if (NULL == (Expand_File_Hook = SLang_get_function (s)))
     {
	msg_error ("The expansion hook has not been defined.");
     }
}

/*}}}*/

int mini_complete (void) /*{{{*/
{
   char *pl, *pb;
   char last[JED_MAX_PATH_LEN], buf[JED_MAX_PATH_LEN], *tmp;
   static char prev[JED_MAX_PATH_LEN];
   int n, last_key_char = SLang_Last_Key_Char;
   static int flag = 0;  /* when flag goes 0, we call open */
   
   if (complete_open == NULL) return ins_char_cmd();
   
   Point = 0;
   push_mark();
   eob();
   if (NULL == (tmp = make_buffer_substring(&n))) return(1);
   
   safe_strcpy(buf, tmp, sizeof (buf));
   SLFREE(tmp);
   
   if ((last_key_char == ' ') && ((long) Last_Key_Function == (long) mini_complete))
     {
	if (!flag || !(flag = (*complete_next)(buf)))
	  {
	     safe_strcpy(buf, prev, sizeof (buf));
	     flag = (*complete_open)(buf);
	  }
	strcpy(last, buf);
	n = -1;
     }
   else
     {
	n = 0;
	strcpy(prev, buf);  /* save this search context */
     }
   
   if (!n)
     {
	if ((Repeat_Factor != NULL)
	    || (complete_open != sys_findfirst) || (Expand_File_Hook == NULL))
	  flag = (*complete_open)(buf);
	else
	  {
	     int do_free;
	     SLang_push_string (buf);
	     SLexecute_function (Expand_File_Hook);
	     if (SLang_Error == 0) SLang_pop_integer (&do_free);
	     if (SLang_Error == 0)
	       {
		  if (do_free == 0)
		    {
		       flag = (*complete_open) (buf);
		       goto start_loop;
		    }
	       }
	     
	     if (SLang_Error || SLpop_string (&tmp))
	       {
		  msg_error ("Error encounter during expansion.  Disabling expansion hook.");
		  Expand_File_Hook = NULL;
		  return 1;
	       }
	     safe_strcpy (last, tmp, sizeof (last));
	     strcpy (prev, last);
	     SLFREE (tmp);
	     n = -1;
	  }
     }
   
   start_loop:
   
   if (!n && flag)
     {
	strcpy(last, buf);
	
	/* This do loop tests all values from complete_next and returns the
	   smallest length match of initial characters of buf */
	do
	  {
	     if ((n == 0) && (last_key_char == '\t'))
	       {
		  set_buffer (Completion_Buffer);
		  erase_buffer ();
		  CBuf->flags |= BURIED_BUFFER;
		  insert_string ("!!! Use Page Up/Down keys to scroll this window. !!!\n");
	       }
	     
	     n++;
	     pl = last;
	     pb = buf;
#ifdef __os2__
 	     if (complete_open == open_bufflist)
	       while (*pl && (tolower(*pl) == tolower(*pb))) pl++, pb++;
 	     else  /* next statement */
#endif
	       while (*pl && (*pl == *pb)) pl++, pb++;
	     *pl = 0;
	     
	     if (last_key_char == '\t')
	       {
		  while (*pb) pb++;
		  quick_insert ((unsigned char *)buf, (int) (pb - buf));
		  newline ();
	       }
	  }
	
	while(0 != (flag = (*complete_next)(buf)));
	
#ifdef __os2__
	/* OS/2 uses case-insensitive search on buffer-names. Set the 
	 * flag if there is an exact match, so that completion will
	 * cycle without repeats through all the matches. */
	
	if (complete_open == open_bufflist) 
	  {
	     strcpy(buf, last);
	     (*complete_open)(buf);
	     do 
	       {
		  if (!strcmp(buf, last)) 
		    {
		       flag = 1; break;
		    }
	       }
	     while ((*complete_next)(buf));
	  }
#endif
     }
   
   if ((n > 1) && (last_key_char == '\t') && (Last_Completion_Buffer == NULL))
     {
	Last_Completion_Windows = num_windows () - 1;   /* not including mini */
	Last_Completion_Buffer = pop_to_buffer (Completion_Buffer);
	bob ();
     }
   
   while ((CBuf != MiniBuffer) || !IS_MINIBUFFER) other_window ();
   
   if (n)
     {
	erase_buffer();
	/* strcpy(last, buf); */
	insert_string(last);
	if ((n == 1) && ((long) Last_Key_Function == (long) mini_complete))
	  message("[Sole Completion.]");
     }
   else msg_error("No Match!");
   
   return(1);
}

/*}}}*/

int what_char() /*{{{*/
{
   if (eobp()) return(0);
   return( (int) *(CLine->data + Point) );
}

/*}}}*/

void update_cmd(int *force) /*{{{*/
{
   if (Batch) return;
   JWindow->trashed = 1;
   update((Line *) NULL, *force, 0);
}

/*}}}*/

void call_cmd(char *str) /*{{{*/
{
   SLKeyMap_List_Type *km;
   int (*fp)(void);
   
   km = CBuf->keymap;
   if (NULL == (fp = (int (*)(void)) (SLang_find_key_function(str, km))))
     {
	msg_error("Function does not exist!");
     }
   else (void) (*fp)();
}

/*}}}*/

void copy_region_cmd(char *name) /*{{{*/
{
   Buffer *buf;
   
   if (NULL != (buf = find_buffer(name)))
     {
	copy_region_to_buffer(buf);
     }
   else msg_error("Unable to find buffer.");
}

/*}}}*/

#ifndef pc_system
void screen_w80() /*{{{*/
{
   tt_narrow_width();
   change_screen_size(80, *tt_Screen_Rows);
}

/*}}}*/
void screen_w132() /*{{{*/
{
   tt_wide_width();
   change_screen_size(132, *tt_Screen_Rows);
}

/*}}}*/
#endif

char *make_line_string(char *string) /*{{{*/
{
   unsigned char *tmp, *p1, *p2;
   int n;
   
   if (CBuf->marks == NULL)
     {
	p1 = CLine->data + Point;
	p2 = CLine->data + CLine->len;
     }
   else
     {
	p1 = CLine->data + CBuf->marks->point;
	p2 = CLine->data + Point;
	if (p2 < p1)
	  {
	     tmp = p1; p1 = p2; p2 = tmp;
	  }
	pop_mark(&Number_Zero);
     }
   n = (int) (p2 - p1);
   if (n > 254) n = 254;
   SLMEMCPY(string, (char *) p1, n);
   string[n] = 0;
   return(string);
}

/*}}}*/

char *make_buffer_substring(int *np) /*{{{*/
{
   Line *tthis, *beg;
   int n = 1, dn, thisp;
   unsigned char *buf;
   
   if (!check_region(&n)) return (NULL);      /* spot pushed */
   /* Point now at end of the region */
   
   beg = tthis = CBuf->marks->line;
   thisp = CBuf->marks->point;
   n = 0;
   pop_mark(&n);
   
   while (tthis != CLine)
     {
	n += tthis->len;
	tthis = tthis->next;
     }
   n -= thisp;
   n += Point;
   
   if (NULL == (buf = (unsigned char *) SLMALLOC(n + 1)))
     {
	msg_error("Malloc Error");
	pop_spot();
	return (NULL);
     }
   
   if (CLine == (tthis = beg))
     {
	SLMEMCPY((char *)buf, (char *) (tthis->data + thisp), n);
     }
   else
     {
	n = 0;
	while (tthis != CLine)
	  {
	     dn = tthis->len - thisp;
	     SLMEMCPY((char *)(buf + n), (char *) (tthis->data + thisp), dn);
	     tthis = tthis->next;
	     thisp = 0;
	     n += dn;
	  }
	SLMEMCPY((char *)(buf + n), (char *) tthis->data, Point);
	n += Point;
     }
   buf[n] = 0;
   *np = n;
   pop_spot();
   return ((char *) buf);
}

/*}}}*/

void buffer_substring() /*{{{*/
{
   char *buf;
   int n;
   if (NULL == (buf = make_buffer_substring(&n))) return;
   SLang_push_malloced_string((char *)buf);
}

/*}}}*/

int markp(void) /*{{{*/
{
   return (CBuf->marks != NULL);
}

/*}}}*/

int dup_mark(void) /*{{{*/
{
   if (CBuf->marks == NULL) return(0);
   
   push_spot();
   goto_mark(CBuf->marks);
   push_mark();
   pop_spot();
   return(1);
}

/*}}}*/

void mini_read(char *prompt, char *def, char *stuff) /*{{{*/
{
   char *buf;
   int n;
   
   complete_open = NULL;
   if (NULL == (buf = read_from_minibuffer(prompt, def, stuff, &n)))
     SLang_push_string ("");
   else SLang_push_malloced_string(buf);
}

/*}}}*/

void get_buffer_info(void) /*{{{*/
{
   SLang_push_string(CBuf->file);
   SLang_push_string(CBuf->dir);
   SLang_push_string(CBuf->name);
   SLang_push_integer(CBuf->flags);
}

/*}}}*/

void set_buffer_info(char *file, char *dir, char *name, int *flags) /*{{{*/
{
   safe_strcpy(CBuf->file, file, sizeof (CBuf->file));
   
   /* I have to handle this in this way because expand_filename does not modify
    * its argument.
    */
   safe_strcpy(CBuf->dir, dir, sizeof (CBuf->dir));
   fixup_dir(CBuf->dir);
   strcpy (CBuf->dir, expand_filename (CBuf->dir));
   
   safe_strcpy(CBuf->name, name, sizeof (CBuf->name));
   CBuf->flags = *flags;
}

/*}}}*/

void make_buffer_list(void) /*{{{*/
{
   int n = 0;
   Buffer *b;
   
   b = CBuf;
   
   do
     {
	SLang_push_string(b->name);
	b = b->next;
	n++;
     }
   while (b != CBuf);
   SLang_push_integer(n);
}

/*}}}*/

int window_size_intrinsic(int *what) /*{{{*/
{
   register int n = 0;
   switch (*what)
     {
      case 'r': n = JWindow->rows; break;
      case 'c': n = JWindow->column; break;
      case 't': n = JWindow->top; break;
      case 'w': n = JWindow->width; break;
      default: SLang_Error = UNKNOWN_ERROR;
     }
   return (n);
}

/*}}}*/

int what_mode(void) /*{{{*/
{
   SLang_push_string (CBuf->mode_str);
   return CBuf->modes;
}

/*}}}*/

/* Given a file name with wildcards return expanded list to S-Lang stack
 *  with number.  This does NOT work on unix with wild cards.  Instead the 
 *  expansion is file* (completion) */
int expand_wildcards(char *file) /*{{{*/
{
   char buf[JED_MAX_PATH_LEN];
   int n = 0;
   
   safe_strcpy(buf, file, sizeof (buf));
   
   if (sys_findfirst(buf))
     {
	do
	  {
	     n++;
	     SLang_push_string(buf);
	  }
	while (sys_findnext(buf));
     }
   return (n);
}

/*}}}*/

static void jed_traceback(char *s) /*{{{*/
{
   char *n;
   if (!Batch)
     {
	n = CBuf->name;
	set_buffer("*traceback*");
	eob();
	insert_string(s);
	set_buffer(n);
     }
   else fprintf(stderr, s);
}

/*}}}*/

#if 0
static struct /*{{{*/
{
   int depth = 0;
   char *name[20];
   int marks[20];
}

/*}}}*/
FName_Stack;

void enter_function(char *name) /*{{{*/
{
   if (depth > 20)
     {
	msg_error("Function Stack too deep.");
	return;
     }
   FName_Stack->name[depth] = name;
   FName_Stack->marks[depth] = 0;
}

/*}}}*/
void exit_function(char *name) /*{{{*/
{
   int n = FName_Stack->marks[depth];
   
}

/*}}}*/
#endif


#if 0
void make_istring(char *buf, int i) /*{{{*/
{
   register char *b, *b1, ch;
   int sign, d;
   
   b = buf;
   if (i >= 0)
     {
	sign = 1;
     }
   else
     {
	sign = -1;
	i = -i;
     }
   do
     {
	d = i % 10;
	i = i / 10;
	*b++ = d + '0';
     }
   while (i > 0);
   
   if (sign == -1) *b++ = '-';
   *b-- = 0;
   b1 = buf;
   /* now reverse it */
   while (b > b1)
     {
	ch = *b;
	*b-- = *b1;
	*b1++ = ch;
     }
}

/*}}}*/
#include <stdarg.h>
char *simple_sprintf(char *str, char *fmt, ...) /*{{{*/
{
   va_list ap;
   register char ch, *s = str, *string;
   char *fmtmax = fmt + strlen(fmt);
   int dig;
   char buf[80];
   
   va_start(ap, fmt);
   
   
   while (fmt < fmtmax)
     {
	ch = *fmt++;
	switch(ch)
	  {
	   case '%': ch = *fmt++;
	     string = buf;
	     switch(ch)
	       {
		case 'c': ch = (char) va_arg(ap, int);  /* drop */
		case '%': buf[0] = ch; buf[1] = 0; break;
		case 's':
		  string = va_arg(ap, char *); break;
		case 'l':
		case 'd':
		default:
		  dig = va_arg(ap, int);
		  make_istring(buf, dig);
		  break;
	       }
	     while (*string) *s++ = *string++;
	     break;
	   case '\\':
	     ch = *fmt++;
	     switch(ch)
	       {
		case 'b': ch = '\b'; break;
		case 'r': ch = 13; break;
		case 'n': ch = '\n'; break;
		case 'e': ch = 27; break;
		case 'a': ch = 7; break;
	       }
	     /* drop */
	   default:
	     *s++ = ch;
	  }
     }
   va_end(ap);
   *s = 0;
   return(str);
}

/*}}}*/
#endif


char *command_line_argv(int *nn) /*{{{*/
{
   int n = *nn;
   if ((n >= Main_Argc) || (n < 0))
     {
	msg_error("Argc out of bounds.");
	n = 0;
     }
   
   return Main_Argv[n];
}

/*}}}*/

void count_chars(void) /*{{{*/
{
   unsigned long n = 0, m = 0;
   int ch;
   char buf[64];
   Line *l = CBuf->beg;
   
   while (l != NULL)
     {
	n += l->len;
	l = l->next;
     }
   l = CBuf->beg;
   while (l != CLine)
     {
	m += l->len;
	l = l->next;
     }
   m += Point + 1;
   ch = eobp() ? 0 : (int) *(CLine->data + Point);
   sprintf(buf, "'@'=%d/0x%x/%#o, point %lu of %lu", ch, ch, ch, m, n);
   if (ch != 0) buf[1] = ch;
   else buf[0] = '^';
   SLang_push_string(buf);
}

/*}}}*/

static void jed_clear_error(void) /*{{{*/
{
   *Error_Buffer = 0;
   SLKeyBoard_Quit = 0;
}

/*}}}*/

int (*X_Init_SLang_Hook)(void);

void init_minibuffer() /*{{{*/
{
   Buffer *tmp;
   
   tmp = CBuf;
   
   The_MiniBuffer = make_buffer();
   The_MiniBuffer->modes = 0;
   strcpy(The_MiniBuffer->name, " <mini>");
   /* do some initializing */
   switch_to_buffer(The_MiniBuffer);
   remake_line(132);
   The_MiniWindow = create_window(*tt_Screen_Rows, 1, 1, *tt_Screen_Cols);
   The_MiniWindow->buffer = CBuf;
   Buffer_Local.tab = 0;
   switch_to_buffer(tmp);
   SLang_Dump_Routine = jed_traceback;
   
#ifdef __GO32__
   SLang_Interrupt = i386_check_kbd;
#endif
   
#if 0
   SLang_Enter_Function = enter_function;
   SLang_Exit_Function = exit_function;
#endif
   if (!init_SLang()
#ifdef FLOAT_TYPE
       || !init_SLmath()
#endif
#ifndef pc_system
#ifdef __unix__
       || !init_SLunix()
#endif
#endif
       || !init_jed_intrinsics()
       || !register_jed_classes ()
       || ((X_Init_SLang_Hook != NULL) && !(*X_Init_SLang_Hook)()))
     {
	exit_error("Unable to initialize S-Lang!", 0);
     }
   
   /* use jed rouotines instead of default slang ones */
   SLang_Error_Routine = msg_error;
   SLang_User_Open_Slang_Object = jed_open_object;
   SLang_User_Close_Slang_Object = jed_close_object;
   SLang_User_Clear_Error = jed_clear_error;
}

/*}}}*/
