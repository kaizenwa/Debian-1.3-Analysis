/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#ifndef __JED_BUFFER_H_  
#define  __JED_BUFFER_H_

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <limits.h>

#ifdef PATH_MAX
# define JED_MAX_PATH_LEN PATH_MAX
#else
# ifdef pc_system
#  define JED_MAX_PATH_LEN 256
# else
#  define JED_MAX_PATH_LEN 1024
# endif
#endif

#include <slang.h>
#include "jdmacros.h"

#include "keymap.h"
#include "undo.h"
#include "indent.h"
#include "blocal.h"

/* 
#define sprintf simple_sprintf
extern char *simple_sprintf(char *, char *, ...);
*/

typedef struct Line
{
   struct Line *next;               /* pointer to next line */
   struct Line *prev;               /* pointer to prev line */
   unsigned char *data;             /* data for the line */
   int len;                         /* actual length of line */
#ifdef KEEP_SPACE_INFO
   int space;		       /* space allocated for line */
#endif
#if JED_HAS_LINE_ATTRIBUTES
   /* This is a bitmapped structure that defines attributes for the 
    * current line.  Actually the color mask field indicates the color 
    * attribute of the line.
    */
   unsigned int flags;
# define JED_LINE_HIDDEN	0x0040
# define JED_LINE_IS_READONLY	0x0080
   
# define JED_LINE_READONLY JED_LINE_IS_READONLY
#endif
} Line;

/* This is the price we pay for a linked list approach.  With straight
   buffer gap, this would be an integer.  Sigh. */
typedef struct Mark
  {
     Line *line;                      /* line that marker points at */
     int point;                       /* offset from beginning */
     unsigned int n;		       /* line number in buffer */
     struct Mark *next;
     unsigned int flags;	       /* visible mark if non-zero */
#define MARK_COLOR_MASK		0x0FF
#define MARK_INVALID		0x100
#define VISIBLE_MARK		0x200
#define NARROW_REGION_MARK	0x400
#define JED_LINE_MARK		0x800
  }
Mark;

#define JED_MAX_MARK_ARRAY_SIZE 5
typedef struct Jed_Mark_Array_Type
{
   struct Jed_Mark_Array_Type *next;
   unsigned int num_marks;
   Mark marks[JED_MAX_MARK_ARRAY_SIZE];
}
Jed_Mark_Array_Type;

extern unsigned int LineNum;	       /* current line number */
extern unsigned int Max_LineNum;       /* max line number */

typedef struct Narrow_Type
{
   struct Narrow_Type *next;
   unsigned int nup, ndown;	       /* (relative) lines above this narrow */
   Line *beg, *end;		       /* pointers to lines to linkup with */
   Line *beg1, *end1;		       /* beg and end before narrow */
   int is_region;
} Narrow_Type;


#if JED_HAS_SAVE_NARROW
typedef struct _Jed_Save_Narrow_Type
{
   Mark *beg, *end;
   struct _Jed_Save_Narrow_Type *next;
}
Jed_Save_Narrow_Type;
#endif

/* These are buffer local variables that slang can access */
typedef struct 
{
   int tab;			       /* tab width */
   int sd;			       /* selective display */
} Buffer_Local_Type;

extern Buffer_Local_Type Buffer_Local;

typedef struct _Buffer
{
   Line *beg;			       /* Top line of buffer */
   Line *end;			       /* Bottom line */
   Line *line;		       /* current line */
   int point;			       /* current offset */
   unsigned int linenum;	       /* current line number */
   unsigned int max_linenum;	       /* lines in buffer */
   char file[JED_MAX_PATH_LEN];		       /* filename sans dir */
   char dir[JED_MAX_PATH_LEN];		       /* directory of file */
#ifdef REAL_UNIX_SYSTEM
   int device;			       /* inode and device of DIRECTORY that file
					* resides in */
   int inode;
#endif
   char name[50];		       /* name of this buffer */
   unsigned int flags;	       /* flags  (autosave, etc...) */
   Narrow_Type *narrow;	       /* info for use by widen */
   unsigned int nup;		       /* lines above narrow (absolute) */
   unsigned int ndown;	       /* lines below narrow */
   Mark *marks;
   Mark *spots;
   Mark *user_marks;
   unsigned int modes;	       /* c-mode, wrap, etc... */
   SLKeyMap_List_Type *keymap;       /* keymap attached to this buffer */
   struct _Buffer *next;	       /*  */
   struct _Buffer *prev;
   char mode_str[13];
   int hits;			       /* number of hits on buffer since 
					* last autosave.  A hit is the number
					* of times the buffer was hit on at top level  */
   unsigned long m_time;	       /* time when buffer first modified */
   unsigned long c_time;	       /* time when buffer first created or  */
   /* when file visited */
   Undo_Type *undo;		       /* pointer to undo ring */
   Buffer_Local_Type local_vars;
   
#define SPOT_ARRAY_SIZE 4
     
   Jed_Mark_Array_Type *spot_array;
   Jed_Mark_Array_Type *mark_array;
   int vis_marks;		       /* number of visible marks */
   char status_line[80];
   SLang_Name_Type *par_sep;		       /* paragraph sep function */
   SLang_Name_Type *indent_hook;
   SLang_Name_Type *newline_indent_hook;
   SLang_Name_Type *wrap_hook;
   SLang_Name_Type *bob_eob_error_hook;
#ifdef HAS_MOUSE
   SLang_Name_Type *mouse_down_hook;
   SLang_Name_Type *mouse_up_hook;
   SLang_Name_Type *mouse_drag_hook;
# if JED_HAS_MULTICLICK
   SLang_Name_Type *mouse_2click_hook;
   SLang_Name_Type *mouse_3click_hook;
# endif
#endif
#if JED_HAS_COLOR_COLUMNS
   unsigned int coloring_style;
   unsigned char *column_colors;
#endif
#if JED_HAS_ABBREVS
   int abbrev_table_handle;
#endif
   Syntax_Table_Type *syntax_table;
#if JED_HAS_SUBPROCESSES
   int subprocess;		       /* 1 + subprocess id */
   int locked;
#endif
#if JED_HAS_SAVE_NARROW
   Jed_Save_Narrow_Type *save_narrow;
#endif
#if JED_HAS_BUFFER_LOCAL_VARS
   Jed_BLocal_Table_Type *blocal_table;
#endif
} Buffer;

extern char Default_Status_Line[80];

/* flags */
#define BUFFER_TRASHED 0x01

/* This flag cannot be used with the AUTO_SAVE_JUST_SAVE flag */
#define AUTO_SAVE_BUFFER 0x02
/* these two flags are to tell user that the buffer and the file on disk
   have been modified--- see update_marks and main editor loop */
#define FILE_MODIFIED 0x04
#define READ_ONLY 0x08
#define OVERWRITE_MODE 0x10
#define UNDO_ENABLED 0x20

/* skip this buffer if looking for a pop up one. */
#define BURIED_BUFFER 0x40

/* Instead of autosaving saving the buffer, just save it.  This flag
 * is only used when SIGHUP or something like that hits.  It is also
 * used when exiting the editor.  It will cause the buffer to be silently 
 * saved.  It is possible that I need another flag for this.
 */
#define AUTO_SAVE_JUST_SAVE 0x80
#define NO_BACKUP_FLAG  0x100
#define BINARY_FILE  0x200
#define ADD_CR_ON_WRITE_FLAG 0x400

#define ABBREV_MODE 0x800

#ifndef VMS
#define MAP_CR_TO_NL_FLAG 0x1000
#endif

extern char *Read_Only_Error;
extern char *Line_Read_Only_Error;
#ifdef JED_LINE_READONLY
#define CHECK_READ_ONLY\
    if (CBuf->flags & READ_ONLY) { msg_error(Read_Only_Error); return(1);}\
    if (CLine->flags & JED_LINE_READONLY) {msg_error(Line_Read_Only_Error); return 1;}
#else
#define CHECK_READ_ONLY\
    if (CBuf->flags & READ_ONLY) { msg_error(Read_Only_Error); return(1);}
#endif
    
    
#ifdef JED_LINE_READONLY
#define CHECK_READ_ONLY_VOID\
    if (CBuf->flags & READ_ONLY) { msg_error(Read_Only_Error); return;}\
    if (CLine->flags & JED_LINE_READONLY) {msg_error(Line_Read_Only_Error); return;}
#else
#define CHECK_READ_ONLY_VOID\
    if (CBuf->flags & READ_ONLY) { msg_error(Read_Only_Error); return;}
#endif

				 

#define NO_MODE 0x00
#define WRAP_MODE 0x01
#define C_MODE 0x02
#define LANG_MODE 0x04		       /* to be a replacement for C_MODE */
#define SL_MODE 0x08		       /* S-Lang mode (ored with C_MODE) */
#define F_MODE 0x10		       /* Fortran mode */
#define TEX_MODE 0x20		       /* ored with TEXT_MODE */

extern Buffer *CBuf;
extern Line *CLine;


extern int bob(void);
extern int eob(void);                  /* point to end of buffer */
extern int bol(void);
extern int eol(void);

extern int bobp(void);
extern int eobp(void);
extern int eolp(void);
extern int bolp(void);

extern int prevline(int *);
extern int nextline(int *);

extern int forwchars(int *);
extern int backwchars(int *);
extern void goto_line(int *);

extern Line *make_line1(int);
extern unsigned char *make_line(int);
extern unsigned char *remake_line(int);

extern Buffer *make_buffer(void);
extern void uniquely_name_buffer(char *);
extern void buffer_filename(char *, char *);
extern Buffer *find_file_buffer(char *);
extern Buffer *find_buffer(char *);
extern int delete_line(void);
extern void delete_buffer(Buffer *);
extern int switch_to_buffer(Buffer *);
extern int get_percent(void);
extern int what_line(void);
extern int erase_buffer(void);
extern void mark_buffer_modified(int *);
extern Line *dup_line(Line *);
extern void free_line(Line *);
extern void check_buffers(void);
extern int buffer_exists(Buffer *);
extern int Point;
extern int Number_Zero;
extern int Number_One;
extern int Number_Two;
extern int Number_Ten;
extern void mark_undo_boundary(Buffer *);
extern void delete_undo_ring(Buffer *);

extern int Batch;		       /* JED used in batch mode. */
extern void touch_screen(void);
extern void check_line(void);
extern Buffer *MiniBuffer;
#endif
