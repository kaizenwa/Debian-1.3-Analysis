/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"
/*{{{ Include Files */

#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <process.h>
#include "sysdep.h"

#include <dir.h>
#include <dos.h>
#include <bios.h>
#include <io.h>
#include <conio.h>
#include <time.h>
#include <stddef.h>

/* for stat */
#include <sys/stat.h>
#include <errno.h>


/*}}}*/

#ifdef __cplusplus
#define _DOTS_ ...
#else
#define _DOTS_ void
#endif

void interrupt (*oldint9)(_DOTS_);

static unsigned char far *Shift_Ptr = (unsigned char far *) 0x417;

static unsigned int far *kbptr = (unsigned int far *) 0x41a;
static unsigned int far *kbnext = (unsigned int far *) 0x41c;
static unsigned int far *kbbeg = (unsigned int far *) 0x480;
static unsigned int far *kbend = (unsigned int far *) 0x482;

int Abort_Char = 34;

#include "dos_os2.c"

/* Macro to get char from BIOS not dos */
void interrupt int9_handler(_DOTS_) /*{{{*/
{
   unsigned int scan = (unsigned int) Abort_Char; /* 34 = scan code for ^G */
   unsigned char s, s1;
   unsigned int offset, f1_scan = 0x3B00;  /* f1 key */
   unsigned int *p;
   unsigned int numlock = 0x45;
   

   s1 = *Shift_Ptr & 0xF;  /* ignoring caps, ins, num lock, scroll lock */
   s = inp(0x60);
   if (s1 & 0x04)      /* control key */
     {
	if (s == scan)
	  {
	     if (Ignore_User_Abort == 0) SLang_Error = 2;
	     SLKeyBoard_Quit = 1;
	  }
	else if (s == 28)   /* Control - enter, ignore it!! */
	  {
	     s = inp(0x61);
	     outportb(0x61, s | 0x80);
	     outportb(0x61, s);
	     outportb(0x20, 0x20);
	     return;
	  }
     }
   else if (NumLock_Is_Gold && ((s & 0x7F) == numlock))
     {
	if (s == numlock)
	  {
	     offset = *kbnext;
	     offset += 2;
	     if (offset == *kbend) offset = *kbbeg;
	     if (offset != *kbptr)  /* buffer not full */
	       {
		  p = (unsigned int *) (0x400L + (*kbnext));
		  *p = f1_scan;
		  *kbnext = offset;
	       }
	  }
	s = inp(0x61);
	outportb(0x61, s | 0x80);
	outportb(0x61, s);
	outportb(0x20, 0x20);
	return;
     }
   (*oldint9)();
}

/*}}}*/

void init_int9_handler(void) /*{{{*/
{
   oldint9 = getvect(9);
   setvect(9, int9_handler);
}

/*}}}*/

void restore_int9_handler(void) /*{{{*/
{
   setvect(9, oldint9);
}

/*}}}*/

static void set_ctrl_break(int state) /*{{{*/
{
   static int prev = 0;

   asm {
      mov dl, byte ptr prev
      mov ax, state
      cmp ax, 0
      jne L1
      mov ah, 33h
      mov al, 0
      mov dl, byte ptr prev
      int 21h
      xor ax, ax
      mov al, dl
      mov prev, ax
      mov dl, 0
   }
   L1:
   asm {
      mov al, 1
      mov ah, 33h
      int 21h
   }   
}

/*}}}*/

#if 0
/* Here I just flag the error and return to the user-- hopefully jed will
   catch it and user will know what to do. */
static char *int24_errs[] = /*{{{*/
{
   "write protect",
   "bad unit",
   "drive not ready",
   "unknown",
   "CRC error",
   "request error",
   "seek error",
   "unknown media",
   "sector not found",
   "printer out of paper",
   "general write failure",
   "general read failure",
   "general failure", "reserved", "reserved", "invalid disk change"
};

/*}}}*/

int int24_handler(int err, int ax, int bp, int si) /*{{{*/
{
   unsigned int di;
   
   di = _DI;
   (void) ax;
   (void) bp;
   msg_error(int24_errors[di & 0x000F]);
}

/*}}}*/

#endif

static int ctrl_break(void) /*{{{*/
{
   msg_error("Control Break ignored!");
   return(1);
}

/*}}}*/


#define BIOSKEY(x) bioskey((x) | bios_key_f)
static int bios_key_f;
static void (*delay_ptr) (unsigned int);

static void my_delay (unsigned int ms)
{
   union REGS r;
   unsigned long us;
   
   us = (unsigned long)ms * 1000;  /* Micro seconds */
   r.h.ah = 0x86;
   r.x.cx = (unsigned int) (us >> 16);
   r.x.dx = (unsigned int) (us & 0xFFFF);
   int86(0x15, &r, &r);
}

static int TTY_Initialized;

int init_tty (void) /*{{{*/
{
   union REGS r;
   struct SREGS s;
   
   if (TTY_Initialized) return 0;
   bios_key_f = peekb(0x40,0x96) & 0x10; /* enhanced keyboard flag */
   
   /* Check for system type */
   delay_ptr = delay;
   r.x.ax = 0xC000;
   int86x (0x15, &r, &r, &s);
   if ((r.x.cflag == 0) && (r.h.al == 0))
     {
	unsigned char *table;
	
	table = MK_FP(s.es, r.x.bx);
	switch (table[2])
	  {
	   case 0xFF:		       /* pc */
	   case 0xFE:		       /* xt */
	   case 0xFB:		       /* xt */
	   case 0xFD:		       /* pc-jr */
	     break;
	     
	   /* AT and higher */
	   default:
	     delay_ptr = my_delay;
	  }
     }
   
	
   set_ctrl_break(0); 
   ctrlbrk(ctrl_break);
   init_int9_handler();
   TTY_Initialized = 1;
   
   if (Batch == 0)
     {
	if (X_Open_Mouse_Hook != NULL) (*X_Open_Mouse_Hook) ();
     }
   return 0;
}

/*}}}*/

void reset_tty (void) /*{{{*/
{
   if (TTY_Initialized == 0)
     return;
   TTY_Initialized = 0;
   restore_int9_handler();
   set_ctrl_break(1);
   if (Batch) return;
#ifdef HAS_MOUSE
   if (X_Close_Mouse_Hook != NULL) (*X_Close_Mouse_Hook) ();
#endif
   SLtt_reset_video();
}

/*}}}*/

#include "pcscan.c"

unsigned char sys_getkey() /*{{{*/
{
   unsigned int shift;
   unsigned int i;
   unsigned char chbuf[16];
   int timeout;
   
   timeout = 300;
   if (BIOSKEY(1) == 0) while (!sys_input_pending(&timeout, 0))
     {
	if (Display_Time)
	  {
	     JWindow->trashed = 1;
	     update((Line *) NULL, 0, 1);
	  }
     }
#ifdef HAS_MOUSE
   /* This can only be set by the mouse */
   if (Input_Buffer_Len) return my_getkey ();
   if (JMouse_Hide_Mouse_Hook != NULL) (*JMouse_Hide_Mouse_Hook) (0);
#endif
   
   shift = *Shift_Ptr & 0xF;
   i = BIOSKEY(0);
   
   i = jed_scan_to_key (i, shift, chbuf);
   while (i > 1)
     {
	int ch;
	i--;
	ch = chbuf[i];
	ungetkey (&ch);
     }
   
   return chbuf[0];
}

/*}}}*/


/* sleep for *tsecs tenths of a sec waiting for input */
static int sys_input_pending(int *tsecs, int unused) /*{{{*/
{
   int count = *tsecs * 5;
   
   (void) unused;
   
   if (Batch || Input_Buffer_Len) return(Input_Buffer_Len);
   
   if (count)
     {
	while (count > 0)
	  {
	     if (BIOSKEY(1)
#ifdef HAS_MOUSE
		 || ((JMouse_Event_Hook != NULL)
		     && ((*JMouse_Event_Hook)() > 0))
#endif
		 ) break;
	     (*delay_ptr) (20);
	     count--;
	  }
	return (count);
     }
   
   if (BIOSKEY(1)
#ifdef HAS_MOUSE
       || ((JMouse_Event_Hook != NULL)
	   && ((*JMouse_Event_Hook)() > 0))
#endif
       ) return 1;
   
   return 0;
}

/*}}}*/

void sys_pause (int ms) /*{{{*/
{
   (*delay_ptr) (ms);
}

/*}}}*/

/* Thanks to Robert Schmidt - robert@alkymi.unit.no - Buuud@IRC
   for some of code below */
int get_term_dimensions(int *w, int *h) /*{{{*/
{
   int scan_lines = 8;
   
   *h = 0;

   /* Get BIOS's screenwidth, this works on ALL displays. */
   *w = *((int *)MK_FP(0x40, 0x4a));
   
   /* Use Ralf Brown test for EGA or greater */
   asm mov ah, 12h
   asm mov bl, 10h
   asm mov bh, 0xFF  /* EGA or greater will change this */
   asm int 10h
   asm cmp bh, 0xFF
   asm je L1
	/* if EGA or compatible: Get BIOS's number of rows. */      
     *h = *(char *)MK_FP(0x40, 0x84) + 1;
   scan_lines = *(int *) 0x485;
   
   L1:
      if (*h <= 0) *h = 25;
   return scan_lines;
}

/*}}}*/

int sys_chmod(char *file, int what, int *mode, short *dum1, short *dum2) /*{{{*/
{
   int flag = 0, m = *mode;
   (void) dum1; (void) dum2;
   
   file = msdos_pinhead_fix_dir (file);
   
   asm mov ah, 43h
   asm mov al, byte ptr what
   asm mov cx, m
   asm push ds
   asm lds dx, dword ptr file
   asm int 21h
   asm pop ds
   asm mov m, cx
   asm jnc L1
   asm mov flag, ax
   
   /* Here if carry flag is set */
   if (flag == 0x2) return(0);     /* file not found */
   if (flag == 0x3) return(-2);
	/* msg_error("Path does not exist."); */
	/* else return (-3); */
	/* sprintf(buf, "chmod: Unknown Error. %d", out.x.ax);
	   msg_error(buf);
	*/
   return(-1);
    
   /* carry flag is 0 */
   L1:
   if (what == 0)
     {
	*mode = m;
     }

   if (m & 0x10)
     {
	/* msg_error("File is a directory."); */
	return(2);
     }

   return(1);
}

/*}}}*/

typedef struct Dos_DTA_Type /*{{{*/
{
   unsigned char undoc[21];
   unsigned char attr;
   unsigned int time;
   unsigned int date;
   unsigned char low_size[2];
   unsigned char high_size[2];
   char name[13];
} Dos_DTA_Type;

/*}}}*/

static Dos_DTA_Type Dos_DTA;

static void set_dta (void) /*{{{*/
{
   Dos_DTA_Type *dummy = &Dos_DTA;
   
   asm mov ah, 0x1A
   asm push ds
   asm lds dx, dword ptr dummy
   asm int 21h
   asm pop ds
}

/*}}}*/


static int File_Attr;

#define HIDDEN 0x2
#define SYSTEM 0x4
#define SUBDIR 0x10
#define READON 0x1

static char Found_Dir[JED_MAX_PATH_LEN];

#define lcase(x) if (((x) >= 'A') && ((x) <= 'Z')) (x) |= 0x20

void dta_fixup_name(char *file) /*{{{*/
{
   int dir;
   char *p, name[13];

   strcpy(file, Found_Dir);
   strcpy(name, Dos_DTA.name);
   dir = (Dos_DTA.attr & SUBDIR);
   p = name;
   while (*p)
     {
	lcase(*p);
	p++;
     }
   strcat(file, name);
   if (dir) strcat(file, "\\");
}

/*}}}*/

int sys_findfirst(char *thefile) /*{{{*/
{
   char *f, the_path[JED_MAX_PATH_LEN], *file, *f1;
   char *pat, *fudge;
   
   set_dta();
   File_Attr = READON | SUBDIR;

   file = expand_filename(thefile);
   f1 = f = extract_file(file);
   strcpy (Found_Dir, file);
   
   
   Found_Dir[(int) (f - file)] = 0;

   strcpy(the_path, file);
   
   while (*f1 && (*f1 != '*')) f1++;
   if (! *f1)
     {
	while (*f && (*f != '.')) f++;
	if (*f) strcat(the_path, "*"); else strcat(the_path, "*.*");
     }
   pat = the_path;
   
   /* Something is very wrong after this returns.  
    * thefile gets trashed for some reason.  Here I fudge until I figure 
    *  out what is going on 
    * 
    * Note: This has been fixed.  I am just too lazy to remove the fudge. 
    */
   fudge = thefile;
   
   asm mov ah, 0x4e
   asm mov cx, File_Attr
   asm push ds
   asm lds dx, dword ptr pat
   asm int 21h
   asm pop ds
   asm jc L1 
   
   thefile = fudge;
   
   /* fprintf(stderr, "asm:%lu\t|%s|\n", thefile, thefile); */
   
   dta_fixup_name(file);
   strcpy(thefile, file);
   return(1);
   L1:
   return 0;
}

/*}}}*/

int sys_findnext(char *file) /*{{{*/
{
   asm mov ah, 0x4F
   asm int 21h
   asm jc L1
   dta_fixup_name(file);
   return(1);
   L1:
   return(0);
}

/*}}}*/

#if 0
unsigned long sys_file_mod_time(char *file) /*{{{*/
{
   struct stat buf;
   
   if (stat(file, &buf) != 0) return(0);
   return ((unsigned long) buf.st_mtime);
   close(fd);
   return ((unsigned long) buf.st_mtime);
   /*
   asm mov ah, 57h
   asm mov al, 0
   asm mov bx, fd
   asm int 21h
   asm mov cx, d
   asm mov dx, t
   close(fd);
   fprintf(stderr, "Time ok.");
   return(1); */
}

/*}}}*/
#endif

/* Here we do a find first followed by calling routine to conver time */
unsigned long sys_file_mod_time(char *file) /*{{{*/
{
   struct time t;
   struct date d;
   /* struct tm *local; */
   time_t secs;
   unsigned int dat, tim;
   
   File_Attr = READON | SUBDIR;
   
   set_dta();

   asm mov ah, 0x4e
   asm mov cx, File_Attr
   asm push ds
   asm lds dx, dword ptr file
   asm int 21h
   asm pop ds
   asm jnc A_LABEL
   return 0;
   
   A_LABEL:
   tim = Dos_DTA.time;
   dat = Dos_DTA.date;
   t.ti_min = (tim >> 5) & 63;
   t.ti_hour = (tim >> 11) & 31;
   t.ti_hund = 0;
   t.ti_sec = 2 * (tim & 31);
   d.da_day = dat & 31;
   d.da_mon = (dat >> 5) & 15;
   d.da_year = 1980 + ((dat >> 9) & 0x7F);
   secs = dostounix(&d, &t);
   return((unsigned long) secs);
}

/*}}}*/


