/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

#include "config.h"
#include "jed-feat.h"

#include <stdio.h>
#ifndef __WATCOMC__
#include <sys/types.h>
#include <sys/stat.h>
#include <dir.h>
#include <pc.h>
#include <signal.h>
#else
#include <sys\types.h>
#include <sys\stat.h>
#include <direct.h>
#include <conio.h>
#include <io.h>
 /* for some reason _bios_keybrd seems to work better than kbhit() 
  * even though the documentation indicates they do the same thing.
  */
#define kbhit() _bios_keybrd(_NKEYBRD_READY)
#endif /* NOT WATCOM */

#include <errno.h>

#include "sysdep.h"

#define BIOSKEY i386_bioskey

#include <dos.h>
#include <bios.h>
#include <process.h>

#include "dos_os2.c"

int Abort_Char = 7;		       /* scan code for G (control) */

#ifndef __WATCOMC__
unsigned int i386_bioskey(int f)
{
   union REGS in, out;
   in.h.ah = f | 0x10;		       /* use enhanced kbd */
   int86(0x16, &in, &out);
   return(out.x.ax & 0xFFFF);
}
#endif

#include "pcscan.c"
static void read_and_buffer_keys (void)
{
   unsigned int scan;
   unsigned char keybuf[16];
   unsigned int shift;
   unsigned int num;
   
#ifdef __WATCOMC__
   scan = _bios_keybrd(_NKEYBRD_READ);
   shift = _bios_keybrd(_NKEYBRD_SHIFTSTATUS) & 0xF;
#else
   scan = (unsigned int) BIOSKEY(0);
   shift = BIOSKEY(2) & 0xF;
#endif
   
   num = jed_scan_to_key (scan & 0xFFFF, shift, keybuf);
   buffer_keystring (keybuf, num);
}



/* Here I also map keys to edt keys */

unsigned char sys_getkey()
{
   int wit = 300;
   
   if ( 
#ifdef __WATCOMC__
       (!_bios_keybrd(_NKEYBRD_READY))
#else
       (!kbhit()) 
#endif
       ) while (!sys_input_pending(&wit, 0))
	{
	   if (Display_Time)
	     {
		JWindow->trashed = 1;
		update((Line *) NULL, 0, 1);
	     }
	}
   
#ifdef HAS_MOUSE
   /* This can only be set by the mouse */
   if (JMouse_Hide_Mouse_Hook != NULL) (*JMouse_Hide_Mouse_Hook) (0);
   if (Input_Buffer_Len)
     return my_getkey ();
#endif
   
   read_and_buffer_keys ();
   return my_getkey ();
}


void sys_pause (int ms)
{
   /* read one of the RTC registers to ensure delay will
    * work as designed */
   outportb(0x70,0x8c); inportb(0x71);
   delay (ms);
}

static int sys_input_pending(int *tsecs, int unused)
{
   int count = *tsecs * 5;
   
   (void) unused;
   if (Batch || Input_Buffer_Len) return(Input_Buffer_Len);
   if (kbhit()) return 1;
   
   while (count > 0)
     {
	sys_pause (20);		       /* 20 ms or 1/50 sec */
	if (kbhit()
#ifdef HAS_MOUSE
	    || ((JMouse_Event_Hook != NULL)
		&& ((*JMouse_Event_Hook)() > 0))
#endif
	    ) break;
	count--;
     }
   
   return (count);
}


/*  This is to get the size of the terminal  */
#ifndef __WATCOMC__

/* DJGPP */
#include <go32.h>
#include <sys/farptr.h>

int get_term_dimensions(int *cols, int *rows)
{
   *rows = ScreenRows();
   *cols = ScreenCols();
   /* Number of scanlines/character */
   return _farpeekw (_go32_conventional_mem_selector (), 0x485);
}
#else
#include <graph.h>
int get_term_dimensions(int *cols, int *rows)
{
   struct videoconfig vc;
   _getvideoconfig(&vc);
   
   *rows = vc.numtextrows;
   *cols = vc.numtextcols;
   return 0;
}
#endif


/* returns 0 if file does not exist, 1 if it is not a dir, 2 if it is */
int sys_chmod(char *file, int what, int *mode, short *dum1, short *dum2)
{
   struct stat buf;
   int m;
   *dum1 = *dum2 = 0;
   
   file = msdos_pinhead_fix_dir (file);
   
   if (what)
     {
	chmod(file, *mode);
	return(0);
     }

   if (stat(file, &buf) < 0) switch (errno)
     {
	case EACCES: return(-1); /* es = "Access denied."; break; */
	case ENOENT: return(0);  /* ms = "File does not exist."; */
	case ENOTDIR: return(-2); /* es = "Invalid Path."; */
	default: return(-3); /* "stat: unknown error."; break;*/
     }

   m = buf.st_mode;

/* AIX requires this */
#ifdef _S_IFDIR
#ifndef S_IFDIR
#define S_IFDIR _S_IFDIR
#endif
#endif

   *mode = m & 0777;

#ifndef __WATCOMC__
   if (m & S_IFDIR) return (2);
#else
   if (S_ISDIR(m)) return(2);
#endif
   return(1);
}

int i386_access (char *file, int mode)
{
   struct stat buf;

   if (stat(file, &buf) < 0) return -1;
   if (mode == W_OK)
     {
        if (buf.st_mode & S_IWRITE) return 0;
	return -1;
     }
   return 0;
}

unsigned long sys_file_mod_time(char *file)
{
   struct stat buf;

   if (stat(file, &buf) < 0) return(0);
   return((unsigned long) buf.st_mtime);
}

#ifndef __WATCOMC__
# ifdef __GO32__
static int cbreak;
# endif
#endif

void reset_tty()
{
#ifndef __WATCOMC__
# ifdef __GO32__
#  if __DJGPP__ > 1
   signal (SIGINT, SIG_IGN);	       /* was sig_dfl */
#  endif
   setcbrk(cbreak);
# endif
#endif
   
#ifdef HAS_MOUSE
   if (X_Close_Mouse_Hook != NULL) (*X_Close_Mouse_Hook) ();
#endif
}

int init_tty()
{
#ifndef __WATCOMC__
# if __DJGPP__ > 1
   signal (SIGINT, SIG_IGN);
# endif
   cbreak = getcbrk();
   setcbrk(0);
#endif
   
#ifdef HAS_MOUSE
   if (X_Open_Mouse_Hook != NULL) (*X_Open_Mouse_Hook) ();
#endif
   
   return 0;
}


#ifndef __WATCOMC__
static struct ffblk Dos_DTA;
#else
static struct find_t fileinfo;
#endif

static char Found_Dir[JED_MAX_PATH_LEN], *Found_File;
/* found_File is a pointer into found_Dir such that the
 * full pathname is stored in the following form
 * "c:/dir/path/\0filename.ext\0"
 */

#define lcase(x) if (((x) >= 'A') && ((x) <= 'Z')) (x) |= 0x20

static void dta_fixup_name (char *file)
{
   char *p;

#ifndef __WATCOMC__
   strcpy (Found_File, Dos_DTA.ff_name);
#else
   strcpy (Found_File, fileinfo.name);
#endif
   
   p = Found_File;
   while (*p)
     {
	lcase(*p);
	p++;
     }
   
   strcpy(file, Found_Dir);
   strcat(file, Found_File);
   
#ifndef __WATCOMC__
   if (Dos_DTA.ff_attrib & FA_DIREC) strcat(file, "\\");
#else
   if (fileinfo.attrib & _A_SUBDIR) strcat(file, "\\");
#endif
}



int sys_findfirst(char *file)
{
   char *f;

   strcpy(Found_Dir, expand_filename(file) );
   Found_File = extract_file( Found_Dir );

   f = Found_File;
   
   while (*f && (*f != '*')) f++;
   if (! *f)
     {
	f = Found_File;
	while (*f && (*f != '.')) f++;
	if (*f) strcat(Found_Dir, "*"); 
	else strcat(Found_Dir, "*.*");
     }

#ifndef __WATCOMC__
   if (findfirst(Found_Dir, &Dos_DTA, FA_RDONLY | FA_DIREC))
#else
   if (_dos_findfirst(Found_Dir, _A_RDONLY | _A_SUBDIR, &fileinfo))
#endif
     {
	*Found_File++ = 0;
	return 0;
     }
   *Found_File++ = 0;
   
   dta_fixup_name(file);
   return(1);
}

int sys_findnext(char *file)
{
#ifndef __WATCOMC__
   if (findnext(&Dos_DTA)) return(0);
#else
   if (_dos_findnext (&fileinfo)) return(0);
#endif
   dta_fixup_name(file);
   return(1);
}

/* This routine is called from S-Lang inner interpreter.  It serves
   as a poor mans version of an interrupt 9 handler */

void i386_check_kbd()
{
   while (kbhit()) 
     {
	read_and_buffer_keys ();
     }
}

#if 0
char *djgpp_current_time (void)
{
   union REGS rg;
   unsigned int year;
   unsigned char month, day, weekday, hour, minute, sec;
   char days[] = "SunMonTueWedThuFriSat";
   char months[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
   static char the_date[26];
   
   rg.h.ah = 0x2A;
#ifndef __WATCOMC__
   int86(0x21, &rg, &rg);
   year = rg.x.cx & 0xFFFF;
#else
   int386(0x21, &rg, &rg);
   year = rg.x.ecx & 0xFFFF;
#endif    
  
   month = 3 * (rg.h.dh - 1);
   day = rg.h.dl;
   weekday = 3 * rg.h.al;
   
   rg.h.ah = 0x2C;
   
#ifndef __WATCOMC__
   int86(0x21, &rg, &rg);
#else
   int386(0x21, &rg, &rg);
#endif
  
   hour = rg.h.ch;
   minute = rg.h.cl;
   sec = rg.h.dh;
   
   /* we want this form: Thu Apr 14 15:43:39 1994\n  */
   sprintf(the_date, "%.3s %.3s%3d %02d:%02d:%02d %d\n",
	   days + weekday, months + month, 
	   day, hour, minute, sec, year);
   return the_date;
}
#endif
	   
   


