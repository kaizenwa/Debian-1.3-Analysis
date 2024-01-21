/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */
#include <config.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "slang.h"
#include "_slang.h"

#ifndef NO_STDLIB_H
#include <stdlib.h>
#endif

#ifndef NO_UNISTD_H
# include <unistd.h>
#endif

#ifdef __DECC
/* These get prototypes for write an sleep */
/* # include <unixio.h> */
# include <signal.h>
#endif

#include <termios.h>

#include <sys/ioctl.h>

#ifdef linux_unicode
#include <linux/kd.h>
#endif

/* Colors:  These definitions are used for the display.  However, the 
 * application only uses object handles which get mapped to this
 * internal representation.  The mapping is performed by the Color_Map
 * structure below. */

#define CHAR_MASK	0x000000FF
#define FG_MASK		0x0000FF00
#define BG_MASK		0x00FF0000
#define ATTR_MASK	0x1F000000
#define BGALL_MASK	0x0FFF0000

/* The 0x10000000 bit represents the alternate character set.  BGALL_MASK does
 * not include this attribute.
 */



#define GET_FG(color) ((color & FG_MASK) >> 8)
#define GET_BG(color) ((color & BG_MASK) >> 16)
#define MAKE_COLOR(fg, bg) (((fg) | ((bg) << 8)) << 8)

int SLtt_Screen_Cols;
int SLtt_Screen_Rows;
int SLtt_Term_Cannot_Insert;
int SLtt_Term_Cannot_Scroll;
int SLtt_Use_Ansi_Colors;
int SLtt_Blink_Mode = 1;
int SLtt_Use_Blink_For_ACS = 0;
int SLtt_Newline_Ok = 0;
int SLtt_Has_Alt_Charset = 0;
int SLtt_Unicode = 0;
int SLtt_DefaultUnicode = 0;

static int Automatic_Margins;
/* static int No_Move_In_Standout; */
static int Worthless_Highlight;

static int Linux_Console;

static int is_xterm;

/* It is crucial that JMAX_COLORS must be less than 128 since the high bit
 * is used to indicate a character from the ACS (alt char set).  The exception
 * to this rule is if SLtt_Use_Blink_For_ACS is true.  This means that of 
 * the highbit is set, we interpret that as a blink character.  This is 
 * exploited by DOSemu.
 */
#define JMAX_COLORS 256
#define JNORMAL_COLOR 0

typedef struct 
{
   SLtt_Char_Type fgbg;
   SLtt_Char_Type mono;
   char *custom_esc;
} Ansi_Color_Type;

#define RGB1(r, g, b)   ((r) | ((g) << 1) | ((b) << 2))
#define RGB(r, g, b, br, bg, bb)  ((RGB1(r, g, b) << 8) | (RGB1(br, bg, bb) << 16))

static Ansi_Color_Type Ansi_Color_Map[JMAX_COLORS] = 
{
   {RGB(1, 1, 1, 0, 0, 0), 0x00000000, NULL},
   {RGB(0, 0, 1, 0, 0, 0), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 0, 0, 0), SLTT_REV_MASK, NULL},
   {RGB(1, 0, 0, 0, 0, 0), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 0, 0, 1), SLTT_REV_MASK, NULL},
   {RGB(1, 0, 0, 0, 0, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 0, 1, 0, 1, 0), SLTT_REV_MASK, NULL},
   {RGB(1, 0, 0, 0, 1, 0), SLTT_REV_MASK, NULL},
   {RGB(0, 0, 1, 1, 0, 0), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 1, 0, 0), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 1, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(1, 1, 0, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(1, 0, 1, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 0, 0, 0, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 1, 1, 1), SLTT_REV_MASK, NULL},
   {RGB(0, 1, 0, 1, 1, 1), SLTT_REV_MASK, NULL}
};


/* This is the string to use to use when outputting color information.
 */
#ifdef M_UNIX
/* work around for sco console bug that can't handle combined sequences */
  static char *Color_Escape_Sequence = "\033[3%dm\033[4%dm";
#else
  static char *Color_Escape_Sequence = "\033[3%d;4%dm";
#endif

char *SLtt_Graphics_Char_Pairs;	       /* ac termcap string -- def is vt100 */

   
/* 1 if terminal lacks the ability to do into insert mode or into delete
   mode. Currently controlled by S-Lang but later perhaps termcap. */

static char *UnderLine_Vid_Str;
static char *Blink_Vid_Str;
static char *Bold_Vid_Str;
static char *Ins_Mode_Str; /* = "\033[4h"; */   /* ins mode (im) */
static char *Eins_Mode_Str; /* = "\033[4l"; */  /* end ins mode (ei) */
static char *Scroll_R_Str; /* = "\033[%d;%dr"; */ /* scroll region */
static char *Cls_Str; /* = "\033[2J\033[H"; */  /* cl termcap STR  for ansi terminals */
static char *Rev_Vid_Str; /* = "\033[7m"; */    /* mr,so termcap string */
static char *Norm_Vid_Str; /* = "\033[m"; */   /* me,se termcap string */
static char *Del_Eol_Str; /* = "\033[K"; */	       /* ce */
static char *Del_Char_Str; /* = "\033[P"; */   /* dc */
static char *Del_N_Lines_Str; /* = "\033[%dM"; */  /* DL */
static char *Add_N_Lines_Str; /* = "\033[%dL"; */  /* AL */
static char *Rev_Scroll_Str;
static char *Curs_Up_Str;
static char *Curs_F_Str;    /* RI termcap string */
static char *Cursor_Visible_Str;    /* vs termcap string */
static char *Cursor_Invisible_Str;    /* vi termcap string */

static char *Start_Alt_Chars_Str;  /* as */
static char *End_Alt_Chars_Str;   /* ae */
static char *Enable_Alt_Char_Set;  /* eA */

static char *Term_Init_Str;
static char *Term_Reset_Str;

/* static int Len_Curs_F_Str = 5; */

/* cm string has %i%d since termcap numbers columns from 0 */
/* char *CURS_POS_STR = "\033[%d;%df";  ansi-- hor and vert pos */
static char *Curs_Pos_Str; /* = "\033[%i%d;%dH";*/   /* cm termcap string */


/* scrolling region */
static int Scroll_r1 = 0, Scroll_r2 = 23;
static int Cursor_r, Cursor_c;	       /* 0 based */

/* current attributes --- initialized to impossible value */
static SLtt_Char_Type Current_Fgbg = 0xFFFFFFFF;

static int Cursor_Set;		       /* 1 if cursor position known, 0
					* if not.  -1 if only row is known
					*/


#define MAX_OUTPUT_BUFFER_SIZE 4096

static unsigned char Output_Buffer[MAX_OUTPUT_BUFFER_SIZE];
static unsigned char *Output_Bufferp = Output_Buffer;

unsigned long SLtt_Num_Chars_Output;

int SLtt_flush_output (void)
{
   int nwrite = 0;
   int n = (int) (Output_Bufferp - Output_Buffer);
   
   SLtt_Num_Chars_Output += n;
   
   while (n > 0)
     {
	nwrite = write (fileno(stdout), (char *) Output_Buffer + nwrite, n);
	if (nwrite == -1)
	  {
	     nwrite = 0;
#ifdef EAGAIN
	     if (errno == EAGAIN) continue;
#endif
#ifdef EINTR
	     if (errno == EINTR) continue;
#endif
	     break;
	  }
	n -= nwrite;
     }
   Output_Bufferp = Output_Buffer;
   return n;
}


int SLtt_Baud_Rate;
static void tt_write(char *str, int n)
{
   static unsigned long last_time;
   static int total;
   unsigned long now;
   int ndiff;
   
   if ((str == NULL) || (n <= 0)) return;
   total += n;
   
   while (1)
     {
	ndiff = MAX_OUTPUT_BUFFER_SIZE - (int) (Output_Bufferp - Output_Buffer);
	if (ndiff < n)
	  {
	     MEMCPY ((char *) Output_Bufferp, (char *) str, ndiff);
	     Output_Bufferp += ndiff;
	     SLtt_flush_output ();
	     n -= ndiff;
	     str += ndiff;
	  }
	else 
	  {
	     MEMCPY ((char *) Output_Bufferp, str, n);
	     Output_Bufferp += n;
	     break;
	  }
     }
   
   if (((SLtt_Baud_Rate > 150) && (SLtt_Baud_Rate <= 9600))
       && (10 * total > SLtt_Baud_Rate))
     {
	total = 0;
	if ((now = (unsigned long) time(NULL)) - last_time <= 1)
	  {
	     SLtt_flush_output ();
	     sleep((unsigned) 1);
	  }
	last_time = now;
     }
}


void SLtt_write_string(char *str)
{
   if (str != NULL) tt_write(str, strlen(str));
}


void SLtt_putchar(char ch)
{
   SLtt_normal_video ();
   if (Cursor_Set == 1)
     {
	if (ch >= ' ') Cursor_c++;
	else if (ch == '\b') Cursor_c--;
	else if (ch == '\r') Cursor_c = 0;
	else Cursor_Set = 0;
	
	if ((Cursor_c + 1 == SLtt_Screen_Cols) 
	    && Automatic_Margins) Cursor_Set = 0;
     }
   
   if (Output_Bufferp < Output_Buffer + MAX_OUTPUT_BUFFER_SIZE)
     {
	*Output_Bufferp++ = (unsigned char) ch;
     }
   else tt_write (&ch, 1);
}

/* this is supposed to be fast--- also handles 
   termcap: %d, &i, %., %+, %r strings as well as terminfo stuff */
static int tt_sprintf(char *buf, char *fmt, int x, int y)
{
   register unsigned char *f = (unsigned char *) fmt, *b, ch;
   int offset = 0, tinfo = 0;
   int stack[10];
   int i = 0, z;
   stack[0] = y; stack[1] = x; i = 2;
   
   b = (unsigned char *) buf;
   if (fmt != NULL) while ((ch = *f++) != 0)
     {
	if (ch != '%') *b++ = ch;
	else 
	  {
	     ch = *f++;
	     if (tinfo)
	       {
		  if ((ch <= '3') && (ch >= '0'))
		    {
		       /* map it to termcap.  Since this is terminfo,
			* it must be one of:
			*   %2d, %3d, %02d, %03d
			* 
			* I am assuming that a terminal that understands
			* %2d form will also understand the %02d form.  These
			* only differ by a space padding the field.
			*/
		       
		       /* skip the 'd'-- hope it is there */
		       if (ch == '0') 
			 {
			    ch = *f;
			    f += 2;
			 }
		       else f++;
		    }
	       }
	     
	     if (ch == 'p')
	       {
		  tinfo = 1;
		  ch = *f++;
		  if (ch == '1') stack[i++] = x; else stack[i++] = y;
	       }
	     else if (ch == '\'')   /* 'x' */
	       {
		  stack[i++] = *f++;
		  f++;
	       }
	     else if ((ch == 'd') || (ch == '2') || (ch == '3'))
	       {
		  z = stack[--i];
		  z += offset;
		  if (z >= 100)
		    {
		       *b++ = z / 100 + '0';
		       z = z % 100;
		       goto ten;
		    }
		  else if (ch == 3) 
		    {
		       *b++ = '0';
		       ch = '2';
		    }
		  
		  if (z >= 10)
		    {
		       ten:
		       *b++ = z / 10 + '0';
		       z = z % 10;
		    }
		  else if (ch == 2) *b++ = '0';
		  
		  *b++ = z + '0';
	       }
	     else if (ch == 'i') 
	       {
		  offset = 1;
	       }
	     else if (ch == '+')
	       {
		  if (tinfo) 
		    {
		       z = stack[--i];
		       stack[i-1] += z;
		    }
		  else
		    {
		       ch = *f++;
		       if ((unsigned char) ch == 128) ch = 0;
		       ch = ch + (unsigned char) stack[--i];
		       if (ch == '\n') ch++;
		       *b++ = ch;
		    }
	       }
	     else if (ch == 'r')
	       {
		  stack[0] = x;
		  stack[1] = y;
	       }
	     else if ((ch == '.') || (ch == 'c'))
	       {
		  ch = (unsigned char) stack[--i];
		  if (ch == '\n') ch++;
		  *b++ = ch;
	       }
	     else *b++ = ch;
	  }
     }
   *b = 0;
   return((int) (b - (unsigned char *) buf));
}

static void tt_printf(char *fmt, int x, int y)
{
   char buf[256];
   int n;
   if (fmt == NULL) return;
   n = tt_sprintf(buf, fmt, x, y);
   tt_write(buf, n);
}


void SLtt_set_scroll_region (int r1, int r2)
{
   Scroll_r1 = r1;
   Scroll_r2 = r2;
   tt_printf (Scroll_R_Str, Scroll_r1, Scroll_r2);
   Cursor_Set = 0;
}

void SLtt_reset_scroll_region (void)
{
    SLtt_set_scroll_region(0, SLtt_Screen_Rows - 1);
}

int SLtt_set_cursor_visibility (int show)
{
   if (show) 
     {
	if (Cursor_Visible_Str != NULL) SLtt_write_string (Cursor_Visible_Str);
	else return -1;
	return 0;
     }
   if (Cursor_Invisible_Str != NULL) SLtt_write_string (Cursor_Invisible_Str);
   else return -1;
   return 0;
}

   
/* the goto_rc function moves to row relative to scrolling region */
void SLtt_goto_rc(int r, int c)
{
   char *s = NULL;
   int n;
   char buf[6];
   
   if (c < 0)
     {
	c = -c - 1;
	Cursor_Set = 0;
     }
   
   /* if (No_Move_In_Standout && Current_Fgbg) SLtt_normal_video (); */
   r += Scroll_r1;
   
   if ((Cursor_Set > 0) || ((Cursor_Set < 0) && !Automatic_Margins))
     {
	n = r - Cursor_r;
	if ((n == -1) && (Cursor_Set > 0) && (Cursor_c == c) 
	    && (Curs_Up_Str != NULL))
	  {
	     s = Curs_Up_Str;
	  }
	else if ((n >= 0) && (n <= 4))
	  {
	     if ((n == 0) && (Cursor_Set == 1) 
		 && ((c > 1) || (c == Cursor_c)))
	       {
		  if (Cursor_c == c) return;
		  if (Cursor_c == c + 1) 
		    {
		       s = buf;
		       *s++ = '\b'; *s = 0;
		       s = buf;
		    }
	       }
	     else if (c == 0)
	       {
		  s = buf;
		  if ((Cursor_Set != 1) || (Cursor_c != 0)) *s++ = '\r';
		  while (n--) *s++ = '\n';
#ifdef VMS
		  /* Need to add this after \n to start a new record.  Sheesh. */
		  *s++ = '\r';
#endif
		  *s = 0;
		  s = buf;
	       }
	     /* Will fail on VMS */
#ifndef VMS
	     else if (SLtt_Newline_Ok && (Cursor_Set == 1) && 
		      (Cursor_c >= c) && (c + 3 > Cursor_c))
	       {
		  s = buf;
		  while (n--) *s++ = '\n';
		  n = Cursor_c - c;
		  while (n--) *s++ = '\b';
		  *s = 0;
		  s = buf;
	       }
#endif
	  }
     }
   if (s != NULL) SLtt_write_string(s);
   else tt_printf(Curs_Pos_Str, r, c);
   Cursor_c = c; Cursor_r = r;
   Cursor_Set = 1;
}

void SLtt_begin_insert (void)
{
   SLtt_write_string(Ins_Mode_Str);
}

void SLtt_end_insert (void)
{
   SLtt_write_string(Eins_Mode_Str);
}

void SLtt_delete_char (void)
{
   SLtt_normal_video ();
   SLtt_write_string(Del_Char_Str);
}

void SLtt_erase_line (void)
{
   char *s;
   
   Current_Fgbg = 0xFFFFFFFF;
   SLtt_write_string("\r");
   Cursor_Set = 1; Cursor_c = 0;
   if (SLtt_Use_Ansi_Colors) s = "\033[0m";
   else s = Norm_Vid_Str;
   SLtt_write_string(s);
   SLtt_del_eol();
}

void SLtt_delete_nlines (int n)
{
   int r1, curs;
   char buf[132];
   if (!n) return;
   SLtt_normal_video ();
   if (Del_N_Lines_Str != NULL) tt_printf(Del_N_Lines_Str,n, 0);
   else
   /* get a new terminal */
     {
	r1 = Scroll_r1;
	curs = Cursor_r;
	SLtt_set_scroll_region(curs, Scroll_r2);
	SLtt_goto_rc(Scroll_r2 - Scroll_r1, 0);
	MEMSET(buf, '\n', n);
	tt_write(buf, n);
	/* while (n--) tt_putchar('\n'); */
	SLtt_set_scroll_region(r1, Scroll_r2);
	SLtt_goto_rc(curs, 0);
     }
}

void SLtt_cls (void)
{
   SLtt_normal_video();
   SLtt_reset_scroll_region ();
   SLtt_write_string(Cls_Str);
}

void SLtt_reverse_index (int n)
{
   if (!n) return;
   
   SLtt_normal_video();
   if (Add_N_Lines_Str != NULL) tt_printf(Add_N_Lines_Str,n, 0);
   else
     {
	while(n--) SLtt_write_string(Rev_Scroll_Str);
     }
}


int SLtt_Ignore_Beep = 3;
static char *Visible_Bell_Str;

void SLtt_beep (void)
{
   if (SLtt_Ignore_Beep & 0x1) SLtt_putchar('\007');
   
   if (SLtt_Ignore_Beep & 0x2)
     {
	if (Visible_Bell_Str != NULL) SLtt_write_string (Visible_Bell_Str);
#ifdef linux
	else if (Linux_Console)
	  {
	     SLtt_write_string ("\033[?5h");
	     SLtt_flush_output ();
	     usleep (50000);
	     SLtt_write_string ("\033[?5l");
	  }
#endif
     }
   SLtt_flush_output ();
}

void SLtt_del_eol (void)
{
   if (Current_Fgbg != 0xFFFFFFFF) SLtt_normal_video ();
   SLtt_write_string(Del_Eol_Str);
}

typedef struct
{
   char *name;
   SLtt_Char_Type color;
}
Color_Def_Type;

static Color_Def_Type Color_Defs[16] =
{
   {"black",		0x00000000},
   {"red",		0x00000100},
   {"green",		0x00000200},
   {"brown",		0x00000300},
   {"blue",		0x00000400},
   {"magenta",		0x00000500},
   {"cyan",		0x00000600},
   {"lightgray",	0x00000700},
   {"gray",		0x00000800},
   {"brightred",	0x00000900},
   {"brightgreen",	0x00000A00},
   {"yellow",		0x00000B00},
   {"brightblue",	0x00000C00},
   {"brightmagenta",	0x00000D00},
   {"brightcyan",	0x00000E00},
   {"white",		0x00000F00}
};

void SLtt_set_mono (int obj, char *what, SLtt_Char_Type mask)
{
   if ((obj < 0) || (obj >= JMAX_COLORS))
     {
	return;
     }   
   Ansi_Color_Map[obj].mono = mask & ATTR_MASK;
}



static unsigned char FgBg_Stats[JMAX_COLORS];

void SLtt_set_color_object (int obj, SLtt_Char_Type attr)
{
   char *cust_esc;
   
   if ((obj < 0) || (obj >= JMAX_COLORS)) return;
     
   cust_esc = Ansi_Color_Map[obj].custom_esc;
   if (cust_esc != NULL) 
     {
	SLFREE (cust_esc);
	FgBg_Stats[(Ansi_Color_Map[obj].fgbg >> 8) & 0x7F] -= 1;
	Ansi_Color_Map[obj].custom_esc = NULL;
     }
   
   Ansi_Color_Map[obj].fgbg = attr;
}

void SLtt_add_color_attribute (int obj, SLtt_Char_Type attr)
{
   if ((obj < 0) || (obj >= JMAX_COLORS)) return;
     
   Ansi_Color_Map[obj].fgbg |= (attr & ATTR_MASK);
}


void SLtt_set_color (int obj, char *what, char *fg, char *bg)
{
   int i;
   SLtt_Char_Type f = 0xFFFFFFFF, g = 0xFFFFFFFF;
   SLtt_Char_Type attr = 0;
   
   if ((obj < 0) || (obj >= JMAX_COLORS))
     {
	return;
     }
   
   i = 0; while (i < 16)
     {
	if (!strcmp(fg, Color_Defs[i].name))
	  {
	     f = Color_Defs[i].color;
	     if (f & 0x800)
	       {
		  f &= 0x700;
		  attr = SLTT_BOLD_MASK;
	       }
	     
	     break;
	  }
	i++;
     }
   i = 0; while (i < 16)
     {
	if (!strcmp(bg, Color_Defs[i].name))
	  {
	     g = Color_Defs[i].color;
	     g &= 0x700;
	     g = g << 8;
	     break;
	  }
	i++;
     }
   
   if ((f == 0xFFFFFFFF) || (g == 0xFFFFFFFF) 
       || ((f == g) && (attr == 0)))
     return;

   SLtt_set_color_object (obj, f | g | attr);
}

void SLtt_set_color_esc (int obj, char *esc)
{
   char *cust_esc;
   SLtt_Char_Type fgbg = 0;
   int i;
   
   if ((obj < 0) || (obj >= JMAX_COLORS))
     {
	return;
     }
   
   cust_esc = Ansi_Color_Map[obj].custom_esc;
   if (cust_esc != NULL) 
     {
	SLFREE (cust_esc);
	FgBg_Stats[(Ansi_Color_Map[obj].fgbg >> 8) & 0x7F] -= 1;
     }
   
   cust_esc = (char *) SLMALLOC (strlen(esc) + 1);
   if (cust_esc != NULL) strcpy (cust_esc, esc);
   
   Ansi_Color_Map[obj].custom_esc = cust_esc;
   if (cust_esc == NULL) fgbg = 0;
   else
     {
	/* The whole point of this is to generate a unique fgbg */
	for (i = 0; i < JMAX_COLORS; i++)
	  {
	     if (FgBg_Stats[i] == 0) fgbg = i;
	     
	     if (obj == i) continue;
	     if ((Ansi_Color_Map[i].custom_esc) == NULL) continue;
	     if (!strcmp (Ansi_Color_Map[i].custom_esc, cust_esc))
	       {
		  fgbg = (Ansi_Color_Map[i].fgbg >> 8) & 0x7F;
		  break;
	       }
	  }
	FgBg_Stats[fgbg] += 1;
     }
   
   fgbg |= 0x80;
   Ansi_Color_Map[obj].fgbg = (fgbg | (fgbg << 8)) << 8;
}



void SLtt_set_alt_char_set (int i)
{
   static int last_i;
   if (SLtt_Has_Alt_Charset == 0) return;
   if (i == last_i) return;
   SLtt_write_string (i ? Start_Alt_Chars_Str : End_Alt_Chars_Str );
   last_i = i;
}

static void write_attributes (SLtt_Char_Type fgbg)
{
   int bg0, fg0;
   
   if (Worthless_Highlight) return;
   if (fgbg == Current_Fgbg) return;
   
   /* Before spitting out colors, fix attributes */
   if ((fgbg & ATTR_MASK) != (Current_Fgbg & ATTR_MASK))
     {
	if (Current_Fgbg & ATTR_MASK)
	  {
             SLtt_write_string(Norm_Vid_Str);
	     /* In case normal video turns off ALL attributes: */
	     if (fgbg & SLTT_ALTC_MASK)
	       Current_Fgbg &= ~SLTT_ALTC_MASK;
	     SLtt_set_alt_char_set (0);
	  }
	
	if ((fgbg & SLTT_ALTC_MASK) 
	    != (Current_Fgbg & SLTT_ALTC_MASK))
	  {
	     SLtt_set_alt_char_set ((int) (fgbg & SLTT_ALTC_MASK));
	  }
	
	if (fgbg & SLTT_ULINE_MASK) SLtt_write_string (UnderLine_Vid_Str);
	if (fgbg & SLTT_BOLD_MASK) SLtt_bold_video ();
	if (fgbg & SLTT_REV_MASK) SLtt_write_string (Rev_Vid_Str);
	if (fgbg & SLTT_BLINK_MASK)
	  {
	     /* Someday Linux will have a blink mode that set high intensity
	      * background.  Lets be prepared.
	      */
	     if (SLtt_Blink_Mode) SLtt_write_string (Blink_Vid_Str);
	  }
     }
   
   if (SLtt_Use_Ansi_Colors)
     {
	fg0 = (int) GET_FG(fgbg);
	bg0 = (int) GET_BG(fgbg);
	tt_printf(Color_Escape_Sequence, fg0, bg0);
     }
   Current_Fgbg = fgbg;
}

static int Video_Initialized;

void SLtt_reverse_video (int color)
{
   SLtt_Char_Type fgbg;
   char *esc;
   
   if (Worthless_Highlight) return;
   if ((color < 0) || (color >= JMAX_COLORS)) return;
   
   if (Video_Initialized == 0)
     {
	if (color == JNORMAL_COLOR)
	  {
	     SLtt_write_string (Norm_Vid_Str);
	  }
	else SLtt_write_string (Rev_Vid_Str);
	Current_Fgbg = 0xFFFFFFFF;
	return;
     }
   
   if (SLtt_Use_Ansi_Colors) 
     {
	fgbg = Ansi_Color_Map[color].fgbg;
	if ((esc = Ansi_Color_Map[color].custom_esc) != NULL)
	  {
	     if (fgbg != Current_Fgbg)
	       {
		  Current_Fgbg = fgbg;
		  SLtt_write_string (esc);
		  return;
	       }
	  }
     }
   else fgbg = Ansi_Color_Map[color].mono;

   if (fgbg == Current_Fgbg) return;
   write_attributes (fgbg);
}




void SLtt_normal_video (void)
{
   SLtt_reverse_video(JNORMAL_COLOR);
}

void SLtt_narrow_width (void)
{
    SLtt_write_string("\033[?3l");
}

void SLtt_wide_width (void)
{
    SLtt_write_string("\033[?3h");
}

/* Highest bit represents the character set. */
#ifdef linux_unicode
#define COLOR_MASK 0x7F0000
#else
#define COLOR_MASK 0x7F00
#endif

#ifdef linux_unicode
#define COLOR_OF(x) (((unsigned int)(x) & COLOR_MASK) >> 16)
#else
#define COLOR_OF(x) (((unsigned int)(x) & COLOR_MASK) >> 8)
#endif
/*
#define COLOR_EQS(a, b) \
   (Ansi_Color_Map[COLOR_OF(a)].fgbg == Ansi_Color_Map[COLOR_OF(b)].fgbg)
*/

#define COLOR_EQS(a, b) \
   (SLtt_Use_Ansi_Colors \
    ? (Ansi_Color_Map[COLOR_OF(a)].fgbg == Ansi_Color_Map[COLOR_OF(b)].fgbg)\
    :  (Ansi_Color_Map[COLOR_OF(a)].mono == Ansi_Color_Map[COLOR_OF(b)].mono))


#define CHAR_EQS(a, b) (((a) == (b))\
			|| ((((a) & ~COLOR_MASK) == ((b) & ~COLOR_MASK))\
			    && COLOR_EQS((a), (b))))

#ifdef linux_unicode
/* Kernel doesn't support 32bit Unicode, so there is no reason to convert it */
static char *SLtt_Unicode_UTF (unsigned short ch, char *p)
{
   if (ch < 0x80)
     *p++ = (unsigned char) ch;
   else if (ch < 0x800)
     {
       *p++ = (unsigned char) (0xC0 + (ch >> 6));
       *p++ = (unsigned char) 0x80 + (ch & 0x3f);
     }
   else
     {
       *p++ = (unsigned char) (0xE0 + (ch >> 12));
       *p++ = (unsigned char) 0x80 + ((ch >> 6) & 0x3f);
       *p++ = (unsigned char) 0x80 + (ch & 0x3f);
     }
   return p;
}
#endif

static void send_attr_str(SLang_Char_Type *s)
{
   unsigned char out[256], *p;
#ifdef linux_unicode
   unsigned char *q;
   unsigned short ch;
#else
   unsigned char ch;
#endif
   register SLtt_Char_Type attr;
   register SLang_Char_Type sh;
   int color, last_color = -1;
   
   p = out;
   while (0 != (sh = *s++))
     {
#ifdef linux_unicode
        ch = sh & 0xFFFF;
        color = ((int) sh & 0xFF0000) >> 16;
#else
	ch = sh & 0xFF;
	color = ((int) sh & 0xFF00) >> 8;
#endif	  
	if (color != last_color)
	  {
	     if (SLtt_Use_Ansi_Colors) attr = Ansi_Color_Map[color & 0x7F].fgbg;
	     else attr = Ansi_Color_Map[color & 0x7F].mono;
	     
	     if (color & 0x80) /* alternate char set */
	       {
		  if (SLtt_Use_Blink_For_ACS)
		    {
		       if (SLtt_Blink_Mode) attr |= SLTT_BLINK_MASK;
		    }
		  else attr |= SLTT_ALTC_MASK;   
	       }
	     
	     if (attr != Current_Fgbg)
	       {
		  if ((ch != ' ') ||
		      /* it is a space so only consider it different if it
		       * has different attributes.
		       */
		      (attr & BGALL_MASK) != (Current_Fgbg & BGALL_MASK))
		    {
		       if (p != out)
			 {
			    *p = 0;
			    SLtt_write_string((char *) out);
			    Cursor_c += (int) (p - out);
			    p = out;
			 }
		       
		       if (SLtt_Use_Ansi_Colors && (NULL != Ansi_Color_Map[color & 0x7F].custom_esc))
			 {
			    SLtt_write_string (Ansi_Color_Map[color & 0x7F].custom_esc);
                           /* Just in case the custom escape sequence screwed up
                            * the alt character set state...
                            */
                           if ((attr & SLTT_ALTC_MASK) != (Current_Fgbg & SLTT_ALTC_MASK))
                             SLtt_set_alt_char_set ((int) (attr & SLTT_ALTC_MASK));
			    Current_Fgbg = attr;
			 }
		       else write_attributes (attr);
		       
		       last_color = color;
		    }
	       }
	  }
#ifdef linux_unicode
	if (SLtt_Unicode)
	  {
	    if (p > out + sizeof (out) - 6)
	      {
		*p = 0;
		SLtt_write_string((char *) out);
		Cursor_c += (int) (p - out);
		p = out;
	      }
	    q = SLtt_Unicode_UTF (ch, p);
	    Cursor_c -= (int) (q - p - 1);
	    p = q;
	  }
	else
#endif	  
	  *p++ = (unsigned char) ch;
     }
   *p = 0;
   if (p != out) SLtt_write_string((char *) out);
   Cursor_c += (int) (p - out);
}

static void forward_cursor (int n, int row)
{
   char buf[30];
   
   
   if (n <= 4) 
     {
	Cursor_c += n;
	SLtt_normal_video ();
	MEMSET (buf, ' ', n);
	tt_write (buf, n);
     }
   else if (Curs_F_Str != NULL)
     {
	Cursor_c += n;
	n = tt_sprintf(buf, Curs_F_Str, n, 0);
	tt_write(buf, n);
     }
   else SLtt_goto_rc (row, Cursor_c + n);
}

#ifdef linux_unicode
#define SPACE_CHAR (0x20 | (JNORMAL_COLOR << 16))
#else
#define SPACE_CHAR (0x20 | (JNORMAL_COLOR << 8))
#endif

void SLtt_smart_puts(SLang_Char_Type *neww, SLang_Char_Type *oldd, int len, int row)
{
   register SLang_Char_Type *p, *q, *qmax, *pmax, *buf;
   SLang_Char_Type buffer[256];
   int n_spaces;
   SLang_Char_Type *space_match, *last_buffered_match;
   
   q = oldd; p = neww;
   qmax = oldd + len;
   pmax = p + len;
   
   /* Find out where to begin --- while they match, we are ok */
   for (;;)
     {
	if (q == qmax) return;
	if (!CHAR_EQS(*q, *p)) break;
	q++; p++;
     }

   /*position the cursor */
   SLtt_goto_rc (row, (int) (p - neww));

   /* Find where the last non-blank character on old/new screen is */
   
   while (qmax > q)
     {
	qmax--;
	if (!CHAR_EQS(*qmax, SPACE_CHAR)) 
	  {
	     qmax++;
	     break;
	  }
     }
   
   while (pmax > p)
     {
	pmax--;
	if (!CHAR_EQS(*pmax, SPACE_CHAR))
	  {
	     pmax++;
	     break;
	  }
     }
   
   
   last_buffered_match = buf = buffer;		       /* buffer is empty */
   
   /* loop using overwrite then skip algorithm until done */
   while (1)
     {
	/* while they do not match and we do not hit a space, buffer them up */
	n_spaces = 0;
	while (p < pmax)
	  {
	     if (CHAR_EQS(*q,SPACE_CHAR) && CHAR_EQS(*p, SPACE_CHAR))
	       {
		  /* If *q is not a space, we would have to overwrite it.  
		   * However, if *q is a space, then while *p is also one, 
		   * we only need to skip over the blank field.
		   */
		  space_match = p;
		  p++; q++;
		  while ((p < pmax) 
			 && CHAR_EQS(*q,SPACE_CHAR) 
			 && CHAR_EQS(*p, SPACE_CHAR))
		    {
		       p++;
		       q++;
		    }
		  n_spaces = (int) (p - space_match);
		  break;
	       }
	     if (CHAR_EQS(*q, *p)) break;
	     *buf++ = *p++;
	     q++;
	  }
	*buf = 0;
	
	if (buf != buffer) send_attr_str (buffer);
	buf = buffer;
	
	if (n_spaces && (p < pmax)) 
	  {
	     forward_cursor (n_spaces, row);
	  }
	
	/* Now we overwrote what we could and cursor is placed at position 
	 * of a possible match of new and old.  If this is the case, skip 
	 * some more.
	 */
	
	while ((p < pmax) && CHAR_EQS(*p, *q))
	  {
	     *buf++ = *p++;
	     q++;
	  }
	
	last_buffered_match = buf;
	if (p >= pmax) break;
	
	/* jump to new position is it is greater than 5 otherwise
	 * let it sit in the buffer and output it later.
	 */
	if ((int) (buf - buffer) >= 5) 
	  {
	     forward_cursor (buf - buffer, row);
	     last_buffered_match = buf = buffer;
	  }
     }
   if (buf != buffer)
     {
	if (q < qmax)
	  {
	     if ((buf == last_buffered_match) 
		 && ((int) (buf - buffer) >= 5))
	       {
		  forward_cursor (buf - buffer, row);
	       }
	     else
	       {
		  *buf = 0;
		  send_attr_str (buffer);
	       }
	  }
     }
   if (q < qmax) SLtt_del_eol ();
   if (Automatic_Margins && (Cursor_c + 1 >= SLtt_Screen_Cols)) Cursor_Set = 0;
}  


/* termcap stuff */

#ifndef unix
#define unix
#endif

#ifdef unix
#define EXTERN extern

#ifndef USE_TERMCAP
static char *Tbuf;
static char *Tstr_Buf;

#define tgetstr SLtt_tigetstr
#define tgetent SLtt_tigetent
#define TGETNUM(x) SLtt_tigetnum((x), &Tbuf)
#define TGETFLAG(x) SLtt_tigetflag((x), &Tbuf)

#else

EXTERN char *tgetstr(char *, char **);
EXTERN int tgetent(char *, char *);
EXTERN int tgetnum(char *);
EXTERN int tgetflag(char *);
static char Tstr_Buf[1024];
static char Tbuf[4096];
#define TGETNUM tgetnum
#define TGETFLAG tgetflag
#endif

static char *my_tgetstr(char *what, char **p)
{
   register char *w, *w1;
   char *wsave;
   what = tgetstr(what, p);
   if (what != NULL)
     {
	/* lose pad info --- with today's technology, term is a loser if
	   it is really needed */
	while ((*what == '.') || 
	       ((*what >= '0') && (*what <= '9'))) what++;
	if (*what == '*') what++;	
	
	/* lose terminfo padding--- looks like $<...> */
        w = what;
	while (*w) if ((*w++ == '$') && (*w == '<'))
	  {
	     w1 = w - 1;
	     while (*w && (*w != '>')) w++;
	     if (*w == 0) break;
	     w++;
	     wsave = w1;
	     while ((*w1++ = *w++) != 0);
	     w = wsave;
	  }
	if (*what == 0) what = NULL; 
     }
   return(what);
}

char *SLtt_tgetstr (char *s)
{
#ifdef USE_TERMCAP
   static
#endif
     char *p = Tstr_Buf;
   return my_tgetstr (s, &p);
}

int SLtt_tgetnum (char *s)
{
   return TGETNUM (s);
}
int SLtt_tgetflag (char *s)
{
   return TGETFLAG (s);
}


static int Vt100_Like = 0;

void SLtt_get_terminfo (void)
{
   char *term, *t, ch;
   static int been_here;

#ifdef TIOCGWINSZ
#ifndef SCO_FLAVOR
   struct winsize size;

   if (ioctl (0, TIOCGWINSZ, &size) == 0){
       if (size.ws_row && size.ws_col){
	   SLtt_Screen_Cols = size.ws_col;
	   SLtt_Screen_Rows = size.ws_row;
       }
   }
#endif
#endif
   
   if (been_here)
       return;
   else
       been_here = 1;
   
   SLtt_Use_Ansi_Colors = (NULL != getenv ("COLORTERM"));

   if (NULL == (term = (char *) getenv("TERM")))
     {
	SLang_exit_error("TERM environment variable needs set.");
     }
   
   Linux_Console = (!strncmp (term, "linux", 5)
#ifdef linux
       || (!strncmp(term, "con", 3))
#endif
      );
   
   t = term;
   
   if (strcmp(t, "vt52") && (*t++ == 'v') && (*t++ == 't')
       && (ch = *t, (ch >= '1') && (ch <= '9'))) Vt100_Like = 1; 

#ifndef USE_TERMCAP
   if (NULL == (Tbuf = tgetent (term))) 
     {
	char err_buf[512];
	/* Special cases. */
	if (Vt100_Like
	    || Linux_Console
	    || !strcmp (term, "screen"))
	  {
	     int vt102 = 1;
	     if (!strcmp (term, "vt100")) vt102 = 0;

	     if (!(SLtt_Screen_Cols || SLtt_Screen_Rows)){
		 SLtt_Screen_Cols = 80;
		 SLtt_Screen_Rows = 24;
	     }
	     if (Linux_Console) SLtt_Screen_Rows++;
   	     SLtt_set_term_vtxxx (&vt102);
	     return;
	  }
	sprintf (err_buf, "Unknown terminal: %s\n\
Check the TERM environment variable.\n\
Also make sure that the terminal is defined in the terminfo database.\n\
Alternatively, set the TERMCAP environment variable to the desired\n\
termcap entry.", term);
	SLang_exit_error(err_buf);
     }
   Tstr_Buf = Tbuf;
#else				       /* USE_TERMCAP */
   if (1 != tgetent(Tbuf, term)) SLang_exit_error("Unknown terminal.");
#endif				       /* NOT USE_TERMCAP */
   
   if ((NULL == (Cls_Str = SLtt_tgetstr("cl"))) 
       || (NULL == (Curs_Pos_Str = SLtt_tgetstr("cm"))))
     {
	SLang_exit_error("Terminal not powerful enough for SLang.");
     }
   
   if ((NULL == (Ins_Mode_Str = SLtt_tgetstr("im")))
       || ( NULL == (Eins_Mode_Str = SLtt_tgetstr("ei")))
       || ( NULL == (Del_Char_Str = SLtt_tgetstr("dc"))))
     SLtt_Term_Cannot_Insert = 1;
   
   Visible_Bell_Str = SLtt_tgetstr ("vb");
   Curs_Up_Str = SLtt_tgetstr ("up");
   Rev_Scroll_Str = SLtt_tgetstr("sr");
   Del_N_Lines_Str = SLtt_tgetstr("DL");
   Add_N_Lines_Str = SLtt_tgetstr("AL");
   
   /* Actually these are used to initialize terminals that use cursor 
    * addressing.  Hard to believe.
    */
   Term_Init_Str = SLtt_tgetstr ("ti");
   Term_Reset_Str = SLtt_tgetstr ("te");
   
   /* Make up for defective termcap/terminfo databases */
   if ((Vt100_Like && (term[2] != '1'))
       || Linux_Console
       )
     {
	if (Del_N_Lines_Str == NULL) Del_N_Lines_Str = "\033[%dM";
	if (Add_N_Lines_Str == NULL) Add_N_Lines_Str = "\033[%dL";
     }
   
   Scroll_R_Str = SLtt_tgetstr("cs");
   
   if ((SLtt_Screen_Rows == 0) || (SLtt_Screen_Cols == 0))
     {
	char *envptr;
	if ((SLtt_Screen_Cols = TGETNUM("co")) <= 0) SLtt_Screen_Cols = 80;
	if ((SLtt_Screen_Rows = TGETNUM("li")) <= 0) SLtt_Screen_Rows = 24;
	envptr = getenv ("LINES");
	if (envptr != NULL)
	  {
	     if ((SLtt_Screen_Rows = atoi (envptr)) <= 0) 
	       SLtt_Screen_Rows = 24;
	  }
	
	envptr = getenv ("COLUMNS");
	if (envptr != NULL)
	  {
	     if ((SLtt_Screen_Cols = atoi (envptr)) <= 0) 
	       SLtt_Screen_Cols = 80;
	  }
     }
   
   is_xterm = (!strncmp (term, "xterm", 5) || !strcmp (term, "vs100"));
   
   if ((Scroll_R_Str == NULL) 
       || (((NULL == Del_N_Lines_Str) || (NULL == Add_N_Lines_Str))
	   && (NULL == Rev_Scroll_Str)))
     {
	if (is_xterm)
	  {
	     /* Defective termcap mode!!!! */
	     SLtt_set_term_vtxxx (&is_xterm);
	  }
	else SLtt_Term_Cannot_Scroll = 1;
     }

   if (!strncmp (term, "97801", 5))
       SLtt_Term_Cannot_Scroll = 1;

   Del_Eol_Str = SLtt_tgetstr("ce");

   Rev_Vid_Str = SLtt_tgetstr("mr");
   if (Rev_Vid_Str == NULL) Rev_Vid_Str = SLtt_tgetstr("so");
   
   Bold_Vid_Str = SLtt_tgetstr("md");
   Blink_Vid_Str = SLtt_tgetstr("mb");
   UnderLine_Vid_Str = SLtt_tgetstr("us");
   
   Start_Alt_Chars_Str = SLtt_tgetstr ("as");   /* smacs */
   End_Alt_Chars_Str = SLtt_tgetstr ("ae");   /* rmacs */
   Enable_Alt_Char_Set = SLtt_tgetstr ("eA");   /* enacs */
   SLtt_Graphics_Char_Pairs = SLtt_tgetstr ("ac");
   
   /* I have had it with defective console/linux terminfo entries provided 
    * by ncurses.  They are not reliable.  I cannot track every ncurses 
    * release and fudge my stuff to work with defective stuff.  For that
    * reason, I am hardcoding some things.
    */
   if (Linux_Console)
     {
#if 0
       char *lgcp = "l\332m\300k\277j\331u\264t\303v\301w\302q\304x\263n\053o\176s\137`\004a\260f\370g\361~\011,\020+\021.\031-\030h\261i\0250\333";

       if ((SLtt_Graphics_Char_Pairs == NULL)
           || (0 == strncmp (SLtt_Graphics_Char_Pairs, lgcp, 8)))
         {
            Start_Alt_Chars_Str = "\033(B\033)U\016";
            End_Alt_Chars_Str = "\033(B\033)0\017";
            Enable_Alt_Char_Set = NULL;
         }
#else
       char *lgcp = "`\004a\261f\370g\361h\260j\331k\277l\332m\300n\305o\302q\304r\362s_t\303u\264v\301w\302x\263y\371z\372{\373|\374}\375~";

       if ((SLtt_Graphics_Char_Pairs == NULL)
           || (0 == strncmp (SLtt_Graphics_Char_Pairs, lgcp, 16)))
         {
            SLtt_Graphics_Char_Pairs = lgcp;
            Start_Alt_Chars_Str = "\033[11m";
            End_Alt_Chars_Str = "\033[10m";
            Enable_Alt_Char_Set = NULL;
         }
#endif
     }
   else if (NULL == SLtt_Graphics_Char_Pairs)
     {
	/* make up for defective termcap/terminfo */
	if (Vt100_Like)
	  {
	     Start_Alt_Chars_Str = "\016";
	     End_Alt_Chars_Str = "\017";
	     Enable_Alt_Char_Set = "\033)0";
	  }
     }
   
   if (is_xterm)
     {
	Start_Alt_Chars_Str = "\016";
	End_Alt_Chars_Str = "\017";
	Enable_Alt_Char_Set = "\033(B\033)0";
     }
   
   if ((SLtt_Graphics_Char_Pairs == NULL) && 
       ((Start_Alt_Chars_Str == NULL) || (End_Alt_Chars_Str == NULL)))
     {
	SLtt_Has_Alt_Charset = 0;
	Enable_Alt_Char_Set = NULL;
     }
   else SLtt_Has_Alt_Charset = 1;
   
       
   
   if (NULL == (Norm_Vid_Str = SLtt_tgetstr("me"))) 
     {
	Norm_Vid_Str = SLtt_tgetstr("se");
     }
   
   Cursor_Invisible_Str = SLtt_tgetstr("vi");
   Cursor_Visible_Str = SLtt_tgetstr("vs");
   
   Curs_F_Str = SLtt_tgetstr("RI");
   
#if 0
   if (NULL != Curs_F_Str)
     {
	 Len_Curs_F_Str = strlen(Curs_F_Str);
     }
   else Len_Curs_F_Str = strlen(Curs_Pos_Str);
#endif
   
   Automatic_Margins = TGETFLAG ("am");
   /* No_Move_In_Standout = !TGETFLAG ("ms"); */
   Worthless_Highlight = TGETFLAG ("xs");
   
   if (Worthless_Highlight == 0)
     {
	Worthless_Highlight = (TGETNUM ("sg") > 0);
     }
}

#endif
/* Unix */

/* specific to vtxxx only */
void SLtt_enable_cursor_keys (void)
{
#ifdef unix
   if (Vt100_Like ||
#ifdef linux
       Linux_Console ||
#endif
       is_xterm)
#endif
   SLtt_write_string("\033=\033[?1l");
}

#ifdef VMS
void SLtt_get_terminfo ()
{
   int zero = 0;
   SLtt_set_term_vtxxx(&zero);
   Start_Alt_Chars_Str = "\016";
   End_Alt_Chars_Str = "\017";
   SLtt_Has_Alt_Charset = 1;
   SLtt_Graphics_Char_Pairs = "aaffgghhjjkkllmmnnooqqssttuuvvwwxx";
   Enable_Alt_Char_Set = "\033(B\033)0";    
}
#endif

/* This sets term for vt102 terminals it parameter vt100 is 0.  If vt100
 * is non-zero, set terminal appropriate for a only vt100  
 * (no add line capability). */
							   
void SLtt_set_term_vtxxx(int *vt100)
{
   Norm_Vid_Str = "\033[m";
   
   Scroll_R_Str = "\033[%i%d;%dr"; 
   Cls_Str = "\033[2J\033[H";
   Rev_Vid_Str = "\033[7m";
   Bold_Vid_Str = "\033[1m";
   Blink_Vid_Str = "\033[5m";
   UnderLine_Vid_Str = "\033[4m";
   Del_Eol_Str = "\033[K";
   Rev_Scroll_Str = "\033M";
   Curs_F_Str = "\033[%dC";
   /* Len_Curs_F_Str = 5; */
   Curs_Pos_Str = "\033[%i%d;%dH";
   if (*vt100 == 0)
     {
	Ins_Mode_Str = "\033[4h";
	Eins_Mode_Str = "\033[4l";
	Del_Char_Str =  "\033[P";
	Del_N_Lines_Str = "\033[%dM";
	Add_N_Lines_Str = "\033[%dL";
	SLtt_Term_Cannot_Insert = 0;
     }
   else
     {
	Del_N_Lines_Str = NULL;
	Add_N_Lines_Str = NULL;
	SLtt_Term_Cannot_Insert = 1;
     }
   SLtt_Term_Cannot_Scroll = 0;
   /* No_Move_In_Standout = 0; */
}

#ifdef linux_unicode
static unsigned short unifix[] = {
SLUNI_DHLINE_CHAR, SLUNI_HLINE_CHAR,
SLUNI_DVLINE_CHAR, SLUNI_VLINE_CHAR,
SLUNI_DULCORN_CHAR, SLUNI_ULCORN_CHAR,
SLUNI_DURCORN_CHAR, SLUNI_URCORN_CHAR,
SLUNI_DLLCORN_CHAR, SLUNI_LLCORN_CHAR,
SLUNI_DLRCORN_CHAR, SLUNI_LRCORN_CHAR,
SLUNI_DRTEE_CHAR, SLUNI_RTEE_CHAR,
SLUNI_DLTEE_CHAR, SLUNI_LTEE_CHAR,
SLUNI_DUTEE_CHAR, SLUNI_UTEE_CHAR,
SLUNI_DDTEE_CHAR, SLUNI_DTEE_CHAR,
SLUNI_DCROSS_CHAR, SLUNI_CROSS_CHAR,
SLUNI_HLINE_CHAR, '|',
SLUNI_VLINE_CHAR, '-',
SLUNI_ULCORN_CHAR, '+',
SLUNI_URCORN_CHAR, '+',
SLUNI_LLCORN_CHAR, '+',
SLUNI_LRCORN_CHAR, '+',
SLUNI_RTEE_CHAR, '+',
SLUNI_LTEE_CHAR, '+',
SLUNI_UTEE_CHAR, '+',
SLUNI_DTEE_CHAR, '+',
SLUNI_CROSS_CHAR, '+',
SLUNI_DSRTEE_CHAR, SLUNI_RTEE_CHAR,
SLUNI_DSLTEE_CHAR, SLUNI_LTEE_CHAR,
SLUNI_DSUTEE_CHAR, SLUNI_UTEE_CHAR,
SLUNI_DSDTEE_CHAR, SLUNI_DTEE_CHAR,
SLUNI_SDRTEE_CHAR, SLUNI_RTEE_CHAR,
SLUNI_SDLTEE_CHAR, SLUNI_LTEE_CHAR,
SLUNI_SDUTEE_CHAR, SLUNI_UTEE_CHAR,
SLUNI_SDDTEE_CHAR, SLUNI_DTEE_CHAR,
SLUNI_CKBRD_CHAR, 'o'
};

unsigned short SLtt_fix_unicode (unsigned short ch)
{
   int i;
   
   for (i = 0; i < sizeof (unifix) / sizeof (unifix [0]); i += 2)
      if (ch == unifix [i])
          return unifix [i + 1];
   return ch;
}

int SLtt_unicode_init (void)
{
   struct unipair *list = NULL;
   struct unimapdesc descr;
   int i, j;
   
   descr.entry_ct = 0;
   descr.entries = 0;
   if (ioctl(0, GIO_UNIMAP, (unsigned long) &descr))
      {
          if (errno != ENOMEM || descr.entry_ct == 0)
            {
               SLtt_Unicode = 0;
               return 0;
            }
          descr.entries = list = (struct unipair *) SLMALLOC(descr.entry_ct * 
               sizeof (struct unipair));
          if (ioctl (0, GIO_UNIMAP, (unsigned long) &descr))
            {
               SLFREE(list);
               SLtt_Unicode = 0;
               return 0;
            }
      }
   for (i = 0; i < descr.entry_ct; i++)
      for (j = 0; j < sizeof (unifix) / sizeof (unifix [0]); j += 2)
         if (list [i].unicode == unifix [j])
            {
               unifix [j + 1] = unifix [j];
               break;
            }
   for (i = 0; i < sizeof (unifix) / sizeof (unifix [0]); i += 2)
     {
        if (unifix [i] == unifix [i + 1] || unifix [i + 1] < 127)
           continue;
        for (j = 0; j < sizeof (unifix) / sizeof (unifix [0]); j += 2)
           if (unifix [j] == unifix [i + 1])
              {
                 unifix [i + 1] = unifix [j + 1];
                 break;
              }
     }
   if (list)
      SLFREE(list);
   return 1;
}
#else /*linux unicode */
unsigned short SLtt_fix_unicode (unsigned short ch)
{
   return ch;
}
#endif

void SLtt_init_video (void)
{
   static int last_i;
   /*   send_string_to_term("\033[?6h"); */
   /* relative origin mode */
   last_i = 0;
   SLtt_write_string (Term_Init_Str);
   SLtt_reset_scroll_region();
   SLtt_end_insert();
   SLtt_write_string (Enable_Alt_Char_Set);
#ifdef linux_unicode
   if (!SLtt_Unicode)
     {
        char *term = (char *) getenv ("TERM");
   
        if (term && (!strncmp(term, "con", 3) || !strncmp (term, "linux", 5)))
          {
             term = (char *) getenv ("LC_CTYPE");
             if (!term) term = (char *) getenv ("LANG");
             if (term && strchr (term, '8') && 
                 (strstr (term, "UTF") || strstr (term, "utf")))
               {
                  SLtt_Unicode = 1;
                  SLtt_DefaultUnicode = 1;
               }
          }
     }
   if (SLtt_Unicode)
     {
        if (SLtt_unicode_init ())
            SLtt_write_string ("\033%8");
     }
#endif   
   Video_Initialized = 1;
}

void SLtt_want_unicode (void)
{
#ifdef linux_unicode
   char *term = (char *) getenv ("TERM");
   
   if (term)
      if (!strncmp(term, "con", 3) || !strncmp (term, "linux", 5))
         SLtt_Unicode = 1;
#endif
}

void SLtt_reset_video (void)
{
   SLtt_goto_rc (SLtt_Screen_Rows - 1, 0);
   Cursor_Set = 0;
   SLtt_normal_video ();	       /* MSKermit requires this  */
   SLtt_write_string(Norm_Vid_Str);
   Current_Fgbg = 0xFFFFFFFF;
   SLtt_set_alt_char_set (0);
   SLtt_write_string ("\n");
   SLtt_del_eol ();
#ifdef linux_unicode
   if (SLtt_Unicode && !SLtt_DefaultUnicode)
     {
        SLtt_write_string ("\033%@");
     }
#endif   
   SLtt_write_string (Term_Reset_Str);
   SLtt_flush_output ();
   Video_Initialized = 0;
}

void SLtt_bold_video (void)
{
   SLtt_write_string (Bold_Vid_Str);
}

#if 0
/* Midnight Commandes does not use this */
int SLtt_set_mouse_mode (int mode, int force)
{
   char *term;
   
   if (force == 0)
     {
       if (NULL == (term = (char *) getenv("TERM"))) return -1;
       if (strncmp ("xterm", term, 5))
         return -1;
     }
   
   if (mode)
     SLtt_write_string ("\033[?9h");
   else
     SLtt_write_string ("\033[?9l");
   
   return 0;
}
#endif
