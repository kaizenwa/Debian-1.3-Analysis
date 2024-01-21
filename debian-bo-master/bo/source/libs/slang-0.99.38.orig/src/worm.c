/* 
 *	  @@@        @@@    @@@@@@@@@@     @@@@@@@@@@@    @@@@@@@@@@@@
 *	  @@@        @@@   @@@@@@@@@@@@    @@@@@@@@@@@@   @@@@@@@@@@@@@
 *	  @@@        @@@  @@@@      @@@@   @@@@           @@@@ @@@  @@@@
 *	  @@@   @@   @@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	  @@@  @@@@  @@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	  @@@@ @@@@ @@@@  @@@        @@@   @@@            @@@  @@@   @@@
 *	   @@@@@@@@@@@@   @@@@      @@@@   @@@            @@@  @@@   @@@
 *	    @@@@  @@@@     @@@@@@@@@@@@    @@@            @@@  @@@   @@@
 *	     @@    @@       @@@@@@@@@@     @@@            @@@  @@@   @@@
 * 
 *				  Eric P. Scott
 *			   Caltech High Energy Physics
 *				  October, 1980
 *
 * Bug fixes and S-Lang support by JED.
 */
#include "config.h"

#include <string.h>

#include "slang.h"
#include "jdmacros.h"

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <stdio.h>
#include <signal.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef __WATCOMC__
# include <string.h>
#endif

static unsigned char *safe_malloc (unsigned int size)
{
   unsigned char *p;
   p = (unsigned char *) SLMALLOC (size);
   if (p == NULL)
     {
	fprintf(stderr, "Malloc error.");
	exit (-1);
     }
   SLMEMSET ((char *) p, 0, size);
   return p;
}


#define cursor(col,row) SLsmg_gotorc(row,col)

/* #define ACS_CHAR '`' */
#define ACS_CHAR ((char) ('+' | 0x80))

int Wrap;
short *ref[128];
static char flavor[]=
{
   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     'A', 'B', 'C', 'D', 'E', 'F'
};
static short xinc[]=
{
   1,  1,  1,  0, -1, -1, -1,  0
}, yinc[]=
{
   -1,  0,  1,  1,  1,  0, -1, -1
};

#define MAX_WORMS 1000
static struct worm 
{
   int orientation, head;
   short *xpos, *ypos;
}
worm[MAX_WORMS];
static char *field;
static int length=16, number=3, trail=' ';
static struct options 
{
   int nopts;
   int opts[3];
}
normal[8]=
{
     { 3, { 7, 0, 1 } },
     { 3, { 0, 1, 2 } },
     { 3, { 1, 2, 3 } },
     { 3, { 2, 3, 4 } },
     { 3, { 3, 4, 5 } },
     { 3, { 4, 5, 6 } },
     { 3, { 5, 6, 7 } },
     { 3, { 6, 7, 0 } }
}, upper[8]=
{
     { 1, { 1, 0, 0 } },
     { 2, { 1, 2, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 2, { 4, 5, 0 } },
     { 1, { 5, 0, 0 } },
     { 2, { 1, 5, 0 } }
}, left[8]=
{
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 2, { 2, 3, 0 } },
     { 1, { 3, 0, 0 } },
     { 2, { 3, 7, 0 } },
     { 1, { 7, 0, 0 } },
     { 2, { 7, 0, 0 } }
}, right[8]=
{
     { 1, { 7, 0, 0 } },
     { 2, { 3, 7, 0 } },
     { 1, { 3, 0, 0 } },
     { 2, { 3, 4, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 2, { 6, 7, 0 } }
}, lower[8]=
{
     { 0, { 0, 0, 0 } },
     { 2, { 0, 1, 0 } },
     { 1, { 1, 0, 0 } },
     { 2, { 1, 5, 0 } },
     { 1, { 5, 0, 0 } },
     { 2, { 5, 6, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } }
}, upleft[8]=
{
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 1, { 3, 0, 0 } },
     { 2, { 1, 3, 0 } },
     { 1, { 1, 0, 0 } }
}, upright[8]=
{
     { 2, { 3, 5, 0 } },
     { 1, { 3, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 1, { 5, 0, 0 } }
}, lowleft[8]=
{
     { 3, { 7, 0, 1 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 1, { 1, 0, 0 } },
     { 2, { 1, 7, 0 } },
     { 1, { 7, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } }
}, lowright[8]=
{
     { 0, { 0, 0, 0 } },
     { 1, { 7, 0, 0 } },
     { 2, { 5, 7, 0 } },
     { 1, { 5, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } },
     { 0, { 0, 0, 0 } }
};

static void draw_the_box (void)
{
   char *msg = "Press Any Key to Quit.";
   int r = 7, c = 45, dr = 8, dc = 4 + strlen (msg);
   
   SLsmg_set_color (1);
# ifndef pc_system
   SLsmg_set_char_set (1);
# endif
   SLsmg_fill_region (r + 1, c + 1, dr - 2, dc - 2, SLSMG_CKBRD_CHAR);
# ifndef pc_system
   SLsmg_set_char_set (0);
# endif
   SLsmg_set_color (0);
   SLsmg_gotorc (r + dr/2, c + 2); SLsmg_write_string (msg);
   SLsmg_draw_box (r, c, dr, dc);
   SLsmg_gotorc (0, 0);
}



static void onsig (int);
static float ranf (void);

int main(int argc, char *argv[])
{
   struct worm *w;
   struct options *op;
   short *ip;
   int h, n, x, y, last, bottom, counts = -1;
   char *fg = NULL, *bg = NULL;
   
   for (x = 1; x < argc; x++)
     {
	register char *p;
	p = argv[x];
	if (*p == '-' ) p++;
	switch (*p)
	  {
	   case 'f':
	     field="WORM";
	     break;
	   case 'l':
	     if (++x==argc) goto usage;
	     if ((length=atoi(argv[x]))<2||length>1024) 
	       {
		  fprintf(stderr,"%s: Invalid length\n",*argv);
		  exit(1);
	       }
	     break;
	   case 'n':
	     if ( ++x == argc)
	       goto usage;
	     if ((number=atoi(argv[x]))<1||number>MAX_WORMS)
	       {
		  fprintf(stderr,"%s: Invalid number of worms (%d max)\n",
			  *argv, MAX_WORMS);
		  exit(1);
	       }
	     break;
	   case 't':
	     trail='.';
	     break;
	     
	   case 'c':		/* -color */
	     if (++x == argc)  goto usage;
	     fg = argv[x];
	     if (++x == argc)  goto usage;
	     bg = argv[x];
	     break;
	   default:
	     usage:
	     fprintf (stderr,
		      "usage: %s [-field] [-length #] [-number #] [-trail] [-color fg bg]\n",
		      *argv);
	     exit (1);
	     break;
	  }
     }
   
   SLang_init_tty (-1, 0, 0);
   /* SLtt_init_video (); */
   if ( fg != NULL && bg != NULL )
     SLtt_set_color (0, NULL, fg, bg);
   SLtt_set_cursor_visibility (0);

#ifdef SIGINT
   signal(SIGINT, onsig);
#endif
   
#ifdef msdos
   SLtt_Use_Ansi_Colors = 1;
#endif
   SLtt_Term_Cannot_Scroll = 1;
   
   SLtt_get_terminfo ();
   SLsmg_init_smg ();
   
   if (SLtt_Has_Status_Line > 0)
     SLtt_write_to_status_line ("SLANG WORM DEMO", 0);
   
   bottom = SLtt_Screen_Rows - 1;
   last = SLtt_Screen_Cols - 1;
   
   ip=(short *)safe_malloc(SLtt_Screen_Rows*SLtt_Screen_Cols*sizeof (short));
   
   for (n=0;n<SLtt_Screen_Rows;) 
     {
	ref[n++]=ip; ip+=SLtt_Screen_Cols;
     }
   for (ip=ref[0],n=SLtt_Screen_Rows*SLtt_Screen_Cols;--n>=0;) *ip++=0;
   ref[bottom][last]=0;
   for (n=number, w= &worm[0];--n>=0;w++) 
     {
	w->orientation=w->head=0;
	if (!(ip=(short *)safe_malloc(length*sizeof (short)))) 
	  {
	     fprintf(stderr,"%s: out of memory\n",*argv);
	     exit(1);
	  }
	w->xpos=ip;
	for (x = 0; x < length; x++) ip[x] = -1;
	if (!(ip=(short *)safe_malloc(length*sizeof (short)))) 
	  {
	     fprintf(stderr,"%s: out of memory\n",*argv);
	     exit(1);
	  }
	w->ypos=ip;
	for (y = 0; y < length; y++) ip[y] = -1;
     }
   if (field) 
     {
	register char *p;
	p=field;
	for (y=bottom;--y>=0;) 
	  {
	     for (x=SLtt_Screen_Cols;--x>=0;) 
	       {
		  SLsmg_write_char(*p++);
		  if (!*p) p=field;
	       }
	     SLsmg_gotorc (y, 0);
	  }
     }
   draw_the_box ();
   SLsmg_refresh();
   
   while (counts--)
     {
	for (n=0, w = &worm[0]; n < number;n++,w++)
	  {
	     SLsmg_set_color (n % 16);
	     /* Worm starts at bottom left */
	     if ((x=w->xpos[h=w->head])<0)
	       {
		  char ch;
		  cursor(x=w->xpos[h]=0,y=w->ypos[h]=bottom);
		  
		  ch = flavor[n % 16];
#ifndef pc_system
		  if (ch == ACS_CHAR) SLsmg_set_char_set (1);
#endif
		  SLsmg_write_char(ch);
#ifndef pc_system
		  if (ch == ACS_CHAR) SLsmg_set_char_set (0);
#endif
		  ref[y][x]++;
	       }
	     else y=w->ypos[h];
	     
	     if (++h==length) h=0;
	     
	     if (w->xpos[w->head=h]>=0)
	       {
		  /* No need to worry about ypos since it is greater than 
		   * zero from above */
		  register int x1, y1;
		  x1=w->xpos[h]; y1=w->ypos[h];
		  if (--ref[y1][x1]==0)
		    {
		       cursor(x1,y1);
		       SLsmg_set_color (0);
		       SLsmg_write_char(trail);
		       SLsmg_set_color (n % 16);
		    }
	       }
	     
	     if (x == 0)
	       {
		  if (y == 0) op = upleft;
		  else if (y == bottom)
		    op = lowleft;
		  else op = left;
	       }
	     else if (x == last)
	       {
		  if (y == 0) op = upright;
		  else if (y == bottom) op = lowright;
		  else op = right;
	       }
	     else if (y == 0) op = upper;
	     else if (y == bottom) op = lower;
	     else op = normal;
	     
	     op += w->orientation;
	     switch (op->nopts)
	       {
		case 0:
		  break;
		case 1:
		  w->orientation=op->opts[0];
		  break;
		default:
		  w->orientation=op->opts[(int)(ranf()*(float)op->nopts)];
	       }
	     
	     cursor(x += xinc[w->orientation], y += yinc[w->orientation]);
	     if (y < 0) y = 0;
	     if (x < 0) x = 0;
	     
	     if (!Wrap || x!=last || y!=bottom)
	       {
		  char ch;
		  ch = flavor[n % 16];
#ifndef pc_system
		  if (ch == ACS_CHAR) SLsmg_set_char_set (1);
#endif
		  SLsmg_write_char(ch);
#ifndef pc_system
		  if (ch == ACS_CHAR) SLsmg_set_char_set (0);
#endif
	       }
	     
	     
	     ref[w->ypos[h]=y][w->xpos[h]=x]++;
	  }
	draw_the_box ();
	SLsmg_refresh();
	if (SLang_input_pending (1)) break;
     }
   draw_the_box ();
   SLsmg_refresh ();
   onsig (0);
   return 0;
}

static void onsig (int sig)
{
   SLang_reset_tty ();
   SLsmg_reset_smg ();
   
   if (SLtt_Has_Status_Line > 0)
     SLtt_disable_status_line ();
   SLtt_set_cursor_visibility (1);
   exit (sig);
}

static float ranf(void)
{
   float rv;
   long r = rand();
   
   r &= 077777;
   rv =((float)r/32767.);
   return rv;
}
