/* we_xterm.c                                             */
/* Copyright (C) 1993 Fred Kruse                          */
/* This is free software; you can redistribute it and/or  */
/* modify it under the terms of the                       */
/* GNU General Public License, see the file COPYING.      */

#include "edit.h"

#ifdef XWINDOW

#include "xkeys.h"

int e_sw_color();
int e_x_change();

#undef exit

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  #ifndef S4UX   */
#include <bitmaps/icon>
/*
#else
#include <X11/bitmaps/icon>
#endif
*/

#define icon_bitmap_width  icon_width
#define icon_bitmap_height icon_height
#define icon_bitmap_bits   icon_bits
#define exit(n) e_exit(n)


#define BITMAPDEPTH 1

#ifndef ALTMASK
#ifdef __linux__			/* a.r. */
#define ALTMASK Mod1Mask
#else
#define ALTMASK Mod4Mask
#endif
#endif
#ifndef NEWSTYLE
#define NOXCACHE
#endif

#define SMALL 1
#define OK 0
#define BUFSIZE 80

extern char *e_tmp_dir;

/*  Globale Konstanten  */

/*  Fuer XWindow   */
Display *display = NULL;
int screen, altmask = ALTMASK;
Window win, root;
GC gc;
XFontStruct *font_info;
XSizeHints size_hints;
int XColorCell[16];
char **e_x_colname;
Atom X_delete_atom, X_protocol_atom;
int e_pt_old = EDCUR;

/*  Fuer TextSchirm   */

char *altschirm = NULL;
#ifdef NEWSTYLE
char *extbyte = NULL, *altextbyte = NULL;
#endif
int old_cursor_x = 0, old_cursor_y = 0, cur_on = 1;
int font_width, font_height;
PIC *e_X_l_pic = NULL;

#if MOUSE
struct mouse e_mouse;
#endif

#ifdef NEWSTYLE
int e_get_pic_xrect(xa, ya, xe, ye, pic)
     int xa;
     int ya;
     int xe;
     int ye;
     PIC *pic;
{  int i = xa, j, ebbg;
   ebbg = (xe - xa + 1) * 2 * (ye - ya + 1);
   for (j = ya; j <= ye; ++j)
   for (i = xa; i <= xe; ++i)
          *( pic->p + ebbg + (j-ya)*(xe-xa+1) + (i-xa) ) =
						extbyte[j*MAXSCOL + i];
   return(i);
}

int e_put_pic_xrect(pic)
     PIC *pic;
{
   int i = 0, j, ebbg = (pic->e.x - pic->a.x + 1)
				* 2 * (pic->e.y - pic->a.y + 1);
   for (j = pic->a.y; j <= pic->e.y; ++j)
   for (i = pic->a.x; i <= pic->e.x; ++i)
   extbyte[j*MAXSCOL+i] = *( pic->p + ebbg +
		   (j-pic->a.y)*(pic->e.x-pic->a.x+1) + (i-pic->a.x) );
   return(i);
}

int e_make_xrect_abs(xa, ya, xe, ye, sw)
     int xa;
     int ya;
     int xe;
     int ye;
     int sw;
{
   int j;
   for(j = xa; j <= xe; j++)
	*(extbyte+ya*MAXSCOL+j) = *(extbyte+ye*MAXSCOL+j) = 0;
   for(j = ya; j <= ye; j++)
	*(extbyte+j*MAXSCOL+xa) = *(extbyte+j*MAXSCOL+xe) = 0;
   return(e_make_xrect(xa, ya, xe, ye, sw));
}

int e_make_xrect(xa, ya, xe, ye, sw)
     int xa;
     int ya;
     int xe;
     int ye;
     int sw;
{
   int j;
   if(sw & 2)
   {  sw = (sw & 1) ? 16 : 0;
      for(j = xa+1; j < xe; j++)
      {  *(extbyte+ya*MAXSCOL+j) |= (sw | 4);
	 *(extbyte+ye*MAXSCOL+j) |= (sw | 1);
      }
      for(j = ya+1; j < ye; j++)
      {  *(extbyte+j*MAXSCOL+xa) |= (sw | 2);
	 *(extbyte+j*MAXSCOL+xe) |= (sw | 8);
      }
   }
   else
   {  sw = (sw & 1) ? 16 : 0;
      for(j = xa; j <= xe; j++)
      {  *(extbyte+ya*MAXSCOL+j) |= (sw | 1);
	 *(extbyte+ye*MAXSCOL+j) |= (sw | 4);
      }
      for(j = ya; j <= ye; j++)
      {  *(extbyte+j*MAXSCOL+xa) |= (sw | 8);
	 *(extbyte+j*MAXSCOL+xe) |= (sw | 2);
      }
   }
   return(j);
}

int e_make_xr_rahmen(xa, ya, xe, ye, sw)
     int xa;
     int ya;
     int xe;
     int ye;
     int sw;
{
   if(!sw)
   {  e_make_xrect(xa, ya, xe, ye, 0);
      e_make_xrect(xa, ya, xe, ye, 2);
   }
   else
   {  e_make_xrect(xa+1, ya, xe-1, ya, 0);
      e_make_xrect(xa+1, ye, xe-1, ye, 0);
      e_make_xrect(xa, ya+1, xa, ye-1, 0);
      e_make_xrect(xe, ya+1, xe, ye-1, 0);
      e_make_xrect(xa, ya, xa, ya, 0);
      e_make_xrect(xe, ya, xe, ya, 0);
      e_make_xrect(xe, ye, xe, ye, 0);
      e_make_xrect(xa, ye, xa, ye, 0);
   }
   return(sw);
}

#ifdef NOXCACHE

#define e_print_xrect(x, y, n)                                               \
  if(extbyte[n])							     \
  {									     \
   XSetForeground(display, gc, 						     \
			!(extbyte[n] & 16) ? XColorCell[0] : XColorCell[15]);\
   if(extbyte[n] & 2)							     \
	XDrawLine(display, win, gc, font_width*((x)+1)-1, 		     \
		font_height*(y), font_width*((x)+1)-1, 			     \
					   font_height*((y)+1)-1);	     \
   if(extbyte[n] & 4)							     \
   	XDrawLine(display, win, gc, font_width*(x), 			     \
		font_height*((y)+1)-1, 					     \
		font_width*((x)+1)-1, font_height*((y)+1)-1);		     \
									     \
   XSetForeground(display, gc, 						     \
		!(extbyte[n] & 16) ? XColorCell[15] : XColorCell[0]);	     \
   if(extbyte[n] & 8)							     \
	XDrawLine(display, win, gc, font_width*(x), 			     \
		font_height*(y), font_width*(x), 				     \
		font_height*((y)+1)-1);					     \
   if(extbyte[n] & 1)							     \
	XDrawLine(display, win, gc, font_width*(x), 			     \
		font_height*(y), font_width*((x)+1)-1, 			     \
		font_height*(y));					     \
  }


#else				/* cached  a.r. */

#define MAXSEG 1000
int nseg[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
XSegment seg[8][MAXSEG];
int scol[8] = { 0, 0, 15, 15, 15, 15, 0, 0 };

void e_flush_xrect()
{
   int i;
   for(i = 0; i < 8; i++)
   if(nseg[i])
   {  XSetForeground(display, gc, XColorCell[scol[i]]);
      XDrawSegments(display, win, gc, seg[i], nseg[i]);
      nseg[i] = 0;
   }
}

void e_print_xrect(x, y, n)
     int x;
     int y;
     int n;
{
  int c = extbyte[n] & 16 ? 4 : 0;
  if(extbyte[n])
  {
   if (     nseg[0] > MAXSEG || nseg[1] > MAXSEG
	 || nseg[2] > MAXSEG || nseg[3] > MAXSEG
	 || nseg[4] > MAXSEG || nseg[5] > MAXSEG
	 || nseg[6] > MAXSEG || nseg[7] > MAXSEG
      )
	e_flush_xrect();
   if(extbyte[n] & 2)
	{
	seg[c][nseg[c]].x1 = font_width*((x)+1)-1;
	seg[c][nseg[c]].y1 = font_height*(y);
	seg[c][nseg[c]].x2 = font_width*((x)+1)-1;
	seg[c][nseg[c]].y2 = font_height*((y)+1)-1;
	nseg[c]++;
	}
   if(extbyte[n] & 4)
	{
   	seg[c+1][nseg[c+1]].x1 = font_width*(x);
	seg[c+1][nseg[c+1]].y1 = font_height*((y)+1)-1;
	seg[c+1][nseg[c+1]].x2 = font_width*((x)+1)-1;
	seg[c+1][nseg[c+1]].y2 = font_height*((y)+1)-1;
	nseg[c+1]++;
	}
   if(extbyte[n] & 8)
	{
	seg[c+2][nseg[c+2]].x1 = font_width*(x);
	seg[c+2][nseg[c+2]].y1 = font_height*(y);
	seg[c+2][nseg[c+2]].x2 = font_width*(x);
	seg[c+2][nseg[c+2]].y2 = font_height*((y)+1)-1;
	nseg[c+2]++;
	}
   if(extbyte[n] & 1)
	{
	seg[c+3][nseg[c+3]].x1 = font_width*(x);
	seg[c+3][nseg[c+3]].y1 = font_height*(y);
	seg[c+3][nseg[c+3]].x2 = font_width*((x)+1)-1;
	seg[c+3][nseg[c+3]].y2 = font_height*(y);
	nseg[c+3]++;
	}
  }
}
#endif

#endif

int fk_show_cursor()
{  int x;
   if(!cur_on) return(0);
   x = 2*old_cursor_x + 2*MAXSCOL*old_cursor_y;
   XSetForeground(display, gc, XColorCell[schirm[x+1] % 16]);
   XSetBackground(display, gc, XColorCell[schirm[x+1] / 16]);
   XDrawImageString(display, win, gc, font_width*old_cursor_x,
    	font_height*(old_cursor_y+1) - font_info->max_bounds.descent,
							schirm + x, 1);
#ifdef NEWSTYLE
   e_print_xrect(old_cursor_x, old_cursor_y, x/2);
#ifndef NOXCACHE
   e_flush_xrect();
#endif
#endif
   x = 2*cur_x + 2*MAXSCOL*cur_y;
   
   XSetForeground(display, gc, XColorCell[schirm[x+1] / 16]);
   XSetBackground(display, gc, XColorCell[schirm[x+1] % 16]);
   XDrawImageString(display, win, gc, font_width*cur_x,
  	font_height*(cur_y+1) - font_info->max_bounds.descent,
							schirm + x, 1);
   old_cursor_x = cur_x;  old_cursor_y = cur_y;
   return(cur_on);
}

int e_ini_size()
{
   font_height = font_info->max_bounds.ascent
				 + font_info->max_bounds.descent;
   font_width = font_info->max_bounds.width;
   old_cursor_x = cur_x; old_cursor_y = cur_y;
   MAXSCOL = size_hints.width / font_width;
   MAXSLNS = size_hints.height / font_height;
   if(schirm) FREE(schirm);
   if(altschirm) FREE(altschirm);
   schirm = MALLOC(2 * MAXSCOL * MAXSLNS);
   altschirm = MALLOC(2 * MAXSCOL * MAXSLNS);
#ifdef NEWSTYLE
   if(extbyte) FREE(extbyte);
   if(altextbyte) FREE(altextbyte);
   extbyte = MALLOC(MAXSCOL * MAXSLNS);
   altextbyte = MALLOC(MAXSCOL * MAXSLNS);
   if(!schirm || !altschirm || !extbyte || !altextbyte) return(-1);
#else
   if(!schirm || !altschirm) return(-1);
#endif
   return(0);
}


int get_GC()
{
   unsigned long valuemask = 0;
   XGCValues values;
   unsigned int line_width = 1;
   int line_style = LineSolid;  /*  LineOnOffDash;  */
   int cap_style = CapRound;
   int join_style = JoinRound;
   int dash_offset = 0;
   static char dash_list[] = {  12,  24  };
   int list_length = 2;
   
   gc = XCreateGC(display, win, valuemask, &values);
   
   XSetFont(display, gc, font_info->fid);
   
   XSetForeground(display, gc, BlackPixel(display, screen));
   
   XSetLineAttributes(display, gc, line_width, line_style, cap_style,
				join_style);
   
   XSetDashes(display, gc, dash_offset, dash_list, list_length);
   return(0);
}

int load_font(fontname)
     char *fontname;
{
   if((font_info = XLoadQueryFont(display, fontname)) == NULL)
   {  fprintf(stderr, "Basic: Cannot open %s font\n", fontname);
      exit(-1);
   }
   return(0);
}

#define A_Normal 	16
#define A_Reverse 	1
#define A_Standout	1
#define A_Underline	1
#define A_Bold		16


int e_X_sw_color(fb)
     FARBE *fb;
{
   fb->er = e_n_clr(A_Normal);
   fb->et = e_n_clr(A_Normal);
   fb->ez = e_n_clr(A_Reverse);
   fb->es = e_n_clr(A_Normal);
   fb->em = e_n_clr(A_Standout);
   fb->ek = e_n_clr(A_Underline);
   fb->nr = e_n_clr(A_Standout);
   fb->nt = e_n_clr(A_Reverse);
   fb->nz = e_n_clr(A_Normal);
   fb->ns = e_n_clr(A_Bold);
   fb->mr = e_n_clr(A_Standout);
   fb->mt = e_n_clr(A_Standout);
   fb->mz = e_n_clr(A_Normal);
   fb->ms = e_n_clr(A_Normal);
   fb->fr = e_n_clr(A_Normal);
   fb->ft = e_n_clr(A_Normal);
   fb->fz = e_n_clr(A_Standout);
   fb->fs = e_n_clr(A_Standout);
   fb->of = e_n_clr(A_Standout);
   fb->df = e_n_clr(A_Normal);
   fb->dc = 0x02;
#ifdef DEBUGGER
   fb->db = e_n_clr(A_Standout);
   fb->dy = e_n_clr(A_Standout);
#endif
   return(0);
}

int e_init_colplane()
{
   Colormap cmap;
   XColor exact_def;
   Visual *visual;
   int i, depth;
   
   if(!e_x_colname[0]) e_x_colname[0] ="Black";
   if(!e_x_colname[1]) e_x_colname[1] ="Dark Slate Blue";
   if(!e_x_colname[2]) e_x_colname[2] ="Forest Green";
   if(!e_x_colname[3]) e_x_colname[3] ="Turquoise3";       /* "Medium Turquoise";  */
   if(!e_x_colname[4]) e_x_colname[4] ="Red3";       /*  "Red";  */
   if(!e_x_colname[5]) e_x_colname[5] ="Violet Red";
   if(!e_x_colname[6]) e_x_colname[6] ="Brown";
   if(!e_x_colname[7]) e_x_colname[7] ="Light Grey";
   if(!e_x_colname[8]) e_x_colname[8] ="Dark Slate Grey";
   if(!e_x_colname[9]) e_x_colname[9] ="Blue";
   if(!e_x_colname[10]) e_x_colname[10] ="Green";
   if(!e_x_colname[11]) e_x_colname[11] ="Turquoise1";  /*  "Turquoise";  */
   if(!e_x_colname[12]) e_x_colname[12] ="Red1";     /*  "Coral";   */
   if(!e_x_colname[13]) e_x_colname[13] ="Violet";
   if(!e_x_colname[14]) e_x_colname[14] ="Yellow";
   if(!e_x_colname[15]) e_x_colname[15] ="White";
   
   cmap = DefaultColormap(display, screen);
   depth = DisplayPlanes(display, screen);
   visual = DefaultVisual(display, screen);
   
   if(depth == 1)
   {  extern struct EXT h_error;
      fprintf(stderr, "No Color Screen\n");
      e_X_sw_color(h_error.cn->fb);
      XColorCell[0] = BlackPixel(display, screen);
      XColorCell[1] = WhitePixel(display, screen);
      return(1);
   }
   for(i = 0; i < 16; i++)
   {  if(!XParseColor(display, cmap, e_x_colname[i], &exact_def))
      fprintf(stderr, "Color %s not in Database\n", e_x_colname[i]);
      if(!XAllocColor(display, cmap, &exact_def))
      {  fprintf(stderr, "All Colorcells allocated\n"); return(1);  }
      XColorCell[i] = exact_def.pixel;
   }
   return(0);
}
/*
int e_cmp_a_set(char *str, char *name, char **set)
{
   int i, j;
   for(i = 0; str[i] != '\0' && isspace(str[i]); i++);
   for(j = 0; str[i] != '\0' && name[j] != '\0' && str[i] == name[j]; 
								i++, j++);
   if(!str[i] || name[j]) return(1);
   for(; str[i] != '\0' && isspace(str[i]); i++);
   for(j = i; str[j] != '\0' && !isspace(str[j]); j++);
   if(j-i <= 0) return(1);
   *set = malloc((j-i+1) * sizeof(char));
   if(!(*set)) return(0); 
   for(j = 0; str[i] != '\0' && !isspace(*(*set+j) = str[i]); i++, j++);
   *(*set+j) = '\0';
   return(0);
}

int e_read_x_opt(char **fontname, char **geometry, int *initial)
{
   extern struct EXT h_error;
   ECNT *cn = h_error.cn;
   FILE *fp;
   char *ic, str[128];
   char *xoptfile=e_mkfilename(getenv("HOME"), XOPTFILE);
   int i;

   e_x_colname = malloc(16 * sizeof(char *));
   for(i = 0; i < 16; i++) e_x_colname[i] = NULL;

   if(!(fp = fopen(xoptfile, "r")))
   {	FREE(xoptfile);
	xoptfile = e_mkfilename(cn->edtdrct, XOPTFILE);
	fp = fopen(xoptfile, "r");
   }
   FREE(xoptfile);
   if(!fp) return(0);
   while(fgets(str, 128, fp))
   {	if(!e_cmp_a_set(str, "xwpe.font:", fontname));
	else if(!e_cmp_a_set(str, "xwpe.geometry:", geometry));
	else if(!e_cmp_a_set(str, "xwpe.iconic:", &ic))
	{  if(!strcmp(ic, "yes") || !strcmp(ic, "YES")) *initial = 3;  }
	else if(!e_cmp_a_set(str, "xwpe.color1:", e_x_colname+0));
	else if(!e_cmp_a_set(str, "xwpe.color2:", e_x_colname+1));
	else if(!e_cmp_a_set(str, "xwpe.color3:", e_x_colname+2));
	else if(!e_cmp_a_set(str, "xwpe.color4:", e_x_colname+3));
	else if(!e_cmp_a_set(str, "xwpe.color5:", e_x_colname+4));
	else if(!e_cmp_a_set(str, "xwpe.color6:", e_x_colname+5));
	else if(!e_cmp_a_set(str, "xwpe.color7:", e_x_colname+6));
	else if(!e_cmp_a_set(str, "xwpe.color8:", e_x_colname+7));
	else if(!e_cmp_a_set(str, "xwpe.color9:", e_x_colname+8));
	else if(!e_cmp_a_set(str, "xwpe.color10:", e_x_colname+9));
	else if(!e_cmp_a_set(str, "xwpe.color11:", e_x_colname+10));
	else if(!e_cmp_a_set(str, "xwpe.color12:", e_x_colname+11));
	else if(!e_cmp_a_set(str, "xwpe.color13:", e_x_colname+12));
	else if(!e_cmp_a_set(str, "xwpe.color14:", e_x_colname+13));
	else if(!e_cmp_a_set(str, "xwpe.color15:", e_x_colname+14));
	else if(!e_cmp_a_set(str, "xwpe.color16:", e_x_colname+15));
   }
   fclose(fp);
   return(0);
}	
*/

int e_read_x_opt(fontname, geometry, initial)
     char **fontname;
     char **geometry;
     int *initial;
{
   char *ic = NULL, *tmp, progname[80];
   int i;
   
   e_x_colname = malloc(16 * sizeof(char *));
   for(i = 0; i < 16; i++) e_x_colname[i] = NULL;
   strcpy(progname, "xwpe");
   
   if((tmp = XGetDefault(display, progname, "font")) != NULL) *fontname = tmp;
   if((tmp = XGetDefault(display, progname, "geometry")) != NULL) *geometry = tmp;
   if((tmp = XGetDefault(display, progname, "iconic")) &&
        (!strcmp(ic, "yes") || !strcmp(ic, "YES")) ) *initial = 3;
   if((tmp = XGetDefault(display, progname, "color1")) != NULL) e_x_colname[0] = tmp;
   if((tmp = XGetDefault(display, progname, "color2")) != NULL) e_x_colname[1] = tmp;
   if((tmp = XGetDefault(display, progname, "color3")) != NULL) e_x_colname[2] = tmp;
   if((tmp = XGetDefault(display, progname, "color4")) != NULL) e_x_colname[3] = tmp;
   if((tmp = XGetDefault(display, progname, "color5")) != NULL) e_x_colname[4] = tmp;
   if((tmp = XGetDefault(display, progname, "color6")) != NULL) e_x_colname[5] = tmp;
   if((tmp = XGetDefault(display, progname, "color7")) != NULL) e_x_colname[6] = tmp;
   if((tmp = XGetDefault(display, progname, "color8")) != NULL) e_x_colname[7] = tmp;
   if((tmp = XGetDefault(display, progname, "color9")) != NULL) e_x_colname[8] = tmp;
   if((tmp = XGetDefault(display, progname, "color10")) != NULL) e_x_colname[9] = tmp;
   if((tmp = XGetDefault(display, progname, "color11")) != NULL) e_x_colname[10] = tmp;
   if((tmp = XGetDefault(display, progname, "color12")) != NULL) e_x_colname[11] = tmp;
   if((tmp = XGetDefault(display, progname, "color13")) != NULL) e_x_colname[12] = tmp;
   if((tmp = XGetDefault(display, progname, "color14")) != NULL) e_x_colname[13] = tmp;
   if((tmp = XGetDefault(display, progname, "color15")) != NULL) e_x_colname[14] = tmp;
   if((tmp = XGetDefault(display, progname, "color16")) != NULL) e_x_colname[15] = tmp;
   if((tmp = XGetDefault(display, progname, "altmask")) != NULL)
   {  if(!strncmp(tmp, "mod", 3)) i = atoi(tmp+3);
      else i = 0;
      switch(i)
      {  case 1:  altmask = Mod1Mask;  break;
         case 2:  altmask = Mod2Mask;  break;
         case 3:  altmask = Mod3Mask;  break;
         case 4:  altmask = Mod4Mask;  break;
         case 5:  altmask = Mod5Mask;  break;
      }
   }
   return(0);
}


int e_x_initscr(argc, argv)
     int argc;
     char **argv;
{
   XWMHints wmhints;
   int width, height;
   int anum, i, x = 10, y = 10, argn = argc, initial = 1;
   Atom *natm, *atm;
   unsigned int border_width = 4;
   unsigned int display_width, display_height;
#ifdef PROG
   char *window_name = "Window Programming Environment";
   char *icon_name = "WPE";
#else
   char *window_name = "Window Editor";
   char *icon_name = "WE";
#endif
   Pixmap icon_pixmap;
   char *fontname = "8x13";
   char *display_name = NULL;
   char *geometry = NULL;
   Cursor cursor;
   
   if( (display = XOpenDisplay(display_name)) == NULL )
   {  fprintf(stderr, "basicwin: Cannot connect to X server %s\n",
			XDisplayName(display_name));
      exit(-1);
   }
   
   e_read_x_opt(&fontname, &geometry, &initial);
   
   for(i = 1; i < argn; )
   {  if(!strcmp(argv[i], "-display"))
      argn = e_get_arg(i, argn, argv, &display_name);
      else if(!strcmp(argv[i], "-fn") || !strcmp(argv[i], "-font"))
      argn = e_get_arg(i, argn, argv, &fontname);
      else if(!strcmp(argv[i], "-g") || !strcmp(argv[i], "-geometry"))
      argn = e_get_arg(i, argn, argv, &geometry);
      else if(!strcmp(argv[i], "-iconic")) {  initial = 3;  i++;  }
      else i++;
   }
   
   screen = DefaultScreen(display);
   
   load_font(fontname);
   
   display_width = DisplayWidth(display, screen);
   display_height = DisplayHeight(display, screen);
   
   width = font_info->max_bounds.width * 80;
   height = 3 * display_height/4;
   
   font_height = font_info->max_bounds.ascent
				 + font_info->max_bounds.descent;
   font_width = font_info->max_bounds.width;
   if(!e_get_geometry(geometry, &width, &height, &x, &y,
	font_width, font_height))
   {  if(x < 0 && (x = display_width - width - 9 +x) < 0) x = 0;
      if(y < 0 && (y = display_height - height - 30 + y) < 0) y = 0;
   }
   if(width > display_width) width = display_width;
   if(height > display_height) height = display_height;
   width = (width/font_width) * font_width;
   height = (height/font_height) * font_height;
   win = XCreateSimpleWindow(display, RootWindow(display, screen),
	x, y, width, height, border_width,
	BlackPixel(display, screen), WhitePixel(display, screen));
   icon_pixmap = XCreateBitmapFromData(display, win, icon_bitmap_bits,
	icon_bitmap_width, icon_bitmap_height);
   
   size_hints.flags = USPosition | USSize | PPosition | PSize 
      | PResizeInc | PMinSize;
   size_hints.x = x;
   size_hints.y = y;
   size_hints.width = width;
   size_hints.height = height;
   size_hints.width_inc = font_width;
   size_hints.height_inc = font_height;
   size_hints.min_width = font_width * 80;
   size_hints.min_height = font_height * 24;
   
   XSetStandardProperties(display, win, window_name, icon_name,
	icon_pixmap, argv, argc, &size_hints);
   
   wmhints.flags = InputHint | StateHint; /*  Damit er beim Einschalten */
   wmhints.input = True;                  /*  das Fenster auch bedient */
   wmhints.initial_state = initial;       /*  Fenster oder Icon  */
   
   XSetWMHints(display, win, &wmhints);
   
   root = RootWindow(display, screen);
   
   XSelectInput(display, win, ExposureMask | KeyPressMask
	 | ButtonPressMask | StructureNotifyMask);
   
   if(!XGetWMProtocols(display, win, &atm, &anum)) anum = 0;
   natm = MALLOC((anum+1)*sizeof(int));
   for(i = 0; i < anum; i++) natm[i] = atm[i];
   X_delete_atom = XInternAtom(display, "WM_DELETE_WINDOW", False);
   X_protocol_atom = XInternAtom(display, "WM_PROTOCOLS", False);
   natm[anum] = X_delete_atom;
   XSetWMProtocols(display, win, natm, anum+1);
   FREE(natm);
   
   get_GC();
   
   e_init_colplane();
   
   XMapWindow(display, win);
   
   if(e_ini_size()) return(-1);
   
   cursor = XCreateFontCursor(display, EDCUR);
   XDefineCursor(display, win, cursor);
   
   e_abs_refr();
   
   return(argn);
}

int e_get_arg(n, max, argv, str)
     int n;
     int max;
     char **argv;
     char **str;
{
   int i;
   *str = argv[n+1];
   for(i = n; i < max - 2; i++)  argv[i] = argv[i + 2];
   return(max - 2);
}

int e_get_geometry(str, w, h, x, y, d_w, d_h)
     char *str;
     int *w;
     int *h;
     int *x;
     int *y;
     int d_w;
     int d_h;
{
   int i, j, n;
   char snum[80];
   if(!str) return(-1);
   for(i = j = 0; isdigit((snum[j] = str[i])) && str[i] != '\0'; i++, j++);
   snum[j] = '\0';
   if(isdigit(snum[0]) && (n = atoi(snum)) > 0) *w = n * d_w;
   if(str[i] == 'x')
   {  for(j = 0, i++; isdigit((snum[j] = str[i])) && str[i] != '\0'; i++, j++);
      snum[j] = '\0';
      if(isdigit(snum[0]) && (n = atoi(snum)) > 0) *h = n * d_h;
   }
   if(str[i] == '+' || str[i] == '-')
   {  snum[0] = str[i];
      for(j = 1, i++; isdigit((snum[j] = str[i])) && str[i] != '\0'; i++, j++);
      snum[j] = '\0';
      if(isdigit(snum[1]) && (n = atoi(snum+1)) >= 0) *x = n;
      if(snum[0] == '-') *x = -(*x+1);
   }
   if(str[i] == '+' || str[i] == '-')
   {  snum[0] = str[i];
      for(j = 1, i++; isdigit((snum[j] = str[i])) && str[i] != '\0'; i++, j++);
      snum[j] = '\0';
      if(isdigit(snum[1]) && (n = atoi(snum+1)) >= 0) *y = n;
      if(snum[0] == '-') *y = -(*y+1);
   }
   return(0);
}


int e_x_refresh()
{
#ifndef NOXCACHE				/* a.r. */
#define STRBUFSIZE 1024
   unsigned long oldback = 0, oldfore = 0;
   static char stringbuf[STRBUFSIZE];
   int stringcount = 0, oldI = 0, oldX = 0, oldY = 0, oldJ = 0;
#endif
   int i, j, x, y, cur_tmp = cur_on;
   fk_cursor(0);
   for(i = 0; i < MAXSLNS; i++)
   for(j = 0; j < MAXSCOL; j++)
   {  y = j + MAXSCOL*i;
      x = 2*y;
#ifdef NEWSTYLE
      if(schirm[x] != altschirm[x] || schirm[x+1] != altschirm[x+1]
		|| extbyte[y] != altextbyte[y])
#else
      if(schirm[x] != altschirm[x] || schirm[x+1] != altschirm[x+1])
#endif
      {
#ifdef NOXCACHE
	 XSetForeground(display, gc, XColorCell[schirm[x+1] % 16]);
	 XSetBackground(display, gc, XColorCell[schirm[x+1] / 16]);
	 XDrawImageString(display, win, gc, font_width*j,
    		font_height*(i+1) - font_info->max_bounds.descent,
							schirm + x, 1);
#else
	 if (   oldback != XColorCell[schirm[x+1] / 16]  	/* a.r. */
	     || oldfore != XColorCell[schirm[x+1] % 16]
	     || i != oldI
	     || j > oldJ+1	/* is there a more elegant solution? */
	     || stringcount >= STRBUFSIZE
            )
	   {
	        XDrawImageString(display, win, gc,
		    		 oldX, oldY, stringbuf, stringcount);
	        oldback = XColorCell[schirm[x+1] / 16];
	        oldfore = XColorCell[schirm[x+1] % 16];
	 	XSetForeground(display, gc, oldfore );
	 	XSetBackground(display, gc, oldback );
		oldX = font_width*j;
    		oldY = font_height*(i+1) - font_info->max_bounds.descent;
		oldI = i;
		stringcount = 0;
		stringbuf[stringcount++] = schirm[x];
	   }
	 else
		stringbuf[stringcount++] = schirm[x];
#endif
#ifndef NEWSTYLE
	 if(schirm[x] == 16)
	 {  XFillRectangle(display, win, gc, font_width*j,
    		font_height*(i), font_width,
			(font_height + font_info->max_bounds.descent)/2);
	 }
	 else if(schirm[x] == 20)
	 {  XFillRectangle(display, win, gc, font_width*j,
    		(int)(font_height*(i+1./2)), font_width,
			(font_height + font_info->max_bounds.descent)/2);
	 }
#endif
	 altschirm[x] = schirm[x];
	 altschirm[x+1] = schirm[x+1];
#ifdef NEWSTYLE
	 e_print_xrect(j, i, y);
	 altextbyte[y] = extbyte[y];
#endif
#ifndef NOXCACHE
	 oldJ = j;
#endif
      }
   }
#ifndef NOXCACHE
   XDrawImageString(display, win, gc,
		    oldX,
    		    oldY,
		    stringbuf,
		    stringcount);
#ifdef NEWSTYLE
   e_flush_xrect();
#endif
#endif
   fk_cursor(cur_tmp);
   fk_show_cursor();
   XFlush(display);
   return(0);
}

int e_x_change(pic)
     PIC *pic;
{
   Window tmp_win, tmp_root;
   XEvent report;
   KeySym keysym;
   XComposeStatus compose;
   unsigned char buffer[BUFSIZE];
   int root_x, root_y, x, y, charcount;
   unsigned int key_b;
   while (XCheckMaskEvent(display, KeyPressMask | ButtonPressMask |
		ExposureMask | StructureNotifyMask, &report) == True)
   {  switch(report.type)
      {  case Expose:
	    e_abs_refr();
	    e_refresh();
	    break;
	 case ConfigureNotify:
            {  size_hints.width = (report.xconfigure.width / font_width) * font_width;
	       size_hints.height = (report.xconfigure.height / font_height) * font_height;
/*	       XResizeWindow(display, win, size_hints.width, size_hints.height);
*/	       if(size_hints.width != MAXSCOL * font_width ||
                  size_hints.height != MAXSLNS * font_height)
	       {  extern struct EXT h_error;
                  e_x_repaint_desk(h_error.cn->f[h_error.cn->mxedt]);
	       }
	    }
	    while (XCheckMaskEvent(display,
		   	StructureNotifyMask, &report) == True);
	    break;
	 case KeyPress:
	    charcount = XLookupString(&report.xkey, buffer, BUFSIZE,
				&keysym, &compose);
	    if(charcount == 1 && *buffer == CtrlC) return(CtrlC);
	    break;
	 case ButtonPress:
	    if(!pic) break;
	    XQueryPointer(display, win, &tmp_root, &tmp_win,
				      &root_x, &root_y, &x, &y, &key_b);
	    if(report.xbutton.button == 1)
	    {  e_mouse.k = (key_b & ShiftMask) ? 3 : 0 +
		         (key_b & ControlMask) ? 4 : 0 +
		         (key_b & altmask) ? 8 : 0;
	       e_mouse.x = report.xbutton.x/font_width;
	       e_mouse.y = report.xbutton.y/font_height;
	       if(e_mouse.x > (pic->e.x + pic->a.x - 10)/2
		&& e_mouse.x < (pic->e.x + pic->a.x + 6)/2 )
	       return(CtrlC);
	    }
	    break;
      }
   }
   return(0);
}

int e_x_getch()
{
   Window tmp_win, tmp_root;
   XEvent report;
   KeySym keysym;
   XComposeStatus compose;
   int charcount;
   unsigned char buffer[BUFSIZE];
   int c, root_x, root_y, x, y;
   unsigned int key_b;
   
   e_refresh();
   
   XQueryPointer(display, win, &tmp_root, &tmp_win,
				&root_x, &root_y, &x, &y, &key_b);
   if(key_b & (Button1Mask | Button2Mask | Button3Mask))
   {  e_mouse.x = x/font_width;
      e_mouse.y = y/font_height;
      c = 0;
      if(key_b & Button1Mask) c |= 1;
      if(key_b & Button2Mask) c |= 4;
      if(key_b & Button3Mask) c |= 2;
      return(-c);
   }
   
   while(1)  {
      
      XNextEvent(display, &report);
      
      switch(report.type)
      {  case Expose:
	    while (XCheckMaskEvent(display,
		   ExposureMask | StructureNotifyMask, &report) == True);
/*
	 case MapNotify:
	 case UnmapNotify:
*/
/*
		while(XCheckTypedEvent(display, Expose, &report));
*/
	    e_abs_refr();
	    e_refresh();
	    break;
	 case ConfigureNotify:
            {  size_hints.width = (report.xconfigure.width / font_width) * font_width;
	       size_hints.height = (report.xconfigure.height / font_height) * font_height;
/*	       XResizeWindow(display, win, size_hints.width, size_hints.height);
*/	       if(size_hints.width != MAXSCOL * font_width ||
                  size_hints.height != MAXSLNS * font_height)
	       {  extern struct EXT h_error;
                  e_x_repaint_desk(h_error.cn->f[h_error.cn->mxedt]);
	       }
	    }
	    while (XCheckMaskEvent(display,
		   	StructureNotifyMask, &report) == True);
	    break;
	 case ClientMessage:
	    if(report.xclient.message_type == X_protocol_atom
		    && ((report.xclient.format == 8
			&& report.xclient.data.b[0] == X_delete_atom)
		     || (report.xclient.format == 16
			&& report.xclient.data.s[0] == X_delete_atom)
		     || (report.xclient.format == 32
			&& report.xclient.data.l[0] == X_delete_atom)))
	    {  extern struct EXT h_error;
	       e_quit(h_error.cn->f[h_error.cn->mxedt]);
	    }
	    break;
	 case KeyPress:
	    charcount = XLookupString(&report.xkey, buffer, BUFSIZE,
				&keysym, &compose);
	    if(charcount == 1)
	    {
	       XQueryPointer(display, win, &tmp_root, &tmp_win,
				      &root_x, &root_y, &x, &y, &key_b);
	       if(*buffer == 127)
	       {  if(key_b & ControlMask) return(CENTF);
		  else if(key_b & ShiftMask) return(ShiftDel);
		  else if(key_b & altmask) return(AltDel);
		  else return(ENTF);
	       }
	       if(key_b & altmask)
	       c = e_tast_sim(key_b & ShiftMask ?
					toupper(*buffer) : *buffer);
	       else return(*buffer);
	    }
	    else
	    {  c = 0;
	       XQueryPointer(display, win, &tmp_root, &tmp_win,
				      &root_x, &root_y, &x, &y, &key_b);
	       if(key_b & ControlMask)
	       {  if(keysym == XK_Left) c = CCLE;
		  else if(keysym == XK_Right) c = CCRI;
		  else if(keysym == XK_Home) c = CPS1;
		  else if(keysym == XK_End) c = CEND;
		  else if(keysym == XK_Insert) c = CEINFG;
		  else if(keysym == XK_Delete) c = CENTF;
		  else if(keysym == XK_Prior) c = CBUP;
		  else if(keysym == XK_Next) c = CBDO;
		  else if(keysym == XK_F1) c = CF1;
		  else if(keysym == XK_F2) c = CF2;
		  else if(keysym == XK_F3) c = CF3;
		  else if(keysym == XK_F4) c = CF4;
		  else if(keysym == XK_F5) c = CF5;
		  else if(keysym == XK_F6) c = CF6;
		  else if(keysym == XK_F7) c = CF7;
		  else if(keysym == XK_F8) c = CF8;
		  else if(keysym == XK_F9) c = CF9;
		  else if(keysym == XK_F10) c = CF10;
	       }
	       else if(key_b & altmask)
	       {  if(keysym == XK_F1) c = AF1;
		  else if(keysym == XK_F2) c = AF2;
		  else if(keysym == XK_F3) c = AF3;
		  else if(keysym == XK_F4) c = AF4;
		  else if(keysym == XK_F5) c = AF5;
		  else if(keysym == XK_F6) c = AF6;
		  else if(keysym == XK_F7) c = AF7;
		  else if(keysym == XK_F8) c = AF8;
		  else if(keysym == XK_F9) c = AF9;
		  else if(keysym == XK_F10) c = AF10;
		  else if(keysym == XK_Insert) c = AltEin;
		  else if(keysym == XK_Delete) c = AltDel;
	       }
	       else
	       {  if(keysym == XK_Left) c = CLE;
		  else if(keysym == XK_Right) c = CRI;
		  else if(keysym == XK_Up) c = CUP;
		  else if(keysym == XK_Down) c = CDO;
		  else if(keysym == XK_Home) c = POS1;
		  else if(keysym == XK_End) c = ENDE;
		  else if(keysym == XK_Insert) c = EINFG;
		  else if(keysym == XK_Delete) c = ENTF;
		  else if(keysym == XK_BackSpace) c = CtrlH;
		  else if(keysym == XK_Prior) c = BUP;
		  else if(keysym == XK_Next) c = BDO;
		  else if(keysym == XK_F1) c = F1;
		  else if(keysym == XK_F2) c = F2;
		  else if(keysym == XK_F3) c = F3;
		  else if(keysym == XK_F4) c = F4;
		  else if(keysym == XK_F5) c = F5;
		  else if(keysym == XK_F6) c = F6;
		  else if(keysym == XK_F7) c = F7;
		  else if(keysym == XK_F8) c = F8;
		  else if(keysym == XK_F9) c = F9;
		  else if(keysym == XK_F10) c = F10;
		  else if(keysym == XK_L1) c = STOP;
		  else if(keysym == XK_L2) c = AGAIN;
		  else if(keysym == XK_L3) c = PROPS;
		  else if(keysym == XK_L4) c = UNDO;
		  else if(keysym == XK_L5) c = FRONT;
		  else if(keysym == XK_L6) c = COPY;
		  else if(keysym == XK_L7) c = OPEN;
		  else if(keysym == XK_L8) c = PASTE;
		  else if(keysym == XK_L9) c = FID;
		  else if(keysym == XK_L10) c = CUT;
		  else if(keysym == XK_Help) c = HELP;
	       }
	    }
	    if(c != 0)
	    {  if(key_b & ShiftMask) c = c + 512;
	       return(c);
	    }
	    break;
	 case ButtonPress:
	    XQueryPointer(display, win, &tmp_root, &tmp_win,
				      &root_x, &root_y, &x, &y, &key_b);
	    e_mouse.k = (key_b & ShiftMask) ? 3 : 0 +
			    (key_b & ControlMask) ? 4 : 0 +
			    (key_b & altmask) ? 8 : 0;
	    e_mouse.x = report.xbutton.x/font_width;
	    e_mouse.y = report.xbutton.y/font_height;
	    c = 0;
	    if(report.xbutton.button == 1) c |= 1;
	    if(report.xbutton.button == 2) c |= 2;
	    if(report.xbutton.button == 3) c |= 4;
	    return(-c);
	 default:
	    break;
      }  }
   return(0);
}

int e_x_kbhit()
{
   Window tmp_win, tmp_root;
   XEvent report;
   KeySym keysym;
   XComposeStatus compose;
   int charcount;
   unsigned char buffer[BUFSIZE];
   int c, root_x, root_y, x, y;
   unsigned int key_b;
   
   e_refresh();
   
   if(XCheckMaskEvent(display, ButtonPressMask | KeyPressMask, &report) == False)
   return(0);
   
   if(report.type == ButtonPress)
   {  XQueryPointer(display, win, &tmp_root, &tmp_win,
				      &root_x, &root_y, &x, &y, &key_b);
      e_mouse.k = (key_b & ShiftMask) ? 3 : 0;
      e_mouse.x = report.xbutton.x/font_width;
      e_mouse.y = report.xbutton.y/font_height;
      c = 0;
      if(report.xbutton.button == 1) c |= 1;
      if(report.xbutton.button == 2) c |= 2;
      if(report.xbutton.button == 3) c |= 4;
      return(-c);
   }
   else
   {  charcount = XLookupString(&report.xkey, buffer, BUFSIZE,
						&keysym, &compose);
      if(charcount == 1) return(*buffer);
      else return(0);
   }
}
int e_setlastpic(pic)
     PIC *pic;
{
   extern PIC *e_X_l_pic;
   return((int)(e_X_l_pic = pic));
}

int fk_x_locate(x, y)
     int x;
     int y;
{
   cur_x = x;  return(cur_y = y);
}

int fk_x_cursor(x)
     int x;
{
   return(cur_on = x);
}

int e_x_sys_ini()
{
   return(0);
}

int e_x_sys_end()
{
   return(0);
}

int fk_x_putchar(c)
     char c;
{
   return(fputc(c, stdout));
}

int x_bioskey(sw)
     int sw;
{
   return(e_mouse.k);
}


int e_x_system(exe)
     char *exe;
{
   FILE *fp;
   int ret;
   char file[80];
   char *string;
   sprintf(file, "%s/we_sys_tmp", e_tmp_dir);
   string = MALLOC(strlen(exe) + strlen(file) + strlen(user_shell) + 40);
   if( !(fp = fopen(file, "w+") ) )
   {  FREE(string);  return(-1);  }
   fputs("$*\necho type \\<Return\\> to continue\nread i\n", fp);
   fclose(fp);
   chmod(file, 0700);
   if(exe[0] == '/')
   sprintf(string, "xterm -geometry 80x25-0-0 +sb -e %s %s %s", user_shell, file, exe);
   else
   sprintf(string, "xterm -geometry 80x25-0-0 +sb -e %s %s ./%s", user_shell, file, exe);
   ret = system(string);
   remove(file);
   FREE(string);
   return(ret);
}

int e_x_repaint_desk(f)
     FENSTER *f;
{
   ECNT *cn = f->ed;
   int i, g[4];
   extern PIC *e_X_l_pic;
   PIC *sv_pic = NULL, *nw_pic = NULL;
   if(e_we_sw & 1)
   {  if(e_X_l_pic && e_X_l_pic != cn->f[cn->mxedt]->pic)
      {  sv_pic = e_X_l_pic;
	 nw_pic = e_open_view(e_X_l_pic->a.x, e_X_l_pic->a.y,
				e_X_l_pic->e.x, e_X_l_pic->e.y, 0, 2);
      }
      e_ini_size();
   }
   if(cn->mxedt < 1)
   {  e_cls(f->fb->df.fb, f->fb->dc);
      e_ini_desk(f->ed);
      if((e_we_sw & 1) && nw_pic)
      {  e_close_view(nw_pic, 1);
	 e_X_l_pic = sv_pic;
      }
      return(0);
   }
   ini_repaint(cn);
   e_abs_refr();
   for (i = cn->mxedt; i >= 1; i--)
   {  FREE(cn->f[i]->pic->p);
      FREE(cn->f[i]->pic);
   }
   for ( i = 0; i <= cn->mxedt; i++)
   {  if(cn->f[i]->e.x >= MAXSCOL) cn->f[i]->e.x = MAXSCOL-1;
      if(cn->f[i]->e.y >= MAXSLNS-1) cn->f[i]->e.y = MAXSLNS-2;
      if(cn->f[i]->e.x - cn->f[i]->a.x < 26)
      cn->f[i]->a.x = cn->f[i]->e.x - 26;
      if(cn->f[i]->dtmd <= 'Z' && cn->f[i]->e.y - cn->f[i]->a.y < 9)
      cn->f[i]->a.y = cn->f[i]->e.y - 9;
      else if(cn->f[i]->dtmd > 'Z' && cn->f[i]->e.y - cn->f[i]->a.y < 3)
      cn->f[i]->a.y = cn->f[i]->e.y - 3;
   }
   for ( i = 1; i < cn->mxedt; i++)
   {  e_firstl(cn->f[i], 0);
      e_schirm(cn->f[i], 0);
   }
   e_firstl(cn->f[i], 1);
   e_schirm(cn->f[i], 1);
   if((e_we_sw & 1) && nw_pic)
   {  e_close_view(nw_pic, 1);
      e_X_l_pic = sv_pic;
   }
#if  MOUSE
   g[0] = 2; fk_mouse(g);
#endif
   end_repaint();
   e_cursor(cn->f[i], 1);
#if  MOUSE
   g[0] = 0; fk_mouse(g);
   g[0] = 1; fk_mouse(g);
#endif
   return(0);
}


#if MOUSE

int fk_x_mouse(g)
     int *g;
{
   Window tmp_win, tmp_root;
   int root_x, root_y, x, y;
   unsigned int key_b;
   
   if(!XQueryPointer(display, win, &tmp_root, &tmp_win,
				&root_x, &root_y, &x, &y, &key_b))
   {  g[2] = e_mouse.x * 8;
      g[3] = e_mouse.y * 8;
      g[0] = g[1] = 0;
      return(0);
   }
   g[0] = 0;
   if(key_b & Button1Mask) g[0] |= 1;
   if(key_b & Button2Mask) g[0] |= 4;
   if(key_b & Button3Mask) g[0] |= 2;
   g[1] = g[0];
   g[2] = x/font_width * 8;
   g[3] = y/font_height * 8;
   return(g[1]);
}

int fk_x_pointer(sw)
     int sw;
{
   static int old = EDCUR;
   Cursor cursor;
   if(!display) return(0);
   if(sw == LASTCUR) sw = old;
   else if(sw == EDCUR || sw == DBCUR) old = sw;
   cursor = XCreateFontCursor(display, sw);
   XDefineCursor(display, win, cursor);
   XFlush(display);
   return(sw);
}

#endif

int e_cp_X_to_buffer(f)
     FENSTER *f;
{
   BUFFER *b0 = f->ed->f[0]->b;
   SCHIRM *s0 = f->ed->f[0]->s;
   int i, j, k, n;
   char *str;
   for(i = 1; i < b0->mxlines; i++)
   FARFREE(b0->bf[i].s);
   b0->mxlines = 1;
   *(b0->bf[0].s) = WR;
   *(b0->bf[0].s+1) = '\0';
   b0->bf[0].len = 0;
   str = XFetchBytes(display, &n);
   for(i = k = 0; i < n; i++, k++)
   {  for(j = 0; i < n && str[i] != '\n' && j < b0->mx.x-1; j++, i++)
					       b0->bf[k].s[j] = str[i];
      if(i < n)
      {  e_new_line(k+1, b0);
         if(str[i] == '\n')
	 {  b0->bf[k].s[j] = WR;
	    b0->bf[k].nrc = j+1;
	 }
	 else b0->bf[k].nrc = j;
	 b0->bf[k].s[j+1] = '\0';
	 b0->bf[k].len = j;
      }
      else
      {  b0->bf[k].s[j] = '\0';
	 b0->bf[k].nrc = b0->bf[k].len = j;
      }
   }
   s0->ka.x = s0->ka.y = 0;
   s0->ke.y = b0->mxlines-1;
   s0->ke.x = b0->bf[b0->mxlines-1].len;
   return(0);
}

int e_copy_X_buffer(f)
     FENSTER *f;
{
   e_cp_X_to_buffer(f);
   e_edt_einf(f);
   return(0);
}

int e_paste_X_buffer(f)
     FENSTER *f;
{
   BUFFER *b0 = f->ed->f[0]->b;
   SCHIRM *s0 = f->ed->f[0]->s;
   int i, j, n;
   char *str;
   e_edt_copy(f);
   if((s0->ke.y == 0 && s0->ke.x == 0) || s0->ke.y < s0->ka.y) return(0);
   if(s0->ke.y == s0->ka.y)
   {  if(s0->ke.x < s0->ka.x) return(0);
      str = MALLOC((n = s0->ke.x - s0->ka.x) * sizeof(char));
      XStoreBytes(display, b0->bf[s0->ka.y].s+s0->ka.x, n);
      FREE(str);
      return(0);
   }
   str = MALLOC(b0->bf[s0->ka.y].nrc * sizeof(char));
   for(n = 0, j = s0->ka.x; j < b0->bf[s0->ka.y].nrc; j++, n++)
   str[n] = b0->bf[s0->ka.y].s[j];
   n = j-s0->ka.x;
   for(i = s0->ka.y+1; i < s0->ke.y; i++)
   {  str = REALLOC(str, (n + b0->bf[i].nrc)*sizeof(char));
      for(j = 0; j < b0->bf[i].nrc; j++, n++) str[n] = b0->bf[i].s[j];
   }
   str = REALLOC(str, (n + s0->ke.x)*sizeof(char));
   for(j = 0; j < s0->ke.x; j++, n++) str[n] = b0->bf[i].s[j];
   XStoreBytes(display, str, n);
   FREE(str);
   return(0);
}


#endif
