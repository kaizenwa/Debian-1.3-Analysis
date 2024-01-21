/* plotibm.c  94.10.25
 * Copyright 1983-1992   Albert Davis
 * IBM video graphics primitives
 */
#include "pixelh.h"
#include <stdio.h>
#include <dos.h>
#include <string.h>
/*--------------------------------------------------------------------------*/
struct graph *initibm(struct graph*);
static void text(int,int,const char*,int);
static void setpixel(int,int,int);
static void finish(void);
static void cancel(void);
static void mode(int);
static void scr_curs(int,int);
static void scr_puts(char*,int);
/*--------------------------------------------------------------------------*/
#define VIDEO_INT   0x10
#define MAX_STRING_SIZE 100
static struct graph *d;
/*--------------------------------------------------------------------------*/
struct graph *initibm(struct graph *g)
{
 d = g;
 d->spx = setpixel;
 d->lin = NULL;
 d->box = NULL;
 d->txt = text;
 d->can = cancel;
 d->fin = finish;
 mode(d->gmode);
 return d;
}
/*--------------------------------------------------------------------------*/
static void text(int col, int lin, const char *string, int color)
{
 char scribble[MAX_STRING_SIZE];	
 strncpy(scribble,string, MAX_STRING_SIZE-1);
 			/* a fudge because Borland C may clobber the string */
 col /= d->ppc;
 lin /= d->lpc;
 scr_curs(lin,col);
 scr_puts(scribble,color);
}
/*--------------------------------------------------------------------------*/
static void setpixel(int x, int y, int color)
{
 union REGS inregs;
 static int skip, oldcolor;
 
 if (!(--skip) || (color!=oldcolor)){
    oldcolor = color;
    skip = (color>=0) ? 1 : -color;

    inregs.h.ah = 12;	/* write dot */
    inregs.h.al = (unsigned char)((color>=0) ? color : d->pri) ;
    inregs.x.bx = 0;	/* page, palette??????? */
    inregs.x.cx = x;
    inregs.x.dx = y;
    int86(VIDEO_INT, &inregs, &inregs); /*inregs changed */
 }
}
/*--------------------------------------------------------------------------*/
static void finish(void)
{;}
/*--------------------------------------------------------------------------*/
static void cancel(void)
{
 if (d->gmode != d->tmode)
    mode(d->tmode);
}
/*--------------------------------------------------------------------------*/
static void mode(int video_mode)
{
 union REGS inregs;
 
 inregs.h.al = (unsigned char)video_mode;
 inregs.h.ah = 0; /* set mode */
 int86(VIDEO_INT, &inregs, &inregs);
}
/*--------------------------------------------------------------------------*/
static void scr_curs(int lin, int col)
{
 union REGS inregs;
 
 inregs.h.ah = 2; /* move cursor */
 inregs.h.dh = (unsigned char)lin;
 inregs.h.dl = (unsigned char)col;
 inregs.h.bh = 0; /* page */
 int86(VIDEO_INT, &inregs, &inregs);
}
/*--------------------------------------------------------------------------*/
static void scr_puts(char *string, int color)
{
 union REGS inregs, outregs;
 
 inregs.h.ah = 14; /* write char, advance */
 inregs.h.bh = 0; /* page */
 inregs.h.bl = (unsigned char)((color>=0) ? color : d->pri) ;
 while (*string){
    inregs.h.al = *string++;
    int86(VIDEO_INT, &inregs, &outregs);
 }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
