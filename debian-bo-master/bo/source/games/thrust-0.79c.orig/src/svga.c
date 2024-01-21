
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#if defined(HAVE_GETOPT_H) && defined(HAVE_GETOPT_LONG_ONLY)
# include <getopt.h>
#elif !defined(HAVE_GETOPT_LONG_ONLY)
# include "getopt.h"
#endif

#include <vga.h>
#include <vgagl.h>
#include <string.h>
#include <stdio.h>

#include "thrust.h"
#include "fast_gr.h"
#include "gr_drv.h"
#include "options.h"

#define VGAMODE G320x200x256
#define PSTARTX ((WIDTH-PUSEX)>>1)
#define PSTARTY ((HEIGHT-PUSEY-24)>>1)

void
clearscr(void)
{
  gl_clearscreen(0);
}

void
putarea(unsigned char *source,
	int x, int y, int width, int height, int bytesperline,
	int destx, int desty)
{
  unsigned char *tmp;
  int res=PSTARTX+destx+WIDTH*(PSTARTY+desty);
  int dy, seg, lastseg, endseg;

  tmp=source+y*bytesperline+x;

  seg=res>>16;
  vga_setpage(seg);
  lastseg = seg;
  for(dy=y; dy<y+height; res+=WIDTH, dy++, tmp+=bytesperline) {
    /*
    vga_drawscansegment(tmp, PSTARTX, PSTARTY+res/WIDTH, PUSEX);
    */
    seg=res>>16;
    endseg=(res+width-1)>>16;
    if(seg != lastseg) {
      vga_setpage(seg);
      lastseg = seg;
    }
    if(seg == endseg)
      memcpy(graph_mem+(res&65535), tmp, width);
    else {
      int len = (endseg<<16)%WIDTH-PSTARTX-destx;
      memcpy(graph_mem+(res&65535), tmp, len);
      vga_setpage(endseg);
      lastseg = endseg;
      memcpy(graph_mem, tmp+len, width-len);
    }
  }
  vga_setpage(0);
}

void
puthline(unsigned char *source, int x, int y, int length)
{
  vga_drawscansegment(source, PSTARTX+x, PSTARTY+y, title_cols);
}

void
putpixel(int x, int y, unsigned char color)
{
  gl_setpixel(PSTARTX+x, PSTARTY+y, color);
}

int
getpixel(int x, int y)
{
  return vga_getpixel(PSTARTX+x, PSTARTY+y);
}

void
syncscreen(void)
{
  vga_waitretrace();
}

void
displayscreen(void)
{
}

void
fadepalette(int first, int last, unsigned char *RGBtable, int fade, int flag)
{
/*
  static Palette tmpRGBtable;
  int entries,e,i;
  byte *c;

  entries=(e=last-first+1)*3;
  memcpy(&tmpRGBtable, RGBtable, sizeof(Palette));
  c=(char *)&tmpRGBtable;
  for(i=0; i<sizeof(Palette); i++)
    *(c++)=((*c)*fade)>>8;
  if(flag)
    vga_waitretrace();
  gl_setpalettecolors(first, e, &tmpRGBtable);
*/
  static int tmpRGBtable[768];
  int entries,i;
  int *c;
  unsigned char *d;

  entries=last-first+1;
  
  c=(int *)&tmpRGBtable;
  d=(unsigned char *)RGBtable;
  i=0;

  while(i<3*entries) {
    *c = (*d * fade) >> 8;
    if(++i<3*entries) {
      c++;
      d++;
    }
  }

  if(flag)
    vga_waitretrace();
  vga_setpalvec(first, entries, tmpRGBtable);
}

void
fade_in(void)
{
  int i;

  if(PSTARTY>0 && PSTARTX>0) {
    gl_hline(PSTARTX-3, PSTARTY-3, PSTARTX+PUSEX+2, 1);
    gl_hline(PSTARTX-4, PSTARTY-4, PSTARTX+PUSEX+3, 2);
    gl_hline(PSTARTX-5, PSTARTY-5, PSTARTX+PUSEX+4, 3);
    gl_hline(PSTARTX-3, PSTARTY+PUSEY+26, PSTARTX+PUSEX+2, 3);
    gl_hline(PSTARTX-4, PSTARTY+PUSEY+27, PSTARTX+PUSEX+3, 2);
    gl_hline(PSTARTX-5, PSTARTY+PUSEY+28, PSTARTX+PUSEX+4, 1);
    gl_line(PSTARTX-3, PSTARTY-3, PSTARTX-3, PSTARTY+PUSEY+26, 1);
    gl_line(PSTARTX-4, PSTARTY-4, PSTARTX-4, PSTARTY+PUSEY+27, 2);
    gl_line(PSTARTX-5, PSTARTY-5, PSTARTX-5, PSTARTY+PUSEY+28, 3);
    gl_line(PSTARTX+PUSEX+2, PSTARTY-3, PSTARTX+PUSEX+2, PSTARTY+PUSEY+26, 3);
    gl_line(PSTARTX+PUSEX+3, PSTARTY-4, PSTARTX+PUSEX+3, PSTARTY+PUSEY+27, 2);
    gl_line(PSTARTX+PUSEX+4, PSTARTY-5, PSTARTX+PUSEX+4, PSTARTY+PUSEY+28, 1);
  }
  else if(PSTARTY>0) {
    gl_hline(PSTARTX, PSTARTY-3, PSTARTX+PUSEX-1, 1);
    gl_hline(PSTARTX, PSTARTY-4, PSTARTX+PUSEX-1, 2);
    gl_hline(PSTARTX, PSTARTY-5, PSTARTX+PUSEX-1, 3);
    gl_hline(PSTARTX, PSTARTY+PUSEY+26, PSTARTX+PUSEX-1, 3);
    gl_hline(PSTARTX, PSTARTY+PUSEY+27, PSTARTX+PUSEX-1, 2);
    gl_hline(PSTARTX, PSTARTY+PUSEY+28, PSTARTX+PUSEX-1, 1);
  }

  for(i=1; i<=64; i++)
    fadepalette(0, 255, bin_colors, i, 1);
}

void
fade_out(void)
{
  int i;

  for(i=64; i; i--)
    fadepalette(0, 255, bin_colors, i, 1);
  clearscr();
  usleep(500000L);
}




int __vga_name2number(char *);

void
graphics_preinit(void)
{
#ifndef RUN_WITH_SVGA
  uid_t ruid;
  gid_t rgid, egid;

  ruid = getuid();      /* Remember the real user */
  rgid = getgid();      /* Remember the real group */
  egid = getegid();     /* Remember the effective group */
  setreuid(0, -1);      /* Set the real user to root. Only possible if
                           already root or suid root which is a must for
                           svgalib programs */
  vga_init();           /* Init svga, sets the effective user and group
                           to the real user and group */
  setregid(rgid, egid); /* Restore the original real and effective group */
  setreuid(ruid, ruid); /* Restore the original real user and set the
                           effective user to the original real user,
                           throwing away the suid bit. */
#endif
}

int
graphicsinit(int argc, char **argv)
{
  int mode;
  vga_modeinfo *modeinfo;
  char *svgamode = NULL;
  int optc;

  optind=0;
  do {
    static struct option longopts[] = {
      OPTS,
      SVGA_OPTS,
      { 0, 0, 0, 0 }
    };

    optc=getopt_long_only(argc, argv, OPTC SVGA_OPTC, longopts, (int *)0);
    switch(optc) {
    case 's':
      svgamode = strdup(optarg);
      break;
    }
  } while(optc != EOF);

  if(svgamode != NULL) {
    mode = __vga_name2number(svgamode);
    if(mode==-1) {
      printf("Illegal Svgalib graphics mode specified.\n");
      return(-1);
    }
  }
  else
    mode = VGAMODE;

  modeinfo = vga_getmodeinfo(mode);
  if(modeinfo->width < PUSEX
     || modeinfo->height < PUSEY+24
     || modeinfo->bytesperpixel != 1
     || modeinfo->colors != 256) {
    printf("Svgalib graphics mode unusable.\n"
	   "Must be at least 320x200 pixels and must be one byte per pixel.\n");
    return(-1);
  }

  if(!vga_hasmode(mode)) {
    printf("Svgalib graphics mode is not available.\n");
    return(-1);
  }
  vga_setmode(mode);
  gl_setcontextvga(mode);

  fadepalette(0, 255, bin_colors, 1, 0);

  return 0;
}

int
graphicsclose(void)
{
#ifndef RUN_WITH_SVGA
  printf("Restoring textmode...");
  vga_setmode(TEXT);
  printf("done.\n");
#endif

  return 0;
}

char *
graphicsname(void)
{
  static char name[] = "SVGA";

  return name;
}
