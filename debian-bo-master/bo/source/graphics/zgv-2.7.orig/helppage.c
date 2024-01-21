/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * helppage.c - for the online help page displays.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <vga.h>
#include "3deffects.h"
#include "font.h"
#include "readnbkey.h"
#include "rc_config.h"
#include "rcfile.h"



helpscreenon()
{
vga_setmode(G640x480x16);
vga_setcolor(15);

vga_setpalette(0,cfg.medium_r,cfg.medium_g,cfg.medium_b);
vga_setpalette(1,cfg.dark_r,cfg.dark_g,cfg.dark_b);
vga_setpalette(2,cfg.light_r,cfg.light_g,cfg.light_b);
vga_setpalette(15,cfg.black_r,cfg.black_g,cfg.black_b);

draw3dbox(0,0,639,479,2,1, 2,1);
draw3dbox(10,10,629,469,1,0, 2,1);
}


/* the caller is expected to reset the mode and redraw the screen afterwards,
 * if necessary. help is the array pointer (see vgadisp.c and zgv.c).
 */
showhelp(ttyfd,title,help)
int ttyfd;
char *title,help[][80];
{
int f,quit,gotkey,key;

quit=0;

while(!quit)
  {
  gotkey=0;
  helpscreenon();
  drawtext3d(90,50,5,title,1, 2,1,15);
  drawtext3d(210,86,3,"- press a key to return -",1, 2,1,15);
  f=0;
  while(strlen(help[f])>0)
    {
    f++;
    drawtext3d(90,100+f*20,3,help[f],0, 2,1,15);
    }

  while(!gotkey)
    {
    key=readnbkey(ttyfd);
    if(key!=0)
      {
      gotkey=1;
      quit=1;
      }
      
    usleep(10000);
    }
  }
}
