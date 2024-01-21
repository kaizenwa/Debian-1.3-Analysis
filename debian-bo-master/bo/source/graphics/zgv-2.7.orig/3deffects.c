/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * 3deffects.c - provides the '3d' style boxes, text etc.
 *                used by zgv.c (for file selector) and vgadisp.c
 *                (for help screen)
 */


#include <unistd.h>
#include <vga.h>
#include "3deffects.h"
#include "readnbkey.h"
#include "zgv.h"


/* produce tacky 3d text like Microsloth Windoze */
drawtext3d(x,y,s,str,isout,lite,dark,txt)
int x,y,s;
char *str;
int isout,lite,dark,txt;
{
vga_setcolor(isout?lite:dark);
vgadrawtext(x-1,y-1,s,str);
vga_setcolor(isout?dark:lite);
vgadrawtext(x+1,y+1,s,str);
vga_setcolor(txt);
vgadrawtext(x,y,s,str);
}


/* restore sanity */
undrawtext3d(x,y,s,str)
int x,y,s;
char *str;
{
vga_setcolor(idx_medium);
vgadrawtext(x-1,y-1,s,str);
vgadrawtext(x+1,y+1,s,str);
vgadrawtext(x,y,s,str);
}


/* render each bock in 3d */
draw3dbox(x1,y1,x2,y2,depth,isout,lite,dark)
int x1,y1,x2,y2,depth,isout,lite,dark;
{
int f;

for(f=0;f<depth;f++)
  {
  vga_setcolor(isout?lite:dark);
  vga_drawline(x1+f,y2-f,x1+f,y1+f);
  vga_drawline(x1+f,y1+f,x2-f,y1+f);
  vga_setcolor(isout?dark:lite);
  vga_drawline(x2-f,y1+f,x2-f,y2-f);
  vga_drawline(x2-f,y2-f,x1+f,y2-f);
  }
}


/* Undraw each relevant bock */
undraw3dbox(x1,y1,x2,y2,depth)
int x1,y1,x2,y2,depth;
{
int f;

vga_setcolor(idx_medium);
for(f=0;f<depth;f++)
  {
  vga_drawline(x1+f,y2-f,x1+f,y1+f);
  vga_drawline(x1+f,y1+f,x2-f,y1+f);
  vga_drawline(x2-f,y1+f,x2-f,y2-f);
  vga_drawline(x2-f,y2-f,x1+f,y2-f);
  }
}


/* this blasts whatever is 'under' it - you have to save or redraw it */
int msgbox(ttyfd,message,replytype,lite,dark,txt)
int ttyfd;
char *message;
int replytype,lite,dark,txt;
{
int f,x1,y1,x2,y2,wide,high,key;

high=90;
wide=vgatextsize(3,message)+60;
x1=((vga_getxdim()-wide)>>1);
y1=((vga_getydim()-high)>>1);
x2=((vga_getxdim()+wide)>>1);
y2=((vga_getydim()+high)>>1);

vga_setcolor(idx_medium);
for(f=y1;f<y2;f++)
  vga_drawline(x1,f,x2,f);

/* draw outer box */
draw3dbox(x1 ,y1  ,x2  ,y2  ,2,1, lite,dark);
draw3dbox(x1+9,y1+9,x2-9,y2-9,1,0, lite,dark);

/* finally, I've got around to doing different types of msgbox! */
switch(replytype)
  {
  /* a box with a single 'OK' button, for warnings, errors, etc. */
  case MSGBOXTYPE_OK:
    /* draw 'button' */
    draw3dbox((vga_getxdim()-40)>>1,y2-45,
              (vga_getxdim()+40)>>1,y2-20, 1,1, lite,dark);

    vga_setcolor(txt);
    vgadrawtext(x1+30,y1+20,3,message);
    drawtext3d((vga_getxdim()-15)>>1,y2-39,3,"OK",0,lite,dark,txt);

    do
      {
      usleep(10000);
      key=readnbkey(ttyfd);
      }
    while((key!=RK_ESC)&&(key!=RK_ENTER));
    return(1);

  /* a box with two buttons, 'Yes' and 'No'. Enter or 'y' selects yes,
   * Esc or 'n' selects no.
   */    
  case MSGBOXTYPE_YESNO:
    /* draw 'yes' button */
    draw3dbox((vga_getxdim()>>1)-50,y2-45,
              (vga_getxdim()>>1)-10,y2-20, 1,1, lite,dark);
    vga_setcolor(txt);
    vgadrawtext(x1+30,y1+20,3,message);		/* draw message */
    drawtext3d((vga_getxdim()>>1)-43,y2-39,3,"Yes",0,lite,dark,txt);

    /* draw 'no' button */
    draw3dbox((vga_getxdim()>>1)+10,y2-45,
              (vga_getxdim()>>1)+50,y2-20, 1,1, lite,dark);
    vga_setcolor(txt);
    drawtext3d((vga_getxdim()>>1)+23,y2-39,3,"No",0,lite,dark,txt);

    do
      {
      usleep(10000);
      key=readnbkey(ttyfd);
      }
    while(key!=RK_ESC && key!=RK_ENTER && key!='y' && key!='n');

    return(key==RK_ENTER || key=='y');
  }
return(0);
}
