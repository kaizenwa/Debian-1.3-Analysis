/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks.
 *
 * zgv.c - This provides the zgv file selector, and interfaces to the
 *         vga display routines (vgadisp.c)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/vt.h>
#include <vga.h>

#include "zgv.h"
#include "gifeng.h"
#include "vgadisp.h"
#include "readnbkey.h"
#include "font.h"
#include "3deffects.h"
#include "helppage.h"
#include "rc_config.h"
#include "rcfile.h"
#include "readjpeg.h"   /* we only use this for my_error_exit() */
#include "readpnm.h"	/* for the dithering - should be moved! */
#include "resizepic.h"
#include "mouse.h"

#include "zgvlogopck.h"


char zgv_pngerr[128];	/* for error messages from pnglib */

char zgvhelp[][80]={
  " ", " ",
  "? (question mark) \t this help page",
  " ",
  "up (also k or q) \t move file selection cursor up",
  "down (also j or a) \t move file selection cursor down",
  "left (also h or o) \t move file selection cursor left",
  "right (also l or p) \t move file selection cursor right",
  " ",
  "Enter \t\t display file or change directory",
  " ",
  "v \t\t\t visual selection mode on/off",
  "u \t\t\t create/update thumbnails in visual mode",
  "Ctrl-R \t\t update directory listing / redraw screen",
  "D or Delete \t\t delete file",
  " ",
  "Esc or x \t\t exit zgv",
  ""
  };

#define fwinxpos(f) (40+(((f)-1)/YSIZ)*BARWIDTH)
#define fwinypos(f) (180+barheight*(((f)-1)%YSIZ))

/* from 18-bit RGB (as used by VGA palette) to 3:3:2 palette index */
#define MAKE332COL(r,g,b) (((r)>>3)*32+((g)>>3)*4+((b)>>4))


#define LIGHT		2
#define DARK		1
#define BLACK		15
#define MIDGREY		0
#define MARKEDCOL	14

#define GDFSIZ			3
#define BARWIDTH		138
#define BAR_RESTRICT_WIDTH	BARWIDTH-6

#define COLS		600
#define MDIRSIZ		1000
#define YSIZ		(285/barheight)

/* number of greyscales *less one* used in selector when in 640x480x16 mode */
#define GREY_MAXVAL	10


int idx_light,idx_medium,idx_dark,idx_black,idx_marked;
int updating_index=0;
int v256=1;	/* 1=256-col selector, 0=16-col selector */

int barheight;
extern int sgres;
int has_mouse=0;


int gcompare(void *,void *);
void showhowfar(int,int);
void smallhowfar(int,int);
void exitproperly();
void switchback();


struct GDIR {
  char name[256];
  char isdir;            /* 0=file, 1=dir. */
  int xvw,xvh;		/* xvpic width and height, zero if none */
  char marked;
  } gifdir[MDIRSIZ];
int gifdirsiz;

int zgv_ttyfd,howfar_upto;
jmp_buf setjmpbuf;   /* in case someone aborts decompressing a file */
static int xv332_how_far_xpos,xv332_how_far_ypos;
static int one_file_only=0;
int original_vt,separate_vt=0;
int zgv_vt;
int tagview_mode=0;



main(argc,argv)
int argc;
char **argv;
{
int argsleft;

/* kludgetastic, mate */
if(argc>1 && strcmp(argv[1],"-d")==0) cfg.dontchangestdin=1;

if(fixvt()==0)
  {
  fprintf(stderr,"Not running on console and no free VTs found.\n");
  exit(1);
  }

atexit(switchback);		/* this gets called after svgalib stuff */

vga_disabledriverreport();    /* quieten svgalib */
vga_init();
/* root permissions should now have been ditched */

pixelsize=1;
getconfig();				/* see rcfile.c... */
argsleft=parsecommandline(argc,argv);	/* ...for these two. */
    
copyfromconfig();
    
if(cfg.mtype != NO_MOUSE)
  has_mouse=ms_init(MACCEL, MBAUD, MDELTA, MDEV, MTOGGLE, MSAMPLE, cfg.mtype);

cleartext();

/* do one-file-only if have one arg. */
/* (does a chdir and carries on if it's a dir.) */
if(argsleft==1)
  load_one_file(argv[argc-1]);

/* do slideshow if more than one arg */
/* XXX this should be shoved into a separate routine sometime */
if(argsleft>1)
  {
  int i, entnum;
  struct stat buf;
  for(i=argc-argsleft,entnum=1;i<=argc-1;i++)
    if(stat(argv[i],&buf) != -1 && !S_ISDIR(buf.st_mode))
      {
      strcpy(gifdir[entnum].name,argv[i]);
      gifdir[entnum].marked=1;
      gifdir[entnum].isdir=0;
      entnum++;
      }
  gifdirsiz = entnum;
  one_file_only=1;
  screenon();
  openstdin_nonblocking();
  load_tagged_files();
  screenoff();
  exit(0);
  }

/* normal zgv startup */
cfg.errignore=0;	/* don't ignore errs if using this way */
screenon();
openstdin_nonblocking();
mainloop();
screenoff();

exit(0);
}


openstdin_nonblocking()
{
zgv_ttyfd=fileno(stdin);
fcntl(zgv_ttyfd,F_SETFL,O_NONBLOCK);
}


load_one_file(filename)
char *filename;
{
hffunc hf;
struct stat sbuf;

/* before we load it, see if it's a directory. If so, we run zgv with
 * much the same result as typing '( cd whatever;zgv )'.
 */

if(stat(filename,&sbuf)!=-1 && S_ISDIR(sbuf.st_mode))
  {
  chdir(filename);
  return(0);
  }
  
openstdin_nonblocking();	/* we hadn't done this yet */
one_file_only=1;
if(cfg.onefile_progress)
  {
  screenon();
  inithowfar();
  hf=showhowfar;
  }
else 
  {
  hf=NULL;
  fprintf(stderr,"Loading...");
  }

if(cfg.onefile_progress && hf!=NULL) vga_runinbackground(1);  

if(readpicture(filename,hf,1)!=_PIC_OK)
  {
  if(cfg.onefile_progress) screenoff();
  fprintf(stderr,"\rError loading file.\n");
  exit(1);
  }
else
  {
  if(hf==NULL)
    fprintf(stderr,"\r          \r");
  }
      
wait_for_foreground();
screenoff();
exit(0);
}


copyfromconfig()
{
curvgamode=cfg.videomode;
zoom=cfg.zoom;
vkludge=cfg.vkludge;
brightness=cfg.brightness;
contrast=cfg.contrast;
if((curvgamode==G320x400x256)||(curvgamode==G360x480x256))
  virtual=1;
else
  virtual=0;
}


mainloop()
{
int quit,key,curent; /* quit, key pressed, current entry */
int oldent,startfrom,oldstart,f,markchange;
extern int x,y,mx,my;
int left_button=0, right_button=0, butstatep=0,px,py;
struct ms_event ev;

quit=0; curent=1; oldent=1;
startfrom=1;

readgifdir();
showgifdir(startfrom,0);
showbar(curent,1,startfrom);

mx=my=1000;
px=x=mx/2;
py=y=my/2;
while(!quit)
  {
  oldent=curent;
  markchange=0;
  usleep(10000);
  key=readnbkey(zgv_ttyfd);
  
  /* grok mouse movements and buttons */
  if(has_mouse)
    {
    if(mx != 1000 || !x || !y || x==mx || y==my)
      {
      mx=my=1000;
      px=x=mx/2;
      py=y=my/2;
      }
      
    if(get_ms_event(&ev)>0)
      {
      if (y < py-20)
        {
        if(curent>1) curent--;
        py = y;
        }
      else if (y > py+20)
        {
        if(curent<gifdirsiz) curent++;
        py = y;
        }
          
      if (x < px-20)
        {
        curent-=YSIZ;
        if(curent<1) curent=1;
        px = x;
        }
      else if (x > px+20)
        {
        curent+=YSIZ;
        if(curent>gifdirsiz) curent=gifdirsiz;
        px = x;
        }
          
      if(ev.ev_butstate != butstatep)
        {
        butstatep = ev.ev_butstate;
        if(left_button && !(ev.ev_butstate & 4))
          key = RK_ENTER;
        if(right_button && !(ev.ev_butstate & 1))
          key = ' ';
        left_button = ev.ev_butstate & 4;
        right_button = ev.ev_butstate & 1;
        }
      }
    }	/* end of 'if(has_mouse)' */
      
  switch(key)
    {
    case 'u':
      if(cfg.xvpic_index)
        {
        updating_index=1;
        update_xvpics();
        updating_index=0;
        redrawall(curent,startfrom);
        }
      break;
    case 'v':
      cfg.xvpic_index=!cfg.xvpic_index;
      redrawall(curent,startfrom);
      break;
    case 'c':
      if(v256) break;
      cfg.fs16col=!cfg.fs16col;
      redrawall(curent,startfrom);
      break;
    case '?':
      showhelp(zgv_ttyfd,"- KEYS FOR FILE SELECT SCREEN -",zgvhelp);
      redrawall(curent,startfrom);
      break;
    case 18: case 12:   /* ^R and ^L */
      readgifdir();
      if(curent>gifdirsiz) curent=gifdirsiz;
      oldent=curent;
      redrawall(curent,startfrom);
      break;
    case RK_HOME: case 'A'-0x40: /* home or ^a */
      curent=1;
      break;
    case RK_END:  case 'E'-0x40: /* end  or ^e */
      curent=gifdirsiz;
      break;
    case RK_PAGE_UP: case 'U'-0x40: /* pgup or ^u */
      curent-=YSIZ*3;
      if(curent<1) curent=1;
      break;
    case RK_PAGE_DOWN: case 'V'-0x40: /* pgdn or ^v */
      curent+=YSIZ*3;
      if(curent>gifdirsiz) curent=gifdirsiz;
      break;
    case 'D': case RK_DELETE:   /* shift-D or the 'del' key */
      if(gifdir[curent].isdir) break;
      if(delete_file(gifdir[curent].name))
        {
        readgifdir();
        if(curent>gifdirsiz) curent=gifdirsiz;
        oldent=curent;
        }
      redrawall(curent,startfrom);
      break;
    case 't':	/* tag current file and move on */
      if(gifdir[curent].isdir==0)
        {
        gifdir[curent].marked=1;
        markchange=1;
        }
      if(curent<gifdirsiz) curent++;
      break;
    case 'n':	/* untag current file and move on */
      if(gifdir[curent].isdir==0)
        {
        gifdir[curent].marked=0;
        markchange=1;
        }
      if(curent<gifdirsiz) curent++;
      break;
    case ' ':   /* toggle tag/untag flag of current file and move on */
      if(gifdir[curent].isdir==0)
        {
        gifdir[curent].marked=1-gifdir[curent].marked;
        markchange=1;
        }
      if(curent<gifdirsiz) curent++;
      break;
    case 'L':	/* toggle loop or not in slideshow */
      cfg.loop=1 - cfg.loop;
      if(cfg.loop) 
         msgbox(zgv_ttyfd,"Loop in slideshow.",
                MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
      else
         msgbox(zgv_ttyfd,"No looping in slideshow.",
                MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
      redrawall(curent,startfrom);
      break;
    case 'T':	/* tag all files */
      for(f=1;f<=gifdirsiz;f++) if(gifdir[f].isdir==0) gifdir[f].marked=1;
      showgifdir(startfrom,0);
      break;
    case 'N':	/* untag all files */
      for(f=1;f<=gifdirsiz;f++) if(gifdir[f].isdir==0) gifdir[f].marked=0;
      showgifdir(startfrom,0);
      break;
    case 'q': case 'k': case RK_CURSOR_UP:
      if(curent>1) curent--; break;
    case 'a': case 'j': case RK_CURSOR_DOWN:
      if(curent<gifdirsiz) curent++; break;
    case 'o': case 'h': case RK_CURSOR_LEFT:
      curent-=YSIZ;
      if(curent<1) curent=1;
      break;
    case 'p': case 'l': case RK_CURSOR_RIGHT:
      curent+=YSIZ;
      if(curent>gifdirsiz) curent=gifdirsiz;
      break;
    case RK_ENTER:
      /* uhhhhh my head hurts */
      sgres = 0;
      do
        {
        if(permissiondenied(gifdir[curent].name))
          {
          msgbox(zgv_ttyfd,"Permission denied or file not found",
          	MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
          redrawall(curent,startfrom);
          pic_incr=0;	/* stop here *even if ^p or ^n used* */
          break;
          }
          
        if(gifdir[curent].isdir)
          {
          showbar(curent,0,startfrom);
          showgifdir(startfrom,1);
          chdir(gifdir[curent].name);
          readgifdir();
          showgifdir(1,0); curent=oldent=startfrom=1;
          showbar(curent,1,startfrom);
          break;
          }
          
        tagview_mode=0;
	if(sgres>=2)
          {
	  if(load_single_file(curent,0)==0) pic_incr=0;
	  }
        else
          if(load_single_file(curent,1)==0) pic_incr=0;

	if(sgres==3) gifdir[curent].marked=1;
        curent+=pic_incr;
        if(curent>gifdirsiz) { curent--; pic_incr=0; }
        if(curent<1 || gifdir[curent].isdir) { curent++; pic_incr=0; }
        oldstart=startfrom;
        while(curent<startfrom)
          startfrom-=YSIZ;
        while(fwinxpos(curent-startfrom+1)+BARWIDTH>COLS)
          startfrom+=YSIZ;
        if(startfrom<1) startfrom=1;
        if(sgres<2 || pic_incr==0)
          {
          oldent=-1;
          redrawall(curent,startfrom);
          }
        }
      while(pic_incr);
      break;
      
    case '\t':		/* tab */
      load_tagged_files();
      redrawall(curent,startfrom);
      break;
    case 'x': case RK_ESC:
      quit=1;
    }
    
  oldstart=startfrom;
  while(curent<startfrom)
    startfrom-=YSIZ;
  while(fwinxpos(curent-startfrom+1)+BARWIDTH>COLS)
    startfrom+=YSIZ;
  if(startfrom<1) startfrom=1;
  if(startfrom!=oldstart)
    {
    showbar(oldent,0,oldstart);
    showgifdir(oldstart,1);
    showgifdir(startfrom,0);
    showbar(curent,1,startfrom);
    }
  else  
    if(oldent!=-1 && (curent!=oldent || markchange))
      {
      showbar(oldent,0,startfrom);
      showbar(curent,1,startfrom);
      }
  }
}



int load_single_file(curent,do_howfar)
int curent,do_howfar;
{
static char buf[1024];

if(cfg.cmd[0])
  {
  /* alternative command */
  /* avoid using sprintf() just in case of stray %'s etc. */
  char *ptr,sav;
  ptr=strstr(cfg.cmd,"%s");	/* we know it's there - see rcfile.c */
  sav=*ptr; *ptr=0;
  strcpy(buf,cfg.cmd);
  strcat(buf,gifdir[curent].name);
  strcat(buf,ptr+2);
  *ptr=sav;
  screenoff();
  system(buf);
  }
else
  {
  int tmp;
  
  if(do_howfar)
    inithowfar();
    
  vga_runinbackground(1);

  /* save context for possible abort */
  if(setjmp(setjmpbuf))
    {
    /* if we get here, someone aborted loading a file. */
    if(!tagview_mode || (tagview_mode && do_howfar))
      {
      wait_for_foreground();
      msgbox(zgv_ttyfd,"File view aborted",MSGBOXTYPE_OK,
         	idx_light,idx_dark,idx_black);
      }
    return(0);
    }
  else
    {
    if((tmp=readpicture(gifdir[curent].name,
    		do_howfar?showhowfar:smallhowfar,1))!=_PIC_OK)
      {
      wait_for_foreground();
      if(tagview_mode) screenon();
      showerrmessage(tmp);
      return(0);
      }
    }
  }

return(1);
}


load_tagged_files()
{
int f,t;
int dohf;

for(f=1,t=0;f<=gifdirsiz;f++)
  if(gifdir[f].marked) t++;

if(t==0)
  {
  msgbox(zgv_ttyfd,"No files tagged",MSGBOXTYPE_OK,
  	idx_light,idx_dark,idx_black);
  return(0);
  }

/* vgadisp.c sets tagview_mode==0 if esc is pressed */
tagview_mode=1;
dohf=1;
do {
 for(f=1;f<=gifdirsiz && tagview_mode;f++)
  {
  if(gifdir[f].marked==0) continue;
  if(permissiondenied(gifdir[f].name))
    {
    if(f>1) screenon();
    msgbox(zgv_ttyfd,"Permission denied or file not found",
    	MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
    return(0);
    }
  if(load_single_file(f,dohf)==0) break;
  dohf=0;
  }
 } while (cfg.loop && sgres != 1);
return(0);
}


/* this could also be file not found of course */
int permissiondenied(fname)
char *fname;
{
FILE *junk;
if((junk=fopen(fname,"rb"))==NULL)
  return(1);
fclose(junk);
return(0);
}


redrawall(curent,startfrom)
int curent,startfrom;
{
screenon();

showgifdir(startfrom,0);
showbar(curent,1,startfrom);
}

inithowfar()
{
int f;

vga_setcolor(idx_medium);
for(f=220;f<=260;f++)
  vga_drawline(100,f,540,f);
draw3dbox(100,220,540,260,2,1, idx_light,idx_dark);
draw3dbox(109,229,531,251,1,0, idx_light,idx_dark);

howfar_upto=0;
drawtext3d(250,235,2,"Decompressing - please wait",0,
					idx_light,idx_dark,idx_black);
}


void showhowfar(sofar,total)
int sofar,total;
{
int f,d;

/* test for abort */
smallhowfar(sofar,total);

if(((sofar%10)==0)||(sofar==total))
  {
  d=(420*sofar)/total;
  if(d>howfar_upto)
    {
    vga_lockvc();
    if(!vga_oktowrite())
      {
      vga_unlockvc();
      return;
      }
    vga_setcolor(idx_light); /* we set this always in case of a VC switch */
    for(f=howfar_upto;f<=d;f++)
      vga_drawline(110+f,230,110+f,250);
    vga_unlockvc();
    howfar_upto=f;
    }
  }
}


/* minimal 'how far' function that just tests for an abort */
void smallhowfar(sofar,total)
int sofar,total;
{
/* we jump back to an abort message if Esc was pressed */
if(!one_file_only && !cfg.repeat_timer)
  if(readnbkey(zgv_ttyfd)==RK_ESC)
    {
    /* these routines blast the malloc'ed stuff, which has *for sure*
     * been allocated by now, because we must already be reading the file
     * in for us to get here!
     */
    aborted_file_cleanup();
    longjmp(setjmpbuf,1);
    }
}


showbar(entnum,selected,startfrom)
int entnum,selected;
int startfrom;
{
char ctmp[100];
int xpos,ypos;
int xt;

xpos=fwinxpos(entnum-startfrom+1);
if((xpos<1)||(xpos+BARWIDTH>COLS)) return(0);
ypos=fwinypos(entnum-startfrom+1);
prettyfile(&ctmp,&(gifdir[entnum]));

set_max_text_width(BAR_RESTRICT_WIDTH);
xt=cfg.xvpic_index?centreseltxt(xpos,GDFSIZ,ctmp):xpos+3;
if(cfg.blockcursor)
  {
  /* block-style cursor - hopefully easier to read/see. */
  if(selected)
    draw3dbox(xpos-2,ypos-2,xpos+BARWIDTH+1,ypos+barheight+1,4,1,
    	idx_dark,idx_dark);
  else
    undraw3dbox(xpos-2,ypos-2,xpos+BARWIDTH+1,ypos+barheight+1,4);
  
  /* in case the last file was just marked/unmarked */
  vga_setcolor(gifdir[entnum].marked?idx_marked:idx_black);
  vgadrawtext(xt,ypos+3,GDFSIZ,ctmp);
  }
else
  {
  if(selected)
    {
    draw3dbox(xpos,ypos,xpos+BARWIDTH-1,ypos+barheight-1,1,1,
    	idx_light,idx_dark);
    drawtext3d(xt,ypos+3,GDFSIZ,ctmp,0, idx_light,idx_dark,
    		gifdir[entnum].marked?idx_marked:idx_black);
    /* box if cfg.xvpic_index and is an xvpic being used */
    if(cfg.xvpic_index && gifdir[entnum].xvw!=0)
      draw3dbox(xpos+(BARWIDTH-gifdir[entnum].xvw)/2-2,
                ypos+GDFSIZ*6+39-gifdir[entnum].xvh/2-2,
                xpos+(BARWIDTH-gifdir[entnum].xvw)/2+gifdir[entnum].xvw+1,
                ypos+GDFSIZ*6+39-gifdir[entnum].xvh/2+gifdir[entnum].xvh+1,
                1,0, idx_light,idx_dark);
    }
  else
    {
    undraw3dbox(xpos,ypos,xpos+BARWIDTH-1,ypos+barheight-1,1);
    undrawtext3d(xt,ypos+3,GDFSIZ,ctmp);
    vga_setcolor(gifdir[entnum].marked?idx_marked:idx_black);
    vgadrawtext(xt,ypos+3,GDFSIZ,ctmp);
    /* undraw box if cfg.xvpic_index and is an xvpic being used */
    if(cfg.xvpic_index && gifdir[entnum].xvw!=0)
      undraw3dbox(xpos+(BARWIDTH-gifdir[entnum].xvw)/2-2,
                  ypos+GDFSIZ*6+39-gifdir[entnum].xvh/2-2,
                  xpos+(BARWIDTH-gifdir[entnum].xvw)/2+gifdir[entnum].xvw+1,
                  ypos+GDFSIZ*6+39-gifdir[entnum].xvh/2+gifdir[entnum].xvh+1,
                  1);
    }
  }
set_max_text_width(NO_CLIP_FONT);
return(0);
}


int centreseltxt(x,fsiz,str)
int x,fsiz;
char *str;
{
int a;

a=vgatextsize(fsiz,str);
return(x+(BARWIDTH-a)/2);
}


showgifdir(startfrom,unshow)
int startfrom,unshow;
{
char cdir[MAXPATHLEN+1],*ptr;
static char ctmp[1024],ctmp2[1024];
int f,ypos,xpos,w,h,y,xt;
unsigned char *image;

getcwd(cdir,MAXPATHLEN);
if(updating_index)
  sprintf(ctmp,"Updating index of %s",cdir);
else
  sprintf(ctmp,"Directory of %s",cdir);

set_max_text_width(560);
if(unshow)
  undrawtext3d(40,135,4,ctmp);
else
  drawtext3d(40,135,4,ctmp,1, idx_light,idx_dark,idx_black);

set_max_text_width(BAR_RESTRICT_WIDTH);
for(f=startfrom;f<=gifdirsiz;f++)
  {
  xpos=fwinxpos(f-startfrom+1);
  if(xpos+BARWIDTH>COLS) break;
  ypos=fwinypos(f-startfrom+1);
  prettyfile(&ctmp,&(gifdir[f]));
  xt=cfg.xvpic_index?centreseltxt(xpos,GDFSIZ,ctmp):xpos+3;
  vga_setcolor(unshow?idx_medium:(gifdir[f].marked?idx_marked:idx_black));
  vgadrawtext(xt,ypos+3,GDFSIZ,ctmp);
  if(cfg.thicktext && cfg.blockcursor)
    vgadrawtext(xt+1,ypos+3,GDFSIZ,ctmp);
  vga_setcolor(unshow?idx_medium:idx_black);
  if(cfg.xvpic_index)
    {
    /* load and draw index file (or undraw it) */
    if(unshow)
      {
      image=malloc(96);
      if(image!=NULL)
        {
        memset(image,idx_medium,96);
        for(y=-2;y<62;y++)
          vga_drawscansegment(image,
          	xpos+(BARWIDTH-80)/2-2,ypos+y+GDFSIZ*6+9,96);
        free(image);
        }
      }
    else
      {
      sprintf(ctmp ,".xvpics/%s",gifdir[f].name);
      sprintf(ctmp2,"%s/.xvpics/",getenv("HOME")?getenv("HOME"):"");
      ptr=ctmp2+strlen(ctmp2);
      getcwd(ptr,sizeof(ctmp2)-strlen(ctmp2)-1); ctmp2[strlen(ctmp2)]=0;
      /* convert /foo/bar/baz to _foo_bar_baz */
      while((ptr=strchr(ptr,'/'))!=NULL) *ptr++='_';
      strcat(ctmp2,"/"); strcat(ctmp2,gifdir[f].name);
      
      gifdir[f].xvw=gifdir[f].xvh=0;
      if(gifdir[f].isdir)
        {
        /* 'folder' icon, as usual for these type of things */
        vga_setcolor(unshow?idx_medium:idx_black);
        xt=xpos+(BARWIDTH-80)/2;
        ypos+=GDFSIZ*6+9;
        
        /* main bit */
        vga_drawline(xt+10,ypos+50,xt+70,ypos+50);
        vga_drawline(xt+70,ypos+50,xt+70,ypos+20);
        vga_drawline(xt+70,ypos+20,xt+65,ypos+15);
        vga_drawline(xt+65,ypos+15,xt+15,ypos+15);
        vga_drawline(xt+15,ypos+15,xt+10,ypos+20);
        vga_drawline(xt+10,ypos+20,xt+10,ypos+50);
        
        /* top bit */
        vga_drawline(xt+15,ypos+15,xt+20,ypos+10);
        vga_drawline(xt+20,ypos+10,xt+35,ypos+10);
        vga_drawline(xt+35,ypos+10,xt+40,ypos+15);
        ypos-=GDFSIZ*6+9;
        
        gifdir[f].xvw=w=80; gifdir[f].xvh=h=60;
        }
      else
        if(read_xv332(ctmp ,&image,&w,&h)==_PIC_OK ||
           read_xv332(ctmp2,&image,&w,&h)==_PIC_OK)
          {
	  int xwant=xpos+(BARWIDTH-w)/2,w8=0;
	  
          gifdir[f].xvw=w; gifdir[f].xvh=h;
	  if(v256==0)
	    {
	    w8=((w+7)&~7)+8;
	    greyfix332(&image,w,h,xwant&7,w8);
	    xwant&=~7;
	    }

          if(image!=NULL)
            {
            for(y=0;y<h;y++)
              vga_drawscansegment(image+y*(v256?w:w8),xwant,
                ypos+y+GDFSIZ*6+39-h/2,v256?w:w8);
            free(image);
            }
          }
        else
          {
          /* a default icon-type-thing here */
          vga_setcolor(unshow?idx_medium:idx_black);
          xt=xpos+(BARWIDTH-80)/2;
          ypos+=GDFSIZ*6+9;
          
          /* main bit */
          vga_drawline(xt+20,ypos+50,xt+60,ypos+50);
          vga_drawline(xt+60,ypos+50,xt+60,ypos+20);
          vga_drawline(xt+60,ypos+20,xt+50,ypos+10);
          vga_drawline(xt+50,ypos+10,xt+20,ypos+10);
          vga_drawline(xt+20,ypos+10,xt+20,ypos+50);
          
          /* 'folded' bit */
          vga_drawline(xt+50,ypos+10,xt+50,ypos+20);
          vga_drawline(xt+50,ypos+20,xt+60,ypos+20);
          
          ypos-=GDFSIZ*6+9;
          gifdir[f].xvw=w=80; gifdir[f].xvh=h=60;
          }
      
      if(gifdir[f].xvw!=0)
        {
        draw3dbox(xpos+(BARWIDTH-w)/2-1,ypos+GDFSIZ*6+38-h/2,
                  xpos+(BARWIDTH-w)/2+w,ypos+GDFSIZ*6+39-h/2+h,1,0,
                  idx_light,idx_dark);
        }
      }
    }
  }
set_max_text_width(NO_CLIP_FONT);
}

readgifdir()
{
DIR *dirfile;
struct dirent *anentry;
struct stat buf;
char cdir[MAXPATHLEN+1];
int entnum,l,isdir;

getcwd(cdir,MAXPATHLEN);
dirfile=opendir(".");
entnum=0;
while((anentry=readdir(dirfile))!=NULL)
  {
  if(entnum==MDIRSIZ) break;
  if(anentry->d_name[0]=='.' && anentry->d_name[1]!='.')
    continue;	/* skip 'dot' files */
  if((strcmp(anentry->d_name,".")!=0)&&       /* no . and no .. if at root */
     (!((strcmp(cdir,"/")==0)&&(strcmp(anentry->d_name,"..")==0))))
    {
    if((stat(anentry->d_name,&buf))==-1)
      buf.st_mode=0;
    isdir=S_ISDIR(buf.st_mode);
    /* directories, GIF/JPG/PBM/PGM/PPM tested here */
    /* if I didn't know better I'd think this was a mess */
    if(((l=strlen(anentry->d_name))>4) || isdir)
      if((!strcasecmp(anentry->d_name+l-4,".gif")) ||
         (!strcasecmp(anentry->d_name+l-4,".jpg")) ||
         (!strcasecmp(anentry->d_name+l-5,".jpeg")) ||
         (!strcasecmp(anentry->d_name+l-4,".bmp")) ||
         (!strcasecmp(anentry->d_name+l-4,".tga")) ||
#ifdef PNG_SUPPORT
         (!strcasecmp(anentry->d_name+l-4,".png")) ||
#endif
         (!strcasecmp(anentry->d_name+l-4,".pbm")) ||
         (!strcasecmp(anentry->d_name+l-4,".pgm")) ||
         (!strcasecmp(anentry->d_name+l-4,".ppm")) || isdir)
        {
        entnum++;
        gifdir[entnum].isdir=isdir;
        strcpy(gifdir[entnum].name,anentry->d_name);
        gifdir[entnum].marked=0;
        }
    }
  }
closedir(dirfile);
gifdirsiz=entnum;
qsort(&(gifdir[1]),gifdirsiz,sizeof(struct GDIR),(void *)gcompare);
}


int gcompare(gn1,gn2)
void *gn1,*gn2;
{
struct GDIR *g1,*g2;
char buf1[80],buf2[80];

g1=(struct GDIR *)gn1; g2=(struct GDIR *)gn2;
prettyfile(buf1,g1); prettyfile(buf2,g2);
return(strcmp(buf1,buf2));
}


prettyfile(buf,gifdptr)
char *buf;
struct GDIR *gifdptr;
{
if(gifdptr->isdir)
  {
  buf[0]='(';
  strcpy(buf+1,gifdptr->name);
  strcat(buf,")");
  }
else
  strcpy(buf,gifdptr->name);
}


screenon()
{
int r,g,b,n;
unsigned char *tmp;

if(cfg.xvpic_index)
  {
  if(vga_hasmode(G640x480x256) && cfg.force16fs==0)
    {
    vga_setmode(G640x480x256); v256=1;
    
    /* construct 3:3:2 palette */
    for(r=n=0;r<8;r++)
      for(g=0;g<8;g++)
        for(b=0;b<4;b++,n++)
          vga_setpalette(n,r*63/7,g*63/7,b*63/3);
	  
    /* find approximations to file selector colours.
     * these are then blasted with the *real* file selector colours
     * unless 'perfectindex' is set.
     */
    idx_medium=MAKE332COL(cfg.medium_r,cfg.medium_g,cfg.medium_b);
    idx_dark  =MAKE332COL(cfg.dark_r  ,cfg.dark_g  ,cfg.dark_b  );
    idx_light =MAKE332COL(cfg.light_r ,cfg.light_g ,cfg.light_b );
    idx_black =MAKE332COL(cfg.black_r ,cfg.black_g ,cfg.black_b );
    idx_marked=MAKE332COL(cfg.marked_r,cfg.marked_g,cfg.marked_b);
    }
  else
    {
    /* if 16 colour mode... */
    vga_setmode(G640x480x16); v256=0;

    if(cfg.fs16col)
      /* colour */
      for(r=0;r<2;r++)
        for(g=0;g<2;g++)
          for(b=0;b<2;b++)
            vga_setpalette(r*4+g*2+b+1,r*63,g*63,b*63);
    else
      /* greyscale */
      for(b=0;b<=GREY_MAXVAL;b++)
        vga_setpalette(b,b*63/GREY_MAXVAL,b*63/GREY_MAXVAL,b*63/GREY_MAXVAL);

    idx_black=14; idx_marked=12; idx_dark=13; idx_medium=0; idx_light=15;
    }
  
  /* fix bar height */
  barheight=GDFSIZ*6+6+70;
  }
else
  {
  /* even for the old file selector we use an 8-bit mode if possible,
   * since it's much faster.
   */
  vga_setmode((vga_hasmode(G640x480x256) && cfg.force16fs==0)?
                G640x480x256:G640x480x16);
  cfg.xvpic_index=0;
  barheight=GDFSIZ*6+6;
  idx_medium=MIDGREY; idx_dark=DARK; idx_light=LIGHT; idx_black=BLACK;
  if(one_file_only && vga_hasmode(G640x480x256) && cfg.force16fs==0)
    idx_medium=255;	/* well, not 0 at any rate */
  }

if(!(cfg.xvpic_index && cfg.perfectindex))
  {
  vga_setpalette(idx_medium,cfg.medium_r,cfg.medium_g,cfg.medium_b);
  vga_setpalette(idx_dark  ,cfg.dark_r,cfg.dark_g,cfg.dark_b);
  vga_setpalette(idx_light ,cfg.light_r,cfg.light_g,cfg.light_b);
  vga_setpalette(idx_black ,cfg.black_r,cfg.black_g,cfg.black_b);
  vga_setpalette(idx_marked,cfg.marked_r,cfg.marked_g,cfg.marked_b);
  }

if(one_file_only) return(0);

if(cfg.xvpic_index || v256==0)
  {
  /* clear screen with 'medium' (i.e. background) colour. */
  tmp=malloc(640);
  if(tmp!=NULL)
    {
    memset(tmp,idx_medium,640);
    for(n=0;n<480;n++)
      vga_drawscanline(n,tmp);
    free(tmp);
    }
  }

draw3dbox(0,0,639,99,2,1,	idx_light,idx_dark);
draw3dbox(10,10,629,89,1,0,	idx_light,idx_dark);
draw3dbox(0,100,639,479,2,1,	idx_light,idx_dark);
draw3dbox(10,110,629,469,1,0,	idx_light,idx_dark);

drawzgvlogo(10,10);
return(0);
}


screenoff()
{
vga_setmode(TEXT);
cleartext();
}


drawzgvlogo(a,b)
int a,b;
{
int x,y,bw,c=0;
byte *ptr;

ptr=zgvlogo;
bw=logow>>3;
if((logow%8)>0) bw+=1;
vga_setcolor(idx_black);
for(y=0;y<logoh;y++)
  {
  ptr=zgvlogo+y*bw;
  for(x=0;x<logow;x++)
    {
    if((x%8)==0)
      c=*ptr;
    if((c&128)==0) vga_drawpixel(a+x,b+y);
    c<<=1;
    c&=0xff;
    if((x%8)==7)
      ptr++;
    }
  }
}


cleartext()
{
if(cfg.cleartext)
  fprintf(stderr,"\033[H\033[J");
}


/* this shows any error message from GIF reading.
 * Notice that JPEG errors have already been reported!
 */
showerrmessage(errnumber)
int errnumber;
{
char buf[256];

if(updating_index) return(0);	/* ignore if updating thumbnail index */

strcpy(buf,"Error reading file - ");
switch(errnumber)
  {
  case _PICERR_NOFILE:
    strcat(buf,"file not found"); break;
  case _PICERR_NOMEM:
    strcat(buf,"out of memory"); break;
  case _PICERR_BADMAGIC:
    strcat(buf,"not a supported format or bad extension"); break;
  case _PICERR_NOCOLOURMAP:
    strcat(buf,"no global colour map"); break;
  case _PICERR_NOIMAGE:
    strcat(buf,"no image block found"); break;
  case _PICERR_UNSUPPORTED:
    strcat(buf,"unsupported image sub-type"); break;
  case _PICERR_CORRUPT:
    strcat(buf,"corrupt or short file"); break;
  case _PICERR_ISRLE:
    strcat(buf,"can't handle RLE BMP files"); break;
  case _PICERR_SHOWN_ALREADY:
    /* this only happens with JPEGs */
    return(0);
#ifdef PNG_SUPPORT
  case _PICERR_PNG_ERR:		/* a PNG error in zgv_pngerr */
    strcpy(buf,zgv_pngerr); break;
#endif /* PNG_SUPPORT */
  default:
    strcat(buf,"unknown error (ulp!)");
  }
msgbox(zgv_ttyfd,buf,MSGBOXTYPE_OK, idx_light,idx_dark,idx_black);
return(0);
}


/* ok, a couple of people want move, copy, and (especially) delete,
 * and it sounds like a good idea, so here goes.
 *
 * [ move and copy aren't done yet :-( ]
 */

/* delete is the easiest.
 * we also delete any matching thumbnail file in .xvpics, and
 * attempt to remove the .xvpics directory also - relying on the
 * OS to do the Right Thing if other thumbnail files remain
 * (which it does, of course).
 */
delete_file(filename)
char *filename;
{
char buf[270];
int retn=0;

if(cfg.nodelprompt==0)
  {
  sprintf(buf,"Really delete %s?",filename);
  retn=msgbox(zgv_ttyfd,buf,MSGBOXTYPE_YESNO, idx_light,idx_dark,idx_black);
  }

if(retn==1 || cfg.nodelprompt)
  {
  if(remove(filename)==-1)
    msgbox(zgv_ttyfd,"Unable to delete file!",MSGBOXTYPE_OK,
    				idx_light,idx_dark,idx_black);
  else
    {
    sprintf(buf,".xvpics/%s",filename);
    remove(buf);		/* don't care if it fails */
    rmdir(".xvpics");	/* same here */
    }
  }

return(retn);
}


void xv332_how_far(sofar,total)
int sofar,total;
{
char tmp[128];
int done;

done=sofar*100/total;

vga_lockvc();
if(vga_oktowrite() && ((done%5)==0 || sofar==total))
  {
  clear_xvpic(xv332_how_far_xpos,xv332_how_far_ypos);

  if(sofar!=total)
    {
    vga_setcolor(idx_black);
    sprintf(tmp,"Reading - %2d%%",done);
    vgadrawtext(xv332_how_far_xpos+(BARWIDTH-70)/2,
                xv332_how_far_ypos+GDFSIZ*6+39-4,2,tmp);
    }
  }
vga_unlockvc();
}


clear_xvpic(xpos,ypos)
int xpos,ypos;
{
unsigned char tmp[96];
int y;

memset(tmp,idx_medium,96);
for(y=-3;y<63;y++)
  vga_drawscansegment(tmp,xpos+(BARWIDTH-80)/2-3,ypos+y+GDFSIZ*6+9,96);
}


/* if howfar equals 0, no progress report is done.
 * if it is >0, then the high 16 bits are 'xpos' and low 16 are 'ypos'.
 */
int makexv332(filename,xvpicfn,howfar)
char *filename,*xvpicfn;
unsigned int howfar;	/* not that signedness should really matter */
{
FILE *out;
int tmp;
int w,h,y;
unsigned char *smallpic;

pixelsize=1;		/* ouch */

if(howfar)
  {
  /* given the way the progress reporting from readpicture() works,
   * I have to use some global variables here. :( 
   */
  xv332_how_far_xpos=howfar>>16;
  xv332_how_far_ypos=howfar&0xFFFF;
  }

if((tmp=readpicture(filename,howfar?xv332_how_far:NULL,0))!=_PIC_OK)
  return(tmp);

/* image is pointed to by theimage, image_palette */

vga_lockvc();
if(vga_oktowrite())
  {
  clear_xvpic(xv332_how_far_xpos,xv332_how_far_ypos);
  vgadrawtext(xv332_how_far_xpos+(BARWIDTH-62)/2,
              xv332_how_far_ypos+GDFSIZ*6+39-4,2,"Resampling...");
  }
vga_unlockvc();

/* resize */
w=80; h=60;
smallpic=resizepic(theimage,image_palette+512,image_palette+256,image_palette,
		width,height,&w,&h);
free(theimage); free(image_palette);	/* finished with these */

vga_lockvc();
if(vga_oktowrite())
  {
  clear_xvpic(xv332_how_far_xpos,xv332_how_far_ypos);
  vga_setcolor(idx_black);
  vgadrawtext(xv332_how_far_xpos+(BARWIDTH-55)/2,
              xv332_how_far_ypos+GDFSIZ*6+39-4,2,"Dithering...");
  }
vga_unlockvc();

/* dither */
ditherinit(w);
for(y=0;y<h;y++)
  ditherline(smallpic+y*80*3,y,w);
ditherfinish();

/* write */
if((out=fopen(xvpicfn,"wb"))==NULL)
  return(_PICERR_NOFILE);		/* well, kind of */

fprintf(out,"P7 332\n");
fprintf(out,"# xv-compatible thumbnail file produced by zgv v%s\n",ZGV_VER);
fprintf(out,"#END_OF_COMMENTS\n");
fprintf(out,"%d %d 255\n",w,h);

for(y=0;y<h;y++)
  fwrite(smallpic+y*80*3,1,w,out);

fclose(out);
free(smallpic);

return(_PIC_OK);
}


/* update indexes (xvpics) of current directory.
 * we draw the stuff as we go along, and allow Esc to abort
 * the process. Checking for Esc happens between creating xvpics.
 * the directory '.xvpics' is created if needed.
 */
update_xvpics()
{
FILE *test;
int f;
int curent,oldstart,oldent,startfrom;
struct stat realpic,xvpic;
char buf[1024],*ptr;
int redraw_needed=0;


/* for each picture in the current directory, we check to see if
 * a file exists in the .xvpics directory with the same filename.
 * If not, it is created. If a file does exist, we check the
 * modification times. If the picture file is newer than the index
 * file, a new one gets created. Hope that's clear now. :)
 */

redrawall(1,1);
curent=startfrom=1;

vga_runinbackground(1);

for(f=1;f<=gifdirsiz;f++)
  {
  if(gifdir[f].isdir==0)
    {
    if(stat(gifdir[f].name,&realpic)==-1)
      continue;			/* the picture file doesn't exist */

    sprintf(buf,".xvpics/%s",gifdir[f].name);
    if(stat(buf,&xvpic)==-1 || realpic.st_mtime>xvpic.st_mtime)
      {
      /* this is pretty BFI and messy */
      
      if(mkdir(".xvpics",0777)!=-1 || errno==EEXIST || errno==EACCES)
        {
        /* check if we can write to the file */
        if((test=fopen(buf,"wb"))!=NULL)
          fclose(test);
        else
          goto usehomedir;
        }
      else	/* if couldn't create/use .xvpics ... */
        {
        usehomedir:
        /* couldn't create ./.xvpics, try ~/.xvpics */
        sprintf(buf,"%s/.xvpics",getenv("HOME")?getenv("HOME"):"");
        if(mkdir(buf,0777)==-1 && errno!=EEXIST && errno!=EACCES)
          {
          wait_for_foreground();
          msgbox(zgv_ttyfd,"Unable to create either .xvpics directory",
          		MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
          return(0);
          }
        
        /* also need to create ~/.xvpics/_foo_bar_baz */
        strcat(buf,"/");
        ptr=buf+strlen(buf);
        getcwd(ptr,sizeof(buf)-strlen(buf)-1); buf[strlen(buf)]=0;
        /* convert /foo/bar/baz to _foo_bar_baz */
        while((ptr=strchr(ptr,'/'))!=NULL) *ptr++='_';
        
        if(mkdir(buf,0777)==-1 && errno!=EEXIST && errno!=EACCES)
          {
          wait_for_foreground();
          msgbox(zgv_ttyfd,"Unable to create -/.xvpics/... directory",
          		MSGBOXTYPE_OK,idx_light,idx_dark,idx_black);
          return(0);
          }
        
        /* make sure filename for xvpic is in buf */
        strcat(buf,"/"); strcat(buf,gifdir[f].name);
        }
      
      makexv332(gifdir[f].name,buf,(fwinxpos(f-startfrom+1)<<16) |
      					fwinypos(f-startfrom+1));
      /* redraw without mode change */
      vga_lockvc();
      if(vga_oktowrite())
        {
        if(redraw_needed)	/* if a full redraw is needed... */
          screenon();
        else
          showbar(curent,0,startfrom);
        showgifdir(startfrom,1);
        showgifdir(startfrom,0);
        if(!redraw_needed)
          showbar(curent,1,startfrom);
        redraw_needed=0;
        }
      else	/* if we're not on current vc */
        redraw_needed=1;
      vga_unlockvc();
      }
    }
    
  /* move down one if possible */
  oldent=curent; oldstart=startfrom;
  if(curent<gifdirsiz) curent++;
  
  /* move right if needed */
  while(fwinxpos(curent-startfrom+1)+BARWIDTH>COLS)
    startfrom+=YSIZ;

  vga_lockvc();

  /* redraw */
  if(vga_oktowrite() && !redraw_needed)
    if(startfrom!=oldstart)
      {
      showbar(oldent,0,oldstart);
      showgifdir(oldstart,1);
      showgifdir(startfrom,0);
      showbar(curent,1,startfrom);
      }
    else  
      if(curent!=oldent)
        {
        showbar(oldent,0,startfrom);
        showbar(curent,1,startfrom);
        }

  vga_unlockvc();      

  /* check for Esc */
  if(readnbkey(zgv_ttyfd)==RK_ESC)
    {
    wait_for_foreground();
    return(0);
    }
  }

wait_for_foreground();
usleep(400000);	/* for effect :) */
return(0);
}



/* if we're not running directly on a console, try to find a free console
 * and move us to it. Notes old VT so we can switch back to it when finished,
 * if we ran on a different one to start off with.
 */
int fixvt()
{
struct stat sbuf;
struct vt_stat vts;
int major,minor;
int fd;
int num;
char vt_filename[20];

/* see if terminal is a console */
fd=dup(fileno(stderr));
fstat(fd,&sbuf);
major=sbuf.st_rdev>>8;
zgv_vt=minor=sbuf.st_rdev&0xff;
close(fd);
if(major==4 && minor<64)
  return(1);	/* if on a console, already ok */

/* otherwise we need to look for a free VT, redirect std{in,out,err},
 * and switch to it. If there's no free VTs, give up now.
 */

separate_vt=1;

/* still root perms, so this shouldn't be a problem... */
if((fd=open("/dev/console",O_RDONLY))<0) return(0);
ioctl(fd,VT_GETSTATE,&vts);
original_vt=vts.v_active;
ioctl(fd,VT_OPENQRY,&num);
if(num==-1) return(0);	/* no VTs free */

/* now, before we go for it, we test the *current* VT to see if RW access
 * is allowed. If so, the user's probably 'genuine'.
 */
sprintf(vt_filename,"/dev/tty%d",original_vt);
stat(vt_filename,&sbuf);
if(getuid()!=sbuf.st_uid)
  {
  fprintf(stderr,"You must be the owner of the current console to run zgv.\n");
  exit(1);
  }

zgv_vt=num;
sprintf(vt_filename,"/dev/tty%d",num);
setsid();
if(cfg.dontchangestdin==0)
  if(freopen(vt_filename,"r",stdin)==NULL)	return(0);
if(freopen(vt_filename,"w",stderr)==NULL)	return(0);
/* we keep stdout - doesn't need to be changed */

/* switch to the new VT */
ioctl(fd,VT_ACTIVATE,num);
close(fd);
/* ok, done it. */
return(1);
}


void switchback()
{
struct vt_mode vtm;

/* also change back to stdin blocking;
 * some versions of bash seem to be a little sensitive to it being
 * left non-blocking.
 */
fcntl(zgv_ttyfd,F_SETFL,0);

if(separate_vt)
  {
  fprintf(stderr,"%c[H%c[J",27,27);	/* seems to get junk-filled... */
  ioctl(zgv_ttyfd,VT_GETMODE,&vtm);
  vtm.mode=VT_AUTO;
  ioctl(zgv_ttyfd,VT_SETMODE,&vtm);
  ioctl(zgv_ttyfd,VT_ACTIVATE,original_vt);
  }
}


/* prints a message saying which the old VT was if reqd. */
oldvttell()
{
if(separate_vt)		/* only if needed to switch VTs */
  fprintf(stderr,"Old VT was %d - press %sAlt-F%d to return.\n",original_vt,
			(original_vt>12)?"Right":"",((original_vt-1)%12)+1);
}


greyfix332(image,w,h,xw7,w8)
unsigned char **image;
int w,h,xw7,w8;
{
unsigned char *new,*ptr;
int x,y,lx,c;
int c0,c1,c2,times2;
int terr0,terr1,terr2,actual0,actual1,actual2;
int start,addon,r,g,b;
int *evenerr,*odderr;
int *thiserr;
int *nexterr;

if((new=malloc(w8*h))==NULL)
  {
  free(*image);
  *image=NULL;
  return(0);
  }

memset(new,idx_medium,w8*h);

ptr=*image;

if(cfg.fs16col)
  {
  /* colour */
  /* uh-huh, case and paste mode on */
  
  if((evenerr=calloc(3*(w+10),sizeof(int)))==NULL ||
     (odderr =calloc(3*(w+10),sizeof(int)))==NULL)
    exit(1);
  
  for(y=0;y<h;y++)
    {
    ptr=*image+w*y;
    
    if((y&1)==0)
      {start=0; addon=1;
      thiserr=evenerr+3; nexterr=odderr+w*3;}
    else
      {start=w-1; addon=-1;
      thiserr=odderr+3; nexterr=evenerr+w*3;}

    nexterr[0]=nexterr[1]=nexterr[2]=0;
    x=start;
    for(lx=0;lx<w;lx++)
      {
      c=ptr[x];
      b=(c&3)*64; g=((c>>2)&7)*32; r=(c>>5)*32;
      r=(r-128)*3/2+128;
      g=(g-128)*3/2+128;
      b=(b-128)*3/2+128;

      terr0=r+((thiserr[0]+8)>>4);
      terr1=g+((thiserr[1]+8)>>4);
      terr2=b+((thiserr[2]+8)>>4);
    
      actual0=(terr0>=128)?255:0;
      actual1=(terr1>=128)?255:0;
      actual2=(terr2>=128)?255:0;
      
      if(actual0<0) actual0=0; if(actual0>255) actual0=255;
      if(actual1<0) actual1=0; if(actual1>255) actual1=255;
      if(actual2<0) actual2=0; if(actual2>255) actual2=255;
      
      c0=terr0-actual0;
      c1=terr1-actual1;
      c2=terr2-actual2;
    
      times2=(c0<<1);
      nexterr[-3] =c0; c0+=times2;
      nexterr[ 3]+=c0; c0+=times2;
      nexterr[ 0]+=c0; c0+=times2;
      thiserr[ 3]+=c0;
    
      times2=(c1<<1);
      nexterr[-2] =c1; c1+=times2;
      nexterr[ 4]+=c1; c1+=times2;
      nexterr[ 1]+=c1; c1+=times2;
      thiserr[ 4]+=c1;
    
      times2=(c2<<1);
      nexterr[-1] =c2; c2+=times2;
      nexterr[ 5]+=c2; c2+=times2;
      nexterr[ 2]+=c2; c2+=times2;
      thiserr[ 5]+=c2;
    
      new[y*w8+x+xw7]=(actual0&1)*4+(actual1&1)*2+(actual2&1)+1;
    
      thiserr+=3;
      nexterr-=3;
      x+=addon;
      }
    }
  
  free(evenerr);
  free(odderr);
  }
else
  /* greyscale */
  for(y=0;y<h;y++)
    for(x=0;x<w;x++,ptr++)
      {
      c=*ptr;
      c=((c&3)*2+((c>>2)&7)+(c>>5))*GREY_MAXVAL/21; /* greyscale 332 colour */
      if(c<0) c=0; if(c>GREY_MAXVAL) c=GREY_MAXVAL;
      new[y*w8+x+xw7]=c+1;
      }
    
free(*image);
*image=new;
return(0);
}


wait_for_foreground()
{
vga_runinbackground(0);
ioctl(zgv_ttyfd,VT_WAITACTIVE,zgv_vt);
}
