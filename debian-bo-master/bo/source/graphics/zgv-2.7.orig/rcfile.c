/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * rcfile.c - config file handling.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>	/* getopt() */
#include <vga.h>
#include "rc_config.h"
#include "zgv.h"	/* for ZGV_VER */

#define boolvalue(x) (((!strncmp(x,"on",2)) || x[0]=='y')?1:0)

struct zgv_config cfg;


 

getconfig()
{
FILE *in;
char cfgfile[1024];
int gotrcfile;

defaultcfg();

gotrcfile=0;
strcpy(cfgfile,getenv("HOME"));
strcat(cfgfile,"/.zgvrc");

if((in=fopen(cfgfile,"r"))!=NULL)
  gotrcfile=1;
else if((in=fopen("/etc/zgv.conf","r"))!=NULL)
  gotrcfile=1;
else if((in=fopen("/etc/system.zgvrc","r"))!=NULL)
  gotrcfile=1;

if(gotrcfile)
  {
  parseconfig(in);
  fclose(in);
  }

if(cfg.jpegspeed<1) cfg.jpegspeed=1;
if(cfg.jpegspeed>3) cfg.jpegspeed=3;
}


parseconfig(in)
FILE *in;
{
char inpline[1024],*ptr;
int ln,f;

ln=0;
while(fgets(inpline,1024,in)!=NULL) 
  {
  ln++;
  if(inpline[strlen(inpline)-1]=='\n') inpline[strlen(inpline)-1]=0;
  if((ptr=strchr(inpline,'#'))!=NULL)
    *ptr=0;
  if(strlen(inpline)>0)
    {
    ptr=inpline;
    findtoken(&ptr);
    if(tokencompare(ptr,"zoom"))
      getbool(ptr,&cfg.zoom);
    else
    if(tokencompare(ptr,"revert"))
      getbool(ptr,&cfg.revert);
    else
    if(tokencompare(ptr,"gamma"))
      getbool(ptr,&cfg.gamma);
    else
    if(tokencompare(ptr,"force16fs"))
      getbool(ptr,&cfg.force16fs);
    else
    if(tokencompare(ptr,"fs16col"))
      getbool(ptr,&cfg.fs16col);
    else
    if(tokencompare(ptr,"blockcursor"))
      getbool(ptr,&cfg.blockcursor);
    else
    if(tokencompare(ptr,"thicktext"))
      getbool(ptr,&cfg.thicktext);
    else
    if(tokencompare(ptr,"hicolmodes"))
      getbool(ptr,&cfg.hicolmodes);
    else
    if(tokencompare(ptr,"nodelprompt"))
      getbool(ptr,&cfg.nodelprompt);
    else
    if(tokencompare(ptr,"onefile_progress"))
      getbool(ptr,&cfg.onefile_progress);
    else
    if(tokencompare(ptr,"cleartext"))
      getbool(ptr,&cfg.cleartext);
    else
    if(tokencompare(ptr,"perfectindex"))
      getbool(ptr,&cfg.perfectindex);
    else
    if(tokencompare(ptr,"visual"))
      getbool(ptr,&cfg.xvpic_index);
    else
    if(tokencompare(ptr,"betterpgm"))
      getbool(ptr,&cfg.betterpgm);
    else
    if(tokencompare(ptr,"centre") || tokencompare(ptr,"center"))
      getbool(ptr,&cfg.centreflag);
    else
    if(tokencompare(ptr,"jpeg24bit"))
      getbool(ptr,&cfg.jpeg24bit);
    else
    if(tokencompare(ptr,"mouse"))
      getmousearg(ptr,&cfg.mtype);
    else
    if(tokencompare(ptr,"jpegspeed"))
      getint(ptr,&cfg.jpegspeed);
    else
    if(tokencompare(ptr,"vkludge"))
      getbool(ptr,&cfg.vkludge);
    else
    if(tokencompare(ptr,"black"))
      getrgbval(ptr,&cfg.black_r,&cfg.black_g,&cfg.black_b);
    else
    if(tokencompare(ptr,"dark"))
      getrgbval(ptr,&cfg.dark_r,&cfg.dark_g,&cfg.dark_b);
    else
    if(tokencompare(ptr,"medium"))
      getrgbval(ptr,&cfg.medium_r,&cfg.medium_g,&cfg.medium_b);
    else
    if(tokencompare(ptr,"light"))
      getrgbval(ptr,&cfg.light_r,&cfg.light_g,&cfg.light_b);
    else
    if(tokencompare(ptr,"tagged"))
      getrgbval(ptr,&cfg.marked_r,&cfg.marked_g,&cfg.marked_b);
    else
    if(tokencompare(ptr,"contrast"))
      getdouble(ptr,&cfg.contrast);
    else
    if(tokencompare(ptr,"brightness"))
      getint(ptr,&cfg.brightness);
    else
    if(tokencompare(ptr,"tagtimeout"))
      getint(ptr,&cfg.tag_timer);
    else
    if(tokencompare(ptr,"allmodesbad"))
      for(f=0;f<256;f++) cfg.mode_allowed[f]=0;
    else
    if(tokencompare(ptr,"allmodesgood"))
      {
      /* we still disallow ones it hasn't got */
      for(f=0;f<31;f++)
        cfg.mode_allowed[f]=vga_hasmode(f)?1:0;
      }
    else
    if((tokencompare(ptr,"badmode"))||(tokencompare(ptr,"goodmode")))
      {
      if(getmodenumber(ptr,&f)==-1)
        {
        fprintf(stderr,"Mode not found on line #%d of rc file.\n",ln);
        exit(1);
        }
      cfg.mode_allowed[f]=tokencompare(ptr,"goodmode");
      }
    else
    if(tokencompare(ptr,"startmode"))
      {
      if(getmodenumber(ptr,&f)==-1)
        {
        fprintf(stderr,"Mode not found on line #%d of rc file.\n",ln);
        exit(1);
        }
      cfg.videomode=f;
      }
    else
      {
      fprintf(stderr,"Error in line #%d of rc file.\n",ln);
      exit(1);
      }
    }
  }
}


/* get mode number from something like '640 480 8' -
 * returns 0 if it can't find one that matches, else 1.
 * (the mode number is put into *mp)
 */
int getmodenumber(tptr,mp)
char *tptr;
int *mp;
{
int x,y,bpp,rtn;

/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
x=atoi(tptr);
tptr+=tokenlength(tptr);

findtoken(&tptr);
y=atoi(tptr);
tptr+=tokenlength(tptr);

findtoken(&tptr);
bpp=atoi(tptr);

rtn=modematch(x,y,bpp);
if(rtn>=0)
  {
  *mp=rtn;
  return(0);
  }
else
  return(-1);
}


getrgbval(tptr,rp,gp,bp)
char *tptr;
int *rp,*gp,*bp;
{
/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
*rp=atoi(tptr);
tptr+=tokenlength(tptr);

findtoken(&tptr);
*gp=atoi(tptr);
tptr+=tokenlength(tptr);

findtoken(&tptr);
*bp=atoi(tptr);
}


getbool(tptr,bp)
char *tptr;
int *bp;
{
/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
if((tokencompare(tptr,"on"))||(tokencompare(tptr,"y")))
  *bp=1;
else
  *bp=0;
}


getint(tptr,ip)
char *tptr;
int *ip;
{
/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
*ip=atoi(tptr);
}


getdouble(tptr,dp)
char *tptr;
double *dp;
{
/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
*dp=atof(tptr);
}


/* convert -M or 'mouse' arg to cfg.mtype value */
mouse_type strmousetype(arg)
char *arg;
{
if(strcmp(arg,"ms")  ==0) return(P_MS);
if(strcmp(arg,"sun") ==0) return(P_SUN);
if(strcmp(arg,"msc") ==0) return(P_MSC);
if(strcmp(arg,"mm")  ==0) return(P_MM);
if(strcmp(arg,"logi")==0) return(P_LOGI);
if(strcmp(arg,"bm")  ==0) return(P_BM);
if(strcmp(arg,"ps2") ==0) return(P_PS2);

return(NO_MOUSE);
}


getmousearg(tptr,mp)
char *tptr;
mouse_type *mp;
{
/* skip past the current token */
tptr+=tokenlength(tptr);
findtoken(&tptr);
*mp=strmousetype(tptr);
}


defaultcfg()
{
int f;

/* it'll use 360x480x256 if you haven't got 640x480x256,
 * and 320x200x256 if you've locked that out.
 */
cfg.videomode=G640x480x256;
cfg.zoom=0;
cfg.vkludge=0;
cfg.brightness=0;
cfg.jpeg24bit=1;	/* only if possible, of course */
cfg.jpegspeed=2;	/* slow int by default */
cfg.betterpgm=0;
cfg.centreflag=1;
cfg.blockcursor=0;
cfg.thicktext=0;
cfg.hicolmodes=0;	/* don't force high-colour mode test to true */
cfg.nodelprompt=0;	/* default to prompting on delete */
cfg.perfectindex=0;	/* make selector cols look nice, not index cols */
cfg.xvpic_index=1;	/* visual index */
cfg.onefile_progress=1;	/* progress report while loading file given as arg */
cfg.cleartext=0;	/* clear text screen on startup/exit? */
cfg.repeat_timer=0;	/* don't reread after a timeout */
cfg.tag_timer=4;	/* 4 seconds per tagged file */
cfg.force16fs=0;        /* don't force 16-colour mode for file selector */
cfg.revert=1;		/* auto-reset scaling to off between pics */
cfg.gamma=1;		/* try to fake more greys/colours in 8-bit modes */
cfg.selecting=0;	/* no selection normally */
cfg.dontchangestdin=0;	/* normally we do want to change if needed */
cfg.fs16col=0;		/* normally use mono in 16-col file selector */
cfg.contrast=(double)1;
cfg.black_r =cfg.black_g =cfg.black_b = 0;
cfg.dark_r  =cfg.dark_g  =cfg.dark_b  =20;
cfg.medium_r=cfg.medium_g=cfg.medium_b=30;
cfg.light_r =cfg.light_g =cfg.light_b =40;
cfg.marked_r=cfg.marked_g=cfg.marked_b= 0; cfg.marked_r+=30;
cfg.loop=0;
cfg.mtype=MTYPE;
for(f=0;f<256;f++) cfg.mode_allowed[f]=0;
for(f=0;f<31;f++)
  cfg.mode_allowed[f]=vga_hasmode(f)?1:0;
cfg.cmd[0]=0;
cfg.errignore=0;
}


findtoken(ptr)
char **ptr;
{
while((*(*ptr)!=0)&&(strchr(" \t",*(*ptr))!=NULL))
  (*ptr)++;
}


int tokenlength(ptr)
char *ptr;
{
int siz;

siz=0;
while((*ptr!=0)&&(strchr(" \t",*ptr)==NULL))
  {
  ptr++;
  siz++;
  }

return(siz);
}


/* returns 1 if equal, 0 otherwise */
int tokencompare(tptr,txt)
char *tptr,*txt;
{
int tlen;

tlen=tokenlength(tptr);
if(tlen!=strlen(txt))
  return(0);
else
  if(strncmp(tptr,txt,tlen))
    return(0);
  else
    return(1);
}


/* returns mode number which matches x,y,bpp or -1 if none did.
 * put '-1' in x,y or bpp to wildcard them.
 *
 * maybe this routine should be somewhere else?
 */
int modematch(x,y,bpp)
int x,y,bpp;
{
int numcols,f;
vga_modeinfo *vminfo;

if((bpp>24)||(bpp==2))
  /* they must have used numcols, not bpp. hmm, let 'em get away with it. */
  numcols=bpp;
else
  numcols=(1<<bpp);

/* we check 0 - 255 at the most */
for(f=0;f<256;f++)
  {
  vminfo=vga_getmodeinfo(f);
  if(vminfo!=NULL)
    if(((x==-1)||(x==vminfo->width))&&
       ((y==-1)||(y==vminfo->height))&&
       ((numcols==-1)||(numcols==vminfo->colors)))
      break;
  }

if(f<255)
  return(f);
else
  return(-1);
}


/* all booleans are actually toggles. parsing happens after rc file reading.
 */
int parsecommandline(argc,argv)
int argc;
char **argv;
{
int done;
char buf[1024];

done=0;
opterr=0;

do
  switch(getopt(argc,argv,"a:bcdghijJ:klM:m:r:sptz"))
    {
    case 'a':	/* alternative command */
      strcpy(cfg.cmd,optarg);
      if(strstr(cfg.cmd,"%s")==NULL)
        {
        fprintf(stderr,"Need '%%s' filename placeholder in command string.\n");
        exit(1);
        }
      break;
    case 'b':	/* blockcursor */
      cfg.blockcursor=!cfg.blockcursor; break;
    case 'c':	/* centre */
      cfg.centreflag=!cfg.centreflag; break;
    case 'd':	/* don't change stdin */
      cfg.dontchangestdin=1; break;
    case 'g':	/* betterpgm */
      cfg.betterpgm=!cfg.betterpgm; break;
    case 'h':	/* help on usage */
      usage_help();
      exit(1);
    case 'i':	/* errignore */
      cfg.errignore=!cfg.errignore; break;
    case 'j':	/* jpeg24bit */
      cfg.jpeg24bit=!cfg.jpeg24bit; break;
    case 'k':	/* vkludge */
      cfg.vkludge=!cfg.vkludge; break;
    case 'l':	/* loop in slideshow */
      cfg.loop=1; break;
    case 'M':    /* mouse type */
      if((cfg.mtype=strmousetype(optarg))==NO_MOUSE)
        {
        fprintf(stderr, "Invalid mouse type for -M.\n");
        usage_help();
        exit(1);
        }
      break;
    case 'm':	/* startup mode (takes arg) */
      {
      int vidnum;
      
      strcpy(buf,"dummytoken ");
      strcat(buf,optarg);
      if(getmodenumber(buf,&vidnum)==-1)
        {
        fprintf(stderr,"Mode '%s' not found.\n",optarg);
        exit(1);
        }
      cfg.videomode=vidnum;
      }
      break;
    case 'r':	/* repeat timer (takes arg) */
      cfg.repeat_timer=atoi(optarg); break;
    case 'J':	/* jpeg speed (takes arg) */
      cfg.jpegspeed=atoi(optarg);
      break;
    case 's':	/* selection - useful with pnmcut */
      cfg.selecting=1;
      break;
    case 'p':   /* onefile_progress */
      cfg.onefile_progress=!cfg.onefile_progress; break;
    case 't':	/* thicktext */
      cfg.thicktext=!cfg.thicktext; break;
    case 'z':	/* zoom */
      cfg.zoom=!cfg.zoom; break;
    case '?':
      switch(optopt)
        {
        case 'm':
          fprintf(stderr,"The -m (startup mode) option takes the mode as an ");
          fprintf(stderr,"argument,\n  e.g.  zgv -m \"640 480 8\"\n");
          break;
        case 'M':
          fprintf(stderr,"You must specify a mouse type with -M. Try ");
          fprintf(stderr,"'zgv -h' for details.\n");
          break;
        case 'a':
          fprintf(stderr,"The -a (alternative view) option takes the command");
          fprintf(stderr," to run as an argument,\n");
          fprintf(stderr,"  e.g.  zgv -a \"djpeg -grey %%s | pgmtopbm |");
          fprintf(stderr," pbmtoascii -2x4 > /dev/tty8\"\n");
          break;
        case 'r':
  	  fprintf(stderr,"The -r (repeat timer) option takes an alarm() ");
	  fprintf(stderr,"arg, i.e. the timeout\nin seconds, or -1 for");
          fprintf(stderr,"'continuous' update.\n");
          break;
        case 'J':
  	  fprintf(stderr,"The -J (jpeg speed) options needs a speed value. ");
	  fprintf(stderr,"Try 'zgv -h' for details.\n");
          break;
        default:
          fprintf(stderr,"Option '%c' not recognised.\n",optopt);
        }
      exit(1);
    case -1:
      done=1;
    }
while(!done);

return(argc-optind);
}


usage_help()
{
printf("Zgv v%s - (c) 1993-1995 Russell Marks for improbabledesigns.",ZGV_VER);

printf(
"
usage: zgv [-bcghjJlktz] [-a \"command\"] [-M mousetype] [-m modespec]
	[-r seconds] [dirname | filename1 [... filenameN]]

	-a	alternative command (see man page for details)
	-b	blockcursor toggle, gives outline cursor not tacky 3d effect
	-c	centre toggle, whether to centre pictures on the screen
	        (defaults to on, so using '-c' will turn it off)
	-g	betterpgm toggle, whether to use 8-bit or high-colour modes
	        for PGM files (defaults to on, so using '-g' will turn it off)
	-h	this usage help.
        -i	ignore errors with GIF and PNG files and display whatever
        	part of the picture could be read anyway, if possible.
	-j	jpeg24bit toggle (see documentation, esp. 24bit.txt - defaults
	        to on, so using '-j' will turn it off)
        -J	sets JPEG speed/quality tradeoff:
        		1 = floating-point - slow but accurate
                        2 = slow integer - faster but not as accurate
                        3 = fast integer - fastest but less accurate still
                              (default is 2)
	-k	vkludge toggle, smooths a bit in 320x400 and 360x480 modes,
	        and also when 'zooming' a big picture down to screen size
	-l	loop forever in slideshow mode
	-M	sets mouse type;
		Microsoft = `ms', Mouse Systems Corp = `msc',
		MM Series = `mm', Logitech = `logi', BusMouse = `bm',
		MSC 3-bytes = `sun', PS/2 = `ps2'.
	-m	startup mode; the 'modespec' should be in quotes but
	        otherwise in the same format as the .zgvrc entry 'startmode'
	        e.g. \"640 480 8\"
        -p      turn off (or on) progress report when loading a single file.
        -r	re-read and redisplay picture every 'seconds' seconds.
        -s	print dimensions of section of picture being viewed to stdout
        	on exit.
	-t	thicktext toggle, makes the text a little bit clearer, kind of
	-z	zoom toggle, i.e. whether to scale pictures to the screen size

      dirname			makes zgv start in a certain directory.
     filename   		makes zgv view that one file only.
filename1 ... filenameN		view the N files as a slideshow.
(i.e. more than one file)

All options are processed after any .zgvrc or /etc/zgv.conf file.
");	/* end of printf() */
}
