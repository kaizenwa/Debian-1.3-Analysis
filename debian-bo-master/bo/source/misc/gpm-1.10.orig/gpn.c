/*
 * gpn.c - support functions for gpm-Linux
 *
 * Copyright 1993        ajh@gec-mrc.co.uk (Andrew Haylett)
 * Copyright 1994,1995   rubini@ipvvis.unipv.it (Alessandro Rubini)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ********/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>        /* strerror(); ?!? memcpy() */
#include <ctype.h>         /* isdigit */
#include <signal.h>
#include <stdarg.h>        /* Log uses it */
#include <errno.h>
#include <unistd.h>        /* getopt(),symlink() */
#include <sys/stat.h>      /* mkdir()  */
#include <sys/param.h>
#include <sys/time.h>      /* timeval */
#include <sys/wait.h>      /* wait() */
#include <sys/types.h>     /* socket() */
#include <sys/socket.h>    /* socket() */
#include <sys/un.h>        /* struct sockaddr_un */
#include <linux/fs.h>      /* MAJOR */

#ifdef	SIGTSTP		/* true if BSD system */
#include <sys/file.h>
#include <sys/ioctl.h>
#endif

#include "gpmInt.h"
#include "gpm.h"

#include "kmouse.h"  /* kmouse ioctl */

#define mkfifo(path, mode) (mknod ((path), (mode) | S_IFIFO, 0))

extern int	errno;

static int check_uniqueness(int retain);

/*===================================================================*
 *     ==>    the logging utility
 *
 * This procedure is used to log info (mostly debugging ones)
 * the log file is quite large, actually, so be careful....
 *-------------------------------------------------------------------*/
#ifdef CONFIG_GPM_LOG      /* only if enabled */
int Log(char *fmt,...)
{
  va_list args;
  static time_t mytime;
  static char str_time[15];
  static FILE *f;
  static int pid;
  static char temp[80];

  if (!f)
    {
    pid=getpid();
    f=fopen(GPM_NODE_LOG,"a");
    if (!f) f=fopen("/dev/null","w");      /* not null, hopefully */
    setvbuf(f,NULL,_IONBF,0);
    }

  time(&mytime);
  strftime(str_time,15,"%H:%M:%S",localtime(&mytime));
  sprintf(temp,"%s - %i: %s\n",str_time,pid,fmt);

  va_start(args,fmt);           /* but the second argument is not used... */
  vfprintf(f,temp,args);
  return 1;
}
#endif


/*===================================================================*/
int _oops(char *f, int n, char *s, int err)
{
  LOG(("%s(%i): Exiting: \"%s: %s\"",f,n,s,strerror(err) ));
  PDEBUG((stderr,"oops() invoked from %s(%i)\n",f,n));
  fprintf(stderr,"%s: %s: %s\n",prgname,s,strerror(err));
  exit(1);
}

/*===================================================================*/
/* octal digit */
static int isodigit(const unsigned char c)
{
  return ((c & ~7) == '0');
}

/* routine to convert digits from octal notation (Andries Brouwer) */
static int getsym(const unsigned char *p0, unsigned char *res)
{
  const unsigned char *p = p0;
  char c;

  c = *p++;
  if (c == '\\' && *p) 
    {
    c = *p++;
    if (isodigit(c)) 
      {
      c -= '0';
      if (isodigit(*p)) c = 8*c + (*p++ - '0');
      if (isodigit(*p)) c = 8*c + (*p++ - '0');
      }
    }
  *res = c;
  return (p - p0);
}

static int loadlut(char *charset)
{
  int i, c, fd;
  unsigned char this, next;
  static __u32 long_array[9]={
    0x05050505, /* ugly, but preserves alignment */
    0x00000000, /* control chars     */
    0x00000000, /* digits            */
    0x00000000, /* uppercase and '_' */
    0x00000000, /* lowercase         */
    0x00000000, /* Latin-1 control   */
    0x00000000, /* Latin-1 misc      */
    0x00000000, /* Latin-1 uppercase */
    0x00000000  /* Latin-1 lowercase */
    };


#define inwordLut (long_array+1)

  for (i=0; charset[i]; )
    {
    i += getsym(charset+i, &this);
    if (charset[i] == '-' && charset[i + 1] != '\0')
      i += getsym(charset+i+1, &next) + 1;
    else
      next = this;
    for (c = this; c <= next; c++)
      inwordLut[c>>5] |= 1 << (c&0x1F);
    }
  
  if ((fd=open(consolename, O_WRONLY)) < 0)
    {
    /* try /dev/console, if /dev/tty0 failed */
    consolename="/dev/console";
    if ((fd=open(consolename, O_WRONLY)) < 0)
      oops(consolename);
    }
  if ((fd<0) || (ioctl(fd, TIOCLINUX, &long_array)<0))
    {
    if (errno==EPERM && getuid())
      fprintf(stderr,"%s: you should probably be root\n",prgname);
    else if (errno==EINVAL)
      fprintf(stderr,"%s: is your kernel compiled with CONFIG_SELECTION on?\n",
	      prgname);
    oops("loadlut");
    }
  close(fd);

  return 0;
}

/*===================================================================*
 * This function transfers "-a -B -p -i -l -p -r" to a running server,
 * or gets the options from another server.
 *-------------------------------------------------------------------*/
int xfer_options(int direction, int fd)
{
Gpm_Connect fake = {0,0,0,0,getpid(),1};
Gpm_Connect req  = {0,0,0,0, 0, GPM_REQ_CONFIG};
struct mouse_features *which_mouse=mouse_table+1; /* local */
char *tty;
struct {int a,B,d,i,p,r;} opts; /* -l already processed */

if (direction==0) /* out */
  {
  opts.a=opt_accel; sscanf(opt_sequence,"%d",&opts.B); opts.d=opt_delta;
  opts.i=opt_time; opts.p=opt_ptrdrag; opts.r=opt_scale;
  fd=check_uniqueness(1);
  if (fd<0) oops(GPM_NODE_CTL);
  tty=ttyname(0); /* stdin */
  if (!tty) oops("ttyname()");
  if (strncmp(tty,"/dev/tty",8) || !isdigit(tty[8]))
    {fprintf(stderr,"%s: %s: not a virtual console\n",prgname,tty);exit(1);}
  write(fd,&fake,sizeof(Gpm_Connect));
  write(fd,&req,sizeof(Gpm_Connect));
  write(fd,&opts,sizeof(opts));
  close(fd);
  return 0;
  }
if (direction==1) /* in */
  {
  read(fd,&opts,sizeof(opts));
  opt_accel=opts.a;
  sprintf(opt_sequence,"%08d",opts.B);
  opt_delta=opts.d;
  opt_time=opts.i; opt_ptrdrag=opts.p; opt_scale=opts.r;
  PDEBUG((stderr,"got %i,%i,%i,%i,%i,%i\n",opt_accel,opts.B,
	  opt_delta,opt_time,opt_ptrdrag,opt_scale));
  kill(getpid(),SIGWINCH); /* update delta's */
  return 0;
  }
return -1;
  

}
/*===================================================================*/
static int usage(char *whofailed)
{
 printf(GPM_NAME " " GPM_RELEASE ", " GPM_DATE "\n");
 if (whofailed)
   {
   printf("Error in the %s specification. Try \"%s -h\".\n",
	  whofailed,prgname);
   return 1;
   }

 printf("Usage: %s [options]\n",prgname);
 printf("  Valid options are (not all of them are implemented)\n"
 "    -a accel         sets the acceleration (default %d)\n"
 "    -b baud-rate     sets the baud rate (default %d)\n"
 "    -B sequence      allows changing the buttons (default '%s')\n"
 "    -d delta         sets the delta value (default %d)\n"
 "    -D               dirty operation (debug only)\n"
 "    -g tap-button    sets the button (1-3) that is emulated by tapping on\n"
 "                     a glidepoint mouse, none by default. (mman/ps2 only)\n"
 "    -i interval      max time interval for multiple clicks (default %i)\n"
 "    -k               kill a running gpm, to start X with a busmouse\n"
 "    -K               run in kernel mode, using the kmouse module\n"
 "    -l charset       loads the inword() LUT (default '%s')\n"
 "    -L charset       load LUT and exit\n"
 "    -m mouse-device  sets mouse device\n"
 "    -M               enable multiple mouse. Following options refer to\n"
 "                     the second device. Forces \"-R\"\n"
 "    -o modem-lines   toggle modem lines (\"dtr\", \"rts\", \"both\")\n"
 "    -p               draw the pointer while striking a selection\n"
 "    -q               quit after changing mouse behaviour\n"
 "    -r number        sets the responsiveness (default %i)\n"
 "    -R               enter repeater mode. X should read /dev/gpmdata\n"
 "                     like it was a MouseSystem mouse device\n"
 "    -s sample-rate   sets the sample rate (default %d)\n"
 "    -S [commands]    enable special commands (see man page)\n"
 "    -t mouse-type    sets mouse type (default '%s')\n"
 "                     Use an unexistent type (e.g. \"help\") to get a list\n"
 "    -T               test: read mouse, no clients\n"
 "    -v               print version and exit\n",
        DEF_ACCEL, DEF_BAUD, DEF_SEQUENCE, DEF_DELTA, DEF_TIME, DEF_LUT,
	DEF_SCALE, DEF_SAMPLE, DEF_TYPE);
  return 1;
}

/*===================================================================*
 * If "retain" is not 0, then a connection is open, and the fd is
 * returned (used by "-q"). Otherwise, a check is performed
 *-------------------------------------------------------------------*/
static int check_uniqueness(int retain)
  {
  struct sockaddr_un ctladdr;
  int ctlfd, pid, len; FILE *f;
  
  bzero((char *)&ctladdr,sizeof(ctladdr));
  ctladdr.sun_family=AF_UNIX;
  strcpy(ctladdr.sun_path, GPM_NODE_CTL);
  len=sizeof(ctladdr.sun_family)+strlen(GPM_NODE_CTL);

  if ( (ctlfd=socket(AF_UNIX,SOCK_STREAM,0))<0 )
	oops("socket()");

  if ( connect(ctlfd,(struct sockaddr *)(&ctladdr),len)>=0 || opt_kill)
	{
	if (retain) return ctlfd;
	/* another gpm is runnin, get its pid */
	f=fopen(GPM_NODE_PID,"r");
	if (f && fscanf(f,"%i",&pid)==1)
	  {
	  if (opt_kill) 
	    {
	    if (kill(pid,SIGUSR1)==-1) oops("kill()");
	    exit(0);
	    }
	  else
	    {
	    fprintf(stderr,"gpm-Linux is already running as pid %i\n",pid);
	    exit(1);
	    }
	  }
	else
	  {
	  fprintf(stderr,"%s: fatal error (should't happen)\n",prgname);
	  exit(1);
	  }
	}

  /* not connected, so I'm alone */
  if (retain) 
    { close(ctlfd); return -1; }

  if (opt_kill)
    {
    fprintf(stderr,"%s: Nobody to kill\n",prgname);
    unlink(GPM_NODE_PID); /* just in case */
    exit(0);
    }
return 0; /* never */
}


/*===================================================================*/
int cmdline(int argc, char **argv)
{
char options[]="a:b:B:d:Dg:hi:Kkl:L:m:Mo:pqr:Rs:S:t:Tv23";
int i, opt, fd, kfd;
struct kmouse_options koptions;
FILE *f;
static struct {char *in; char *out;} seq[] = {
  {"123","01234567"},
  {"132","02134657"},
  {"213","01452367"}, /* warning: these must be readable as integers... */
  {"231","02461357"},
  {"312","04152637"},
  {"321","04261537"},
  {NULL,NULL}
};

  /* initialize for the dual mouse */
  mouse_table[2]=mouse_table[1]=mouse_table[0]; /* copy defaults */
  which_mouse=mouse_table+1; /* use the first */

  while ((opt = getopt(argc, argv, options)) != -1)
    {
    switch (opt)
      {
      case 'a': opt_accel = atoi(optarg); break;
      case 'b': opt_baud = atoi(optarg); break;
      case 'B': opt_sequence = optarg; break;
      case 'd': opt_delta = atoi(optarg); break;
      case 'g':
          opt_glidepoint_tap=atoi(optarg); 
          break;
      case 'h': exit(usage(NULL));
      case 'i': opt_time=atoi(optarg); break;
      case 'k': opt_kill++; break;
      case 'K': opt_kernel++; break;
      case 'l': opt_lut=optarg; break;
      case 'L': opt_lut=optarg; opt_quit++; break;
      case 'm': opt_dev = optarg; break;
      case 'M': opt_double++; which_mouse=mouse_table+2; break;
      case 'o':
          if (!strcmp(optarg,"dtr"))       opt_toggle=TIOCM_DTR;
          else if (!strcmp(optarg,"rts"))  opt_toggle=TIOCM_RTS;
          else if (!strcmp(optarg,"both")) opt_toggle=TIOCM_DTR | TIOCM_RTS;
          else exit(usage("toggle lines"));
          break;
      case 'p': opt_ptrdrag=0; break;
      case 'q': opt_quit++; break;
      case 'r':
	  /* being called responsiveness, I must take the inverse */
	  opt_scale=atoi(optarg);
          if (!opt_scale) opt_scale=100; /* a maximum */
	  else opt_scale=100/opt_scale;
	  break;
      case 'R': opt_repeater++; break;
      case 's': opt_sample = atoi(optarg); break;
      case 'S':
          if (optarg) opt_special=optarg;
          else opt_special="";
          break;
      case 't': opt_type=optarg; opt_explicittype++; break;
      case 'T': opt_test++; break;
      case 'v': printf(GPM_NAME " " GPM_RELEASE ", " GPM_DATE "\n"); exit(0);
      case '2': opt_three=-1; break;
      case '3': opt_three=1; break;
      default:
          exit(usage("commandline"));
      }
    }

  if (!opt_quit) 
    {
    check_uniqueness(0);
    loadlut(opt_lut);
    }
  if (opt_repeater)
    {
    if (mkfifo(GPM_NODE_FIFO,0666) && errno!=EEXIST)
      oops(GPM_NODE_FIFO);
    if ((fifofd=open(GPM_NODE_FIFO, O_RDWR|O_NONBLOCK))<0)
      oops(GPM_NODE_FIFO);
    }


  /* duplicate initialization */

  for (i=1; i <= 1+opt_double; i++)
    {
    which_mouse=mouse_table+i; /* used to access options */

    if (opt_accel<2) exit(usage("acceleration"));
    if (opt_delta<2) exit(usage("delta"));
    if (strlen(opt_sequence)!=3 || atoi(opt_sequence)<100)
      exit(usage("sequence"));
    if (opt_glidepoint_tap>3)
      exit(usage("glidepoint tap button"));
    if (opt_glidepoint_tap)
      opt_glidepoint_tap=GPM_B_LEFT >> (opt_glidepoint_tap-1);

    /* choose the sequence */
    for (opt=0; seq[opt].in && strcmp(seq[opt].in,opt_sequence); opt++)
      ;
    if (!seq[opt].in) exit(usage("button sequence"));
    opt_sequence=strdup(seq[opt].out); /* I can rewrite on it */

    /* now we can put the options */
    if (opt_quit) { exit(xfer_options(0,0)); }


    /* look for the type */
    for (m_type=mice; m_type->fun; m_type++)
      if (!strcmp(opt_type,m_type->name)
	  || !strcasecmp(opt_type,m_type->syn))
	break;

    if (!(m_type->fun)) /* not found */
      exit(M_listTypes());

    /* open the device with ndelay, to catch a locked device */
    if (opt_dev)
      {
      if (!strcmp(opt_dev,"-"))
	fd=0;
      else if ((fd=open(opt_dev,O_RDWR | O_NDELAY))<0)
	oops(opt_dev); /* user failed */
      }
    else /* use "/dev/mouse" */
      {
      opt_dev = "/dev/mouse";
      if ((fd=open(opt_dev,O_RDWR | O_NDELAY))<0)
	oops("/dev/mouse");
      }

  /* and then reset the flag */
  fcntl(fd,F_SETFL,fcntl(fd,F_GETFL) & ~O_NDELAY);

    /* init the device, and change mouse type */
    if (m_type->init)
      m_type=(m_type->init)(fd, m_type->flags, m_type);

    if (opt_toggle)
      {
      unsigned int modem_lines;

      ioctl(fd, TIOCMGET, &modem_lines);
      modem_lines &= ~opt_toggle;
      ioctl(fd, TIOCMSET, &modem_lines);
      }

    which_mouse->fd=fd;
    }

#ifndef DEBUG /* avoid forking under gdb */

   /* go to background and become a session leader (Stefan Giessler) */
  switch(fork())
    {
    case -1: oops("fork()"); /* error */
    case  0: break;          /* child */
    default: exit(0);        /* parent */
    }

  if (!freopen("/dev/console","w",stderr)) /* the currently current console */
    {
    printf("gpm: freopen(stderr) failed\n"); exit(1);
    }
  if (setsid()<0) oops("setsid()");

#endif

  /* chdir */
  if (!opt_kernel) /* FIXME -- this is to find kmouse.o */
    {
    if (chdir(GPM_NODE_DIR) && mkdir(GPM_NODE_DIR,GPM_NODE_DIR_MODE))
      oops(GPM_NODE_DIR);
    if (chdir(GPM_NODE_DIR))
      oops(GPM_NODE_DIR);      /* well, I must create my directory first */
    }

  /* now sign */
  f=fopen(GPM_NODE_PID,"w");
  if (!f)
    {
    if (getuid()) fprintf(stderr,"%s: you're not root, can you write to %s?\n",
			  prgname,GPM_NODE_DIR);
    oops(GPM_NODE_PID);
    }
  fprintf(f,"%i\n",getpid());
  fclose(f);
  LOG(("Signed"));

  /*
   * well, I used to create a symlink in the /tmp dir to be compliant with old
   * executables. ===No more with 1.0
   *
   * symlink(GPM_NODE_CTL,"/tmp/gpmctl");
   */

  if (!opt_kernel)
    return mouse_table[1].fd; /* the second is handled in the main() */

  /*
   * Everything from here onwards is setting up for kmouse
   */

#if 0 /* don't actively call insmod, let kerneld do it */
  switch(fork()) /* insert the module */
    {
    case -1: 
      fprintf(stderr,"%s: fork(): %s\n",prgname,strerror(errno));
      exit(1);
    case 0:
      execl("/sbin/insmod","insmod","kmouse",NULL);
      execlp("insmod","insmod","kmouse",NULL);
      fprintf(stderr,"insmod: %s\n",strerror(errno));
      exit(1);
    default:
      wait(&i);
    }

  /* 
   * Unfortunately, insmod doesn't tell why loading fails.
   * If the module is already there, it's all right. So go on.
   */
  if (i)
    fprintf(stderr,"%s: Warning: insmod failed\n", prgname);
#endif

  if ( (kfd=open("/dev/gpmctl",O_RDONLY)) < 0 )
    {
    fprintf(stderr,"%s: /dev/gpmctl: %s\n", prgname, strerror(errno));
    exit(1);
    }
  if (ioctl(kfd,KMOUSE_GETVER,&i)<0)
    {
    fprintf(stderr,"%s: ioctl(/dev/gpmctl): %s\n", prgname, strerror(errno));
    exit(1);
    }
  if (KMOUSE_V_MAJOR(i) != KMOUSE_V_MAJOR(KMOUSE_VERSION_N))
    {
    fprintf(stderr,"%s: kmouse version %i.%i not supported: %i.xx needed\n",
	    prgname,
	    KMOUSE_V_MAJOR(i),KMOUSE_V_MINOR(i),
	    KMOUSE_V_MAJOR(KMOUSE_VERSION_N));
    exit(1);
    }
  for (i=1; i <= 1+opt_double; i++)
    {
    struct stat buffer;
    which_mouse=mouse_table+i; /* used to access options */
    
    if (stat(opt_dev,&buffer)<0)
      {
      fprintf(stderr,"%s: stat(%s): %s\n",prgname,opt_dev,
	      strerror(errno));
      exit(1);
      }
    koptions.protoid = opt_explicittype ? m_type->protoid : PROTO_UNKNOWN;
    strncpy(koptions.sequence,opt_sequence,8);
    koptions.dev=buffer.st_rdev;
    koptions.three=opt_three;
    koptions.delta=opt_delta;
    koptions.accel=opt_accel;
    koptions.scale=opt_scale;
    koptions.scaley=opt_scaley; /* FIXME */
    koptions.time=opt_time;
    koptions.tap=opt_glidepoint_tap;
    koptions.ptrdrag=opt_ptrdrag;
    if (ioctl(kfd,KMOUSE_SETOPT,&koptions)<0)
      {
      fprintf(stderr,"%s: ioctl(/dev/gpmctl): %s\n",prgname,strerror(errno));
      exit(1);
      }
    i=MAJOR(buffer.st_dev);     /* compare with another serial port */
    stat("/dev/ttyS0",&buffer);
    if (MAJOR(buffer.st_dev)==i)
      {
      i=N_MOUSE;
      if (ioctl(which_mouse->fd,TIOCSETD,&i)<0) 
	{
	fprintf(stderr,"%s: Warning: ioctl(TIOCSETD) failed for %s: %s\n",
		prgname,opt_dev,strerror(errno));
	}
      }
    }
  close(kfd);
  chdir("/");
  return -1; /* no fd to be used */
  
}
  

