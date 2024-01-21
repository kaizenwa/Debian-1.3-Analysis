/*
 * mev.c - simple client to print mouse events (gpm-Linux)
 *
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

/*
 * This client is meant to be used both interactively to check
 * that gpm is working, and as a background process to convert gpm events
 * to textual strings. I'm using it to handle Linux mouse
 * events to emacs
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <termios.h> /* needed by curses.h (Hmmm...) */
#include <curses.h> /* to set raw mode */
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include <linux/keyboard.h> /* to use KG_SHIFT and so on */

#include "gpmCfg.h"  /* need the config one to get GPM_NAME */
#include "gpm.h"

char *prgname;
struct node {char *name; int flag;};
struct node  tableEv[]= {
  {"move",    GPM_MOVE},
  {"drag",    GPM_DRAG},
  {"down",    GPM_DOWN},
  {"up",      GPM_UP},
  {"press",   GPM_DOWN},
  {"release", GPM_UP},
  {"motion",  GPM_UP | GPM_DRAG},
  {"hard",    GPM_HARD},
  {"any",     ~0},
  {NULL,0}
};
struct node  tableMod[]= {
  {"shift",    1<<KG_SHIFT},
  {"anyAlt",   1<<KG_ALT | 1<<KG_ALTGR},
  {"leftAlt",  1<<KG_ALT},
  {"rightAlt", 1<<KG_ALTGR},
  {"control",  1<<KG_CTRL},
  {"any",      ~0},
  {NULL,0}
};


  /* provide defaults */
int opt_mask    = ~0;           /* get everything */
int opt_default = ~GPM_HARD;    /* pass everithing unused */
int opt_minMod  =  0;           /* run always */
int opt_maxMod  = ~0;           /* with any modifier */
int opt_intrct  =  0;
int opt_vc      =  0;      /* by default get the current vc */
int opt_silent  =  0;
int opt_emacs   =  0;
int opt_fit     =  0;
int opt_pointer =  0;


/*===================================================================*/
int user_handler(Gpm_Event *event, void *data)
{
if (opt_fit) Gpm_FitEvent(event);

printf("mouse: event 0x%02X, at %2i,%2i (delta %2i,%2i), "
	   "buttons %i, modifiers 0x%02X\r\n",
	 event->type,
	 event->x, event->y,
	 event->dx, event->dy,
	 event->buttons, event->modifiers);
fflush(stdout);

if (opt_pointer && (event->type & GPM_DRAG))
  Gpm_DrawPointer(event->x, event->y,opt_pointer);
return 0;
}

/*-------------------------------------------------------------------*/
int emacs_handler(Gpm_Event *event, void *data)
{
int i,j;
static int dragX, dragY;
static char buffer[64];
static char *s_mod[]={"S-","M-","C-","M-",NULL};
static char *s_type[]={"motion", "drag-mouse-","down-mouse-","mouse-",NULL};
static char *s_button[]={"3","2","1",NULL};
static char *s_multi[]={"double-","triple-",NULL};

  if (opt_fit) Gpm_FitEvent(event);
  buffer[0]=0;

  /* modifiers */
  for (i=0, j=1; s_mod[i]; i++, j<<=1)
    if (event->modifiers & j)
      strcpy(buffer,s_mod[i]);

  /* multiple */
  for (i=0, j=GPM_DOUBLE; s_multi[i]; i++, j<<=1)
    if (event->type & j)
      strcat(buffer,s_multi[i]);

  /* type */
  for (i=0, j=GPM_MOVE; s_type[i]; i++, j<<=1)
    if (event->type & j)
      strcat(buffer,s_type[i]);

  /* button */
  for (i=0, j=GPM_B_RIGHT; s_button[i]; i++, j<<=1)
    if (event->buttons & j)
      strcat(buffer,s_button[i]);
  
  if (event->type & GPM_DOWN) dragX=event->x, dragY=event->y;

  printf("(%s (%i . %i) (%i . %i))\n",
	 buffer,
	 event->x-1, event->y-1,
	 (event->type & (GPM_DRAG|GPM_UP)) ? dragX-1 : event->dx,
	 (event->type & (GPM_DRAG|GPM_UP)) ? dragY-1 : event->dy);
  fflush(stdout);

  if (opt_pointer && (event->type & GPM_DRAG))
    Gpm_DrawPointer(event->x, event->y,opt_pointer);

  return 0;
}

/*===================================================================*/
int usage(void)
{
 printf( "(" GPM_NAME ") " GPM_RELEASE ", " GPM_DATE "\n"
        "Usage: %s [options]\n",prgname);
 printf("  Valid options are\n"
 "    -C <number>   choose the virtual console (beware of it)\n"
 "    -d <number>   choose the default mask\n"
 "    -e <number>   choose the eventMask\n"
 "    -E            emacs-mode (implies -i, -s, -f, -p)\n"
 "    -i            interact-mode (use stdin to change connection info)\n"
 "    -f            fit drag events inside the screen\n"
 "    -m <number>   minimum modifier mask\n"
 "    -M <number>   maximum modifier mask\n"
 "    -p            shows the pointer while dragging\n"
 "    -s            silent - no messages\n"
 "    -u            user-mode (default)\n"
 );

  return 1;
}

/*===================================================================*/
#define PARSE_EVENTS 0
#define PARSE_MODIFIERS 1
int getmask(char *arg, int which)
{
int last=0, value=0;
char *cur;
struct node *table, *n;

  if (isdigit(arg[0])) return atoi(arg);

  table= which ? tableMod : tableEv;
  while (1)
    {
    while (*arg && !isalnum(*arg)) arg++; /* skip delimiters */
    cur=arg;
    while(isalnum(*cur)) cur++; /* scan the word */
    if (!*cur) last++;
    *cur=0;

    for (n=table;n->name;n++)
      if (!strcmp(n->name,arg))
	{
	value |= n->flag;
	break;
	}
    if (!n->name) fprintf(stderr,"%s: Incorrect flag \"%s\"\n",prgname,arg);
    if (last) break;

    cur++; arg=cur;
    }

  return value;
}
  
/*===================================================================*/
int cmdline(int argc, char **argv, char *options)
{
int opt;
  
  while ((opt = getopt(argc, argv, options)) != -1)
    {
    switch (opt)
      {
      case 'C':  sscanf(optarg,"%x",&opt_vc); break;
      case 'd':  opt_default=getmask(optarg, PARSE_EVENTS); break;
      case 'e':  opt_mask=getmask(optarg, PARSE_EVENTS); break;
      case 'E':  opt_emacs++; opt_intrct++;
                    opt_silent++; opt_pointer++; opt_fit++; break;
      case 'i':  opt_intrct++; break;
      case 'f':  opt_fit++; break;
      case 'm':  opt_minMod=getmask(optarg, PARSE_MODIFIERS); break;
      case 'M':  opt_maxMod=getmask(optarg, PARSE_MODIFIERS); break;
      case 'p':  opt_pointer++;
      case 's':  opt_silent++;
      case 'u':  opt_intrct=0; break;
      default:   return 1;
      }
    }
  return 0;
}
/*===================================================================*/
int interact(char *cmd) /* returns 0 on success and !=0 on error */
{
Gpm_Connect conn;
int argc=0;
char *argv[20];

  if (*cmd && cmd[strlen(cmd)-1]=='\n')
    cmd[strlen(cmd)-1]='\0';
  if (!*cmd) return 0;

/*
 * Interaction is accomplished by building an argv and passing it to
 * cmdline(), to use the same syntax used to invoke the program
 */

  while (argc<19)
    {
    while(isspace(*cmd)) cmd++;
    argv[argc++]=cmd;
    while (*cmd && isgraph(*cmd)) cmd++;
    if (!*cmd) break;
    *cmd=0;
    cmd++;
    }
  argv[argc]=NULL;

  if (!strcmp(argv[0],"pop"))
    return (Gpm_Close()==0 ? 1 : 0); /* a different convention on ret values */

  if (!strcmp(argv[0],"info"))
    fprintf(stderr,"The stack of connection info is %i depth\n",gpm_flag);

  if (!strcmp(argv[0],"quit"))
    exit(0);

  optind=0; /* scan the entire line */
  if (strcmp(argv[0],"push") || cmdline(argc,argv,"d:e:m:M:"))
    {
    fprintf(stderr,"Syntax error in input line\n");
    return 0;
    }

  conn.eventMask=opt_mask;
  conn.defaultMask=opt_default;
  conn.maxMod=opt_maxMod;
  conn.minMod=opt_minMod;

  if (Gpm_Open(&conn,opt_vc)==-1)
    {
    fprintf(stderr,"%s: Can't open mouse connection\n",argv[0]);
    return 1;
    }
  return 0;
}

void sigHandler(int signo)
{
/* just propagate to the father */
  kill(getppid(),SIGWINCH);
  signal(SIGWINCH,sigHandler);
}

/*===================================================================*/
int main(int argc, char **argv)
{
Gpm_Connect conn;
int c,d;
char s[8];
char cmd[128];
struct timeval tv;

  prgname=argv[0];
  gettimeofday(&tv,NULL);
  srand(tv.tv_usec);

  if (cmdline(argc,argv,"C:d:e:Efim:M:psu:"))
    exit(usage());

  if (opt_pointer)
    opt_pointer=open(ttyname(0),O_RDWR); /* can't be 0 */

  signal(SIGWINCH,sigHandler);

/*....................................... Init before printing */

  if (!opt_intrct && isatty(1))
    {
    /*
     * initscr() is required on SunOS, but jumps to the first line,
     * so it's better to clear it all (I don't like it, however)
     */
    initscr(); refresh();
    raw(); nonl();
    noecho();
    }


if (!opt_silent)
  {
  fprintf(stderr,"This program prints on stdout any event it"
	  "gets from stdin and from the mouse\r\n");
  fprintf(stderr,"Type <TAB> to get a snapshot of the state of the world\r\n");
  }


  conn.eventMask=opt_mask;
  conn.defaultMask=opt_default;
  conn.maxMod=opt_maxMod;
  conn.minMod=opt_minMod;

  if (Gpm_Open(&conn,opt_vc)==-1)
    fprintf(stderr,"%s: Can't open mouse connection\n",prgname);

  gpm_handler= opt_emacs ? emacs_handler : user_handler;

/*....................................... Interactive loop */

  if (opt_intrct)
    {
    while ((c=Gpm_Getc(stdin))!=EOF)
      {
      ungetc(c,stdin);                      /* stdin is line buffered, */
      if (interact(fgets(cmd,128,stdin)))  /* so this should work     */
        exit(0);
      }
    }
/*....................................... Raw loop */

  else
    {
	int modifiers; char modstring[32];
	printf("Well, the modifiers below are only valid for virtual consoles\r\n"
		   "\tbut I'm too lazy to check it\r\n");

    while ((c=Gpm_Getc(stdin))!=EOF)
      {
      if (isatty(0) && (c==4)) break; /* ctl-D to exit */
      strcpy(s, c&0x80 ? "M-" : "");
      c &= 0x7F; d=c;
      strcat(s, c<' ' ? "C-" : "");
      if (c<' ') d+=0x40;
      if (c==0x7F) d='?';

	  modifiers=6; /* code for the ioctl */
	  if (ioctl(0,TIOCLINUX,&modifiers)<0)
		modifiers=0;
	  modstring[0]=0;
	  if (modifiers&1) strcat(modstring,", shift");
	  if (modifiers&2) strcat(modstring,", right-alt");
	  if (modifiers&4) strcat(modstring,", control");
	  if (modifiers&8) strcat(modstring,", left-alt");
	  if (!modifiers)  strcpy(modstring,", none");

      printf("key-modifiers: %02X-%02X \t'%s%c'\t%s\r\n",
			 c,modifiers,s,d,modstring+2);

	  if (c=='\t' && !opt_silent)
		{
		Gpm_Event event;
		int i=Gpm_GetSnapshot(&event);
		char *s;

		printf("\tMouse has %d buttons\r\n",i);
		printf("\tCurrently sits at (%d,%d)\r\n",event.x,event.y);
		printf("\tThe window is %d columns by %d rows\r\n",event.dx,event.dy);
		s=Gpm_GetLibVersion(&i);
		printf("\tThe library is version \"%s\" (%i)\r\n",s,i);
		s=Gpm_GetServerVersion(&i);
		printf("\tThe daemon is version \"%s\" (%i)\r\n",s,i);
		printf("\tThe current console is %d, with modifiers 0x%02x\r\n",
			   event.vc,event.modifiers);
		printf("\tThe button mask is 0x%02X\r\n",event.buttons);
		
		switch ((rand()/4+rand()/4+rand()/4+rand()/4)/(RAND_MAX/4))
		  {
		  case 0: printf("\tThe sun is shining and all is well\r\n"); break;
		  case 1: printf("\tThe weather is so and so\r\n"); break;
		  case 2: printf("\tIt's raining, and you'd better stay home\r\n");
		         break;
		  case 3: printf("\tLinus Torvalds has a headache\r\n");
		  }
		}
	  fflush(stdout);
	  }


    if (isatty(1) && !opt_intrct)
      {
      noraw();
      echo();
      endwin();
      }
    }

/*....................................... Done */

  while (Gpm_Close()); /* close all the stack */

  exit(0);
}





