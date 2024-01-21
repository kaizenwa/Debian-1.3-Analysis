/*
 *	xvboxled [ -help ] [ -fg color  ] [ -bg color ] [ -lo color] 
 *		 [ -path path where the incoming messages are ] 
 *               [ -update seconds ] [ -exec command string ] 
 *               [ -display host:dpy ] [ -geometry geom ] [ -all ] 
 *
 *	'emulates' the LED display of an answering machine 
 *      with vbox.0.0.4, needs X11 to run
 *      this software is free to use and comes with absolutley no
 *      warranty. 
 *      Copyright (c) 1996 Jan Schoenepauck & Joachim Gassen
 *
 *      For comments, bug reports, and requests please contact 
 *
 *       schoenep@wrcs3.urz.uni-wuppertal.de
 *       gassen@uni-muenster.de
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/xpm.h>
#include <string.h>
#include <dirent.h>
#include <linux/dirent.h>
#include <linux/unistd.h>
#include <linux/limits.h>
#include <sys/stat.h>
#include <unistd.h>
#include <getopt.h>
#include <setjmp.h>

#include "global.h"
#include "xinit.h"
#include "colors.h"
#include "windows.h"
#include "config.h"

#define MIN(a,b) (((a)<(b))?(a):(b))

char *x_display = NULL;
char *x_geom = NULL;
int winx=0, winy=0, winw=100, winh=100;
char *fg_color, *bg_color, *lo_color;
int check_period;
char *incoming_dir, *vbox_command;
int call_count, old_call_count = -1;
static jmp_buf prog;
int show_all_messages = 0;
static char *default_font_name = "-*-helvetica-medium-r-*-*-12-*";
Win mainwin;


/* This exit function must be called at the end of the program; it also */
/* serves as a signal handler for SIGTERM                               */
void close_all (int s) {
  disconnect_server();
  if (s<0) {
    printf("%s: abnormal program termination.\n",myname);
    exit(-1);
  }
  if (s==0) printf("%s: terminated by user.\n",myname);
  else printf("%s: exiting on signal %d.\n", myname, s);  
  exit(0);
}                  

/* this function is based on the fact that the x flag of the messages 
   is being toggled when the messages have been played by vbox. */ 

int check_new_messages (void) {
  struct dirent *dir_info;
  DIR *dir;
  int count=0;
  struct stat stat_buf; 
  char file_name[PATH_MAX+1];
  dir = opendir (incoming_dir);
  if (dir == NULL) return (-1);
  dir_info = readdir (dir);
  while (dir_info != NULL) {
    sprintf(file_name,"%s/%s",incoming_dir,dir_info->d_name);
    if ((dir_info->d_name[0]!='.')&&(stat (file_name,&stat_buf) >= 0)) { 
      if (show_all_messages) {
	if (stat_buf.st_mode&S_IREAD) count ++;
      }	else 
	if ((stat_buf.st_mode&S_IREAD)&&!(stat_buf.st_mode&S_IXUSR)) count ++;
    }
    dir_info = readdir (dir);
  }
  closedir(dir);
  return (count);
}


int led_lit (int pos, int val) {
  static int lookup[10]= {125,80,55,87,90,79,111,81,127,95};
  return ((1<<pos)&lookup[val]);
}
  

void draw_single_led (int start_x, int start_y, int x, int y, int w, int val) {
  int i;
  for (i=0;i<w;i++) {
    if (led_lit(0,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc, 
	       start_x+1+i, start_y+i, start_x+x-2-i, start_y+i);
    if (led_lit(1,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    if (!(i%2)) XDrawLine (display, mainwin.window, mainwin.gc,
			   start_x+2+(i/2),   start_y+y-(i/2)+1,
			   start_x+x-3-(i/2), start_y+y-(i/2)+1);
    else     XDrawLine (display, mainwin.window, mainwin.gc,
			start_x+2+(i/2)+1,       start_y+y+(i/2)+2,
			start_x+x-3-((i/2)+1), start_y+y+(i/2)+2);
    if (led_lit(2,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc,
	       start_x+1+i,   start_y+2*y-i+2, 
	       start_x+x-2-i, start_y+2*y-i+2);
    if (led_lit(3,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc, 
	       start_x+i, start_y+2+i, start_x+i, start_y+y-i);
    if (led_lit(4,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc,
	       start_x+x-i-1, start_y+2+i, start_x+x-i-1, start_y+y-i);
    if (led_lit(5,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc,
	       start_x+i, start_y+2+i+y, start_x+i, start_y+2*y-i);
    if (led_lit(6,val)) XSetForeground(display,mainwin.gc,led_on);
    else XSetForeground(display,mainwin.gc,led_off);
    XDrawLine (display, mainwin.window, mainwin.gc,
	       start_x+x-i-1, start_y+2+i+y, start_x+x-1-i, start_y+2*y-i);
  }
}


void draw_led (int val, int win_size_x, int win_size_y) {
  int x,y,w;
  int xt,yt;
  XSetForeground(display,mainwin.gc,window_bg);
  XFillRectangle(display,mainwin.window,mainwin.gc,0,0,win_size_x,win_size_y);
  x = win_size_x / 2.75;
  y = win_size_y / 2.5;
  w = MIN(x,y) /6;
  if (w<3) w=3;
  if (!(w%2)) w++;
  xt = x/4;
  yt = y/4;
  if (val>99) val = 99;
  if (val<0)  val = 0;
  draw_single_led (xt,yt,x,y,w,val/10);
  xt = 1.5 * x;
  draw_single_led (xt,yt,x,y,w,val%10);
}

void update_mainwindow (void) {
  old_call_count=call_count;
  draw_led(call_count,mainwin.w,mainwin.h);
}


void resize_mainwindow (XEvent *event) {
  int new_w, new_h;
  new_w = event->xconfigure.width;
  new_h = event->xconfigure.height;
  mainwin.w = new_w;
  mainwin.h = new_h;
}

/* This is called in the main loop: checks for queued events from the X */
/* server, and calls the appropriate handlers if events were received.  */
void check_events (void) {
  XEvent event;
  if (setjmp(prog) != 0) 
    if (call_count != old_call_count) update_mainwindow();
  XNextEvent(display, &event);
  switch (event.type) {
  case Expose :
    discard_exposures(mainwin.window);
    update_mainwindow();
    break;
  case ButtonPress : 
    system(vbox_command);
    break;
  case ConfigureNotify :
    resize_mainwindow(&event);
    break;
  default :
    break;
  }
}


static void usage () {
  static char *help_message[] = {
    "where options include:",
    "    -display host:dpy    X server to contact",
    "    -geometry geom       size of display",
    "    -bg color            background color",
    "    -fg color            foreground color",
    "    -all                 show all messages, not only the new ones",
    "    -update seconds      how often to check for new incoming messages",
    "    -path path           where to look for new messages",
    "    -exec                commandline to execute on button click",  
    "    -help                display this screen and exit",
    NULL
  };
  char **cpp;
  fprintf(stderr, "xvboxled v%s\n\n",VERSION);
  fprintf(stderr, "usage:  %s [-options ...]\n", myname);
  for (cpp = help_message; *cpp; cpp++) {
    fprintf (stderr, "%s\n", *cpp);
  }
  fprintf (stderr, "\n");
  exit (1);
}

void parse_command_line (int argc, char **argv) {
  struct command_options {
    char *command;
    int arg_count;
  } options[] = {
    {"-display",1},
    {"-geometry",1},
    {"-bg",1},
    {"-fg",1},
    {"-lo",1},
    {"-update",1 },
    {"-path",1 },
    {"-exec",1 },
    {"-help",0},
    {"-all",0},
    {NULL,0}
  };
  int i,j;
  /* First get my own program name and put it in <myname> */
  myname = argv[0];
  for (i=1;i<argc;i++) {
    j = 0;
    while (options[j].command) {
      if (strcmp(argv[i],options[j].command)==0) {
	if (options[j].arg_count) {
	  if (argc<i+1) usage();
	}
	switch (j) {
	case 0 : x_display = argv[++i];
	  break;
	case 1 : x_geom = argv[++i];
	  break;
	case 2 : bg_color = argv[++i];
	  break;
	case 3 : fg_color = argv[++i];
	  break;
	case 4 : lo_color = argv[++i];
	  break;
	case 5 : if ((check_period = atoi (argv [++i])) <= 0) usage ();
	  break;
	case 6 : incoming_dir = argv [++i];
	  break;
	case 7 : vbox_command = argv [++i];
	  break;  
	case 8 : usage ();
	  break;
	case 9 : show_all_messages = 1;
	  break;
	default : break;
	}
      }
      j++;
    }
  }
  if (incoming_dir==NULL) {
    incoming_dir = (char *) malloc (strlen (INCOMING_DIR)+1);
    strcpy (incoming_dir,INCOMING_DIR);
  }
  if (check_period==0) check_period = CHECK_PERIOD;
  if (fg_color==NULL) {
    fg_color = (char *) malloc (strlen (FG_COLOR) +1);
    strcpy (fg_color,FG_COLOR);
  }
  if (bg_color==NULL) {
    bg_color = (char *) malloc (strlen (BG_COLOR) +1);
    strcpy (bg_color,BG_COLOR);
  }
  if (lo_color==NULL) {
    lo_color = (char *) malloc (strlen (LO_COLOR) +1);
    strcpy (lo_color,LO_COLOR);
  }
  if (vbox_command==NULL) {
    vbox_command = (char *) malloc (strlen (VBOX_COMMAND)+1);
    strcpy (vbox_command, VBOX_COMMAND);
  }
}


/* This function is called when a SIGALRM signal is received. Checks for */
/* new messages in the incoming messages path and updates the counter    */
/* display if the number changed. The SIGALRM signal is raised in        */
/* regular intervals (check_period, in seconds) as set by the alarm()    */
/* function.                                                             */
void alarm_handler (int sig) {
  call_count = check_new_messages ();
  if (call_count < 0) close_all (-1);
  signal(SIGALRM,alarm_handler);
  alarm(check_period);
  longjmp (prog ,1);
}




int main(int argc, char **argv) {
  parse_command_line(argc,argv);
/* Basic initialization - try connecting the X server */
  connect_server(x_display);
/* set up the signal handlers */
  signal(SIGTERM,close_all);
  signal(SIGALRM,alarm_handler);

  init_cmap();
#ifdef NO_COLOR_ERRORS
  alloc_named_color(cmap,bg_color,&window_bg);
  alloc_named_color(cmap,fg_color,&led_on);
  alloc_named_color(cmap,lo_color,&led_off);
#else
  if (!alloc_named_color(cmap,bg_color,&window_bg)) {
    fprintf(stderr,"%s: couldn't allocate color %s\n",myname,bg_color);
    exit(-1);
  }
  if (!alloc_named_color(cmap,fg_color,&led_on)) {
    fprintf(stderr,"%s: couldn't allocate color %s\n",myname,fg_color);
    exit(-1);
  }
  if (!alloc_named_color(cmap,lo_color,&led_off)) {
    fprintf(stderr,"%s: couldn't allocate color %s\n",myname,lo_color);
    exit(-1);
  }
#endif

  if (x_geom) XParseGeometry(x_geom,&winx,&winy,&winw,&winh);
  if (!load_font(&default_font,default_font_name)) exit(-1);
  create_window(&mainwin,root,winx,winy,winw,winh,"xvboxled",
    window_bg,led_on,default_font);
  XSelectInput(display, mainwin.window, 
	       ExposureMask|ButtonPressMask|StructureNotifyMask);
  map_window(&mainwin,1);

  call_count = check_new_messages ();
  if (call_count < 0) close_all (-1);
  alarm(check_period);
  for (;;) {
    check_events();
  }
  close_all(0);
  return (0);
}
  

  
    























