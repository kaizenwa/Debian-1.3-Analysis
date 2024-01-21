/*
 * top.c              - show top CPU processes
 *
 * Copyright (c) 1992 Branko Lankester
 * Copyright (c) 1992 Roger Binns
 *
 * Copyrigth (c) 1995-1997 Helmut Geyer
 *
 * Snarfed and HEAVILY modified for the YAPPS (yet another /proc ps)
 * by Michael K. Johnson, johnsonm@sunsite.unc.edu.  What is used is what
 * is required to have a common interface.
 *
 * Modified Michael K Johnson's ps to make it a top program.
 * Also borrowed elements of Roger Binns kmem based top program.
 * Changes made by Robert J. Nation (nation@rocket.sanders.lockheed.com)
 * 1/93
 *
 * Modified by Michael K. Johnson to be more efficient in cpu use
 * 2/21/93
 *
 * Changed top line to use uptime for the load average.  Also
 * added SIGTSTP handling.  J. Cowley, 19 Mar 1993.
 *
 * Modified quite a bit by Michael Shields (mjshield@nyx.cs.du.edu)
 * 1994/04/02.  Secure mode added.  "d" option added.  Argument parsing
 * improved.  Switched order of tick display to user, system, nice, idle,
 * because it makes more sense that way.  Style regularized (to K&R,
 * more or less).  Cleaned up much throughout.  Added cumulative mode.
 * Help screen improved.
 *
 * Fixed kill buglet brought to my attention by Rob Hooft.
 * Problem was mixing of stdio and read()/write().  Added
 * getnum() to solve problem.
 * 12/30/93 Michael K. Johnson
 *
 * Added toggling output of idle processes via 'i' key.
 * 3/29/94 Gregory K. Nickonov
 *
 * Fixed buglet where rawmode wasn't getting restored.
 * Added defaults for signal to send and nice value to use.
 * 5/4/94 Jon Tombs.
 *
 * Modified 1994/04/25 Michael Shields <mjshield@nyx.cs.du.edu>
 * Merged previous changes to 0.8 into 0.95.
 * Allowed the use of symbolic names (e.g., "HUP") for signal input.
 * Rewrote getnum() into getstr(), getint(), getsig(), etc.
 * 
 * Modified 1995  Helmut Geyer <Helmut.Geyer@iwr.uni-heidelberg.de> 
 * added kmem top functionality (configurable fields)
 * configurable order of process display
 * Added options for dis/enabling uptime, statistics, and memory info.
 * fixed minor bugs for ELF systems (e.g. SIZE, RSS fields)
 *
 * Modified 1996/05/18 Helmut Geyer <Helmut.Geyer@iwr.uni-heidelberg.de>
 * Use of new interface and general cleanup. The code should be far more
 * readable than before.
 *
 * Modified 1996/098/24 Helmut Geyer <Helmut.Geyer@iwr.uni-heidelberg.de>
 * moved display code into the procps library. All programs from the 
 * procps suite use these functions now, so we get colour for free.
 * Support all fields available for processes in the /proc filesystem.
 */
#define _LINUX_STRING_H

#include <errno.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>
#include <pwd.h>
#include <termcap.h>
#include <termios.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>
#include <setjmp.h>
#include <getopt.h>

#include "proc/sysinfo.h"
#include "proc/ps.h"
#include "proc/whattime.h"
#include "proc/signals.h"
#include "proc/version.h"
#include "proc/readproc.h"
#include "proc/showtask.h"
#include "proc/devname.h"

/*
 * these should be in the readproc.h header or in the ps.h header 
 */
typedef int (*cmp_t) (void *, void *);
extern void reset_sort_options (void);
extern int parse_sort_opt (char *opt);
extern void register_sort_function (int dir, cmp_t func);

#define PUTP(x) (tputs(x,1,putchar))

#include "top.h"		/*
				 * new header for top specific things 
				 */



#define TTEST(x) (topsetup.flags & TOP_ ## x)
#define TSET(x)  (topsetup.flags |= TOP_ ## x)
#define TCLEAR(x)  (topsetup.flags &= ~TOP_ ## x)
#define TTOGGLE(x) (topsetup.flags ^= TOP_ ## x)


#define DTEST(x) (topsetup.disp_opts.flags & DISP_ ## x)
#define DSET(x)  (topsetup.disp_opts.flags |= DISP_ ## x)
#define DCLEAR(x)  (topsetup.disp_opts.flags &= ~DISP_ ## x)
#define DTOGGLE(x) (topsetup.disp_opts.flags ^= DISP_ ## x)


/*
 * #######################################################################
 * ####  Startup routines: parse_options, get_options,      ##############
 * ####                    setup_terminal and main          ##############
 * ####################################################################### 
 */

/*
 * parse the options string as read from the config file(s).
 * if top is in secure mode, disallow changing of the delay time between
 * screen updates.
 */
void parse_options (char *Options, int secure)
{
    int i;
    for (i = 0; i < strlen (Options); i++) {
	switch (Options[i]) {
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    if (!secure)
		topsetup.Sleeptime = (float) Options[i] - '0';
	    break;
        case 'a':
            DTOGGLE(NEWTIMFORM);
            break;
	case 'S':
	    DSET(CUMULATIVE);
            top_header();
	    break;
	case 's':
	    TSET(SECURE);
	    break;
	case 'i':
	    TSET(NOIDLE);
	    break;
	case 'm':
	    TCLEAR(MEM);
	    topsetup.header_lines -= 2;
	    break;
	case 'M':
	    topsetup.sort_type = S_MEM;
	    reset_sort_options ();
	    register_sort_function (-1, (cmp_t) mem_sort);
	    break;
	case 'l':
	    TCLEAR(LOAD);
	    topsetup.header_lines -= 1;
	    break;
	case 'P':
	    topsetup.sort_type = S_PCPU;
	    reset_sort_options ();
	    register_sort_function (-1, (cmp_t) pcpu_sort);
	    break;
	case 't':
	    TCLEAR(STATS);
	    topsetup.header_lines -= 2;
	    break;
	case 'T':
	    topsetup.sort_type = S_TIME;
	    reset_sort_options ();
	    register_sort_function (-1, (cmp_t) time_sort);
	    break;
	case 'b':
	    TSET(BOLD);
	    break;
	case 'B':
	    TCLEAR(BOLD);
	    break;
	case 'e':
	    DSET(COLOR);
	    break;
	case 'E':
	    DCLEAR(COLOR);
	    break;
	case 'c':
	    DSET(CMD);
	    topsetup.proc_flags |= PROC_FILLCMD;
	    break;
	case 'C':
	    DCLEAR(CMD);
	    topsetup.proc_flags &= ~PROC_FILLCMD;
	    break;
        case 'r':
            topsetup.proc_flags |= PROC_REAL;
            break;
	case '\n':
	    break;
	default:
	    fprintf (stderr, "Wrong configuration option %c\n", Options[i]);
	    exit (1);
	    break;
	}
    }
}

/*
 * Read the configuration file(s). There are two files, once SYS_TOPRC 
 * which should only contain the secure switch and a sleeptime
 * value iff ordinary users are to use top in secure mode only.
 * 
 * The other file is $HOME/RCFILE. 
 * The configuration file should contain two lines (any of which may be
 *  empty). The first line specifies the fields that are to be displayed
 * in the order you want them to. Uppercase letters specify fields 
 * displayed by default, lowercase letters specify fields not shown by
 * default. The order of the letters in this line corresponds to the 
 * order of the displayed fileds.
 *
 * all Options but 'q' can be read from this config file
 * The delay time option syntax differs from the commandline syntax:
 *   only integer values between 2 and 9 seconds are recognized
 *   (this is for standard configuration, so I think this should do).
 *
 * usually this file is not edited by hand, but written from top using
 * the 'W' command. 
 */

void get_options (void)
{
    FILE *fp;
    char *pt;
    char rcfile[MAXNAMELEN];
    char Options[256] = "";

    topsetup.header_lines = 7;
    strcpy (rcfile, SYS_TOPRC);
    fp = fopen (rcfile, "r");
    if (fp != NULL) {
	fgets (Options, 254, fp);
	fclose (fp);
    }
    parse_options (Options, 0);
    strcpy (Options, "");
    if (getenv ("HOME")) {
	strcpy (rcfile, getenv ("HOME"));
	strcat (rcfile, "/");
    }
    strcat (rcfile, RCFILE);
    fp = fopen (rcfile, "r");
    if (fp == NULL) {
	strcpy (Fields, DEFAULT_SHOW);
    } else {
	if (fgets (Fields, 2046, fp) != NULL) {
	    pt = strstr (Fields, "\n");
	    if (pt != NULL) *pt = 0; /* This may happen with a broken .toprc */
	}
	fgets (Options, 254, fp);
	fclose (fp);
    }
    parse_options (Options, getuid ()? TTEST(SECURE) : 0);
}

/*
 * Set up the terminal attributes. 
 */
void setup_terminal (void)
{
    char *termtype;
    struct termios newtty;

    termtype = getenv ("TERM");
    if (!termtype) {
	/*
	 * In theory, $TERM should never not be set, but in practice, some
	 * gettys don't.  Fortunately, vt100 is nearly always correct (or
	 * pretty close). 
	 */
	termtype = "vt100";
	/*
	 * fprintf(stderr, PROGNAME ": $TERM not set\n"); 
	 *
         *
	 * exit(1); 
	 */
    }
    if (ioctl (0, TCGETS, &Savetty) == -1) {
	perror (PROGNAME ": ioctl() failed");
	error_end (errno);
    }
    newtty = Savetty;
    newtty.c_lflag &= ~ICANON;
    newtty.c_lflag &= ~ECHO;
    newtty.c_cc[VMIN] = 1;
    newtty.c_cc[VTIME] = 0;
    if (ioctl (0, TCSETSF, &newtty) == -1) {
	printf ("cannot put tty into raw mode\n");
	error_end (1);
    }
    ioctl (0, TCGETS, &Rawtty);

    /*
     * Get termcap entries and window size.
     */
    tgetent (NULL, termtype);
    cm = tgetstr ("cm", 0);
    top_clrtobot = tgetstr ("cd", 0);
    cl = tgetstr ("cl", 0);
    top_clrtoeol = tgetstr ("ce", 0);
    ho = tgetstr ("ho", 0);
    me = tgetstr ("me", 0);
    c_do = tgetstr ("do", 0);
    c_up = tgetstr ("up", 0);
    if (TTEST(BOLD)) {
        md = tgetstr ("md", 0);
        mr = tgetstr ("mr", 0);
    } else {
        mr=me;
	md=me;
    }
}

void usage() {

    fprintf(stderr,
            "usage: top -qsSxbei -d delay -t tty -[uU] user\n" );
}


int main (int argc, char **argv)
{
    /*
     * For select(2). 
     */
    struct timeval tv;
    fd_set in;
    /*
     * For parsing arguments. 
     */
    struct passwd *user_pw;
    char c;    /* The key read in. */
    char *p;           /* for environment variables */
    unsigned int i;
    if (atexit(clean_up)) {
        fprintf (stderr, "couldn´t register clean_up\n");
        exit(1);
    }
    get_options ();
    p=getenv("TOP_FORMAT");
    if (p!=NULL) strcpy(Fields,p);
    
    /*
     * Parse arguments.
     */
    while ((c = getopt (argc, argv, "ahd:u:U:t:qcbsrSievV")) != -1) {
	switch (c) {
        case 'a':
            topsetup.disp_opts.flags ^= DISP_NEWTIMFORM;
            break;
	case 'd':
	    if (sscanf (optarg, "%f", &topsetup.Sleeptime) != 1) {
		fprintf (stderr, PROGNAME ": Bad delay time `%s'\n", optarg);
		exit (1);
	    }
	    break;
	case 'q':
	    if (!getuid ())
		/*
		 * set priority to -10 in order to stay above kswapd  
		 */
		if (setpriority (PRIO_PROCESS, getpid (), -10)) {
		    /*
		     * We check this just for paranoia.  It's not fatal, and
		     * shouldn't happen.  
		     */
		    perror (PROGNAME ": setpriority() failed");
		}
	    topsetup.Sleeptime = 0;
	    break;
	case 'u':
	    if (sscanf (optarg, "%u", &i) != 1) {
	        if ((user_pw=getpwnam(optarg))==NULL) {
		    fprintf (stderr, PROGNAME ": Bad user id '%s'\n", optarg);
		    exit (1);
		} else {
		    uid[uindex]=user_pw->pw_uid;
		}
	    } else {
                uid[uindex] = (uid_t) i;
            }
	    if (nuindex || tindex) {
		fprintf (stderr, PROGNAME ": Use only one of -t, -u or -U!\n");
		exit (1);
	    }
	    uindex++;
	    topsetup.proc_flags |= PROC_UID;
	    if (uindex > 10) {
		fprintf (stderr, PROGNAME ": too many user ids\n");
		exit (1);
	    }
	    break;
	case 'U':
	    if (sscanf (optarg, "%u", &i) != 1) {
	        if ((user_pw=getpwnam(optarg))==NULL) {
		    fprintf (stderr, PROGNAME ": Bad user id '%s'\n", optarg);
		    exit (1);
		} else {
		    nuid[nuindex]=user_pw->pw_uid;
		}
	    } else {
                nuid[nuindex] = (uid_t) i;
            }
	    if (uindex || tindex) {
		fprintf (stderr, PROGNAME ": Use only one of -t, -u or -U!\n");
		exit (1);
	    }
	    nuindex++;
	    topsetup.proc_flags |= PROC_NOTUID;
	    if (nuindex > 10) {
		fprintf (stderr, PROGNAME ": too many User ids\n");
		exit (1);
	    }
	    break;
	case 't':
	    show_ttys[tindex]=tty_to_dev(optarg);
	    if (show_ttys[tindex] == (dev_t)-1) {
	        fprintf(stderr, "the name `%s' is not a tty\n", optarg);
		exit(1);
	    }
	    if (uindex || nuindex) {
		fprintf (stderr, PROGNAME ": Use only one of -t, -u or -U!\n");
		exit (1);
	    }	    
	    tindex++;
	    if (tindex>9) {
	        fprintf(stderr, "more than 10 ttys specified (%d)\n",tindex);
		exit(1);
	    }
	    topsetup.proc_flags = (topsetup.proc_flags | PROC_TTY) & ~(PROC_ANYTTY|PROC_STAT|PROC_UID|PROC_PID|PROC_NOTUID);
	    break;
        case 'r':
            topsetup.proc_flags ^= PROC_REAL;
            break;
	case 'c':
	    DTOGGLE(CMD);
	    if (DTEST(CMD))
                topsetup.proc_flags |= PROC_FILLCMD;
	    else
                topsetup.proc_flags &= ~PROC_FILLCMD;
	    break;
	case 'b':
            TTOGGLE(BOLD);
	    break;
	case 'e':
	    DTOGGLE(COLOR);
	case 'S':
	    DSET(CUMULATIVE);
	    break;
	case 'i':
	    TSET(NOIDLE);
	    break;
	case 's':
	    TSET(SECURE);
	    break;
        case 'h':
            usage();
            exit(0);
	case 'v':
	case 'V':
            display_version ();
            exit(0);
	case '?':
	    exit (1);
	default:
	    fprintf (stderr, PROGNAME ": getopt error -- this should never happen\n");
	    exit (1);
	}
    }

    /*
     * set to PCPU sorting 
     */
    register_sort_function (-1, (cmp_t) pcpu_sort);

    /*
     * for correct handling of some fields, we have to do distinguish  *
     * between kernel versions 
     */
    set_linux_version ();
    /*
     * get kernel symbol table, if needed 
     */
    if (DTEST(WCHAN)) {
	if (open_psdb ()) {
	    DCLEAR(WCHAN);
	} 
    }
    setup_terminal ();
    topsetup.disp_opts.Fieldstr=Fields;
    window_size ();
    setup_colors (&(topsetup.disp_opts), getenv("TOP_COLORS"));
    /*
     * calculate header size, length of cmdline field ...
     */
    top_header ();
    /*
     * Set up signal handlers.
     */
    signal (SIGHUP, (void *) (int) end);
    signal (SIGINT, (void *) (int) end);
    signal (SIGQUIT, (void *) (int) end);
    signal (SIGTSTP, (void *) (int) stop);
    signal (SIGWINCH, (void *) (int) window_size);

    /*
     * loop, collecting process info and sleeping 
     */
    while (1) {
	if (setjmp (redraw_jmp))
	    clear_screen ();

	/*
	 * display the tasks 
	 */
	show_procs ();
	/*
	 * sleep & wait for keyboard input 
	 */
	tv.tv_sec = topsetup.Sleeptime;
	tv.tv_usec = (topsetup.Sleeptime - (int) topsetup.Sleeptime) * 1000000;
	FD_ZERO (&in);
	FD_SET (0, &in);
	if (select (1, &in, 0, 0, &tv) > 0 && read (0, &c, 1) == 1)
	    do_key (c);
    }
}

/*
 * #######################################################################
 * #### Signal handled routines: error_end, end, stop, window_size     ###
 * #### Small utilities: make_header, getstr, getint, getfloat, getsig ###
 * ####################################################################### 
 */

void clean_up()
{
    if (DTEST(WCHAN))
	close_psdb ();
    ioctl (0, TCSETSF, &Savetty);
    PUTP (tgoto (cm, 0, topsetup.Lines - 1));
    fputs ("\r\n", stdout);
    PUTP (me);
}

	/*
	 *  end when exiting with an error.
	 */
void error_end (int rno)
{
    exit (rno);
}
/*
 *  Normal end of execution. 
 */
void end (void)
{
    exit (0);
}

/*
 * SIGTSTP catcher. 
 */
void stop (void)
{
    /*
     * Reset terminal. 
     */
    ioctl (0, TCSETSF, &Savetty);
    PUTP (tgoto (cm, 0, topsetup.Lines - 3));
    fflush (stdout);
    raise (SIGTSTP);
    /*
     * Later... 
     */
    ioctl (0, TCSETSF, &Rawtty);
    signal (SIGTSTP, (void *) (int) stop);
    longjmp (redraw_jmp, 1);
}

/*
 * Reads the window size and clears the window.  This is called on setup, 
 * and also catches SIGWINCHs, and adjusts the header line.  Basically, 
 * this is the central place for window size stuff.
 */
void window_size (void)
{
    struct winsize ws;

    if (ioctl (1, TIOCGWINSZ, &ws) != -1) {
	topsetup.disp_opts.Cols = ws.ws_col;
	topsetup.Lines = ws.ws_row;
    } else {
	topsetup.disp_opts.Cols = tgetnum ("co");
	topsetup.Lines = tgetnum ("li");
    }
    top_header ();
    clear_screen ();
}
/*
 * this adjusts the lines needed for the header to the current value 
 * and corrects Header if needed.
 */
void top_header (void)
{
    topsetup.proc_flags|=make_header(&(topsetup.disp_opts));
    Maxlines = Display_procs ? Display_procs : topsetup.Lines - topsetup.header_lines;
    if (Maxlines > topsetup.Lines - topsetup.header_lines)
	Maxlines = topsetup.Lines - topsetup.header_lines;
}



/*
 * Get a string from the user; the base of getint(), et al.  This really
 * ought to handle long input lines and errors better.  NB: The pointer 
 * returned is a statically allocated buffer, so don't expect it to persist 
 * between calls. 
 */
char *getstr (void)
{
    static char line[BUFSIZ];	/*
				 * BUFSIZ from <stdio.h>; arbitrary 
				 */
    int i = 0;

    /*
     * Must make sure that buffered IO doesn't kill us. 
     */
    fflush (stdout);
    fflush (stdin);		/*
				 * Not POSIX but ok 
				 */

    do {
	read (STDIN_FILENO, &line[i], 1);
    } while (line[i++] != '\n' && i < sizeof (line));
    line[--i] = 0;

    return (line);
}


/*
 * Get an integer from the user.  Display an error message and return -1 *
 * if it's invalid; else return the number. 
 */
int getint (void)
{
    char *line;
    int i;
    int r;

    line = getstr ();

    for (i = 0; line[i]; i++) {
	if (!isdigit (line[i]) && line[i] != '-') {
	    SHOWMESSAGE (("That's not a number!"));
	    return (-1);
	}
    }

    /*
     * An empty line is a legal error (hah!). 
     */
    if (!line[0])
	return (-1);

    sscanf (line, "%d", &r);
    return (r);
}


/*
 * Get a float from the user.  Just like getint(). 
 */
float getfloat (void)
{
    char *line;
    int i;
    float r;

    line = getstr ();

    for (i = 0; line[i]; i++) {
	if (!isdigit (line[i]) && line[i] != '.' && line[i] != '-') {
	    SHOWMESSAGE (("That's not a float!"));
	    return (-1);
	}
    }

    /*
     * An empty line is a legal error (hah!). 
     */
    if (!line[0])
	return (-1);

    sscanf (line, "%f", &r);
    return (r);
}


/*
 * * Get a signal number or name from the user.  Return the number, or -1 *
 * on error. 
 */
int getsig (void)
{
    char *line;

    /*
     * This is easy. 
     */
    line = getstr ();
    return (get_signal2 (line));
}

/*
 * #######################################################################
 * ####  Routine for sorting on used time, resident memory and %CPU  #####
 * ####  It would be easy to include full sorting capability as in   #####
 * ####  ps, but I think there is no real use for something that     #####
 * ####  complicated. Using register_sort_function or parse_sort_opt #####
 * ####  you just have to do the natural thing and it will work.     #####
 * ####################################################################### 
 */

int time_sort (proc_t ** P, proc_t ** Q)
{
    if (DTEST(CUMULATIVE)) {
	if (((*P)->cutime + (*P)->cstime + (*P)->utime + (*P)->stime) <
	    ((*Q)->cutime + (*Q)->cstime + (*Q)->utime + (*Q)->stime))
	    return -1;
	if (((*P)->cutime + (*P)->cstime + (*P)->utime + (*P)->stime) >
	    ((*Q)->cutime + (*Q)->cstime + (*Q)->utime + (*Q)->stime))
	    return 1;
    } else {
	if (((*P)->utime + (*P)->stime) < ((*Q)->utime + (*Q)->stime))
	    return -1;
	if (((*P)->utime + (*P)->stime) > ((*Q)->utime + (*Q)->stime))
	    return 1;
    }
    return 0;
}

int pcpu_sort (proc_t ** P, proc_t ** Q)
{
    if ((*P)->pcpu < (*Q)->pcpu)
	return -1;
    if ((*P)->pcpu > (*Q)->pcpu)
	return 1;
    return 0;
}

int mem_sort (proc_t ** P, proc_t ** Q)
{
    if ((*P)->resident < (*Q)->resident)
	return -1;
    if ((*P)->resident > (*Q)->resident)
	return 1;
    return 0;
}

/*
 * #######################################################################
 * ####  Routine handling the field selection/ordering screens:   ########
 * ####                change_fields                              ########
 * ####################################################################### 
 */
/*
 * Display the specification line of all fields. There are too many fields
 * to display on one screen for most screen sizes. Every field is displayed
 * with the name as described in ps_fields.7 and is assigned a letter. 
 * Pressing the letter will select a field. <return> will toggle displaying
 * the field, '=' will ask for a display length for the field. '<' will move
 * the field to the left, '>' will move it to the right. An added field will
 * be added at the very right.
 * 
 * There are three columns of fields for up to 52 fields on a single screen,
 * and <space> will go to the next page of fields.
 * <CTRL-X> will end this screen.
 */
void change_fields (dopts *dp)
{
    int n, i, j, k, l, row, col, nr, N, l_mod, columns, hl, notyet, sel;
    char *p ,*q, buff[8], c, used=' ';
    char tmp1[300], tmp2[300];
    
    columns=dp->Cols / 24; /* need approx. 21 chars for a complete field */
    if (columns==0) {
        clear_screen ();
        printf ("Screen size too small, press a key to return");
        fflush (stdout);
	ioctl (0, TCSETSF, &Rawtty);
	read (0, &c, 1);
	ioctl (0, TCSETSF, &Savetty);
        return;
    }
    
    n = sizeof headers / sizeof headers[0];
    j = 0;
    sel = -1;
    while (1) {
        clear_screen ();
        PUTP (tgoto (cm, 0, 0));
        notyet=1;
        hl=0;
        strcpy(tmp1, "Fields to display (ordered): ");
        strcat (tmp1, Fields);   
        while (notyet) {
            if (strlen(tmp1) > dp->Cols) {
                p=&(tmp1[dp->Cols-1]);
                while (*p !=',' && p > tmp1) p--;
                if(p > tmp1) {
                    *p='\0';
                    printf("%s,\n",tmp1);
                    hl++;
                    p++;
                    strcpy(tmp2, p);
                } else {
                    printf("malformed Field string \n");
                    return;
                }
                strcpy (tmp1, tmp2);
            } else {
                printf("%s\n",tmp1);
                notyet=0;
                hl++;
            }
        }
            
        N=(topsetup.Lines-hl-3)*columns -1;
        if (N>52) {
            N=52;              /* N fields on a single page, max. 52 */
            l_mod=52/columns;
            if (52%columns != 0) l_mod++;
        } else {
            l_mod=topsetup.Lines-hl-3;
        }

        for (i = 0; i < (N < (n - j*N) ? N : (n - j*N)) ; i++) {
            nr=j*N+i;
            used=' ';
            row = i % l_mod + hl+1;
            col = i / l_mod * 25;
            PUTP (tgoto (cm, col, row));
            for (k=0; k < dp->Numfields ; k++) 
                if (dp->p[k] == nr) used = '*';
            if (nr == sel) PUTP(mr);
            printf ("%c %c: %-11s=%d", used, i<26 ? i + 'A' : i -26 + 'a',
                    tags[nr], headers[nr].ln);
            if (nr == sel) PUTP(me);
        }
	PUTP (tgoto (cm, 0, topsetup.Lines-2));
        if (j == 0 )
            printf ("press <+> for the next page, ^x to return.\n");
        else if (j == n/52)
            printf ("press <-> for the previous page, ^x to return.\n");
        else 
            printf ("press <+> for the next page, <-> for the previous, ^x to return.\n");
        printf("[a-zA-Z] to select, <enter> toggles, =  field length, [<>] move");
        fflush (stdout);
        memset(buff, 0, 8);
	ioctl (0, TCSETSF, &Rawtty);
	read (0, buff, 7);
	ioctl (0, TCSETSF, &Savetty);
        if (buff[0]=='\e' && buff[1]=='[') {
            c=buff[2];
            if (c=='A') {
                c=' ';
                if (sel>0) { 
                    sel--;
                    if (sel< j*N) j--;
                }
            }
            if (c=='B') {
                c=' ';
                if (sel >=0 && sel < n-1) {
                    sel++;
                    if (sel >= (j+1)*N) j++;
                }
            } 
            if (c=='C') c='>';
            if (c=='D') c='<';
        } else  c=buff[0];                
        switch (c) {
        case '+': if ((j+1)*N <n) j++; sel=-1;                         break;
        case '-': if (j > 0)      j--; sel=-1;                         break;
        case 24:  return;                                              break;
        case '=': if (sel <0) break;
            clear_screen ();
            PUTP(tgoto(cm,1,1));
            notyet = 1;
            while (notyet) {
                printf("Valid field lengths are integers > 2 or -1 for stretchable.\n");
                printf("Don't enter -1 unless you know what you're doing\n\n");
                printf("enter new Field length for %s: ", tags[sel]);
                p=getstr();
                l = atoi(p);
                if (l == -1 || l > 2 ) {
                    for (k=0; k < dp->Numfields ; k++) if (dp->p[k] == sel) break; 
                    p=strchr(Fields,',');
                    if (k == 0) {
                        tmp2[0]='\0';
                    } else {
                        for (i=1; i<k;i++){
                            p=strchr(++p,',');
                        }
                        *p = '\0';
                        strcpy(tmp2,Fields);
                        strcat(tmp2,",");
                        p=strchr(++p,',');
                    }
                    sprintf(tmp1,"%s=%d",tags[sel],l);
                    strcat(tmp2, tmp1);
                    if (p!=NULL) strcat(tmp2,p);
                    strcpy (Fields,tmp2);
                    notyet=0;
                } else {
                    clear_screen ();
                    PUTP(tgoto(cm,1,1));
                    printf(" invalid field length\n");
                }
            }
            topsetup.proc_flags|=make_header(dp);
            break;
        case '<': 
            if (sel < 0 ) break; /* nothing to do */
            for (k=0; k < dp->Numfields ; k++) if (dp->p[k] == sel) break; 
            if (k==0 || k==dp->Numfields) break;
            p = strchr(Fields, ',');
            if (k>1) {
                for (i=2; i<k;i++) p=strchr(++p,',');
                *p='\0';
                strcpy (tmp2, Fields);
                strcat (tmp2, ",");
                p++;
                q=strchr(p,',');
            } else {
                tmp2[0]='\0';
                p=Fields;
                q=strchr(p,',');
            }
            if (q!=NULL) {
                *q='\0';
                strcpy (tmp1, p);
                q++;
                p=strchr(q,',');
            } else break;
            if (p!=NULL) {
                *p='\0';
                strcat (tmp2, q);
                strcat (tmp2, ",");
                strcat (tmp2, tmp1);
                strcat (tmp2, ",");
                p++;
                strcat (tmp2, p);
            } else {
                strcat (tmp2, q);
                strcat (tmp2, ",");
                strcat (tmp2, tmp1);
            }
            strcpy (Fields, tmp2);
            topsetup.proc_flags|=make_header (dp);
            break;
        case '>':
            if (sel < 0 ) break;
            for (k=0; k < dp->Numfields ; k++) if (dp->p[k] == sel) break; 
            if (k>=dp->Numfields-1) break;
            p=strchr(Fields,',');
            if (k>0) {
                for (i=1; i<k;i++)  p=strchr(++p,',');
                *p='\0';
                strcpy (tmp2, Fields);
                strcat (tmp2, ",");
                p++;
                q=strchr(p,',');
            } else {
                tmp2[0]='\0';
                p=Fields;
                q=strchr(p,',');
            }
            if (q!=NULL) {
                *q='\0';
                strcpy (tmp1, p);
                q++;
                p=strchr(q,',');
            } else break;
            if (p!=NULL) {
                *p='\0';
                strcat (tmp2, q);
                strcat (tmp2, ",");
                strcat (tmp2, tmp1);
                strcat (tmp2, ",");
                p++;
                strcat (tmp2, p);
            } else {
                strcat (tmp2, q);
                strcat (tmp2, ",");
                strcat (tmp2, tmp1);
            }
            strcpy (Fields, tmp2);
            topsetup.proc_flags|=make_header (dp);
            break;
        case  10:  /* toggle a field */ 
            if (sel < 0) break;  /* no selection */
            for (k=0; k < dp->Numfields ; k++) if (dp->p[k] == sel) break; 
            if (k < dp->Numfields) {                /* remove field */
                p = strchr(Fields,',');
                if (k == 0) {                       /* initial field */
                    p++;
                    strcpy (tmp2, p);
                } else {
                    for (i=1; i<k;i++){
                        p=strchr(++p,',');
                    }
                    *p='\0';
                    strcpy(tmp2,Fields);
                    p++;p=strchr(p,',');
                    if (p!=NULL) strcat (tmp2, p);
                }
                strcpy(Fields, tmp2);
                topsetup.proc_flags|=make_header(dp);
                break;
            }                                        /* add field */
            if (strlen(Fields)+strlen(tags[sel])<256) {
                strcat (Fields, ",");
                strcat (Fields,tags[sel]);
                topsetup.proc_flags|=make_header(dp);
            } else {
                printf(" Too many fields \n");
                return;
            }
            break;
        default:      /* select a field or do nothing */
            if ('A'<= c && c<='Z') sel = c-'A' + j*N;
            if ('a'<= c && c<='z') sel = c-'a' + 26 + j*N;
            break;
        }
        
    }
}
/*
 *#######################################################################
 *####  Routines handling the main top screen:                   ########
 *####                   show_procs, show_meminfo, do_stats      ########
 *#######################################################################
 */

/*
 * This is the real program!  Read process info and display it.
 * One could differentiate options of readproctable2, perhaps it
 * would be useful to support the PROC_UID, PROC_TTY and PROC_PID
 * as command line options.
 */
void show_procs (void)
{
    static proc_t **p_table = NULL;
    int count,disp;
    int current_uptime;
    float elapsed_time;
    unsigned int main_mem;
    static int first = 0;

    if (first == 0) {
	p_table = readproctab2 (PROC_FILLMEM|PROC_FILLSTAT, p_table, NULL,0);
	elapsed_time = get_elapsed_time ();
	do_stats (p_table, elapsed_time, 0);
	sleep (1);
	first = 1;
    }
    /*
     * Display the load averages. 
     */
    current_uptime=uptime(NULL,NULL);
    PUTP (ho);
    PUTP (md);
    if (TTEST(LOAD)) {
	printf ("%s\n", sprint_uptime ());
	PUTP (top_clrtoeol);
    }
    if (uindex) { 
        p_table = readproctab2 (topsetup.proc_flags, p_table, uid, uindex);
    } else if (nuindex) {
        p_table = readproctab2 (topsetup.proc_flags, p_table, nuid, nuindex);
    } else if (tindex) {
        p_table = readproctab2 (topsetup.proc_flags, p_table, show_ttys, 0);
    } else {
        p_table = readproctab2 (topsetup.proc_flags, p_table, NULL, 0);
    }  
    /*
     * Immediately find out the elapsed time for the frame. 
     */
    elapsed_time = get_elapsed_time ();
    /*
     * Display the system stats, calculate percent CPU time * and sort the
     * list. 
     */
    do_stats (p_table, elapsed_time, 1);
    /*
     * Display the memory and swap space usage. 
     */
    main_mem = show_meminfo ();
    PUTP (mr);
    fputs (Header, stdout);
    PUTP (top_clrtoeol);
    PUTP (me);

    /*
     * Finally!  Loop through to find the top task, and display it.
     * Lather, rinse, repeat.
     */
    count = 0;
    disp = 0;
    while ((disp < Maxlines) && (p_table[count]->pid != -1)) {
	char stat;

	stat = p_table[count]->state;

	if (!TTEST(NOIDLE) || (stat != 'S' && stat != 'Z' && stat != 'T')) {

	    /*
	     * Show task info.
	     */
            printf("\n");
	    show_task_info ( p_table[count], main_mem, current_uptime, &(topsetup.disp_opts));
            PUTP (top_clrtoeol);
	    disp++;
	}
	count++;
    }
    PUTP (top_clrtobot);
    PUTP (tgoto (cm, 0, topsetup.header_lines - 2));
    fflush (stdout);
}


/*
 * Finds the current time (in microseconds) and calculates the time
 * elapsed since the last update. This is essential for computing
 * percent CPU usage.
 */
float get_elapsed_time (void)
{
    struct timeval time;
    static struct timeval oldtime = {0,0};
    struct timezone timez;
    float elapsed_time;

    gettimeofday (&time, &timez);
    elapsed_time = (time.tv_sec - oldtime.tv_sec)
	+ (float) (time.tv_usec - oldtime.tv_usec) / 1000000.0;
    oldtime.tv_sec = time.tv_sec;
    oldtime.tv_usec = time.tv_usec;
    return (elapsed_time);
}


/*
 * Reads the memory info and displays it.  Returns the total memory
 * available, for use in percent memory usage calculations.
 */
unsigned show_meminfo (void)
{
    unsigned **mem;

    if (!(mem = meminfo ()) ||	/*read+parse /proc/meminfo */
	mem[meminfo_main][meminfo_total] == 0) {
        /* cannot normalize mem usage */
	fprintf (stderr, "Cannot get size of memory from /proc/meminfo\n");
	error_end (1);
    }
    if (TTEST(MEM)) {
	printf ("Mem:  %6dK av, %6dK used, %6dK free, %6dK shrd, %6dK buff",
		mem[meminfo_main][meminfo_total] >> 10,
		mem[meminfo_main][meminfo_used] >> 10,
		mem[meminfo_main][meminfo_free] >> 10,
		mem[meminfo_main][meminfo_shared] >> 10,
		mem[meminfo_main][meminfo_buffers] >> 10);
	PUTP (top_clrtoeol);
	putchar ('\n');
	printf ("Swap: %6dK av, %6dK used, %6dK free            %6dK cached",
		mem[meminfo_swap][meminfo_total] >> 10,
		mem[meminfo_swap][meminfo_used] >> 10,
		mem[meminfo_swap][meminfo_free] >> 10,
		mem[meminfo_total][meminfo_cached] >> 10);
	PUTP (top_clrtoeol);
	putchar ('\n');
    }
    PUTP (me);
    PUTP (top_clrtoeol);
    putchar ('\n');
    return mem[meminfo_main][meminfo_total];
}

/*
 * Calculates the number of tasks in each state (running, sleeping, etc.).
 * Calculates the CPU time in each state (system, user, nice, etc).
 * Calculates percent cpu usage for each task.
 */
void do_stats (proc_t ** p, float elapsed_time, int pass)
{
    proc_t *this;
    int index, total_time, i, n = 0;
    int sleeping = 0, stopped = 0, zombie = 0, running = 0;
    int system_ticks = 0, user_ticks = 0, nice_ticks = 0, idle_ticks = 1000;
    static int prev_count = 0;
    int stime, utime;
    static struct save_hist save_history[MAX_NR_TASKS];
    static struct save_hist New_save_hist[MAX_NR_TASKS];

    /*
     * Make a pass through the data to get stats.
     */
    index = 0;
    while (p[n]->pid != -1) {
	this = p[n];
	switch (this->state) {
	case 'S':
	case 'D':
	    sleeping++;
	    break;
	case 'T':
	    stopped++;
	    break;
	case 'Z':
	    zombie++;
	    break;
	case 'R':
	    running++;
	    break;
	default:
	    /*
	     * Don't know how to handle this one. 
	     */
	    break;
	}

	/*
	 * Calculate time in this process.  Time is sum of user time
	 * (utime) plus system time (stime).
	 */
	total_time = this->utime + this->stime;
	New_save_hist[index].ticks = total_time;
	New_save_hist[index].pid = this->pid;
	stime = this->stime;
	utime = this->utime;
	New_save_hist[index].stime = stime;
	New_save_hist[index].utime = utime;
	/*
	 * find matching entry from previous pass 
	 */
	i = 0;
	while (i < prev_count) {
	    if (save_history[i].pid == this->pid) {
		total_time -= save_history[i].ticks;
		stime -= save_history[i].stime;
		utime -= save_history[i].utime;

		i = MAX_NR_TASKS;
	    }
	    i++;
	}

	/*
	 * Calculate percent cpu time for this task.
	 */
	this->pcpu = (total_time * 10 * 100 / HZ) / elapsed_time;
	if (this->pcpu > 999)
	    this->pcpu = 999;

	/*
	 * Calculate time in idle, system, user and niced tasks.
	 */
	idle_ticks -= this->pcpu;
	system_ticks += stime;
	user_ticks += utime;
	if (this->nice > 0)
	    nice_ticks += this->pcpu;

	index++;
	n++;
	if (n > MAX_NR_TASKS) {
	    printf (PROGNAME ": Help!  Too many tasks!\n");
	    end ();
	}
    }

    if (idle_ticks < 0)
	idle_ticks = 0;
    system_ticks = (system_ticks * 10 * 100 / HZ) / elapsed_time;
    user_ticks = (user_ticks * 10 * 100 / HZ) / elapsed_time;

    /*
     * Display stats.
     */
    if (pass > 0 && TTEST(STATS)) {
	printf ("%d processes: %d sleeping, %d running, %d zombie, "
		"%d stopped",
		n, sleeping, running, zombie, stopped);
	PUTP (top_clrtoeol);
	putchar ('\n');
	printf ("CPU states: %2d.%d%% user, %2d.%d%% system,"
		" %2d.%d%% nice, %2d.%d%% idle",
		user_ticks / 10, user_ticks % 10,
		system_ticks / 10, system_ticks % 10,
		nice_ticks / 10, nice_ticks % 10,
		idle_ticks / 10, idle_ticks % 10);
	PUTP (top_clrtoeol);
	putchar ('\n');
    }
    /*
     * Save this frame's information.
     */
    for (i = 0; i < n; i++) {
	/*
	 * copy the relevant info for the next pass 
	 */
	save_history[i].pid = New_save_hist[i].pid;
	save_history[i].ticks = New_save_hist[i].ticks;
	save_history[i].stime = New_save_hist[i].stime;
	save_history[i].utime = New_save_hist[i].utime;
    }
    prev_count = n;
    qsort (p, n, sizeof (proc_t *), (void *) mult_lvl_cmp);
}


/*
 * Process keyboard input during the main loop
 */
void do_key (char c)
{
    int numinput, i;
    char rcfile[MAXNAMELEN];
    FILE *fp;

    /*
     * First the commands which don't require a terminal mode switch.
     */
    if (c == 'q')
	end ();
    else if (c == 12) {
	clear_screen ();
	return;
    }
    /*
     * Switch the terminal to normal mode.  (Will the original
     * attributes always be normal?  Does it matter?  I suppose the
     * shell will be set up the way the user wants it.)
     */
    ioctl (0, TCSETS, &Savetty);

    /*
     * Handle the rest of the commands.
     */
    switch (c) {
    case 'a':
        DTOGGLE(NEWTIMFORM);
        break;
    case '?':
    case 'h':
	PUTP (cl);
	PUTP (ho);
	putchar ('\n');
	PUTP (mr);
	printf ("Proc-Top Revision 2.00");
	PUTP (me);
	putchar ('\n');
	printf ("Secure mode ");
	PUTP (md);
	fputs (TTEST(SECURE) ? "on" : "off", stdout);
	PUTP (me);
	fputs ("; cumulative mode ", stdout);
	PUTP (md);
	fputs (DTEST(CUMULATIVE) ? "on" : "off", stdout);
	PUTP (me);
	fputs ("; noidle mode ", stdout);
	PUTP (md);
	fputs (TTEST(NOIDLE) ? "on" : "off", stdout);
	PUTP (me);
	fputs ("\n\n", stdout);
	printf ("%s\n\nPress any key to continue\n", TTEST(SECURE) ? SECURE_HELP_SCREEN : HELP_SCREEN);
	ioctl (0, TCSETS, &Rawtty);
	(void) getchar ();
	break;
    case 'b':
        TTOGGLE(BOLD);
	if (TTEST(BOLD)) {
            md = tgetstr ("md", 0);
            mr = tgetstr ("mr", 0);
	} else {
            mr=me;
	    md=me;
	}
	break;
    case 'e':
    case 'E':
        DTOGGLE(COLOR);
	break;
    case 'i':
	TTOGGLE(NOIDLE);
	SHOWMESSAGE (("No-idle mode %s", TTEST(NOIDLE) ? "on" : "off"));
	break;
    case 'k':
	if (TTEST(SECURE))
	    SHOWMESSAGE (("\aCan't kill in secure mode"));
	else {
	    int pid, signal;
	    PUTP (md);
	    SHOWMESSAGE (("PID to kill: "));
	    pid = getint ();
	    if (pid == -1)
		break;
	    PUTP (top_clrtoeol);
	    SHOWMESSAGE (("Kill PID %d with signal [15]: ", pid));
	    PUTP (me);
	    signal = getsig ();
	    if (signal == -1)
		signal = SIGTERM;
	    if (kill (pid, signal))
		SHOWMESSAGE (("\aKill of PID %d with %d failed: %s",
			      pid, signal, strerror (errno)));
	}
	break;
    case 'l':
	SHOWMESSAGE (("Display load average %s", !TTEST(LOAD) ? "on" : "off"));
	TTOGGLE(LOAD);
	if (TTEST(LOAD)) {
	    topsetup.header_lines++;
	} else {
	    topsetup.header_lines--;
	}
	top_header ();
	break;
    case 'm':
	SHOWMESSAGE (("Display memory information %s", !TTEST(MEM) ? "on" : "off"));
	TTOGGLE(MEM);
	if (TTEST(MEM)) {
	    topsetup.header_lines += 2;
	} else {
	    topsetup.header_lines -= 2;
	}
	top_header ();
	break;
    case 'M':
	SHOWMESSAGE (("Sort by memory usage"));
	topsetup.sort_type = S_MEM;
	reset_sort_options ();
	register_sort_function (-1, (cmp_t) mem_sort);
	break;
    case 'n':
    case '#':
	printf ("Processes to display (0 for unlimited): ");
	numinput = getint ();
	if (numinput != -1) {
	    Display_procs = numinput;
	    window_size ();
	}
	break;
    case 'r':
	if (TTEST(SECURE))
	    SHOWMESSAGE (("\aCan't renice in secure mode"));
	else {
	    int pid, val;

	    printf ("PID to renice: ");
	    pid = getint ();
	    if (pid == -1)
		break;
	    PUTP (tgoto (cm, 0, topsetup.header_lines - 2));
	    PUTP (top_clrtoeol);
	    printf ("Renice PID %d to value: ", pid);
	    val = getint ();
	    if (val == -1)
		val = 10;
	    if (setpriority (PRIO_PROCESS, pid, val))
		SHOWMESSAGE (("\aRenice of PID %d to %d failed: %s",
			      pid, val, strerror (errno)));
	}
	break;
    case 'R':
        topsetup.proc_flags ^= PROC_REAL;
        break;
    case 'P':
	SHOWMESSAGE (("Sort by CPU usage"));
	topsetup.sort_type = S_PCPU;
	reset_sort_options ();
	register_sort_function (-1, (cmp_t) pcpu_sort);
	break;
    case 'c':
	DTOGGLE(CMD);
	if (DTEST(CMD)) {
            topsetup.proc_flags |= PROC_FILLCMD;
	} else {
	    topsetup.proc_flags &= ~PROC_FILLCMD;
	}
	SHOWMESSAGE (("Show %s", !DTEST(CMD) ? "command names" : "command line"));
	break;
    case 'S':
	DTOGGLE(CUMULATIVE);
	SHOWMESSAGE (("Cumulative mode %s", DTEST(CUMULATIVE) ? "on" : "off"));
	top_header ();
	break;
    case 's':
	if (TTEST(SECURE))
	    SHOWMESSAGE (("\aCan't change delay in secure mode"));
	else {
            double tmp;
	    printf ("Delay between updates: ");
	    tmp = getfloat ();
	    if (!(tmp < 0))
		topsetup.Sleeptime = tmp;
	}
	break;
    case 't':
	SHOWMESSAGE (("Display summary information %s", !TTEST(STATS) ? "on" : "off"));
	TTOGGLE(STATS);
	if (TTEST(STATS)) {
	    topsetup.header_lines += 2;
	} else {
	    topsetup.header_lines -= 2;
	}
	top_header ();
	break;
    case 'T':
	SHOWMESSAGE (("Sort by %s time", DTEST(CUMULATIVE) ? "cumulative" : ""));
	topsetup.sort_type = S_TIME;
	reset_sort_options ();
	register_sort_function (-1, (cmp_t) time_sort);
	break;
    case 'f':
    case 'F':
	change_fields (&(topsetup.disp_opts));
	break;
    case 'o':
    case 'O':
	change_fields (&(topsetup.disp_opts));
	break;
    case 'W':
	if (getenv ("HOME")) {
	    strcpy (rcfile, getenv ("HOME"));
	    strcat (rcfile, "/");
	    strcat (rcfile, RCFILE);
	    fp = fopen (rcfile, "w");
	    if (fp != NULL) {
		fprintf (fp, "%s\n", Fields);
		i = (int) topsetup.Sleeptime;
		if (i < 2)
		    i = 2;
		if (i > 9)
		    i = 9;
		fprintf (fp, "%d", i);
		if (TTEST(SECURE))
		    fprintf (fp, "%c", 's');
                if (DTEST(NEWTIMFORM))
		    fprintf (fp, "%c", 'a');                    
		if (DTEST(CUMULATIVE))
		    fprintf (fp, "%c", 'S');
		if (DTEST(CMD))
		    fprintf (fp, "%c", 'c');
		else    
                    fprintf (fp, "%c", 'C');
		if (TTEST(BOLD))
		    fprintf (fp, "%c", 'b');
		else
		    fprintf (fp, "%c", 'B');
		if (TTEST(NOIDLE))
		    fprintf (fp, "%c", 'i');
		if (!TTEST(MEM))
		    fprintf (fp, "%c", 'm');
		if (!TTEST(LOAD))
		    fprintf (fp, "%c", 'l');
		if (!TTEST(STATS))
		    fprintf (fp, "%c", 't');
		if (DTEST(COLOR))
		    fprintf (fp, "%c", 'e');
		else 
		    fprintf (fp, "%c", 'E');
                if (topsetup.proc_flags & PROC_REAL)
                    fprintf (fp, "%c", 'r');
		fprintf (fp, "\n");
		fclose (fp);
		SHOWMESSAGE (("Wrote configuration to %s", rcfile));
	    } else {
		SHOWMESSAGE (("Couldn't open %s", rcfile));
	    }
	} else {
	    SHOWMESSAGE (("Couldn't get $HOME -- not saving"));
	}
	break;
    default:
	SHOWMESSAGE (("\aUnknown command `%c' -- hit `h' for help", c));
    }

    /*
     * Return to raw mode.
     */
    ioctl (0, TCSETS, &Rawtty);
    return;
}


/*
 * #####################################################################
 * #######   A readproctable function that uses already allocated  #####
 * #######   table entries.                                        #####
 * ##################################################################### 
 */
#undef Do

#define Do(x) (flags & PROC_ ## x)

proc_t **readproctab2 (int flags, proc_t ** tab, void * args, int N)
{
    PROCTAB *PT = NULL;
    proc_t *buff;
    int n = 0;
    static int len = 0;

    if (Do (UID) || Do (NOTUID))
	PT = openproc (flags, args, N);
    else if (Do (PID) || Do (TTY) || Do (STAT))
	PT = openproc (flags, args);
    else
	PT = openproc (flags);
    buff = (proc_t *) 1;
    while (n < len && buff) {	
	if (tab[n]->cmdline) {
            free(*tab[n]->cmdline);
            tab[n]->cmdline=NULL;
        }
	buff = readproc (PT, tab[n]);
	n++;
    }
    if (buff) {
	do {			
	    tab = realloc (tab, (n + 1) * sizeof (proc_t *)); 
	    buff = readproc (PT, NULL);	
	    if (buff)
		tab[n] = buff;
	    len++;
	    n++;
	} while (buff);	
	tab[n - 1] = xcalloc (NULL, sizeof (proc_t));
	tab[n - 1]->pid = -1;
    } else {
	tab[n-1]->pid = -1;
    }				
    closeproc (PT);
    return tab;
}
#undef Do
