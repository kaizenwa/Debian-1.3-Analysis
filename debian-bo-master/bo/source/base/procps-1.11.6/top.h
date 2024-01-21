/*
 * top.h header file 1996/05/18, 
 *
 * function prototypes, global data definitions and string constants.
 */

proc_t** readproctab2(int flags, proc_t** tab, void *args, int N);
void parse_options(char *Options, int secure);
void get_options(void);
void clean_up(void);
void error_end(int rno);
void end(void);
void stop(void);
void window_size(void);
void top_header(void);
int getnum(void);
char *getstr(void);
int getsig(void);
float getfloat(void);
int time_sort(proc_t **P, proc_t **Q);
int pcpu_sort(proc_t **P, proc_t **Q);
int mem_sort(proc_t **P, proc_t **Q);
void show_fields(void);
#if 0
void change_order(void);
void change_fields(void);
#endif
void change_fields(dopts *dp);
void show_procs(void);
float get_elapsed_time(void);
unsigned show_meminfo(void);
void do_stats(proc_t** p, float elapsed_time, int pass);
void do_key(char c);

#define MAX_NR_TASKS 4096

	/* Name of the config file (in $HOME)  */
#ifndef RCFILE
#define RCFILE		".topnewrc"
#endif

#ifndef SYS_TOPRC
#define SYS_TOPRC	"/etc/toprc"
#endif

/* this is what procps top does by default, so let's do this, if nothing is
 * specified
 */
#ifndef DEFAULT_SHOW
#define DEFAULT_SHOW    "user,pid,pcpu,pmem,nice,vsz,rss,m_share,tty,state,time,comm"
#endif
char Fields[256] = "";


/* This structure stores some critical information from one frame to
   the next. mostly used for sorting. Added cumulative and resident fields. */
struct save_hist {
    int ticks;
    int pid;
    int pcpu;
    int utime;
    int stime;
};

	/* The original terminal attributes. */
struct termios Savetty;
	/* The new terminal attributes. */
struct termios Rawtty;
	/* Cached termcap entries.
         * cm         move cursor
         * cl         clear screen
         * clrtobot   clear to bottom of screen ("cb")   
         * clrtoeol   clear to end of line ("ce")
         * ho         home
         * c_up       cursor up
         * c_do       cursor down
         * md         enter bold mode
         * mr         enter standout mode (usually the same as reverse)
         * me         end all special modes
         */
char *cm, *cl, *top_clrtobot, *top_clrtoeol, *ho, *md, *me, *mr, *c_do, *c_up;
	/* Current window length.  Note that it is legal to set Display_procs
	   larger than can fit; if the window is later resized, all will be ok.
	   In other words: Display_procs is the specified max number of
	   processes to display (zero for infinite), and Maxlines is the actual
	   number. */
int Maxlines, Display_procs;

	/* The top of the main loop. */
jmp_buf redraw_jmp;

/* sorting order: cpu%, mem, time (cumulative, if in cumulative mode) */
enum {
    S_PCPU, S_MEM, S_TIME
};

#define  TOP_SECURE     0x0001 /* Secure mode */
#define  TOP_NOIDLE     0x0002 /* No-Idle mode */
#define  TOP_BOLD       0x0004 /* Use bold & reverse attributes */
#define  TOP_STATS      0x0008 /* Show process statistic */
#define  TOP_MEM        0x0010 /* Show memory statistic */
#define  TOP_LOAD       0x0020 /* Show load average & uptime */

struct top_setup {
    unsigned long flags;
    float Sleeptime;
    int proc_flags, sort_type, header_lines, Lines;
    dopts disp_opts;
    
} topsetup = { ~TOP_SECURE & ~TOP_NOIDLE & 
               (TOP_BOLD | TOP_STATS | TOP_MEM | TOP_LOAD),
 	       3.0, PROC_FILLMEM | PROC_FILLTTY | PROC_FILLUSR | PROC_FILLCMD,
               S_PCPU, 7, 0,
               { ~DISP_CUMULATIVE &  (DISP_WCHAN | DISP_CMD | DISP_COLOR
#ifdef NEW_TIME_DEFAULT
                                      | DISP_NEWTIMFORM
#endif                                      
                                      ),
                 0, 0, 0, 0, PAGE_SHIFT-10, (char *)NULL,
               }              
};

/* for selecting/deselecting userids/ttys on startup */
__uid_t uid[11];
__uid_t nuid[11];
dev_t show_ttys[11]={0,0,0,0,0, 0,0,0,0,0, 0};
int uindex=0;
int nuindex=0;
int tindex=0;

	/* The response to the interactive 'h' command. */
#define HELP_SCREEN "\
Interactive commands are:\n\
\n\
^L\tRedraw the screen\t\th or ?\tPrint this list\n\
r\tRenice a task\t\t\tk\tSend a signal to a task\n\
S\tToggle cumulative mode\t\ti\tToggle idle processes\n\
c\tToggle command name/line\tl\tToggle load average\n\
m\tToggle memory information\tt\tToggle summary information\n\
P\tSort by CPU usage\t\tM\tSort by resident memory usage\n\
T\tSort by time/cumulative time\tq\tQuit\n\
n or #\tSet the number of process\tb\tToggle display modes\n\
fF\tadd, order and remove fields\tR\tToggle use of real ids\n\
s\tSet seconds between updates\ta\tAlternate time format\n\
W\tWrite configuration file ~/.toprc"
#define SECURE_HELP_SCREEN "\
Interactive commands available in secure mode are:\n\
\n\
^L\tRedraw the screen\ta\tAlternate time format\n\
fF\tadd, order and remove fields\n\
h or ?\tPrint this list\n\
S\tToggle cumulative mode\tR\tToggle use of real ids\n\
i\tToggle display of idle proceses\n\
c\tToggle display of command name/line\n\
l\tToggle display of load average\n\
m\tToggle display of memory information\n\
t\tToggle display of summary information\n\
n or #\tSet the number of process to show\n\
W\tWrite configuration file ~/.toprc\n\
q\tQuit"

/* ############## Some Macro definitions for screen handling ######### */
	/* String to use in error messages. */
#define PROGNAME "top"
	/* Clear the screen. */
#define clear_screen() 	PUTP(cl)
	/* Show an error in the context of the spiffy full-screen display. */
#define SHOWMESSAGE(x) do { 			 \
	    PUTP (tgoto(cm, 0, topsetup.header_lines-2)); \
	    PUTP (top_clrtoeol);                 \
	    PUTP (md);                           \
	    PUTP (mr);                           \
	    printf x;				 \
	    PUTP (me);                           \
	    fflush(stdout);                      \
	    sleep(2);				 \
	} while (0)
