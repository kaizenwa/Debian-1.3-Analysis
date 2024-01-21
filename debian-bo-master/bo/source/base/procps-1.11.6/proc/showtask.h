/*
 * showtask.h 
 *
 * Global variables defined:
 * Header, headers, headers2.
 * Markuplist
 * 
 */

/*
 * ##############################################################
 * #####         Macro definitions & typedefs             #######
 * ##############################################################
 */

#define MAXLINES 4096
#define MAXNAMELEN 1024

/*
 * this may be defined in CFLAGS (ALPHAs may need a larger field 
 * length to display longs)
 */
#ifndef MAX_FIELD_LEN
#define MAX_FIELD_LEN 20
#endif
/*
 * These flags control the behaviour of the display function.
 * Note that for DISP_CMD to work, PROC_FILLCMD has to be set
 * when calling readproc!
 */
#define  DISP_CUMULATIVE 0x0001 /* Cumulative mode */
#define  DISP_WCHAN      0x0002 /* do WCHAN name lookup */
#define  DISP_CMD        0x0004 /* show full command line */
#define  DISP_COLOR      0x0008 /* use color markup */
#define  DISP_FOREST     0x0010 /* display forest - don't
                      allow command & environment fields for show_task_info */
#define  DISP_NEWTIMFORM 0x0020 /* use new time format */
#define  DISP_TABLEFORCE 0x0040 /* force field lengths to be unalterable */

#define MAXFIELDS 65 /* remember to change this, if any field is added */

typedef struct {
    int flags,           /* display option flags as above*/ 
        Numfields,       /* number of fields to display */
        Cols,            /* Number of columns on output device */
        Lines,           /* max. number of additional lines to use for
                          * a single task */
        stretch,         /* length of stretchable fields */
        pg_shift;        /* default=PAGE_SHIFT-10: Show k instead of pages */
    char *Fieldstr;
    int p[MAXFIELDS];           /* field types to display, in correct order */
} dopts;


/* 
 * flags for each possible field. seven flags per line
 * At the moment 65 are supported 
 */
enum {
    P_USER=0, P_RUSER, P_GROUP, P_RGROUP, P_PID, P_PPID, P_PGID,
    P_PCPU, P_VSIZE, P_NICE, P_ETIME, P_TIME, P_TTY, P_COMMAND,
    P_ARGS, P_STAT, P_UID, P_RUID, P_SUID, P_FSUID, P_GID,
    P_RGID, P_SGID, P_FSGID, P_SID, P_UTIME, P_STIME, P_PRI,
    P_START, P_FLAGS, P_MINFLT, P_MAJFLT, P_TIMEOUT, P_ALARM, P_RSS,
    P_SCODE, P_ECODE, P_STACKP, P_ESP, P_EIP, P_WCHAN, P_SIGNAL,
    P_SIGBLOCK, P_SIGIGN, P_SIGCATCH, P_PMEM, P_SUSER, P_FSUSER, P_SGROUP,
    P_FSGROUP, P_ENV, P_VLOCK, P_VDATA, P_VSTACK, P_VEXE, P_VLIB,
    P_MSIZE, P_MRES, P_MSHRD, P_MTRS, P_MDRS, P_MDT, P_MLRS,
    P_MSWAP, P_TPGID
};

/*
 * ##############################################################
 * #####         Function declarations                    #######
 * ##############################################################
 */
int make_header (dopts *opts);
int show_task_info (proc_t * task, int main_mem, int current_time, dopts *opts);
void setup_colors (dopts *opts, char *colorenv);

#ifdef PROC_COLOR
char * markup (proc_t * task, int pmem);
#endif
/* 
 * ##############################################################
 * #####         Configurable field support               #######
 * ##############################################################
 */

typedef struct {
    char fld[16];          /* Header  field  */
    int ln;                /* Default length */
    char format[16];       /* sprintf formt string */
    unsigned proc_flags;
} header_t;

 

extern header_t headers[MAXFIELDS];
extern char tags[MAXFIELDS][16];
extern char *headers2[];
extern char Header[MAXLINES];

#ifdef PROC_COLOR    

/*
 * ######################################################################
 * #####           Color markup support                        ##########
 * ######################################################################
 */

#ifdef COLOR_FILE

#ifndef COLOR_RC
#define COLOR_RC        ".proc-color"
#endif

#ifndef SYS_COLOR_RC
#define SYS_COLOR_RC        "/etc/proc-color"
#endif

#endif

#define END_MARKUP "\033[0m"
#define LEFT_MARKUP "\033["
#define RIGHT_MARKUP "m"

/*
 * Markup types:
 * g_mem : mark if global memory usage is above boundary (unsigned   *spec)
 * p_uid : mark if task is owned by user uid             (uid_t      *spec)
 * p_stat: mark if task has status  spec                 (char       *spec)
 * p_mem : mark if task memory usage is above spec%      (int        *spec)
 * p_cpu :  ---------   cpu ---------------              (int        *spec)
 * p_size: mark if task size is above spec k             (unsigned   *spec)
 * p_tty : mark if task has controlling terminal spec    (char        spec[4])
 */
typedef enum { none, g_mem, p_uid, p_stat, p_mem, p_cpu, p_size, p_tty } markup_t;

struct markup_s {
    markup_t type;         /* Markup type */
    void *spec;            /* When to markup: pointer corresponding to type */
    char *colspec;         /* ANSI color specification ("mode;fg;bg") */
    struct markup_s *next; /* next markup */
};

/* Start of Markuptree */
extern struct markup_s *Markuplist;

#endif /* PROC_COLOR */


