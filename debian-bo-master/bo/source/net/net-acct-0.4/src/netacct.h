/* 
 * Network accounting
 * netacct.h - header file *
 * (C) 1994 Ulrich Callmeier
 */

#include <stdio.h>
#include <errno.h>
#include <syslog.h>
#include <time.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#ifdef linux
#include <netinet/protocols.h>
#include <linux/if_ether.h>
#include <linux/tcp.h>
#endif

#ifndef _PATH_UTMP
#define _PATH_UTMP "/var/run/utmp"
#endif

#define DEF_ACCTTAB "/etc/naccttab"
#ifdef linux
#define PID_FILE "/var/run/nacctd.pid"
#else
#define PID_FILE "/etc/nacctd.pid"
#endif

/* default settings for naccttab */
#define DEFAULT_IGNOREMASK "255.255.255.255"
#define DEFAULT_FLUSH 300
#define DEFAULT_ERR_DELAY 3
#define DEFAULT_FDELAY 60

#define FORCE_STAT_TIME 5


  /****************************************/
 /* no user configurable stuff from here */
/****************************************/


#define MIN_DISABLE 2
#define MAX_DISABLE 10

#define DIS_PROTO 2
#define DIS_SRC 3
#define DIS_SRCPORT 4
#define DIS_DST 5
#define DIS_DSTPORT 6
#define DIS_COUNT 7
#define DIS_BYTES 8
#define DIS_DEV 9
#define DIS_USER 10

#define BITMASK(bit) (1 << ((bit) % (CHAR_BIT*sizeof(int))))

/* parsing of config file */
#define DBG_CONFIG	(1 << 1) 
#define DBG_STATE	(1 << 2)
#define DBG_UTMP	(1 << 3)
#define DBG_DYNAMIC	(1 << 4)
#define DBG_SYSCALL	(1 << 5)
#define DBG_IGNORE	(1 << 6)
#define DBG_MISC	(1 << 7)
#define DBG_STATISTICS	(1 << 8)
#define DBG_SIGNAL	(1 << 9)
#define DBG_ERR		(1 << 10)
#define DBG_ANNOYING	(1 << 30)

static char *DBG_TYPE_STRING[31] = {"NONE ", "CONF ", "STATE", "UTMP ", "DYNA ", "SYS  ", "IGN  ", "MISC ", "STATS", "SIG  ", /* 10 */ "ERROR", "", "", "", "", "", "", "", "", "", /* 20 */ "", "", "", "" ,"", "", "", "", "", "", "ANNOY"};

#define DEBUG(level, msg)\
 if((level) & debug_level)\
 {\
  char dbg[255], DBGtmp[255], DBGtype[255]; int DBGi;\
  time_t DBGcurtime = time(NULL);\
  for(DBGi=1; DBGi<=30; DBGi++) if((1 << DBGi) & level) {strcpy(DBGtype, DBG_TYPE_STRING[DBGi]);break;}\
  strftime(DBGtmp, sizeof(DBGtmp), "%d/%m %H:%M:%S ", localtime(&DBGcurtime));\
  msg; fprintf(dbg_file, "%s[%s] %s",DBGtmp,DBGtype,dbg);\
 }

struct ipnetwork
{
    unsigned long netnumber, netmask;
    struct ipnetwork *next;
};

struct promisc_device
{
    char *name; /* name (e.g. eth0) */

    int reset; /* do we have to reset it on exit ? */
    struct ifreq oldifr; /* old settings */
    
    struct promisc_device *next;
};

struct config
{
    char *filename;
    char *dumpname;
    char *debugname;
    int flush; /* in seconds */
    int fdelay; /* in seconds */
    unsigned long int ignoremask;
    int err_delay; /* how many cycles to delay on error ? */
    struct ipnetwork *ignorenet;
    struct ipnetwork *dontignore;
    struct promisc_device *promisc;
    struct promisc_device *notdev;
    struct ipnetwork dynamicnet;
    struct ipnetwork *excludenamelookup;
    char *dynamicip;
    int disabled; /* disabled output fields */
};

struct dev2line
{
    char *netinterface;
    char *line;
    
    struct dev2line *next;
};

struct dynadat
{
    char *netinterface;
    unsigned long addr;
    time_t last_stat, mtime;

    char *user;

    struct dynadat *next;
};

struct statistics
{
    unsigned long int unenc;
    unsigned long int notdev;
    unsigned long int ignored, netignored, local, ip, dropped;		/* sum = total */
    unsigned long int ip_udp, ip_tcp, ip_icmp, ip_other;	/* sum = ip */
};

struct ipdata
{
    unsigned long int src, dst;
    unsigned char proto;
    unsigned short srcport, dstport;
    unsigned long int bytes;
    unsigned count;
    char *devname;
    char *user;
    
    time_t when;

    struct ipdata *next;
};

extern char *rcs_revision_config_c;
extern char *rcs_revision_daemon_c;
extern char *rcs_revision_capture_c;
extern char *rcs_revision_main_c;

extern char *progname;
extern struct config *cfg; 
extern FILE *dbg_file;
extern volatile int debug_level;
extern struct dev2line *dev2line;

extern volatile int running;
extern struct statistics *packets;

extern volatile time_t now; /* current time */

/* capture-xxx.c */
void init_capture(void);
void do_acct(void);
void exit_capture(void);
void packet_loop(void);

/* process.c */

void register_packet(unsigned long int src,unsigned long int dst, unsigned char proto, unsigned short srcport, unsigned short dstport, int size, char *devname, char *user);
void write_log(int force);
void alarm_handler(int sig);
void child_finished(int sig);
void signal_debug(int sig);
void signal_ignore(int sig);

/* daemon.c */
int daemon_start(void);
void daemon_stop(int sig);

/* config.c */
struct config *read_config(char *fname);

/* utils.c */
char *ip_proto_name(unsigned char proto);
char *intoa(unsigned long addr);
char * etheraddr_string(unsigned char *ep);
