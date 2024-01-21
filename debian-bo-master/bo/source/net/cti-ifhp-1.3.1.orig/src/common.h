/*
 ===========================================================================
 =                                                                         =
 =  (C) Copyright 1995, Computer Technology Institute (CTI)                =
 =                                                                         =
 =  Permission to use, copy, modify and distribute this software and       =
 =  its documentation for non-commercial use and without fee is hereby     =
 =  granted, provided that the above copyright notice appear in all        =
 =  copies and that both that copyright notice and this permission         =
 =  notice and warranty disclaimer appear in supporting documentation,     =
 =  and that the name of Computer Technology Institute (CTI) or any of     =
 =  its entities not be used in advertising or publicity pertaining to     =
 =  distribution of the software and its documentation without specific    =
 =  prior permission.                                                      =
 =                                                                         =
 =  Computer Technology Institute (CTI) disclaims all warranties with      =
 =  regard to this software and makes no representations about the         =
 =  suitability of this software and its documentation for any purpose.    =
 =  It is provided "as is" without expressed or implied warranty.          =
 =                                                                         =
 ===========================================================================
 =                                                                         =
 =                        Project CTI-Print                                =
 =                                                                         =
 = File:                                                                   =
 =   common.h                                                             =
 =                                                                         =
 = Synopsis:                                                               =
 =   Declarations and definitions of common variables and functions
 =                                                                         =
 = Author:                                                                 =
 =   Panos Dimakopoulos, Systems Programmer,                                =
 =   Computer Technology Institute,                                        =
 =   Division of Computing Facilities,                                     =
 =   P.O. Box 1122,                                                        =
 =   261 10  Patras,                                                       =
 =   Greece                                                                =
 =   (e-mail: dimakop@cti.gr)                                              =
 =   Tel: +30 61 992061                                                    =
 =   Fax: +30 61 993973                                                    =
 =                                                                         =
 = Created by Patrick Powell  <papowell@sdsu.edu>
 =   for LPRng software Sat Aug 26 06:54:25 PDT 1995
 ===========================================================================
*/


/****************************************************************************
 * Modification History:
 *  Extracted from ifhp4.c
 *
 * 	Revision 1.11	95/08/22	15:01:07
 *		 Version 1.2 initiated and Porting to Solaris.
 *
 */

#ifndef _COMMON_H
#define _COMMON_H 1
#ifdef EXTERN
# undef EXTERN
# undef DEFINE
# define EXTERN
# define DEFINE(X) X
#else
# undef EXTERN
# undef DEFINE
# define EXTERN extern
# define DEFINE(X)
#endif

#ifndef OFF
#	define OFF	0
#endif
#ifndef ON
#	define ON	1
#endif

#ifndef FALSE
#	define FALSE	0
#endif
#ifndef TRUE
#	define TRUE	1
#endif

#include "errorcodes.h"

#define	FILTSUCC	(plp_compat?0:JSUCC)
#define	FILTRETRY	(plp_compat?1:JFAIL)
#define	FILTABORT	(plp_compat?2:JABORT)

#define	PCL		1
#define	POSTSCRIPT	2
#define	LITERAL	3

#define IV          0
#define III         1
#define IIID        2
#define IIISi       3


#define	DIE		1

EXTERN char *name;		/* name of filter */

EXTERN char	*zopts, *class, *job, *login, *accntname, *host, *fqdnhost,
		*accntfile, *bantitle, *format, *filename, 
		*date, *username, *seq_number, *controlfile,
		*device,	/* Patrick Powell Thu Aug 24 21:01:04 PDT 1995 */
		*pr_commnt, *statusfile,
		*stty_args, *model_name, *summaryfile;
EXTERN char *printer DEFINE( = "IFHPfilter" );
EXTERN int	width, length, xwidth, ylength, debug, literal, indent, version;
EXTERN int logall;	/* log all data back from printer */ 
EXTERN int Accounting_fd;

EXTERN char *Upperopts[26];
EXTERN char *Loweropts[26];


extern char *copyright[];

EXTERN int bnr DEFINE(= 1),		/* generate a banner */
    errorcode, 	   /* status the errlog funcs exit with */
    mode,     	   /* PCL or Postscript */
    npages,        /* number of pages printed from device */
	initialpagecount,	/* initial pagecount */
    alrmflag,       /* Set by ALARM signal handling funcs */
    monitpid DEFINE( = -1),       /* pid of the port monitoring child */
	filterpid,		/* pid of the filter */
    printer_ok,
    interrupted,    /* Set when signal received */
    job_started,    /* Flag for the successful initiation of the job */
    job_ended,      /* Flag for the successful termination of the job */
	cartridge DEFINE(= 1),		/* PS cartridge */
	model,			/* model of printer */
	plp_compat,		/* PLP compatible return codes */
	of_filter,		/* OF filter */
	resourcesave,	/* resource saved */
	trace,			/* open a trace file */
	wrap,			/* wrap */
	autodetect,	    /* autodetect to determine job type */
	pagecount DEFINE( =1), /* use PJL INFO PAGECOUNT to get page counter */
#ifdef QUIET
	quiet DEFINE(=1),			/* do not report status messages */
#else
	quiet DEFINE(=0),			/* report status messages */
#endif
	get_status DEFINE( =1 );	/* get status from printer */

EXTERN char    *query[10];          /* Holds the every time pjl commands
                     * sent to the printer port */
EXTERN char **Envp;	/* envp pointer */

EXTERN int wait_time DEFINE(= 30),   /* Time to wait between two retries
                       (secs) */
        retries DEFINE(= 0);    /* Number of retries if printer comes
                       off-line 0 means for ever */
EXTERN int updatepfu;
EXTERN char *pfu_fname DEFINE(= "pfu");
EXTERN char null_str[1024];
EXTERN char *Accounting_script DEFINE( = ACCNTSH ); 
EXTERN char *default_font DEFINE( = DEFLTFONT ); 

#define MAXLINE	1024
EXTERN char    sendline[MAXLINE+1];
EXTERN char    readprin[MAXLINE+1];
EXTERN char    job_start[100];
EXTERN char    job_end[100];

EXTERN int Status_fd;
EXTERN int Max_status_size DEFINE( = 2 );
EXTERN int Min_status_size;

#define STDOUT	fileno(stdout)
			
#define INTV 0
#define STRV 1
#define FLGV 2
struct parm {
	int flag;
	char **var;
	int kind;
};

struct value{
	char *flag;
	char **var;
	int kind;
};


#undef _PARMS__
#ifdef __STDC__
#define _PARMS__(X) X
#else
#define _PARMS__(X) ()
#endif

void sendjob _PARMS__( (int fc, int sockfd) );
void do_of_stream _PARMS__( (FILE *fp, int sockfd) );
void Out_line _PARMS__( ( char *str ) );
int writecn _PARMS__( ( int fd, char *ptr, int nbytes ) );
void cleanup _PARMS__( ( int sig ) );
void wake_up _PARMS__( ( void ) );
void setpcl_on _PARMS__( ( void ) );
void setps_on _PARMS__( ( void ) );
void eofprinter _PARMS__( ( void ) );
void resetprinter _PARMS__( ( void ) );
void pclresetprinter _PARMS__( ( void ) );
int port_readline _PARMS__( ( int fd, char *ptr, int maxlen, int timeout ) );
int file_readline _PARMS__( ( int fd, char *ptr, int maxlen ) );
void doaccnt _PARMS__( ( int start ) );
int readpipe _PARMS__( ( int *inptr, int timeout ) );
int get_num _PARMS__( ( char *s ) );
void forkport _PARMS__( ( void ) );
void monitorport _PARMS__( ( void ) );
void slay _PARMS__( ( int sig ) );
void nochildren _PARMS__( ( int sig ) );
void pr_late _PARMS__( ( int sig ) );
void pr_query _PARMS__( ( int fd, int n ) );
int pr_synch _PARMS__( ( int fd, int timeout ) );
int pr_status _PARMS__( ( int fd ) );
int pr_pagecount _PARMS__( ( int fd ) );
int pr_pspagecount _PARMS__( ( int fd ) );
void pr_timed _PARMS__( ( int fd ) );
void pr_ustatusoff _PARMS__( ( int fd ) );
void pr_ustatus _PARMS__( ( int fd, char *pjlstr ) );
void pr_startjob _PARMS__( ( int fd ) );
void pr_endjob _PARMS__( ( int fd ) );
void check_job_end _PARMS__( ( void ) );
void getpjlargs _PARMS__( ( void ) );
void fexit _PARMS__( ( int i ) );
int check_code _PARMS__( ( int cd ) );
void wait_printer _PARMS__( (int i) );
void write_check _PARMS__( (int fd,char *str,char *msg,int flg) );
void get_retries _PARMS__( ( void ) );
char *getcrstr _PARMS__( ( char *s, char *match, int len ) );
void updaccnt _PARMS__( ( int i ) );
void wrap_set _PARMS__( ( void ) );
void job_page_set _PARMS__( ( void ) );
void page_set _PARMS__( ( void ) );
int sendfont _PARMS__( ( char *font ) );
void selectfont _PARMS__( ( void ) );
char *fnt_scan _PARMS__( (char *mchn) );
void update_pfu _PARMS__( (char *pfu_filename, char *fontfilename) );
void delete_pfu _PARMS__( (char *pfu_filename) );
void newfont _PARMS__( ( char *fontname) );
void open_device _PARMS__( (void) );
void do_monitor _PARMS__( (void) );
void check_pages _PARMS__( (void) );
int open_trace _PARMS__( (void) );
void set_mode _PARMS__( (int m) );
void pr_duplex _PARMS__( (void) );
void getargs _PARMS__( (int argc, char **argv) );
void get_info _PARMS__( (void) );
void setstatus _PARMS__((char *msg ) );
void newstatus _PARMS__(( void ) );
void header_info _PARMS__(( void ) );
int setvalue _PARMS__((char *arg, char *value, struct value *v, int l));

#if defined(HAVE_STDARGS)
void log _PARMS__( (int kind, char *msg,...) );
void fatal _PARMS__( ( char *msg,...) );
void logerr _PARMS__( (int kind, char *msg,...) );
void logerr_die _PARMS__( (int kind, char *msg,...) );
int plp_snprintf (char *str, size_t count, const char *fmt, ...);
int vplp_snprintf (char *str, size_t count, const char *fmt, va_list arg);
#else
void log _PARMS__( (void) );
void fatal _PARMS__( (void) );
void logerr _PARMS__( (void) );
void logerr_die _PARMS__( (void) );
int plp_snprintf ();
int vplp_snprintf ();
#endif

const char *Errormsg _PARMS__( (int err ) );
char *Time_str _PARMS__( (void) );
const char *Sigstr _PARMS__ ((int n));

#endif /* _COMMON_H */
