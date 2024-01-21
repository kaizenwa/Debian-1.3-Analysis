/*** analhea2.h; header file for analog version 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/ */

/*** These things are not intended to be user definable;
     user definable headers are in analhead.h ***/

#ifndef ANALHEAD2
#define ANALHEAD2

/*** First some #define's and #include's. Start with system specific ones ***/

#define VNUMBER "2.0"    /* the version number of this program */

#ifdef __MWERKS__
#define MAC        /* So MWERKS compiler automatically switches to Mac */
#endif

#ifdef VERSION
#undef VERSION
#endif

#ifdef MAC
#define MAC_EVENTS
#define WEBSTAR
#define NETPRESENZ
#define NOPIPES
#define MACDIRENT
#include <MacHeaders.h>
#include <sioux.h>
#define VERSION VNUMBER "/Mac"
#define DIRSEP ':'
#endif

#ifdef DOS
#define NODNS
#define VERSION VNUMBER "/DOS"
#define DIRSEP '\\'
#endif

#ifdef VMS
#define NOPIPES
#define VMSDIRENT
#define VERSION VNUMBER "/VMS"
#define DIRSEP '/'
#endif

#ifdef UNIX
#define VERSION VNUMBER "/Unix"
#define DIRSEP '/'
#endif

#ifdef WIN32
#define VERSION VNUMBER "/Win32"
#define DIRSEP '\\'
#endif

/* If no OS is defined, the compilation will fail by not knowing VERSION */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#ifndef NODIRENT
#ifndef VMSDIRENT
#ifndef MACDIRENT
#include <dirent.h>          /* Normal dirent */
#include <sys/types.h>
#include <sys/stat.h>
#else
#include "macdir.h"          /* Mac dirent */
#endif
#else
#define VMS_FSPEC_MAX 256    /* VMS dirent */
#include <descrip.h>
#endif
#endif
#ifndef NODNS
#ifdef MAC
#include <Gestalt.h>
#include <MacTCP.h>
#include <OpenTransport.h>
#include <OpenTptInternet.h>
#include <AddressXlation.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#define INET_ADDR_ERR (-1)   /* what inet_addr() returns on error */
#endif
#endif
#ifdef MAC_EVENTS
#define MAC_IDLE_FREQ (200)
#endif

#ifdef TRUE  /* I suppose everyone else defines TRUE = 1, but I won't */
#undef TRUE  /* risk it by doing an #ifndef! */
#endif
#define TRUE (1)
#ifdef FALSE
#undef FALSE
#endif
#define FALSE (0)
#ifdef ON
#undef ON
#endif
#define ON (TRUE)
#ifdef OFF
#undef OFF
#endif
#define OFF (FALSE)
#ifdef OK
#undef OK
#endif
#define OK (0)
#ifdef NONE
#undef NONE
#endif
#define NONE (0)
#ifdef ERR
#undef ERR
#endif
#define ERR (-1)
#ifdef UNSET
#undef UNSET
#endif
#define UNSET (-1)
#ifdef ENGLISH           /* output languages */
#undef ENGLISH
#endif
#define ENGLISH (0)
#ifdef FRENCH
#undef FRENCH
#endif
#define FRENCH (1)
#ifdef GERMAN
#undef GERMAN
#endif
#define GERMAN (2)
#ifdef ITALIAN
#undef ITALIAN
#endif
#define ITALIAN (3)
#ifdef SPANISH
#undef SPANISH
#endif
#define SPANISH (4)
#ifdef US_ENGLISH
#undef US_ENGLISH
#endif
#define US_ENGLISH (5)
#ifdef DANISH
#undef DANISH
#endif
#define DANISH (6)
#define SENSITIVE (0)    /* as in, "Case ... filesystem" */
#define INSENSITIVE (1)
#define BYREQUESTS (0)       /* ways of sorting */
#define BYBYTES (1)
#define ALPHABETICAL (2)
#define RANDOMLY (3)
#define BYPAGES (4)
#define APPROX (2)       /* for hostname counting */
#define NETPRESENZLINE (4)
#define WEBSTARLINE (3)
#define COMMON (2)
#define NCSAOLD (1)
#define CORRUPT (0)
#define PREFORMATTED (3)   /* output types */
#define CACHE (2)
#define ASCII (1)
#define HTML (0)
#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) (((a)>(b))?(a):(b))
#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) (((a)<(b))?(a):(b))
#ifdef ROUND
#undef ROUND
#endif            /* round double to nearest integer (as double) */
#define ROUND(x) (((x)-(floor(x)))>=0.5?ceil(x):floor(x))
#define STREQ(a,b) (strcmp(a,b)==0)
#ifdef INFINITY
#undef INFINITY
#endif            /* something very big (but that will fit in a long) */
#define INFINITY (2000000000)
#define SUNDAY (0)
#define MONDAY (1)
#define TUESDAY (2)
#define WEDNESDAY (3)
#define THURSDAY (4)
#define FRIDAY (5)
#define SATURDAY (6)
#define ISLEAPFEB(m,y) (((m)==1)&&(((y)%4)==0))  /* Feb in leap year */
#define ISLEAPJF(m,y) (((m)<=1)&&(((y)%4)==0))  /* Jan or Feb in leap year */
/* NB Every 4th year from 1901 to 2099 is a leap year */
#define MAGICNO(x,s,b) {register unsigned char *r; \
			for(x = 0, r = (unsigned char *)s; *r != '\0'; r++) { \
			  x = (x << 1) + (int)(*r); \
			  while (x >= (b)) \
			    x -= b; }}
/* magicno as macro not fn. for speed, tho' appears to make little diff. */
/* x is answer, s is string to be magicked, b is base (no, of hash bins) */

#define DOMHASHSIZE (1354)
/* = 2 * 26^2 + 2 by description of domain algorithm */
#define NO_ERRS (34)   /* how many error types there are (in init.c) */
#define MAXERRLENGTH (40)  /* the longest one */
#define NO_STATUS (22) /* how many status codes are defined */
#define MAXSTATUSLENGTH (35)

/*** Now include the user-definable header things ***/

#include "analhead.h"

/*** Next some global structures ***/

typedef int flag;

struct timestruct {        /* some things we might want to know about a
			      particular time */
  int date;                /* the day of the month */
  int monthno;             /* 0 for Jan, ... , 11 for Dec */
  int year;                /* 4 digits */
  int hr;                  /* 0 to 23 */
  int min;                 /* 0 to 59 */
  long code;               /* an increasing (but not linear) function of time,
			      for quick comparisons. Defined in timecode(). */
};

struct monthly {           /* a structure for a year's worth of monthly data */
  int reqs[12];            /* the number of requests on each month */
  int pages[12];           /* and page requests ditto */
  double bytes[12];        /* and bytes ditto */
  struct monthly *next;    /* where the next year's data is */
};

struct daily {             /* similarly, for a month's worth of daily data */
  int reqs[31];            /* NB Day n is stored in location (n - 1) */
  int pages[31];
  double bytes[31];
  struct daily *next;
};

struct hourly {            /* and for a day's worth of hourly data */
  int reqs[24];
  int pages[24];
  double bytes[24];
  struct hourly *next;
};

struct weekly {             /* a structure for weekly data */
  int reqs;
  int pages;
  double bytes;
  struct timestruct start;  /* the start of the week */
  struct weekly *next;
};

struct genstruct {           /* relevant information about an object */
  char *name;                /* its name as it appears in the logfile */
  char *alias;               /* its name translated, if different */
  int reqs;                  /* its number of requests */
  int pages;                 /* and page requests due to it */
  double bytes;              /* the number of bytes transferred due to it */
  flag pre7;                 /* has been used before last 7 days? */
  flag last7;                /* has been used in the last 7 days? */
  flag wanted;               /* is this item included or excluded? */
  flag ispage;               /* is this item a page? */
  struct genstruct *next;    /* the next object in the list */
};

struct dnscache {            /* information about DNS lookups */
  char *number;              /* the numerical IP address */
  char *alias;               /* the translation (or NULL if unresolvable) */
  time_t altimecode;         /* the time at which its alias was looked up */
  struct dnscache *next;
};

struct domain {              /* and domains */
  char *id;                  /* can be as long as host for subdomains */
  char *revid;               /* same reversed */
  char *name;                /* the geographical location of the domain */
  int reqs;
  int pages;
  double bytes;
  int nexti;                 /* the index of the next domain alphabetically */
  struct domain *next;       /* the next subdomain of the current domain */
};

struct alias {      /* aliases from the configuration file */
  char from[MAXSTRINGLENGTH];
  char to[MAXSTRINGLENGTH];
  struct alias *next;
};

struct include {   /* a list of included/excluded things */
  char name[MAXSTRINGLENGTH];
  int in;           /* TRUE, FALSE or UNSET */
  struct include *next;
};

struct loglist {   /* a list of logfiles */
  char name[MAXSTRINGLENGTH];
  char prefix[MAXSTRINGLENGTH];
  struct loglist *next;
};

struct stringlist {   /* a simple list of strings */
  char name[MAXSTRINGLENGTH];
  struct stringlist *next;
};

/*** Finally, a list of all the functions defined in analog ***/
/* in alias.c */
extern flag doaliaslist(char *name, struct alias *listhead);
#ifndef NODNS
flag dnsresolve(char *hostn);
#endif
extern int doaliashost(char *hostn);
extern char *reversehostname(char *hostn);
extern int doaliasfile(char *filename);
extern int doaliasref(char *name);
extern int doalias(char *name, char codeletter);
extern void allaliases(struct genstruct **objhead,
  struct genstruct **objhead2, int hashsize, int *totalobjs, int *totalobjs7,
  int *totalnew7, char code);
extern int hosttodomcode(char *hostn);
extern void urltodir(char *filename);
extern char *urltoext(char *filename);
extern flag included(char *name, flag ispage, struct include *listhead);
extern flag itemwanted(char *name, flag ispage, char codeletter);
/* in analog.c -- only main() */
/* in formgen.c */
extern void genopts(FILE *outf, char name[17], char plural[16], int sortby,
  char codeletter);
extern void repchoice(FILE *outf, char name[MAXSTRINGLENGTH], char codeletter,
  flag qq);
extern void formgen(void);
/* in hash.c */
extern struct genstruct *hashadd(struct genstruct **objhead, int hashsize,
  char *name, int reqs, double bytes, int pages, flag last7q, int *totalobjs,
  int *totalobjs7, int *totalnew7, flag al, flag maskq, flag ispage,
  struct genstruct *freespace, int magicnumber, char codeletter);
extern struct dnscache *dnshashadd(char *number, char *alias,
  time_t altimecode);
extern void domhashadd(char *hostn, int reqs, int pages, double bytes);
extern void subdomadd(char *id, char *name);
extern void addref(char *fromurl, char *filename, flag ispage, double bytes,
  flag last7q, flag filemaskq);
extern void addbrowser(char *browser, flag ispage, double bytes, flag last7q);
extern void adderr(char *errstr);
extern flag approxhostfilled(char *space, unsigned int i);
extern void approxhostfill(char *space, unsigned int i);
extern void approxhosthashadd(char *hostn, flag last7q);
extern void addmonthlydata(int year, int monthno, int reqs, int pages,
  double bytes);
extern void adddailydata(int year, int monthno, int date, int reqs, int pages,
  double bytes);
extern void addhourlydata(int year, int monthno, int date, int hr, int reqs,
  int pages, double bytes);
extern void addweeklydata(int year, int monthno, int date, int reqs, int pages,
  double bytes);
extern void datehash(int year, int monthno, int date, int hr, int min,
  long thistimecode, int reqs, int pages, double bytes);
extern void errsort(int errorder[NO_ERRS]);
extern double bytefloor(double bytes, char *str);
extern int reqfloor(int reqs, char *str);
extern struct genstruct *gensort(struct genstruct **objhead, int hashsize,
  int tot_reqs, int tot_pages, double tot_bytes, int sortby, char *minreqstr,
  char *minpagestr, char *minbytestr, struct include *includehead,
  flag alphahost, int *maxreqs, int *maxpages, double *maxbytes,
  int *maxlength);
extern int domsort(void);
extern void subdomsort(void);
/* in init.c */
extern void defaults(void);
extern void init_structs(void);
extern void othervars(void);
extern void configline(char inputline[MAXLINELENGTH]);
extern flag config(char *filename);
extern void commandline(int argc, char **argv);
extern void initialise(int argc, char **argv);
extern void pvfilelist(struct stringlist *head,
  char filetype[MAXSTRINGLENGTH]);
extern void printvbles(void);
/* in init2.c */
extern void domainscan(void);
extern void dnscachescan(void);
extern void configwarning(char *comname, char *inputline);
extern void configwarning2(char *inputline);
extern void configwarning3(char *comname, char *inputline);
extern void addonelogfile(struct loglist **p, char name[MAXSTRINGLENGTH],
  char prefix[MAXSTRINGLENGTH]);
extern void addwildlogs(struct loglist **p, char name[MAXSTRINGLENGTH],
  char prefix[MAXSTRINGLENGTH]);
extern void addlogfile(struct loglist **p, char name[MAXSTRINGLENGTH],
  char prefix[MAXSTRINGLENGTH], flag wildexpand);
extern void includeone(char *name, struct include **p, struct include *head,
  int in, char *comname, char *inputline);
extern void include(char *name, struct include **p, struct include *head,
  int in, char *comname, char *inputline, int rc, flag *maskq);
extern void configalias(char *from, char *to, struct alias **p, char *comname,
  char *inputline, int rc);
extern void fromtodate(char *tstr, struct timestruct *t, flag from,
  char *comname, char *inputline, int rc);
extern void configstr(char *name, char *target, char *comname, char *inputline,
  int rc);
extern void addonestrlist(struct stringlist **p, char *name);
extern void addwildstrlist(struct stringlist **p, char *name);
extern void configstrlist(char *name, struct stringlist **p, char *comname,
  char *inputline, int rc, flag wildexpand);
extern void configcols(char *cols, char *target, char *comname,
  char *inputline, int rc);
extern void configchar(char *str, char *target, char *comname, char *inputline,
  int rc);
extern void configint(char *number, int *target, char *comname,
  char *inputline, int rc);
extern void configsizet(char *number, size_t *target, char *comname,
  char *inputline, int rc);
extern void configsortby(char *method, int *target, char *comname,
  char *inputline, int rc);
extern void onoff(char *method, flag *target, char *comname, char *inputline,
  int rc);
extern void clflag(flag *f, char *arg);
extern void cldaterep(flag *f, char *graph, char *arg);
extern void clgenrep(flag *f, int *sortby, char *minreqstr, char *minpagestr,
  char *minbytestr, char *arg);
extern flag clfile(char *filename, char *arg);
extern void alias_to_lower(struct alias *head);
extern void include_to_lower(struct include *head);
extern void pvcols(char *cols);
extern void pvtime(char name[15], flag q, char graph, int unit, char cols[],
  int rows);
extern void pvgen(char name[17], flag q, int sortby, char *minreqstr,
  char *minpagestr, char *minbytestr, char cols[], char singular[20],
  char plural[21]);
extern void pvinout(char name[14], struct include *head);
extern void pvalias(char name[8], struct alias *head);
/* in macstuff.c */
#ifdef MAC_EVENTS
extern void MacInit(void);
extern void MacFini(void);
extern void MacIdle(void);
#endif
#ifdef MAC
#ifndef NODNS
int IpAddr2Name(char *hostname);
void ResolverCleanup(void);
#endif
#endif
/* in output.c */
extern void output(struct genstruct *rsorthead, struct genstruct *isorthead,
  struct genstruct *tsorthead, struct genstruct *Ssorthead, int firstdom,
  struct genstruct *fsorthead, struct genstruct *bsorthead,
  struct genstruct *Bsorthead, int errorder[]);
/* in output2.c */
extern void gotos(FILE *outf, char c);
extern void htmlputc(char c, FILE *outf);
extern void htmlfprintf(FILE *outf, char string[MAXSTRINGLENGTH]);
extern double finddivider(double bytes, char *bprefix);
extern void asciiline(FILE *outf);
extern void barplot(FILE *outf, int n);
extern void precols(FILE *outf, char *wantcols, char codeletter, flag byq,
  flag pageq);
extern void printcolheads(FILE *outf, char *wantcols, int fieldwidth,
  int pfieldwidth, int bfieldwidth, char bprefix[2], char name[20], char type,
  flag byq, flag pageq, flag name1st);
extern void printcols(FILE *outf, char *wantcols, int reqs, int pages,
  double bytes, int fieldwidth, int pfieldwidth, int bfieldwidth,
  double bdivider, int totreqs, int totpages, double totbytes, char type,
  flag byq, flag pageq);
extern int whatincluded(FILE *outf, int sortby, char *minreqstr,
  char *minpagestr, char *minbytestr, char singular[27], char plural[29],
  flag subdoms, char gender);
extern int whatincludednop(int sortby, char *minreqstr, char *minpagestr,
  char *minbytestr);
extern void genout(FILE *outf, struct genstruct *sorthead, int tot_reqs,
  int tot_pages, double tot_bytes, int sortby, char *minreqstr,
  char *minpagestr, char *minbytestr, int max_reqs, int max_pages,
  double max_bytes, char *wantcols, char anchor[10], char title[36],
  char singular[22], char plural[24], char colhead[24], char gender,
  char codeletter, flag alphahost, flag byq, flag pageq, struct include *links,
  struct alias *aka, char baseurl[MAXSTRINGLENGTH]);
extern void domout(FILE *outf, int firstdom);
extern void datehead(FILE *outf, int maxreq, int maxpages, double maxbytes,
  char *wantcols, char *graphtype, char anchor[11], char title[21],
  char htmltitle[31], char colhead[13], char codeletter, int *unit,
  int *fieldwidth, int *pfieldwidth, int *bfieldwidth, int *graphwidth,
  double *bdivider);
extern void dateline(FILE *outf, int reqs, int pages, double bytes,
  char *wantcols, char graphtype, int fieldwidth, int pfieldwidth,
  int bfieldwidth, int unit, double bdivider);
extern void statusout(FILE *outf);
extern void errout(FILE *outf, int errorder[NO_ERRS]);
extern void gensum(FILE *outf);
/* in sscanf.c */
extern int sscanf_date(char *inputline, int *date, int *monthno, int *year,
  int *hr, int *min);
extern int sscanf_olddate(char *inputline, int *date, int *monthno, int *year,
  int *hr, int *min);
extern int sscanf_common(char *inputline, char hostn[MAXSTRINGLENGTH],
  int *date, int *monthno, int *year, int *hr, int *min,
  char filename[MAXSTRINGLENGTH], char referrer[MAXSTRINGLENGTH],
  char agent[MAXSTRINGLENGTH], int *code, char bytestr[16], size_t preflength);
extern int sscanf_ncsaold(char *inputline, char hostn[MAXSTRINGLENGTH],
  int *monthno, int *date, int *hr, int *min, int *year,
  char filename[MAXSTRINGLENGTH], size_t preflength);
extern int sscanf_domains(char *inputline, char string1[MAXSTRINGLENGTH],
  char string2[MAXSTRINGLENGTH]);
extern int sscanf_config(char *inputline, char string1[MAXSTRINGLENGTH],
  char string2[MAXSTRINGLENGTH], char string3[MAXSTRINGLENGTH]);
extern int sscanf_referrer(char *inputline, int *date, int *monthno, int *year,
  int *hr, int *min, char from[MAXSTRINGLENGTH], char to[MAXSTRINGLENGTH]);
#ifdef WEBSTAR
extern int sscanf_webstar(char *inputline, char hostn[MAXSTRINGLENGTH],
  int *date, int *monthno, int *year, int *hr, int *min,
  char filename[MAXSTRINGLENGTH], char referrer[MAXSTRINGLENGTH],
  char agent[MAXSTRINGLENGTH], int *code, char bytestr[16], size_t preflength);
#endif
#ifdef NETPRESENZ
typedef enum { NP_HTTP, NP_GOPHER, NP_ADMIN, NP_FTP, NP_GETFILE, NP_GETDIR,
		 NP_LOGIN, NP_LOGOUT, NP_CGI, NP_ERROR } NP_type;
typedef struct { char *str; NP_type value; } MatchPairs;
NP_type checkValue(char *typeStr, const MatchPairs table[]);
NP_type checkType(char *typeStr);
void build_agent(char agent[MAXSTRINGLENGTH], NP_type entryType,
  char *entryTypeStr, NP_type actionType, char *actionTypeStr);
int readDigits(char **from);
int scan_path(char *inputline, char filename[MAXSTRINGLENGTH], int maxlen,
  struct include *expandhead);
int sscanf_netpresenz(FILE *logfile, char *inputline,
  char hostn[MAXSTRINGLENGTH], int *date, int *monthno, int *year, int *hr,
  int *min, char filename[MAXSTRINGLENGTH], char referer[MAXSTRINGLENGTH],
  char agent[MAXSTRINGLENGTH], int *code, char bytestr[16], size_t preflength);
#endif
/* in utils.c */
extern flag matchq(char *string, char *pattern, flag whole);
extern flag wildmatch(char *string, char *pattern, char **w1, char **w2);
extern int strtomonth(char month[4]);
extern int dayofdate(int date, int monthno, int year);
extern int minsbetween(int date1, int monthno1, int year1, int hr1, int min1,
  int date2, int monthno2, int year2, int hr2, int min2);
extern long timecode(int date, int monthno, int year, int hr, int min);
extern struct timestruct startofweek(struct timestruct atime);
extern FILE *fopenlog(char *name, char logtype[12], flag *ispipe);
extern int fcloselog(FILE *f, char *name, char logtype[12], flag ispipe);
extern void int3printf(FILE *outf, int x, char sepchar, int fieldwidth);
extern void double3printf(FILE *outf, double x, char sepchar, int fieldwidth);
extern void doublefprintf(FILE *outf, double x);
extern void *xmalloc(size_t size);
extern void *xcalloc(size_t nelem, size_t elsize);
extern char *strtolower(char *string);
extern flag strtolowerf(char *string);
extern char *strtoupper(char *string);
extern int hoststrcmp(char *hostn1, char *hostn2);
extern flag isnumeric(char *name);
#endif  /* not previously included */
