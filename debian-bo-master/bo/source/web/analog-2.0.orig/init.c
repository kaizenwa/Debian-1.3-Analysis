/*** analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** init.c; initialisation routines and declaration of global variables ***/
/* See also init2.c */

#include "analhea2.h"

/*** First declare all global variables. We choose to declare them in this
     file rather than analog.c because more are needed here. ***/

/* lang: need new dayname and monthname here, possibly in HTML and non-HTML
   versions. */
char endayname[7][11] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
char frdayname[7][11] = {"Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam"};
char dedayname[7][11] = {"Son", "Mon", "Die", "Mit", "Don", "Fre", "Sam"};
char itdayname[7][11] = {"Dom", "Lun", "Mar", "Mer", "Gio", "Ven", "Sab"};
char esdayname[7][11] = {"Dom", "Lun", "Mar", "Mie", "Jue", "Vie", "Sab"};
char dkhtmldayname[7][11] = {"s&oslash;n", "man", "tir", "ons", "tor", "fre",
				 "l&oslash;r"};
char dkdayname[7][11] = {"son", "man", "tir", "ons", "tor", "fre",
				 "lor"};
char enmonthname[12][12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
			      "Aug", "Sep", "Oct", "Nov", "Dec"};
char frmonthname[12][12] = {"Janv", "Fevr", "Mars", "Avri", " Mai", "Juin",
			      "Juil", "Aout", "Sept", "Octo", "Nove", "Dece"};
char frhtmlmonthname[12][12] = {"Janv", "F&eacute;vr", "Mars", "Avri", " Mai",
				  "Juin", "Juil", "Ao&ucirc;t", "Sept", "Octo",
				  "Nove", "D&eacute;ce"};
char demonthname[12][12] = {"Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul",
			      "Aug", "Sep", "Okt", "Nov", "Dez"};
char dehtmlmonthname[12][12] = {"Jan", "Feb", "M&auml;r", "Apr", "Mai", "Jun",
				  "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"};
char itmonthname[12][12] = {"Gen", "Feb", "Mar", "Apr", "Mag", "Giu",
			      "Lug", "Ago", "Set", "Ott", "Nov", "Dic"};
char esmonthname[12][12] = {"Ene", "Feb", "Mar", "Abr", "May", "Jun",
			      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"};
char dkmonthname[12][12] = {"jan", "feb", "mar", "apr", "maj", "jun",
			      "jul", "aug", "sep", "okt", "nov", "dec"};
char monthname[12][12];     /* set to whichever is appropriate */
char dayname[7][11];
/* Note: month numbers run from 0 (Jan) to 11 (Dec) internally to this
   program (though not where month numbers are needed in user input and
   output) */
int dateoffset[12] = {0, 31, 59, 90, 120, 151, 181,
			212, 243, 273, 304, 334};
int monthlength[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

char errs[NO_ERRS][MAXERRLENGTH] = {"Send timed out", "File does not exist",
"No file matching URL", "User does not exist", "Send aborted",
"Timed out waiting", "File permissions deny server access",
"Read timed out", "Lost connection to client", "Cannot read directory",
"Client denied by server configuration", "Malformed header from script",
"Killing CGI process", "Could not get local address", "Will not follow link",
"Could not get port number", "Script does not exist", "Script not found",
"Unable to include", "Could not fork new process",
"Unable to fork new process", "Invalid CGI ref",
"Caught SIGTERM, shutting down", "Caught SIGSEGV, dumping core",
"Caught SIGHUP, restarting", "SIGHUP received.  Attempting to restart",
"Successful restart", "Resuming normal operations", "Starting",
"Child error: pass failed", "Socket error: accept failed",
"Child connection closed", "File open error", ""};
      /* last must be "" */    
int errors[NO_ERRS];   /* how many occurrences of each */
int statusnos[NO_STATUS] = {200, 201, 202, 203, 204, 299, 301, 302, 303, 304,
399, 400, 401, 402, 403, 404, 499, 500, 501, 502, 503, 599};
/* all status codes */
char statusstrs[NO_STATUS][MAXSTATUSLENGTH] = {"OK", "Created",
"Accepted for future processing", "Partial information",
"OK, but nothing to send", "[Miscellaneous successes]", "Document moved",
"Document found elsewhere", "Moved; use another request method",
"Not modified since last retrieval", "[Miscellaneous redirections]",
"Bad request", "Authorisation required", "Payment required", "Forbidden",
"Document not found", "[Miscellaneous client/user errors]",
"Internal server error", "Request type not supported",
"Server overloaded", "Gateway timed out", "[Miscellaneous server errors]"};
int status[NO_STATUS];   /* how many of each have been seen */
int status7[NO_STATUS];  /* ditto in last 7 days */

char *outfile;

struct timestruct firsttime, lasttime, starttimec, oldtime, fromtime, totime;
/* first and last log entries, now, a week before now, and
   lower and upper bounds on time interval we want to consider */

struct monthly *firstm, *lastm;
    /* pointers to the first and last years of monthly data */
struct daily *firstD, *lastD;   /* ... months of daily data */
struct hourly *firstH, *lastH;  /* ... days of hourly data */
struct weekly *firstW, *lastW;  /* ... and to the first and last week */

int no_urls;               /* the number of distinct files found so far */
int no_urls7;              /* the number used in the last 7 days */
struct genstruct **rhead;  /* hash table of all files found */
struct genstruct **ihead;  /* and one for all directories */
struct genstruct **thead;  /* one for file types */
struct domain **Ohead;     /* one for subdomains */
struct domain *wildOhead;  /* and one for wild subdomains */
struct domain *nwildOhead; /* and one for wild numerical subdomains */
int no_hosts;              /* the number of hosts so far */
int no_hosts7;             /* the number of all hosts in the last 7 days */
int no_new_hosts7;         /* the number of new hosts in the last 7 days */
struct genstruct **Shead;  /* and a record of all hosts */
char *approxhostspace;     /* start of the space for approx host counting */
char *approxhostspace7;    /* and a bit more for last 7 days accounting */
size_t approxhostsize;     /* the size of those spaces, in bytes */
struct domain **ohead;
struct genstruct **fhead;
struct genstruct **bhead;
struct genstruct **Bhead;
struct genstruct **Shead2; /* for filing pre-aliased filenames */
struct genstruct **rhead2;
struct genstruct **fhead2;
size_t rhashsize, ihashsize, thashsize, Shashsize, fhashsize, bhashsize;
size_t Bhashsize, Ohashsize;  /* sizes of the hash tables */

struct alias *filealiashead, *hostaliashead, *refaliashead, *browaliashead;
struct alias *routaliashead, *ioutaliashead, *Soutaliashead, *foutaliashead;
struct alias *boutaliashead, *toutaliashead, *subdomshead;
    /* subdomshead is not really an alias, but it's the right shape */
struct include *wantfilehead, *wanthosthead, *wantrefhead, *wantreqhead;
struct include *linkhead, *reflinkhead, *ispagehead, *noexpandhead;
struct include *refexpandhead;

int dreq[7], dpag[7];    /* requests and page requests for each day */
int hreq[24], hpag[24];  /* ... and hour */
double dbytes[7];        /* ... and bytes ditto */
double hbytes[24];

/*** NB It often happens that families of variables have a one letter prefix
     representing the different reports (usual letters, plus O = subdoms). ***/
int rmaxreqs;     /* the max reqs for any file */
int imaxreqs;     /* for any directory */
int tmaxreqs;     /* for any file type */
int Smaxreqs;     /* for any host */
int omaxreqs;     /* for any domain */
int fmaxreqs;     /* for any referrer */
int bmaxreqs;     /* for any browser */
int Bmaxreqs;     /* for any browser (full name) */
/* Now the same for max pages and max bytes*/
int rmaxpages, imaxpages, tmaxpages, Smaxpages, omaxpages, fmaxpages;
int bmaxpages, Bmaxpages;
double rmaxbytes, imaxbytes, tmaxbytes, Smaxbytes, omaxbytes, fmaxbytes;
double bmaxbytes, Bmaxbytes;

int Smaxlength;     /* and the same for length of name */

char *dirsuffix;       /* e.g. index.html */
int dirsufflength;     /* the length of dirsuffix */
int onumber, Onumber;  /* the number of (sub)domains in the domain report */
time_t starttime, stoptime;   /* the start and stop time of the program */
struct tm *starttimetm;
int total_succ_reqs, total_fail_reqs, total_other_reqs, total_page_reqs;
int total_succ_reqs7, total_fail_reqs7, total_other_reqs7, total_page_reqs7;
int total_good_brows, total_masked_brows, total_bad_brows;
int total_good_refs, total_masked_refs, total_bad_refs;
int total_ref_pages, total_brow_pages;
double total_bytes, total_bytes7, total_ref_bytes, total_brow_bytes;
int corrupt_lines;         /* the number of corrupt lines in the logfile */
                             /* (Overlong lines, URLs with quotes in...) */
int other_lines;           /* Lines masked out */
int cachereqs, cachereqs7; /* The number of requests from cache */
int cachepages, cachepages7; /* Page requests from cache */
flag byq, refbyq, browbyq; /* whether we want to count bytes; always on until
			      we find a line with no bytes information. */
flag rawbytes;   /* whether bytes are quoted raw or in k notation */
flag graphical;  /* whether to draw pretty graphs or just use ASCII art */
flag filemaskq, hostmaskq, refmaskq;
/* whether there is any masking of hosts, files or refs */

int weekbeginson;  /* which is the first day of the week? */
char sepchar, repsepchar, decpoint;   /* chars as separators in numbers */
char *presep;  /* string separating fields in PREFORMATTED */

char *commandname;  /* the name of the program, as called */

char *baseurl;         /* base for all URLs found */
char rcols[7], icols[7], tcols[7], Scols[7], ocols[7], fcols[7], bcols[7];
char Bcols[7], mcols[7], dcols[7], Dcols[7], Wcols[7], hcols[7], Hcols[7];
char ccols[7], ecols[7];
/* Columns appearing in each report */

struct loglist *logfilehead, *uncompresshead;
/* uncompress is not really a loglist, but it's the right shape */
struct stringlist *cachefilehead, *refloghead, *browloghead, *errloghead;
char *domainsfile, *headerfile, *footerfile, *logourl, *imagedir;
char reportorder[20];  /* the order in which reports occur (room for more) */
/* NB if the size of this changes, so must it in case (REPORTORDER_): below */
flag xq, mq, oq, iq, tq, rq, q7, dq, hq, Hq, Sq, Dq, Wq, fq, bq, Bq, cq, eq;
int sq, aq;  /* a: output type     x->s: whether we want each type of report */
#ifndef NODNS
flag dnsq;            /* DNS lookup? */
struct dnscache **dnshead;
char *dnsfile;
size_t dnshashsize;
time_t dnsstaletime;  /* older than when the DNS information becomes stale */
int dnsfreshhours;    /* how many hours it stays fresh for */
#endif
int lang, dialect;     /* language for output */
flag warnq, anywarns;  /* do we want warnings? any warnings issued? */
flag html2;      /* if aq == HTML, whether output is guaranteed HTML2 */
flag case_insensitive, stdin_used;
char *ominreqstr, *Ominreqstr, *Sminreqstr, *iminreqstr, *rminreqstr;
char *fminreqstr, *bminreqstr, *Bminreqstr, *tminreqstr;
char *ominpagestr, *Ominpagestr, *Sminpagestr, *iminpagestr, *rminpagestr;
char *fminpagestr, *bminpagestr, *Bminpagestr, *tminpagestr;
char *ominbytestr, *Ominbytestr, *Sminbytestr, *iminbytestr, *rminbytestr;
char *fminbytestr, *bminbytestr, *Bminbytestr, *tminbytestr;
int eminreqs;
int munit, Wunit, dunit, Dunit, hunit, Hunit;  /* the value of the mark */
char mgraph, dgraph, Dgraph, hgraph, Hgraph, Wgraph;  /* R or B */
flag mback, Dback, Wback, Hback;   /* backwards graphs? */
int mrows, Drows, Wrows, Hrows, Wrowsdone;    /* max rows in each report */
int osortby, isortby, tsortby, Ssortby, rsortby, fsortby, bsortby, Bsortby;
int dirlevel, pagewidth;
char markchar;
char *hostname, *hosturl;
int debug, progressfreq, no_configs = 0;

flag vblesonly;  /* commandline only. If ON, just print variables and exit */
flag formq;      /* commandline only. If ON, just make a form interface */

/* We also declare the following pointers here. They are used to keep track
   of places in lists between functions in this file and init2.c */

struct alias *filealiasp, *hostaliasp, *refaliasp, *browaliasp, *routaliasp;
struct alias *ioutaliasp, *Soutaliasp, *foutaliasp, *boutaliasp, *toutaliasp;
struct alias *subdomp, *subdomtempp;
struct include *wantfilep, *wanthostp, *wantrefp, *ispagep, *noexpandp;
struct include *refexpandp, *wantreqp, *linkp, *reflinkp;
struct loglist *logfilep, *uncompressp;
struct stringlist *cachefilep, *reflogp, *browlogp, *errlogp;

/*** Now boring functions for initialisation of variables etc. ***/

void defaults(void)
{
  anywarns = OFF;
  html2 = ON;
  stdin_used = OFF;
  domainsfile = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(domainsfile, DOMAINSFILE, MAXSTRINGLENGTH - 1);
  domainsfile[MAXSTRINGLENGTH - 1] = '\0';
  headerfile = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(headerfile, HEADERFILE, MAXSTRINGLENGTH - 1);
  headerfile[MAXSTRINGLENGTH - 1] = '\0';
  footerfile = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(footerfile, FOOTERFILE, MAXSTRINGLENGTH - 1);
  footerfile[MAXSTRINGLENGTH - 1] = '\0';
  outfile = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(outfile, OUTFILE, MAXSTRINGLENGTH - 1);
  outfile[MAXSTRINGLENGTH - 1] = '\0';
#ifndef NODNS
  dnsfile = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(dnsfile, DNSFILE, MAXSTRINGLENGTH - 1);
  dnsfile[MAXSTRINGLENGTH - 1] = '\0';
  dnsq = NUMLOOKUP;
  dnsfreshhours = DNSFRESHHOURS;
  dnshashsize = DNSHASHSIZE;
#endif
  baseurl = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(baseurl, BASEURL, MAXSTRINGLENGTH - 1);
  baseurl[MAXSTRINGLENGTH - 1] = '\0';
  imagedir = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(imagedir, IMAGEDIR, MAXSTRINGLENGTH - 1);
  imagedir[MAXSTRINGLENGTH - 1] = '\0';
  dirsuffix = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(dirsuffix, DIRSUFFIX, MAXSTRINGLENGTH - 1);
  dirsuffix[MAXSTRINGLENGTH - 1] = '\0';
  strncpy(reportorder, REPORTORDER, 19);
  reportorder[19] = '\0';
  strncpy(ocols, DOMCOLS, 6);
  ocols[6] = '\0';
  strncpy(Scols, HOSTCOLS, 6);
  Scols[6] = '\0';
  strncpy(icols, DIRCOLS, 6);
  icols[6] = '\0';
  strncpy(tcols, TYPECOLS, 6);
  tcols[6] = '\0';
  strncpy(rcols, REQCOLS, 6);
  rcols[6] = '\0';
  strncpy(fcols, REFCOLS, 6);
  fcols[6] = '\0';
  strncpy(bcols, BROWCOLS, 6);
  bcols[6] = '\0';
  strncpy(Bcols, FULLBROWCOLS, 6);
  Bcols[6] = '\0';
  strncpy(mcols, MONTHCOLS, 6);
  mcols[6] = '\0';
  strncpy(dcols, DAYCOLS, 6);
  dcols[6] = '\0';
  strncpy(Dcols, FULLDAYCOLS, 6);
  Dcols[6] = '\0';
  strncpy(Wcols, WEEKCOLS, 6);
  Wcols[6] = '\0';
  strncpy(hcols, HOURCOLS, 6);
  hcols[6] = '\0';
  strncpy(Hcols, FULLHOURCOLS, 6);
  Hcols[6] = '\0';
  strcpy(ccols, "R");   /* for the moment */
  strcpy(ecols, "R");
  logourl = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(logourl, LOGOURL, MAXSTRINGLENGTH - 1);
  logourl[MAXSTRINGLENGTH - 1] = '\0';
  mq = MONTHLY;
  Wq = WEEKLY;
  dq = DAILY;
  Dq = FULLDAILY;
  hq = HOURLY;
  Hq = FULLHOURLY;
  oq = DOMAINREP;
  iq = DIRECTORY;
  tq = FILETYPE;
  rq = REQUEST;
  sq = COUNTHOSTS;
  Sq = FULLHOSTS;
  fq = REFERRER;
  bq = BROWSER;
  Bq = FULLBROWSER;
  cq = STATUS;
  eq = ERROR;
  xq = GENERAL;
  q7 = LASTSEVEN;
  aq = OUTPUT;
  lang = LANGUAGE;
  dialect = NONE;
  warnq = WARNINGS;
  progressfreq = PROGRESSFREQ;
  graphical = GRAPHICAL;
  case_insensitive = (CASE == INSENSITIVE);
  munit = 0;
  Wunit = 0;
  hunit = 0;
  Hunit = 0;
  dunit = 0;
  Dunit = 0;
  mgraph = MONTHGRAPH;
  dgraph = DAYGRAPH;
  Dgraph = FULLDAYGRAPH;
  hgraph = HOURGRAPH;
  Hgraph = FULLHOURGRAPH;
  Wgraph = WEEKGRAPH;
  Hback = FULLHOURLYBACK;
  Dback = FULLDAILYBACK;
  Wback = WEEKLYBACK;
  mback = MONTHLYBACK;
  Wrows = WEEKROWS;
  Drows = FULLDAYROWS;
  mrows = MONTHROWS;
  Hrows = FULLHOURROWS;
  osortby = DOMSORTBY;
  isortby = DIRSORTBY;
  tsortby = TYPESORTBY;
  rsortby = REQSORTBY;
  Ssortby = HOSTSORTBY;
  fsortby = REFSORTBY;
  bsortby = BROWSORTBY;
  Bsortby = FULLBROWSORTBY;
  ominreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(ominreqstr, MIN_DOM_REQS, MAXSTRINGLENGTH - 1);
  ominreqstr[MAXSTRINGLENGTH - 1] = '\0';
  ominpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(ominpagestr, MIN_DOM_PAGES, MAXSTRINGLENGTH - 1);
  ominpagestr[MAXSTRINGLENGTH - 1] = '\0';
  ominbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(ominbytestr, MIN_DOM_BYTES, MAXSTRINGLENGTH - 1);
  ominbytestr[MAXSTRINGLENGTH - 1] = '\0';
  Ominreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Ominreqstr, MIN_SUBDOM_REQS, MAXSTRINGLENGTH - 1);
  Ominreqstr[MAXSTRINGLENGTH - 1] = '\0';
  Ominpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Ominpagestr, MIN_SUBDOM_PAGES, MAXSTRINGLENGTH - 1);
  Ominpagestr[MAXSTRINGLENGTH - 1] = '\0';
  Ominbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Ominbytestr, MIN_SUBDOM_BYTES, MAXSTRINGLENGTH - 1);
  Ominbytestr[MAXSTRINGLENGTH - 1] = '\0';
  iminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(iminreqstr, MIN_DIR_REQS, MAXSTRINGLENGTH - 1);
  iminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  iminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(iminpagestr, MIN_DIR_PAGES, MAXSTRINGLENGTH - 1);
  iminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  iminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(iminbytestr, MIN_DIR_BYTES, MAXSTRINGLENGTH - 1);
  iminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  tminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(tminreqstr, MIN_TYPE_REQS, MAXSTRINGLENGTH - 1);
  tminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  tminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(tminpagestr, MIN_TYPE_PAGES, MAXSTRINGLENGTH - 1);
  tminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  tminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(tminbytestr, MIN_TYPE_BYTES, MAXSTRINGLENGTH - 1);
  tminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  Sminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Sminreqstr, MIN_HOST_REQS, MAXSTRINGLENGTH - 1);
  Sminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  Sminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Sminpagestr, MIN_HOST_PAGES, MAXSTRINGLENGTH - 1);
  Sminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  Sminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Sminbytestr, MIN_HOST_BYTES, MAXSTRINGLENGTH - 1);
  Sminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  rminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(rminreqstr, MIN_URL_REQS, MAXSTRINGLENGTH - 1);
  rminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  rminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(rminpagestr, MIN_URL_REQS, MAXSTRINGLENGTH - 1);  /* sic */
  rminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  rminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(rminbytestr, MIN_URL_BYTES, MAXSTRINGLENGTH - 1);
  rminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  fminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(fminreqstr, MIN_REF_REQS, MAXSTRINGLENGTH - 1);
  fminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  fminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(fminpagestr, MIN_REF_PAGES, MAXSTRINGLENGTH - 1);
  fminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  fminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(fminbytestr, MIN_REF_BYTES, MAXSTRINGLENGTH - 1);
  fminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  bminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(bminreqstr, MIN_BROW_REQS, MAXSTRINGLENGTH - 1);
  bminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  bminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(bminpagestr, MIN_BROW_PAGES, MAXSTRINGLENGTH - 1);
  bminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  bminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(bminbytestr, MIN_BROW_BYTES, MAXSTRINGLENGTH - 1);
  bminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  Bminreqstr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Bminreqstr, MIN_FULLBROW_REQS, MAXSTRINGLENGTH - 1);
  Bminreqstr[MAXSTRINGLENGTH - 1] = '\0';
  Bminpagestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Bminpagestr, MIN_FULLBROW_PAGES, MAXSTRINGLENGTH - 1);
  Bminpagestr[MAXSTRINGLENGTH - 1] = '\0';
  Bminbytestr = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(Bminbytestr, MIN_FULLBROW_BYTES, MAXSTRINGLENGTH - 1);
  Bminbytestr[MAXSTRINGLENGTH - 1] = '\0';
  eminreqs = MIN_ERR_OCCS;
  dirlevel = DIRLEVEL;
  pagewidth = PAGEWIDTH;
  markchar = MARKCHAR;
  rawbytes = RAWBYTES;
  hostname = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(hostname, HOSTNAME, MAXSTRINGLENGTH - 1);
  hostname[MAXSTRINGLENGTH - 1] = '\0';
  hosturl = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(hosturl, HOSTURL, MAXSTRINGLENGTH - 1);
  hosturl[MAXSTRINGLENGTH - 1] = '\0';
  weekbeginson = WEEKBEGINSON;
  sepchar = SEPCHAR;
  repsepchar = REPSEPCHAR;
  decpoint = DECPOINT;
  presep = (char *)xmalloc(MAXSTRINGLENGTH);
  strncpy(presep, PRESEP, MAXSTRINGLENGTH - 1);
  presep[MAXSTRINGLENGTH - 1] = '\0';
  vblesonly = OFF;
  formq = OFF;
  fromtime.code = -INFINITY;
  totime.code = INFINITY;
  approxhostsize = APPROXHOSTSIZE;
  debug = DEBUG;
  filemaskq = OFF;
  hostmaskq = OFF;
  refmaskq = OFF;
  rhashsize = REQHASHSIZE;
  ihashsize = DIRHASHSIZE;
  thashsize = TYPEHASHSIZE;
  Shashsize = HOSTHASHSIZE;
  fhashsize = REFHASHSIZE;
  bhashsize = BROWHASHSIZE;
  Bhashsize = FULLBROWHASHSIZE;
  Ohashsize = SUBDOMHASHSIZE;
}

void init_structs(void)
{
  char tempstr[MAXSTRINGLENGTH];

  logfilehead = (struct loglist *)xmalloc(sizeof(struct loglist));
  logfilep = logfilehead;
  if (STREQ(LOGFILE, "none"))
    logfilehead -> name[0] = '\0';
  else {
    strcpy(tempstr, LOGFILE);
    addlogfile(&logfilep, tempstr, "", ON);
  }
  cachefilehead = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  cachefilep = cachefilehead;
  if (STREQ(CACHEFILE, "none"))
    cachefilehead -> name[0] = '\0';
  else {
    strcpy(tempstr, CACHEFILE);
    configstrlist(tempstr, &cachefilep, (char *)NULL, (char *)NULL, 2, ON);
  }
  refloghead = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  reflogp = refloghead;
  if (STREQ(REFERRER_LOG, "none"))
    refloghead -> name[0] = '\0';
  else{
    strcpy(tempstr, REFERRER_LOG);
    configstrlist(tempstr, &reflogp, (char *)NULL, (char *)NULL, 2, ON);
  }
  browloghead = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  browlogp = browloghead;
  if (STREQ(BROWSER_LOG, "none"))
    browloghead -> name[0] = '\0';
  else {
    strcpy(tempstr, BROWSER_LOG);
    configstrlist(tempstr, &browlogp, (char *)NULL, (char *)NULL, 2, ON);
  }
  errloghead = (struct stringlist *)xmalloc(sizeof(struct stringlist));
  errlogp = errloghead;
  if (STREQ(ERROR_LOG, "none"))
    errloghead -> name[0] = '\0';
  else {
    strcpy(tempstr, ERROR_LOG);
    configstrlist(tempstr, &errlogp, (char *)NULL, (char *)NULL, 2, ON);
  }
  uncompresshead = (struct loglist *)xmalloc(sizeof(struct loglist));
  uncompresshead -> name[0] = '\0';
  uncompressp = uncompresshead;
  filealiashead = (struct alias *)xmalloc(sizeof(struct alias));
  filealiashead -> from[0] = '\0';
  filealiasp = filealiashead;
  hostaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  hostaliashead -> from[0] = '\0';
  hostaliasp = hostaliashead;
  refaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  refaliashead -> from[0] = '\0';
  refaliasp = refaliashead;
  browaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  browaliashead -> from[0] = '\0';
  browaliasp = browaliashead;
  routaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  routaliashead -> from[0] = '\0';
  routaliasp = routaliashead;
  ioutaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  ioutaliashead -> from[0] = '\0';
  ioutaliasp = ioutaliashead;
  Soutaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  Soutaliashead -> from[0] = '\0';
  Soutaliasp = Soutaliashead;
  foutaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  foutaliashead -> from[0] = '\0';
  foutaliasp = foutaliashead;
  boutaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  boutaliashead -> from[0] = '\0';
  boutaliasp = boutaliashead;
  toutaliashead = (struct alias *)xmalloc(sizeof(struct alias));
  toutaliashead -> from[0] = '\0';
  toutaliasp = toutaliashead;
  wantfilehead = (struct include *)xmalloc(sizeof(struct include));
  wantfilehead -> in = UNSET;
  wantfilep = wantfilehead;
  wanthosthead = (struct include *)xmalloc(sizeof(struct include));
  wanthosthead -> in = UNSET;
  wanthostp = wanthosthead;
  subdomshead = (struct alias *)xmalloc(sizeof(struct alias));
  subdomshead -> from[0] = '\0';
  subdomp = subdomshead;
  wantrefhead = (struct include *)xmalloc(sizeof(struct include));
  wantrefhead -> in = UNSET;
  wantrefp = wantrefhead;
  wantreqhead = (struct include *)xmalloc(sizeof(struct include));
  wantreqhead -> in = UNSET;
  wantreqp = wantreqhead;
  linkhead = (struct include *)xmalloc(sizeof(struct include));
  linkhead -> in = UNSET;
  linkp = linkhead;
  reflinkhead = (struct include *)xmalloc(sizeof(struct include));
  reflinkhead -> in = UNSET;
  reflinkp = reflinkhead;
  noexpandhead = (struct include *)xmalloc(sizeof(struct include));
  noexpandhead -> in = UNSET;
  noexpandp = noexpandhead;
  refexpandhead = (struct include *)xmalloc(sizeof(struct include));
  refexpandhead -> in = UNSET;
  refexpandp = refexpandhead;
  ispagehead = (struct include *)xmalloc(sizeof(struct include));
  ispagep = ispagehead;
  strcpy(ispagep -> name, "*/");
  ispagep -> in = TRUE;
  ispagep -> next = (struct include *)xmalloc(sizeof(struct include));
  ispagep = ispagep -> next;
  strcpy(ispagep -> name, "*.html");
  ispagep -> in = TRUE;
  ispagep -> next = (struct include *)xmalloc(sizeof(struct include));
  ispagep = ispagep -> next;
  strcpy(ispagep -> name, "*.htm");
  ispagep -> in = TRUE;
  ispagep -> next = (struct include *)xmalloc(sizeof(struct include));
  ispagep = ispagep -> next;
  ispagep -> in = UNSET;
}

void othervars(void)    /* vars initialised after command args and config */
{
  time_t oldtimeno;       /* the time 7 days ago */
  struct tm *oldtimetm;

  int i;

  no_urls = 0;
  no_urls7 = 0;
  no_hosts = 0;
  no_hosts7 = 0;
  no_new_hosts7 = 0;
  rmaxreqs = 0;
  imaxreqs = 0;
  tmaxreqs = 0;
  Smaxreqs = 0;
  fmaxreqs = 0;
  bmaxreqs = 0;
  Bmaxreqs = 0;
  rmaxpages = 0;
  imaxpages = 0;
  tmaxpages = 0;
  Smaxpages = 0;
  fmaxpages = 0;
  bmaxpages = 0;
  Bmaxpages = 0;
  rmaxbytes = 0.0;
  imaxbytes = 0.0;
  tmaxbytes = 0.0;
  Smaxbytes = 0.0;
  fmaxbytes = 0.0;
  bmaxbytes = 0.0;
  Bmaxbytes = 0.0;     /* omaxreqs, omaxpages and omaxbytes are set later */
  Smaxlength = 0;
  for (i = 0; i < NO_STATUS; i++) {
    status[i] = 0;
    status7[i] = 0;
  }
  total_succ_reqs = 0;
  total_succ_reqs7 = 0;
  total_page_reqs = 0;
  total_page_reqs7 = 0;
  total_fail_reqs = 0;
  total_fail_reqs7 = 0;
  total_other_reqs = 0;
  total_other_reqs7 = 0;
  total_good_refs = 0;
  total_masked_refs = 0;
  total_bad_refs = 0;
  total_good_brows = 0;
  total_masked_brows = 0;
  total_bad_brows = 0;
  total_bytes = 0.0;
  total_bytes7 = 0.0;
  total_ref_bytes = 0.0;
  total_brow_bytes = 0.0;
  corrupt_lines = 0;
  other_lines = 0;
  cachereqs = 0;
  cachereqs7 = 0;
  cachepages = 0;
  cachepages7 = 0;
  byq = ON;
  refbyq = ON;
  browbyq = ON;

  /* initialise the date structures */
  if (mq) {
    firstm = (struct monthly *)xmalloc(sizeof(struct monthly));
    for (i = 0; i < 12; i++) {
      firstm -> reqs[i] = 0;
      firstm -> pages[i] = 0;
      firstm -> bytes[i] = 0.0;
    }
    firstm -> next = NULL;
    lastm = firstm;
  }
  if (Dq) {
    firstD = (struct daily *)xmalloc(sizeof(struct daily));
    for (i = 0; i < 31; i++) {
      firstD -> reqs[i] = 0;
      firstD -> pages[i] = 0;
      firstD -> bytes[i] = 0.0;
    }
    firstD -> next = NULL;
    lastD = firstD;
  }
  if (Hq) {
    firstH = (struct hourly *)xmalloc(sizeof(struct hourly));
    for (i = 0; i < 24; i++) {
      firstH -> reqs[i] = 0;
      firstH -> pages[i] = 0;
      firstH -> bytes[i] = 0.0;
    }
    firstH -> next = NULL;
    lastH = firstH;
  }
  if (Wq) {
    firstW = (struct weekly *)xmalloc(sizeof(struct weekly));
    firstW -> reqs = 0;
    firstW -> pages = 0;
    firstW -> bytes = 0;
    firstW -> next = NULL;
    lastW = firstW;
  }
  Wrowsdone = 1;
  if (dq) {
    for (i = 0; i < 7; i++) {
      dreq[i] = 0;
      dpag[i] = 0;
      dbytes[i] = 0.0;
    }
  }
  if (hq) {
    for (i = 0; i < 24; i++) {
      hreq[i] = 0;
      hpag[i] = 0;
      hbytes[i] = 0.0;
    }
  }

#ifndef NODNS
  dnsstaletime = starttime - 3600 * dnsfreshhours;
#endif

  if (q7) {   /* calculate the time 7 days ago */
    if (starttimec.code <= totime.code) {
      oldtimeno = starttime - 604800;        /* seconds in a week */
      oldtimetm = localtime(&oldtimeno);
      oldtime.year = 1900 + oldtimetm -> tm_year;
      oldtime.date = oldtimetm -> tm_mday;
      oldtime.monthno = oldtimetm -> tm_mon;
      oldtime.hr = oldtimetm -> tm_hour;
      oldtime.min = oldtimetm -> tm_min;
    }
    else {    /* totime is earlier than today; take 7 days before that */
      oldtime.hr = 23;
      oldtime.min = 59;
      oldtime.year = totime.year;
      oldtime.monthno = totime.monthno;
      oldtime.date = totime.date - 7;
      if (oldtime.date < 1) {
	oldtime.monthno--;
	if (oldtime.monthno < 0) {
	  oldtime.year--;
	  oldtime.monthno += 12;
	}
	oldtime.date += monthlength[oldtime.monthno] +
	  ISLEAPFEB(oldtime.monthno, oldtime.year);
      }
    }
    oldtime.code = timecode(oldtime.date, oldtime.monthno, oldtime.year,
			    oldtime.hr, oldtime.min);
    if (oldtime.code < fromtime.code)
      q7 = OFF;  /* FROM--TO is all in last 7 days */
  }

  if (lang == ENGLISH) {
    for (i = 0; i < 12; i++)
      strcpy(monthname[i], enmonthname[i]);
    for (i = 0; i < 7; i++)
      strcpy(dayname[i], endayname[i]);
  }
  else if (lang == FRENCH) {
    for (i = 0; i < 7; i++)
      strcpy(dayname[i], frdayname[i]);
    if (aq == HTML)
      for (i = 0; i < 12; i++)
        strcpy(monthname[i], frhtmlmonthname[i]);
    else
      for (i = 0; i < 12; i++)
        strcpy(monthname[i], frmonthname[i]);
  }
  else if (lang == GERMAN) {
    for (i = 0; i < 7; i++)
      strcpy(dayname[i], dedayname[i]);
    if (aq == HTML)
      for (i = 0; i < 12; i++)
        strcpy(monthname[i], dehtmlmonthname[i]);
    else
      for (i = 0; i < 12; i++)
        strcpy(monthname[i], demonthname[i]);
  }
  else if (lang == SPANISH) {
    for (i = 0; i < 7; i++)
      strcpy(dayname[i], esdayname[i]);
    for (i = 0; i < 12; i++)
      strcpy(monthname[i], esmonthname[i]);
  }
  else if (lang == DANISH) {
    if (aq == HTML)
      for (i = 0; i < 7; i++)
	strcpy(dayname[i], dkhtmldayname[i]);
    else
      for (i = 0; i < 7; i++)
	strcpy(dayname[i], dkdayname[i]);
    for (i = 0; i < 12; i++)
      strcpy(monthname[i], dkmonthname[i]);
  }
  else {  /* lang == ITALIAN */
    for (i = 0; i < 7; i++)
      strcpy(dayname[i], itdayname[i]);
    for (i = 0; i < 12; i++)
      strcpy(monthname[i], itmonthname[i]);
  }

  dirsufflength = (int)strlen(dirsuffix);
    
  if (rq || iq || tq) {
    rhead = (struct genstruct **)xcalloc(rhashsize,
					 sizeof(struct genstruct *));
  }
  rhead2 = (struct genstruct **)xcalloc(rhashsize, sizeof(struct genstruct *));
  for (i = 0; i < rhashsize; i++) {
    if (rq || iq || tq) {
      rhead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      rhead[i] -> name = NULL;
    }
    rhead2[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
    rhead2[i] -> name = NULL;
  }
  if (iq) {
    ihead = (struct genstruct **)xcalloc(ihashsize,
					 sizeof(struct genstruct *));
    for (i = 0; i < ihashsize; i++) {
      ihead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      ihead[i] -> name = NULL;
    }
  }
  if (tq) {
    thead = (struct genstruct **)xcalloc(thashsize,
					 sizeof(struct genstruct *));
    for (i = 0; i < thashsize; i++) {
      thead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      thead[i] -> name = NULL;
    }
  }
  if (sq == ON) {
    Shead = (struct genstruct **)xcalloc(Shashsize,
					 sizeof(struct genstruct *));
    Shead2 = (struct genstruct **)xcalloc(Shashsize,
					  sizeof(struct genstruct *));
    for (i = 0; i < Shashsize; i++) {
      Shead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      Shead[i] -> name = NULL;
      Shead2[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      Shead2[i] -> name = NULL;
    }
  }
  else if (sq == APPROX) {
    approxhostspace = (char *) xcalloc(approxhostsize, 1);
    if (q7)
      approxhostspace7 = (char *) xcalloc(approxhostsize, 1);
  }
  if (fq) {
    fhead = (struct genstruct **)xcalloc(fhashsize,
					 sizeof(struct genstruct *));
    fhead2 = (struct genstruct **)xcalloc(fhashsize,
					  sizeof(struct genstruct *));
    for (i = 0; i < fhashsize; i++) {
      fhead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      fhead[i] -> name = NULL;
      fhead2[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      fhead2[i] -> name = NULL;
    }
  }
  if (bq) {
    bhead = (struct genstruct **)xcalloc(bhashsize,
					 sizeof(struct genstruct *));
    for (i = 0; i < bhashsize; i++) {
      bhead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      bhead[i] -> name = NULL;
    }
  }
  if (Bq) {
    Bhead = (struct genstruct **)xcalloc(Bhashsize,
					 sizeof(struct genstruct *));
    for (i = 0; i < Bhashsize; i++) {
      Bhead[i] = (struct genstruct *)xmalloc(sizeof(struct genstruct));
      Bhead[i] -> name = NULL;
    }
  }
  if (eq) {
    for (i = 0; i < NO_ERRS; i++)
      errors[i] = 0;
  }
  if (oq) {
    ohead = (struct domain **)xcalloc(DOMHASHSIZE,
				      sizeof(struct domain *));
    for (i = 0; i < DOMHASHSIZE; i++) {
      ohead[i] = (struct domain *)xmalloc(sizeof(struct domain));
      ohead[i] -> name = NULL;
    }
    Ohead = (struct domain **)xcalloc(Ohashsize,
				      sizeof(struct domain *));
    for (i = 0; i < Ohashsize; i++) {
      Ohead[i] = (struct domain *)xmalloc(sizeof(struct domain));
      Ohead[i] -> name = NULL;
    }
    wildOhead = (struct domain *)xmalloc(sizeof(struct domain));
    wildOhead -> id = NULL;
    nwildOhead = (struct domain *)xmalloc(sizeof(struct domain));
    nwildOhead -> id = NULL;

    domainscan();      /* read in all the domains */

  }   /* end if (oq) */

#ifndef NODNS
  if (dnsq) {
    dnshead = (struct dnscache **)xcalloc(dnshashsize,
					  sizeof(struct dnscache *));
    for (i = 0; i < dnshashsize; i++) {
      dnshead[i] = (struct dnscache *)xmalloc(sizeof(struct dnscache));
      dnshead[i] -> number = NULL;
    }
    dnscachescan();
  }
#endif
}

/*** Now for the functions that parse the configuration commands. ***/

void configline(char inputline[MAXLINELENGTH])  /* process one configline */
{
  enum {BADCOMMAND_, FILEALIAS_, HOSTALIAS_, REFALIAS_, BROWALIAS_,
	  REQOUTPUTALIAS_, DIROUTPUTALIAS_, HOSTOUTPUTALIAS_, REFOUTPUTALIAS_,
	  BROWOUTPUTALIAS_, TYPEOUTPUTALIAS_, FILEINCLUDE_, FILEEXCLUDE_,
	  FILEALLOW_, HOSTINCLUDE_, HOSTEXCLUDE_, HOSTALLOW_, REFINCLUDE_,
	  REFEXCLUDE_, REFALLOW_, REQINCLUDE_, REQEXCLUDE_, REQALLOW_,
	  LINKINCLUDE_,  LINKEXCLUDE_, LINKALLOW_, REFLINKINCLUDE_,
	  REFLINKEXCLUDE_, REFLINKALLOW_, FROM_, TO_, SUBDOMAIN_,
	  WEEKBEGINSON_, APPROXHOSTSIZE_, ISPAGE_, ISNOTPAGE_, SEPCHAR_,
	  REPSEPCHAR_, DECPOINT_, PRESEP_, REPORTORDER_, WITHARGS_,
	  WITHOUTARGS_, REFWITHARGS_, REFWITHOUTARGS_, NOTSUBDOMAIN_,
	  BASEURL_, DOMCOLS_, HOSTCOLS_, DIRCOLS_, TYPECOLS_, REQCOLS_,
	  REFCOLS_, BROWCOLS_,
	  FULLBROWCOLS_, MONTHCOLS_, DAYCOLS_, FULLDAYCOLS_, WEEKCOLS_,
	  HOURCOLS_, FULLHOURCOLS_, MONTHGRAPH_, DAYGRAPH_, FULLDAYGRAPH_,
	  HOURGRAPH_, FULLHOURGRAPH_, WEEKGRAPH_, GRAPHICAL_, LOGFILE_,
	  CACHEFILE_, REFLOG_, BROWLOG_, ERRLOG_, DOMAINSFILE_, HOSTNAME_,
	  HOSTURL_, HOSTMINREQS_, DOMMINREQS_, SUBDOMMINREQS_, DIRMINREQS_,
	  TYPEMINREQS_, REQMINREQS_, REFMINREQS_, BROWMINREQS_,
	  FULLBROWMINREQS_, HOSTMINPAGES_, DOMMINPAGES_, SUBDOMMINPAGES_,
	  DIRMINPAGES_, TYPEMINPAGES_, REQMINPAGES_, REFMINPAGES_,
	  BROWMINPAGES_, FULLBROWMINPAGES_, HOSTMINBYTES_, DOMMINBYTES_,
	  SUBDOMMINBYTES_, DIRMINBYTES_, TYPEMINBYTES_, REQMINBYTES_,
	  REFMINBYTES_, BROWMINBYTES_, FULLBROWMINBYTES_, ERRMINOCCS_,
	  REQSORTBY_, DOMSORTBY_, DIRSORTBY_, TYPESORTBY_, HOSTSORTBY_,
	  REFSORTBY_, BROWSORTBY_, FULLBROWSORTBY_, MARKCHAR_, PAGEWIDTH_,
	  ALLBACK_, MONTHLYBACK_, FULLHOURLYBACK_, FULLDAILYBACK_, WEEKLYBACK_,
	  MONTHROWS_, FULLHOURROWS_, FULLDAYROWS_, WEEKROWS_, MONTHLY_, DAILY_,
	  FULLDAILY_, WEEKLY_, HOURLY_, FULLHOURLY_, DOMAIN_, DIRECTORY_,
	  FILETYPE_, REQUEST_, FULLHOSTS_, REFERRER_, BROWSER_, FULLBROWSER_,
	  STATUS_, ERROR_, DIRLEVEL_, COUNTHOSTS_, LASTSEVEN_, WARNINGS_,
	  CASE_, IMAGEDIR_, DIRSUFFIX_, MONTHLYUNIT_, HOURLYUNIT_,
	  FULLHOURLYUNIT_, DAILYUNIT_, LOGOURL_, HEADERFILE_, FOOTERFILE_,
	  FULLDAILYUNIT_, WEEKLYUNIT_, ALL_, GENERAL_, OUTPUT_, LANGUAGE_,
	  DEBUG_, PROGRESSFREQ_, RAWBYTES_, UNCOMPRESS_, REQHASHSIZE_,
	  DIRHASHSIZE_, TYPEHASHSIZE_, HOSTHASHSIZE_, REFHASHSIZE_,
	  BROWHASHSIZE_, FULLBROWHASHSIZE_, SUBDOMHASHSIZE_,
#ifndef NODNS
	  DNSHASHSIZE_, NUMLOOKUP_, DNSFILE_, DNSFRESHHOURS_,
#endif
	  CONFIGFILE_, OUTFILE_
}
  commandtype;   /* final '_'s to avoid clashes with #defines */

  char string1[MAXSTRINGLENGTH], string2[MAXSTRINGLENGTH],
       string3[MAXSTRINGLENGTH];
  int rc;
  flag tempflag;

  rc = sscanf_config(inputline, string1, string2, string3);
  if (rc > 0) {
    commandtype = BADCOMMAND_;   /* pessimism :) */
    strtoupper(string1);
    /* Note: some of these else if's have been changed to just if's. This is
       substantially less efficient but cures a bug in BSD/OS gcc; anyway this
       portion takes ~0 time to run in the context of the whole program. */
    if (STREQ(string1, "FILEALIAS"))
      commandtype = FILEALIAS_;
    else if (STREQ(string1, "HOSTALIAS"))
      commandtype = HOSTALIAS_;
    else if (STREQ(string1, "REFALIAS"))
      commandtype = REFALIAS_;
    else if (STREQ(string1, "BROWALIAS"))
      commandtype = BROWALIAS_;
    else if (STREQ(string1, "REQOUTPUTALIAS"))
      commandtype = REQOUTPUTALIAS_;
    else if (STREQ(string1, "DIROUTPUTALIAS"))
      commandtype = DIROUTPUTALIAS_;
    else if (STREQ(string1, "HOSTOUTPUTALIAS"))
      commandtype = HOSTOUTPUTALIAS_;
    else if (STREQ(string1, "REFOUTPUTALIAS"))
      commandtype = REFOUTPUTALIAS_;
    else if (STREQ(string1, "BROWOUTPUTALIAS"))
      commandtype = BROWOUTPUTALIAS_;
    else if (STREQ(string1, "TYPEOUTPUTALIAS"))
      commandtype = TYPEOUTPUTALIAS_;
    else if (STREQ(string1, "FILEINCLUDE"))
      commandtype = FILEINCLUDE_;
    else if (STREQ(string1, "FILEEXCLUDE"))
      commandtype = FILEEXCLUDE_;
    else if (STREQ(string1, "FILEALLOW"))
      commandtype = FILEALLOW_;
    else if (STREQ(string1, "HOSTINCLUDE"))
      commandtype = HOSTINCLUDE_;
    else if (STREQ(string1, "HOSTEXCLUDE"))
      commandtype = HOSTEXCLUDE_;
    else if (STREQ(string1, "HOSTALLOW"))
      commandtype = HOSTALLOW_;
    else if (STREQ(string1, "REFINCLUDE"))
      commandtype = REFINCLUDE_;
    else if (STREQ(string1, "REFEXCLUDE"))
      commandtype = REFEXCLUDE_;
    else if (STREQ(string1, "REFALLOW"))
      commandtype = REFALLOW_;
    else if (STREQ(string1, "REQINCLUDE"))
      commandtype = REQINCLUDE_;
    else if (STREQ(string1, "REQEXCLUDE"))
      commandtype = REQEXCLUDE_;
    else if (STREQ(string1, "REQALLOW"))
      commandtype = REQALLOW_;
    else if (STREQ(string1, "LINKINCLUDE"))
      commandtype = LINKINCLUDE_;
    else if (STREQ(string1, "LINKEXCLUDE"))
      commandtype = LINKEXCLUDE_;
    else if (STREQ(string1, "LINKALLOW"))
      commandtype = LINKALLOW_;
    else if (STREQ(string1, "REFLINKINCLUDE"))
      commandtype = REFLINKINCLUDE_;
    if (STREQ(string1, "REFLINKEXCLUDE"))
      commandtype = REFLINKEXCLUDE_;
    else if (STREQ(string1, "REFLINKALLOW"))
      commandtype = REFLINKALLOW_;
    else if (STREQ(string1, "FROM"))
      commandtype = FROM_;
    else if (STREQ(string1, "TO"))
      commandtype = TO_;
    else if (STREQ(string1, "SUBDOMAIN"))
      commandtype = SUBDOMAIN_;
    else if (STREQ(string1, "WEEKBEGINSON"))
      commandtype = WEEKBEGINSON_;
    else if (STREQ(string1, "APPROXHOSTSIZE"))
      commandtype = APPROXHOSTSIZE_;
    else if (STREQ(string1, "ISPAGE"))
      commandtype = ISPAGE_;
    else if (STREQ(string1, "ISNOTPAGE"))
      commandtype = ISNOTPAGE_;
    else if (STREQ(string1, "SEPCHAR"))
      commandtype = SEPCHAR_;
    else if (STREQ(string1, "REPSEPCHAR"))
      commandtype = REPSEPCHAR_;
    else if (STREQ(string1, "DECPOINT"))
      commandtype = DECPOINT_;
    else if (STREQ(string1, "PRESEP"))
      commandtype = PRESEP_;
    else if (STREQ(string1, "REPORTORDER"))
      commandtype = REPORTORDER_;
    else if (STREQ(string1, "WITHARGS"))
      commandtype = WITHARGS_;
    else if (STREQ(string1, "WITHOUTARGS"))
      commandtype = WITHOUTARGS_;
    else if (STREQ(string1, "REFWITHARGS"))
      commandtype = REFWITHARGS_;
    else if (STREQ(string1, "REFWITHOUTARGS"))
      commandtype = REFWITHOUTARGS_;
    else if (STREQ(string1, "NOTSUBDOMAIN"))
      commandtype = NOTSUBDOMAIN_;
    else if (STREQ(string1, "BASEURL"))
      commandtype = BASEURL_;
    else if (STREQ(string1, "DOMCOLS"))
      commandtype = DOMCOLS_;
    else if (STREQ(string1, "HOSTCOLS"))
      commandtype = HOSTCOLS_;
    else if (STREQ(string1, "DIRCOLS"))
      commandtype = DIRCOLS_;
    else if (STREQ(string1, "TYPECOLS"))
      commandtype = TYPECOLS_;
    else if (STREQ(string1, "REQCOLS"))
      commandtype = REQCOLS_;
    if (STREQ(string1, "REFCOLS"))
      commandtype = REFCOLS_;
    else if (STREQ(string1, "BROWCOLS"))
      commandtype = BROWCOLS_;
    else if (STREQ(string1, "FULLBROWCOLS"))
      commandtype = FULLBROWCOLS_;
    else if (STREQ(string1, "MONTHCOLS"))
      commandtype = MONTHCOLS_;
    else if (STREQ(string1, "DAYCOLS"))
      commandtype = DAYCOLS_;
    else if (STREQ(string1, "FULLDAYCOLS"))
      commandtype = FULLDAYCOLS_;
    else if (STREQ(string1, "WEEKCOLS"))
      commandtype = WEEKCOLS_;
    else if (STREQ(string1, "HOURCOLS"))
      commandtype = HOURCOLS_;
    else if (STREQ(string1, "FULLHOURCOLS"))
      commandtype = FULLHOURCOLS_;
    else if (STREQ(string1, "MONTHGRAPH"))
      commandtype = MONTHGRAPH_;
    else if (STREQ(string1, "DAYGRAPH"))
      commandtype = DAYGRAPH_;
    else if (STREQ(string1, "FULLDAYGRAPH"))
      commandtype = FULLDAYGRAPH_;
    else if (STREQ(string1, "HOURGRAPH"))
      commandtype = HOURGRAPH_;
    else if (STREQ(string1, "FULLHOURGRAPH"))
      commandtype = FULLHOURGRAPH_;
    else if (STREQ(string1, "WEEKGRAPH"))
      commandtype = WEEKGRAPH_;
    else if (STREQ(string1, "GRAPHICAL"))
      commandtype = GRAPHICAL_;
    else if (STREQ(string1, "LOGFILE"))
      commandtype = LOGFILE_;
    else if (STREQ(string1, "CACHEFILE"))
      commandtype = CACHEFILE_;
    else if (STREQ(string1, "REFLOG"))
      commandtype = REFLOG_;
    else if (STREQ(string1, "BROWLOG"))
      commandtype = BROWLOG_;
    else if (STREQ(string1, "ERRLOG"))
      commandtype = ERRLOG_;
    else if (STREQ(string1, "DOMAINSFILE"))
      commandtype = DOMAINSFILE_;
    else if (STREQ(string1, "HOSTNAME"))
      commandtype = HOSTNAME_;
    else if (STREQ(string1, "HOSTURL"))
      commandtype = HOSTURL_;
    else if (STREQ(string1, "HOSTMINREQS"))
      commandtype = HOSTMINREQS_;
    if (STREQ(string1, "DOMMINREQS"))
      commandtype = DOMMINREQS_;
    else if (STREQ(string1, "SUBDOMMINREQS"))
      commandtype = SUBDOMMINREQS_;
    else if (STREQ(string1, "DIRMINREQS"))
      commandtype = DIRMINREQS_;
    else if (STREQ(string1, "TYPEMINREQS"))
      commandtype = TYPEMINREQS_;
    else if (STREQ(string1, "REQMINREQS"))
      commandtype = REQMINREQS_;
    else if (STREQ(string1, "REFMINREQS"))
      commandtype = REFMINREQS_;
    else if (STREQ(string1, "BROWMINREQS"))
      commandtype = BROWMINREQS_;
    else if (STREQ(string1, "FULLBROWMINREQS"))
      commandtype = FULLBROWMINREQS_;
    else if (STREQ(string1, "HOSTMINPAGES"))
      commandtype = HOSTMINPAGES_;
    else if (STREQ(string1, "DOMMINPAGES"))
      commandtype = DOMMINPAGES_;
    else if (STREQ(string1, "SUBDOMMINPAGES"))
      commandtype = SUBDOMMINPAGES_;
    else if (STREQ(string1, "DIRMINPAGES"))
      commandtype = DIRMINPAGES_;
    else if (STREQ(string1, "TYPEMINPAGES"))
      commandtype = TYPEMINPAGES_;
    else if (STREQ(string1, "REQMINPAGES"))
      commandtype = REQMINPAGES_;
    else if (STREQ(string1, "REFMINPAGES"))
      commandtype = REFMINPAGES_;
    else if (STREQ(string1, "BROWMINPAGES"))
      commandtype = BROWMINPAGES_;
    else if (STREQ(string1, "FULLBROWMINPAGES"))
      commandtype = FULLBROWMINPAGES_;
    else if (STREQ(string1, "HOSTMINBYTES"))
      commandtype = HOSTMINBYTES_;
    else if (STREQ(string1, "DOMMINBYTES"))
      commandtype = DOMMINBYTES_;
    else if (STREQ(string1, "SUBDOMMINBYTES"))
      commandtype = SUBDOMMINBYTES_;
    else if (STREQ(string1, "DIRMINBYTES"))
      commandtype = DIRMINBYTES_;
    else if (STREQ(string1, "TYPEMINBYTES"))
      commandtype = TYPEMINBYTES_;
    else if (STREQ(string1, "REQMINBYTES"))
      commandtype = REQMINBYTES_;
    else if (STREQ(string1, "REFMINBYTES"))
      commandtype = REFMINBYTES_;
    else if (STREQ(string1, "BROWMINBYTES"))
      commandtype = BROWMINBYTES_;
    if (STREQ(string1, "FULLBROWMINBYTES"))
      commandtype = FULLBROWMINBYTES_;
    else if (STREQ(string1, "ERRMINOCCS"))
      commandtype = ERRMINOCCS_;
    else if (STREQ(string1, "REQSORTBY"))
      commandtype = REQSORTBY_;
    else if (STREQ(string1, "DOMSORTBY"))
      commandtype = DOMSORTBY_;
    else if (STREQ(string1, "DIRSORTBY"))
      commandtype = DIRSORTBY_;
    else if (STREQ(string1, "TYPESORTBY"))
      commandtype = TYPESORTBY_;
    else if (STREQ(string1, "HOSTSORTBY"))
      commandtype = HOSTSORTBY_;
    else if (STREQ(string1, "REFSORTBY"))
      commandtype = REFSORTBY_;
    else if (STREQ(string1, "BROWSORTBY"))
      commandtype = BROWSORTBY_;
    else if (STREQ(string1, "FULLBROWSORTBY"))
      commandtype = FULLBROWSORTBY_;
    else if (STREQ(string1, "MARKCHAR"))
      commandtype = MARKCHAR_;
    else if (STREQ(string1, "PAGEWIDTH"))
      commandtype = PAGEWIDTH_;
    else if (STREQ(string1, "ALLBACK"))
      commandtype = ALLBACK_;
    else if (STREQ(string1, "MONTHLYBACK"))
      commandtype = MONTHLYBACK_;
    else if (STREQ(string1, "FULLHOURLYBACK"))
      commandtype = FULLHOURLYBACK_;
    else if (STREQ(string1, "FULLDAILYBACK"))
      commandtype = FULLDAILYBACK_;
    else if (STREQ(string1, "WEEKLYBACK"))
      commandtype = WEEKLYBACK_;
    else if (STREQ(string1, "MONTHROWS"))
      commandtype = MONTHROWS_;
    else if (STREQ(string1, "FULLHOURROWS"))
      commandtype = FULLHOURROWS_;
    else if (STREQ(string1, "WEEKROWS"))
      commandtype = WEEKROWS_;
    else if (STREQ(string1, "FULLDAYROWS"))
      commandtype = FULLDAYROWS_;
    else if (STREQ(string1, "MONTHLY"))
      commandtype = MONTHLY_;
    else if (STREQ(string1, "DAILY"))
      commandtype = DAILY_;
    else if (STREQ(string1, "FULLDAILY"))
      commandtype = FULLDAILY_;
    else if (STREQ(string1, "WEEKLY"))
      commandtype = WEEKLY_;
    if (STREQ(string1, "HOURLY"))
      commandtype = HOURLY_;
    else if (STREQ(string1, "FULLHOURLY"))
      commandtype = FULLHOURLY_;
    else if (STREQ(string1, "DOMAIN"))
      commandtype = DOMAIN_;
    else if (STREQ(string1, "DIRECTORY"))
      commandtype = DIRECTORY_;
    else if (STREQ(string1, "FILETYPE"))
      commandtype = FILETYPE_;
    else if (STREQ(string1, "REQUEST"))
      commandtype = REQUEST_;
    else if (STREQ(string1, "FULLHOSTS"))
      commandtype = FULLHOSTS_;
    else if (STREQ(string1, "REFERRER") || STREQ(string1, "REFERER"))
      commandtype = REFERRER_;
    else if (STREQ(string1, "BROWSER"))
      commandtype = BROWSER_;
    else if (STREQ(string1, "FULLBROWSER"))
      commandtype = FULLBROWSER_;
    else if (STREQ(string1, "STATUS"))
      commandtype = STATUS_;
    else if (STREQ(string1, "ERROR"))
      commandtype = ERROR_;
    else if (STREQ(string1, "DIRLEVEL"))
      commandtype = DIRLEVEL_;
    else if (STREQ(string1, "COUNTHOSTS"))
      commandtype = COUNTHOSTS_;
    else if (STREQ(string1, "LASTSEVEN"))
      commandtype = LASTSEVEN_;
    else if (STREQ(string1, "WARNINGS"))
      commandtype = WARNINGS_;
    else if (STREQ(string1, "CASE"))
      commandtype = CASE_;
    else if (STREQ(string1, "IMAGEDIR"))
      commandtype = IMAGEDIR_;
    else if (STREQ(string1, "DIRSUFFIX"))
      commandtype = DIRSUFFIX_;
    else if (STREQ(string1, "MONTHLYUNIT"))
      commandtype = MONTHLYUNIT_;
    else if (STREQ(string1, "HOURLYUNIT"))
      commandtype = HOURLYUNIT_;
    else if (STREQ(string1, "FULLHOURLYUNIT"))
      commandtype = FULLHOURLYUNIT_;
    else if (STREQ(string1, "DAILYUNIT"))
      commandtype = DAILYUNIT_;
    else if (STREQ(string1, "FULLDAILYUNIT"))
      commandtype = FULLDAILYUNIT_;
    else if (STREQ(string1, "WEEKLYUNIT"))
      commandtype = WEEKLYUNIT_;
    if (STREQ(string1, "LOGOURL"))
      commandtype = LOGOURL_;
    else if (STREQ(string1, "HEADERFILE"))
      commandtype = HEADERFILE_;
    else if (STREQ(string1, "FOOTERFILE"))
      commandtype = FOOTERFILE_;
    else if (STREQ(string1, "ALL"))
      commandtype = ALL_;
    else if (STREQ(string1, "GENERAL"))
      commandtype = GENERAL_;
    else if (STREQ(string1, "OUTPUT"))
      commandtype = OUTPUT_;
    else if (STREQ(string1, "LANGUAGE"))
      commandtype = LANGUAGE_;
    else if (STREQ(string1, "DEBUG"))
      commandtype = DEBUG_;
    else if (STREQ(string1, "PROGRESSFREQ"))
      commandtype = PROGRESSFREQ_;
    else if (STREQ(string1, "RAWBYTES"))
      commandtype = RAWBYTES_;
    else if (STREQ(string1, "UNCOMPRESS"))
      commandtype = UNCOMPRESS_;
    else if (STREQ(string1, "REQHASHSIZE"))
      commandtype = REQHASHSIZE_;
    else if (STREQ(string1, "DIRHASHSIZE"))
      commandtype = DIRHASHSIZE_;
    else if (STREQ(string1, "TYPEHASHSIZE"))
      commandtype = TYPEHASHSIZE_;
    else if (STREQ(string1, "HOSTHASHSIZE"))
      commandtype = HOSTHASHSIZE_;
    else if (STREQ(string1, "REFHASHSIZE"))
      commandtype = REFHASHSIZE_;
    else if (STREQ(string1, "BROWHASHSIZE"))
      commandtype = BROWHASHSIZE_;
    else if (STREQ(string1, "FULLBROWHASHSIZE"))
      commandtype = FULLBROWHASHSIZE_;
    else if (STREQ(string1, "SUBDOMHASHSIZE"))
      commandtype = SUBDOMHASHSIZE_;
#ifndef NODNS
    else if (STREQ(string1, "DNSHASHSIZE"))
      commandtype = DNSHASHSIZE_;
    else if (STREQ(string1, "NUMLOOKUP"))
      commandtype = NUMLOOKUP_;
    else if (STREQ(string1, "DNSFILE"))
      commandtype = DNSFILE_;
    else if (STREQ(string1, "DNSFRESHHOURS"))
      commandtype = DNSFRESHHOURS_;
#endif
    else if (STREQ(string1, "CONFIGFILE"))
      commandtype = CONFIGFILE_;
    else if (STREQ(string1, "OUTFILE"))
      commandtype = OUTFILE_;
    
    switch(commandtype) {
    case (BADCOMMAND_):
      configwarning2(inputline);
      break;
    case (FILEALIAS_):
      configalias(string2, string3, &filealiasp, string1, inputline, rc);
      break;
    case (HOSTALIAS_):
      configalias(strtolower(string2), strtolower(string3), &hostaliasp,
		  string1, inputline, rc);
      break;
    case (REFALIAS_):
      configalias(string2, string3, &refaliasp, string1, inputline, rc);
      break;
    case (BROWALIAS_):
      configalias(string2, string3, &browaliasp, string1, inputline, rc);
      break;
    case (REQOUTPUTALIAS_):
      configalias(string2, string3, &routaliasp, string1, inputline, rc);
      break;
    case (DIROUTPUTALIAS_):
      configalias(string2, string3, &ioutaliasp, string1, inputline, rc);
      break;
    case (HOSTOUTPUTALIAS_):
      configalias(string2, string3, &Soutaliasp, string1, inputline, rc);
      break;
    case (REFOUTPUTALIAS_):
      configalias(string2, string3, &foutaliasp, string1, inputline, rc);
      break;
    case (BROWOUTPUTALIAS_):
      configalias(string2, string3, &boutaliasp, string1, inputline, rc);
      break;
    case (TYPEOUTPUTALIAS_):
      configalias(string2, string3, &toutaliasp, string1, inputline, rc);
      break;
    case (FILEINCLUDE_):
      include(string2, &wantfilep, wantfilehead, TRUE, string1, inputline, rc,
	      &filemaskq);
      break;
    case (FILEEXCLUDE_):
      include(string2, &wantfilep, wantfilehead, FALSE, string1, inputline,
	      rc, &filemaskq);
      break;
    case (FILEALLOW_):
      include(string2, &wantfilep, wantfilehead, UNSET, string1, inputline, rc,
	      &tempflag);
      break;
    case (HOSTINCLUDE_):
      include(strtolower(string2), &wanthostp, wanthosthead, TRUE, string1,
	      inputline, rc, &hostmaskq);
      break;
    case (HOSTEXCLUDE_):
      include(strtolower(string2), &wanthostp, wanthosthead, FALSE, string1,
	      inputline, rc, &hostmaskq);
      break;
    case (HOSTALLOW_):
      include(strtolower(string2), &wanthostp, wanthosthead, UNSET, string1,
	      inputline, rc, &tempflag);
      break;
    case (REFINCLUDE_):
      include(string2, &wantrefp, wantrefhead, TRUE, string1, inputline, rc,
	      &refmaskq);
      break;
    case (REFEXCLUDE_):
      include(string2, &wantrefp, wantrefhead, FALSE, string1, inputline, rc,
	      &refmaskq);
      break;
    case (REFALLOW_):
      include(string2, &wantrefp, wantrefhead, UNSET, string1, inputline, rc,
	      &tempflag);
      break;
    case (REQINCLUDE_):
      include(string2, &wantreqp, wantreqhead, TRUE, string1, inputline, rc,
	      &tempflag);
      break;
    case (REQEXCLUDE_):
      include(string2, &wantreqp, wantreqhead, FALSE, string1, inputline, rc,
	      &tempflag);
      break;
    case (REQALLOW_):
      include(string2, &wantreqp, wantreqhead, UNSET, string1, inputline, rc,
	      &tempflag);
      break;
    case (LINKINCLUDE_):
      include(string2, &linkp, linkhead, TRUE, string1, inputline, rc,
	      &tempflag);
      break;
    case (LINKEXCLUDE_):
      include(string2, &linkp, linkhead, FALSE, string1, inputline, rc,
	      &tempflag);
      break;
    case (LINKALLOW_):
      include(string2, &linkp, linkhead, UNSET, string1, inputline, rc,
	      &tempflag);
      break;
    case (REFLINKINCLUDE_):
      include(string2, &reflinkp, reflinkhead, TRUE, string1, inputline, rc,
	      &tempflag);
      break;
    case (REFLINKEXCLUDE_):
      include(string2, &reflinkp, reflinkhead, FALSE, string1, inputline, rc,
	      &tempflag);
      break;
    case (REFLINKALLOW_):
      include(string2, &reflinkp, reflinkhead, UNSET, string1, inputline, rc,
	      &tempflag);
      break;
    case (FROM_):
      fromtodate(string2, &fromtime, TRUE, string1, inputline, rc);
      break;
    case (TO_):
      fromtodate(string2, &totime, FALSE, string1, inputline, rc);
      break;
    case (SUBDOMAIN_):
      if (string2[0] == '\0')
	configwarning2(inputline);
      else if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtolower(string2);
	if (rc > 3 ||
	    (rc == 3 && (string2[0] == '*' || 
			 string2[(int)strlen(string2) - 1] == '*' ||
			 string2[0] == '%')))
	  configwarning3(string1, inputline);
	strcpy(subdomp -> from, string2);
	if (rc == 2 || string2[0] == '*' ||
	    string2[(int)strlen(string2) - 1] == '*' || string2[0] == '%')
	  strcpy(subdomp -> to, "?");
	else
	  strcpy(subdomp -> to, string3);
	subdomp -> next =
	  (struct alias *)xmalloc(sizeof(struct alias));
	subdomp = subdomp -> next;
	subdomp -> from[0] = '\0';
      }
      break;
    case (WEEKBEGINSON_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2)
	  configwarning3(string1, inputline);
	strtoupper(string2);
	if (STREQ(string2, "SUNDAY"))
	  weekbeginson = SUNDAY;
	else if (STREQ(string2, "MONDAY"))
	  weekbeginson = MONDAY;
	else if (STREQ(string2, "TUESDAY"))
	  weekbeginson = TUESDAY;
	else if (STREQ(string2, "WEDNESDAY"))
	  weekbeginson = WEDNESDAY;
	else if (STREQ(string2, "THURSDAY"))
	  weekbeginson = THURSDAY;
	else if (STREQ(string2, "FRIDAY"))
	  weekbeginson = FRIDAY;
	else if (STREQ(string2, "SATURDAY"))
	  weekbeginson = SATURDAY;
	else
	  configwarning2(inputline);
      }
      break;
    case (APPROXHOSTSIZE_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2)
	  configwarning3(string1, inputline);
	approxhostsize = atoi(string2);
	if (approxhostsize <= 0)
	  configwarning2(inputline);
      }
      break;
    case (ISPAGE_):
      include(string2, &ispagep, ispagehead, TRUE, string1, inputline, rc,
	      &tempflag);
      break;
    case (ISNOTPAGE_):
      include(string2, &ispagep, ispagehead, FALSE, string1, inputline, rc,
	      &tempflag);
      break;
    case (SEPCHAR_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2 || (string2[0] != '\0' && string2[1] != '\0'))
	  configwarning3(string1, inputline);
	sepchar = string2[0];
      }
      break;
    case (REPSEPCHAR_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2 || (string2[0] != '\0' && string2[1] != '\0'))
	  configwarning3(string1, inputline);
	repsepchar = string2[0];
      }
      break;
    case (DECPOINT_):
      if (rc < 2 || string2[0] == '\0')
	configwarning(string1, inputline);
      else {
	if (rc > 2 || string2[1] != '\0')
	  configwarning3(string1, inputline);
	decpoint = string2[0];
      }
      break;
    case (PRESEP_):
      configstr(string2, presep, string1, inputline, rc);
      break;
    case (REPORTORDER_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2 || (int)strlen(string2) > 19)
	  configwarning3(string1, inputline);
	strncpy(reportorder, string2, 19);
      }
      break;
    case (WITHARGS_):
      include(string2, &noexpandp, noexpandhead, FALSE, string1, inputline,
	      rc, &tempflag);
      break;
    case (WITHOUTARGS_):
      include(string2, &noexpandp, noexpandhead, TRUE, string1, inputline, rc,
	      &tempflag);
      break;
    case (REFWITHARGS_):
      include(string2, &refexpandp, refexpandhead, TRUE, string1, inputline,
	      rc, &tempflag);
      break;
    case (REFWITHOUTARGS_):
      include(string2, &refexpandp, refexpandhead, FALSE, string1, inputline,
	      rc, &tempflag);
      break;
    case (NOTSUBDOMAIN_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtolower(string2);
	if (rc > 2)
	  configwarning3(string1, inputline);
	tempflag = OFF;
	for (subdomtempp = subdomshead; subdomtempp -> from[0] != '\0';
	     subdomtempp = subdomtempp -> next) {
	  if (STREQ(subdomtempp -> from, string2)) {
	    subdomtempp -> from[0] = '?'; /* mark that we don't want it */
	    tempflag = ON;
	  }
	}
	if (!tempflag && warnq) {
	  fprintf(stderr, "%s: Warning: NOTSUBDOMAIN command before corresponding SUBDOMAIN at\n", commandname);
	  fprintf(stderr, "  %s", inputline);
	  if (inputline[MAX(strlen(inputline) - 1, 0)] != '\n')
	    fprintf(stderr, "\n");
	  anywarns = ON;
	}
      }
      break;
    case (BASEURL_):
      configstr(string2, baseurl, string1, inputline, rc);
      break;
    case (DOMCOLS_):
      configcols(string2, ocols, string1, inputline, rc);
      break;
    case (HOSTCOLS_):
      configcols(string2, Scols, string1, inputline, rc);
      break;
    case (DIRCOLS_):
      configcols(string2, icols, string1, inputline, rc);
      break;
    case (TYPECOLS_):
      configcols(string2, tcols, string1, inputline, rc);
      break;
    case (REQCOLS_):
      configcols(string2, rcols, string1, inputline, rc);
      break;
    case (REFCOLS_):
      configcols(string2, fcols, string1, inputline, rc);
      break;
    case (BROWCOLS_):
      configcols(string2, bcols, string1, inputline, rc);
      break;
    case (FULLBROWCOLS_):
      configcols(string2, Bcols, string1, inputline, rc);
      break;
    case (MONTHCOLS_):
      configcols(string2, mcols, string1, inputline, rc);
      break;
    case (DAYCOLS_):
      configcols(string2, dcols, string1, inputline, rc);
      break;
    case (FULLDAYCOLS_):
      configcols(string2, Dcols, string1, inputline, rc);
      break;
    case (WEEKCOLS_):
      configcols(string2, Wcols, string1, inputline, rc);
      break;
    case (HOURCOLS_):
      configcols(string2, hcols, string1, inputline, rc);
      break;
    case (FULLHOURCOLS_):
      configcols(string2, Hcols, string1, inputline, rc);
      break;
    case (MONTHGRAPH_):
      configchar(string2, &mgraph, string1, inputline, rc);
      break;
    case (DAYGRAPH_):
      configchar(string2, &dgraph, string1, inputline, rc);
      break;
    case (FULLDAYGRAPH_):
      configchar(string2, &Dgraph, string1, inputline, rc);
      break;
    case (HOURGRAPH_):
      configchar(string2, &hgraph, string1, inputline, rc);
      break;
    case (FULLHOURGRAPH_):
      configchar(string2, &Hgraph, string1, inputline, rc);
      break;
    case (WEEKGRAPH_):
      configchar(string2, &Wgraph, string1, inputline, rc);
      break;
    case (GRAPHICAL_):
      onoff(string2, &graphical, string1, inputline, rc);
      break;
    case (LOGFILE_):
      if (rc < 2)
	configwarning(string1, inputline);
      else if (STREQ(string2, "none")) {
	logfilep = logfilehead;
	logfilehead -> name[0] = '\0';
      }
      else if (rc == 2)
	addlogfile(&logfilep, string2, "", ON);
      else {
	addlogfile(&logfilep, string2, string3, ON);
	if (rc > 3)
	  configwarning3(string1, inputline);
      }
      break;
    case (CACHEFILE_):
      if (STREQ(string2, "none")) {
	cachefilep = cachefilehead;
	cachefilehead -> name[0] = '\0';
      }
      else
	configstrlist(string2, &cachefilep, string1, inputline, rc, ON);
      break;
    case (REFLOG_):
      if (STREQ(string2, "none")) {
	reflogp = refloghead;
	refloghead -> name[0] = '\0';
      }
      else
	configstrlist(string2, &reflogp, string1, inputline, rc, ON);
      break;
    case (BROWLOG_):
      if (STREQ(string2, "none")) {
	browlogp = browloghead;
	browloghead -> name[0] = '\0';
      }
      else
	configstrlist(string2, &browlogp, string1, inputline, rc, ON);
      break;
    case (ERRLOG_):
      if (STREQ(string2, "none")) {
	errlogp = errloghead;
	errloghead -> name[0] = '\0';
      }
      else
	configstrlist(string2, &errlogp, string1, inputline, rc, ON);
      break;
    case (DOMAINSFILE_):
      configstr(string2, domainsfile, string1, inputline, rc);
      break;
    case (HOSTNAME_):
      configstr(string2, hostname, string1, inputline, rc);
      break;
    case (HOSTURL_):
      configstr(string2, hosturl, string1, inputline, rc);
      break;
    case (HOSTMINREQS_):
      configstr(string2, Sminreqstr, string1, inputline, rc);
      break;
    case (DOMMINREQS_):
      configstr(string2, ominreqstr, string1, inputline, rc);
      break;
    case (SUBDOMMINREQS_):
      configstr(string2, Ominreqstr, string1, inputline, rc);
      break;
    case (DIRMINREQS_):
      configstr(string2, iminreqstr, string1, inputline, rc);
      break;
    case (TYPEMINREQS_):
      configstr(string2, tminreqstr, string1, inputline, rc);
      break;
    case (REQMINREQS_):
      configstr(string2, rminreqstr, string1, inputline, rc);
      break;
    case (REFMINREQS_):
      configstr(string2, fminreqstr, string1, inputline, rc);
      break;
    case (BROWMINREQS_):
      configstr(string2, bminreqstr, string1, inputline, rc);
      break;
    case (FULLBROWMINREQS_):
      configstr(string2, Bminreqstr, string1, inputline, rc);
      break;
    case (HOSTMINPAGES_):
      configstr(string2, Sminpagestr, string1, inputline, rc);
      break;
    case (DOMMINPAGES_):
      configstr(string2, ominpagestr, string1, inputline, rc);
      break;
    case (SUBDOMMINPAGES_):
      configstr(string2, Ominpagestr, string1, inputline, rc);
      break;
    case (DIRMINPAGES_):
      configstr(string2, iminpagestr, string1, inputline, rc);
      break;
    case (TYPEMINPAGES_):
      configstr(string2, tminpagestr, string1, inputline, rc);
      break;
    case (REQMINPAGES_):
      configstr(string2, rminpagestr, string1, inputline, rc);
      break;
    case (REFMINPAGES_):
      configstr(string2, fminpagestr, string1, inputline, rc);
      break;
    case (BROWMINPAGES_):
      configstr(string2, bminpagestr, string1, inputline, rc);
      break;
    case (FULLBROWMINPAGES_):
      configstr(string2, Bminpagestr, string1, inputline, rc);
      break;
    case (HOSTMINBYTES_):
      configstr(string2, Sminbytestr, string1, inputline, rc);
      break;
    case (DOMMINBYTES_):
      configstr(string2, ominbytestr, string1, inputline, rc);
      break;
    case (SUBDOMMINBYTES_):
      configstr(string2, Ominbytestr, string1, inputline, rc);
      break;
    case (DIRMINBYTES_):
      configstr(string2, iminbytestr, string1, inputline, rc);
      break;
    case (TYPEMINBYTES_):
      configstr(string2, tminbytestr, string1, inputline, rc);
      break;
    case (REQMINBYTES_):
      configstr(string2, rminbytestr, string1, inputline, rc);
      break;
    case (REFMINBYTES_):
      configstr(string2, fminbytestr, string1, inputline, rc);
      break;
    case (BROWMINBYTES_):
      configstr(string2, bminbytestr, string1, inputline, rc);
      break;
    case (FULLBROWMINBYTES_):
      configstr(string2, Bminbytestr, string1, inputline, rc);
      break;
    case (ERRMINOCCS_):
      configint(string2, &eminreqs, string1, inputline, rc);
      break;
    case (REQSORTBY_):
      configsortby(string2, &rsortby, string1, inputline, rc);
      break;
    case (DOMSORTBY_):
      configsortby(string2, &osortby, string1, inputline, rc);
      break;
    case (DIRSORTBY_):
      configsortby(string2, &isortby, string1, inputline, rc);
      break;
    case (TYPESORTBY_):
      configsortby(string2, &tsortby, string1, inputline, rc);
      break;
    case (HOSTSORTBY_):
      configsortby(string2, &Ssortby, string1, inputline, rc);
      break;
    case (REFSORTBY_):
      configsortby(string2, &fsortby, string1, inputline, rc);
      break;
    case (BROWSORTBY_):
      configsortby(string2, &bsortby, string1, inputline, rc);
      break;
    case (FULLBROWSORTBY_):
      configsortby(string2, &Bsortby, string1, inputline, rc);
      break;
    case (MARKCHAR_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2 || string2[1] != '\0')
	  configwarning3(string1, inputline);
	markchar = string2[0];
      }
      break;
    case (PAGEWIDTH_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2)
	  configwarning3(string1, inputline);
	pagewidth = atoi(string2);
	if (pagewidth < MINPAGEWIDTH || pagewidth > MAXPAGEWIDTH) {
	  fprintf(stderr, "%s: Page width should be between %d and %d\n",
		  commandname, MINPAGEWIDTH, MAXPAGEWIDTH);
	  configwarning2(inputline);
	  pagewidth = PAGEWIDTH;
	}
      }
      break;
    case (ALLBACK_):  /* onoff() extended */
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtoupper(string2);
	if (STREQ(string2, "ON")) {
	  mback = ON;
	  Hback = ON;
	  Wback = ON;
	  Dback = ON;
	}
	else if (STREQ(string2, "OFF")) {
	  mback = OFF;
	  Hback = OFF;
	  Wback = OFF;
	  Dback = OFF;
	}
	else
	  configwarning2(inputline);
      }
      break;
    case (MONTHLYBACK_):
      onoff(string2, &mback, string1, inputline, rc);
      break;
    case (FULLHOURLYBACK_):
      onoff(string2, &Hback, string1, inputline, rc);
      break;
    case (FULLDAILYBACK_):
      onoff(string2, &Dback, string1, inputline, rc);
      break;
    case (WEEKLYBACK_):
      onoff(string2, &Wback, string1, inputline, rc);
      break;
    case (MONTHROWS_):
      configint(string2, &mrows, string1, inputline, rc);
      break;
    case (FULLHOURROWS_):
      configint(string2, &Hrows, string1, inputline, rc);
      break;
    case (WEEKROWS_):
      configint(string2, &Wrows, string1, inputline, rc);
      break;
    case (FULLDAYROWS_):
      configint(string2, &Drows, string1, inputline, rc);
      break;
    case (MONTHLY_):
      onoff(string2, &mq, string1, inputline, rc);
      break;
    case (DAILY_):
      onoff(string2, &dq, string1, inputline, rc);
      break;
    case (FULLDAILY_):
      onoff(string2, &Dq, string1, inputline, rc);
      break;
    case (HOURLY_):
      onoff(string2, &hq, string1, inputline, rc);
      break;
    case (FULLHOURLY_):
      onoff(string2, &Hq, string1, inputline, rc);
      break;
    case (WEEKLY_):
      onoff(string2, &Wq, string1, inputline, rc);
      break;
    case (DOMAIN_):
      onoff(string2, &oq, string1, inputline, rc);
      break;
    case (FULLHOSTS_):
      onoff(string2, &Sq, string1, inputline, rc);
      break;
    case (DIRECTORY_):
      onoff(string2, &iq, string1, inputline, rc);
      break;
    case (FILETYPE_):
      onoff(string2, &tq, string1, inputline, rc);
      break;
    case (REQUEST_):
      onoff(string2, &rq, string1, inputline, rc);
      break;
    case (REFERRER_):
      onoff(string2, &fq, string1, inputline, rc);
      break;
    case (BROWSER_):
      onoff(string2, &bq, string1, inputline, rc);
      break;
    case (FULLBROWSER_):
      onoff(string2, &Bq, string1, inputline, rc);
      break;
    case (STATUS_):
      onoff(string2, &cq, string1, inputline, rc);
      break;
    case (ERROR_):
      onoff(string2, &eq, string1, inputline, rc);
      break;
    case (ALL_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtoupper(string2);
	if (STREQ(string2, "ON")) {
	  mq = ON;
	  Wq = ON;
	  dq = ON;
	  Dq = ON;
	  hq = ON;
	  Hq = ON;
	  oq = ON;
	  Sq = ON;
	  iq = ON;
	  tq = ON;
	  rq = ON;
	  fq = ON;
	  bq = ON;
	  Bq = ON;
	  cq = ON;
	  eq = ON;
	}
	else if (STREQ(string2, "OFF"))  {
	  mq = OFF;
	  Wq = OFF;
	  dq = OFF;
	  Dq = OFF;
	  hq = OFF;
	  Hq = OFF;
	  oq = OFF;
	  Sq = OFF;
	  iq = OFF;
	  tq = OFF;
	  rq = OFF;
	  fq = OFF;
	  bq = OFF;
	  Bq = OFF;
	  cq = OFF;
	  eq = OFF;
	}
	else
	  configwarning2(inputline);
      }
      break;
    case (GENERAL_):
      onoff(string2, &xq, string1, inputline, rc);
      break;
    case (DIRLEVEL_):
      configint(string2, &dirlevel, string1, inputline, rc);
      break;
    case (COUNTHOSTS_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtoupper(string2);
	if (STREQ(string2, "ON"))
	  sq = ON;
	else if (STREQ(string2, "OFF"))
	  sq = OFF;
	else if (STREQ(string2, "APPROX"))
	  sq = APPROX;
	else
	  configwarning2(inputline);
      }
      break;
    case (LASTSEVEN_):
      onoff(string2, &q7, string1, inputline, rc);
      break;
    case (WARNINGS_):
      onoff(string2, &warnq, string1, inputline, rc);
      break;
    case (CASE_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	strtoupper(string2);
	if (STREQ(string2, "SENSITIVE"))
	  case_insensitive = OFF;
	else if (STREQ(string2, "INSENSITIVE"))
	  case_insensitive = ON;
	else
	  configwarning2(inputline);
      }
      break;
    case (IMAGEDIR_):
      configstr(string2, imagedir, string1, inputline, rc);
      break;
    case (DIRSUFFIX_):
      configstr(string2, dirsuffix, string1, inputline, rc);
      break;
    case (MONTHLYUNIT_):
      configint(string2, &munit, string1, inputline, rc);	  
      break;
    case (HOURLYUNIT_):
      configint(string2, &hunit, string1, inputline, rc);	  
      break;
    case (FULLHOURLYUNIT_):
      configint(string2, &Hunit, string1, inputline, rc);	  
      break;
    case (DAILYUNIT_):
      configint(string2, &dunit, string1, inputline, rc);	  
      break;
    case (FULLDAILYUNIT_):
      configint(string2, &Dunit, string1, inputline, rc);	  
      break;
    case (WEEKLYUNIT_):
      configint(string2, &Wunit, string1, inputline, rc);	  
      break;
    case (LOGOURL_):
      configstr(string2, logourl, string1, inputline, rc);
      break;
    case (HEADERFILE_):
      configstr(string2, headerfile, string1, inputline, rc);
      break;
    case (FOOTERFILE_):
      configstr(string2, footerfile, string1, inputline, rc);
      break;
    case (OUTPUT_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2)
	  configwarning3(string1, inputline);
	strtoupper(string2);
	if (STREQ(string2, "ASCII"))
	  aq = ASCII;
	else if (STREQ(string2, "HTML"))
	  aq = HTML;
	else if (STREQ(string2, "CACHE"))
	  aq = CACHE;
	else if (STREQ(string2, "PREFORMATTED"))
	  aq = PREFORMATTED;
	else
	  configwarning2(inputline);
      }
      break;
    case (LANGUAGE_):
      if (rc < 2)
	configwarning(string1, inputline);
      else {
	if (rc > 2)
	  configwarning3(string1, inputline);
	strtoupper(string2);
	if (STREQ(string2, "ENGLISH"))
	  lang = ENGLISH;
	else if (STREQ(string2, "US-ENGLISH"))
	  lang = US_ENGLISH;
	else if (STREQ(string2, "FRENCH"))
	  lang = FRENCH;
	else if (STREQ(string2, "GERMAN"))
	  lang = GERMAN;
	else if (STREQ(string2, "ITALIAN"))
	  lang = ITALIAN;
	else if (STREQ(string2, "SPANISH"))
	  lang = SPANISH;
	else if (STREQ(string2, "DANISH"))
	  lang = DANISH;
	else
	  configwarning2(inputline);
      }
      break;
    case (DEBUG_):
      configint(string2, &debug, string1, inputline, rc);	  
      break;
    case (PROGRESSFREQ_):
      configint(string2, &progressfreq, string1, inputline, rc);	  
      break;
    case (RAWBYTES_):
      onoff(string2, &rawbytes, string1, inputline, rc);
      break;
    case (UNCOMPRESS_):
      if (rc < 3)
	configwarning(string1, inputline);
      else {
	addlogfile(&uncompressp, string2, string3, OFF);
	if (rc > 3)
	  configwarning3(string1, inputline);
      }
      break;
    case (REQHASHSIZE_):
      configsizet(string2, &rhashsize, string1, inputline, rc);	  
      break;
    case (DIRHASHSIZE_):
      configsizet(string2, &ihashsize, string1, inputline, rc);	  
      break;
    case (TYPEHASHSIZE_):
      configsizet(string2, &thashsize, string1, inputline, rc);	  
      break;
    case (HOSTHASHSIZE_):
      configsizet(string2, &Shashsize, string1, inputline, rc);	  
      break;
    case (REFHASHSIZE_):
      configsizet(string2, &fhashsize, string1, inputline, rc);	  
      break;
    case (BROWHASHSIZE_):
      configsizet(string2, &bhashsize, string1, inputline, rc);	  
      break;
    case (FULLBROWHASHSIZE_):
      configsizet(string2, &Bhashsize, string1, inputline, rc);	  
      break;
    case (SUBDOMHASHSIZE_):
      configsizet(string2, &Ohashsize, string1, inputline, rc);	  
      break;
#ifndef NODNS
    case (DNSHASHSIZE_):
      configsizet(string2, &dnshashsize, string1, inputline, rc);	  
      break;
    case (NUMLOOKUP_):
      onoff(string2, &dnsq, string1, inputline, rc);
      break;
    case (DNSFILE_):
      configstr(string2, dnsfile, string1, inputline, rc);
      break;
    case (DNSFRESHHOURS_):
      configint(string2, &dnsfreshhours, string1, inputline, rc);
      break;
#endif
    case (CONFIGFILE_):
      config(string2);
      break;
    case (OUTFILE_):
      configstr(string2, outfile, string1, inputline, rc);
      break;
    }
  }
}

flag config(char *filename)
{
  FILE *cf;
  flag ispipe;
  char inputline[MAXLINELENGTH];

  if (STREQ(filename, "none"))
    return(0);
  else {
    no_configs++;
    if (no_configs > MAX_CONFIGS) {
      fprintf(stderr, "%s: Error: More than %d configuration files:\n",
	      commandname, MAX_CONFIGS);
      fprintf(stderr, "  probably being called in circles: exiting\n");
      exit(ERR);
    }
    cf = fopenlog(filename, "configuration file", &ispipe);
    if (cf == NULL)
      return(1);
    else {  /* we can read the config. file */
      while (fgets(inputline, MAXLINELENGTH, cf) != NULL)
	configline(inputline);
      fcloselog(cf, filename, "configuration file", ispipe);
      return(0);
    }
  }
}

/*** Now the main commandline command ***/

void commandline(int argc, char **argv)
{
  int i;

  flag Gfound = FALSE;
  char tempstr1[MAXSTRINGLENGTH], tempstr2[MAXSTRINGLENGTH];

  /* First see whether to include the default config. file, by scanning
     backwards through the arguments looking for the (last) occurrence of
     +G or -G. We then run the config. file, then look at the other args. */

  for (i = argc - 1; i >= 1 && !Gfound; i--) {
    if (argv[i][1] == 'G') {
      if (argv[i][2] != '\0' && (argv[i][0] == '+' || argv[i][0] == '-')) {
	if (warnq) {
	  fprintf(stderr,
		  "%s: Warning: ignoring extra text after %cG option\n",
		  commandname, argv[i][0]);
	  anywarns = ON;
	}
      }
      if (argv[i][0] == '+') {
	Gfound = TRUE;
	config(DEFAULTCONFIGFILE);
      }
      else if (argv[i][0] == '-') {
	Gfound = TRUE;
      }
    }
  }
  if (!Gfound) {
    logfilep = logfilehead;
    cachefilep = cachefilehead;
    reflogp = refloghead;
    browlogp = browloghead;
    errlogp = errloghead;
    config(DEFAULTCONFIGFILE);
  }

  logfilep = logfilehead;  /* reset logfile pointers for over-write */
  cachefilep = cachefilehead;
  reflogp = refloghead;
  browlogp = browloghead;
  errlogp = errloghead;

  /* Now read the other arguments */

  for (i = 1; i < argc; i++) {

    if (argv[i][0] != '+' && argv[i][0] != '-') {
      if (STREQ(argv[i], "none")) {
	logfilep = logfilehead;
	logfilehead -> name[0] = '\0';
      }
      else
	addlogfile(&logfilep, argv[i], "", ON);
    }
    else switch (argv[i][1]) {
    case '\0':    /* read stdin */
      addlogfile(&logfilep, "stdin", "", ON);
      break;
#ifndef NODNS
    case '1':
      clflag(&dnsq, argv[i]);
      break;
#endif
    case '7':     /* stats for last 7 days */
      clflag(&q7, argv[i]);
      break;
    case 'a':     /* ASCII output */
      clflag(&aq, argv[i]);  /* This works because ON = ASCII, OFF = HTML */
      break;      
    case 'A':     /* all reports */
      if (argv[i][0] == '-')
	configline("ALL OFF");
      else
	configline("ALL ON");
      if (argv[i][2] != '\0' && warnq) {
	fprintf(stderr, "%s: Warning: ignoring extra text after %cA option\n",
		commandname, argv[i][0]);
	anywarns = ON;
      }
      break;
    case 'b':     /* browser summary */
      clgenrep(&bq, &bsortby, bminreqstr, bminpagestr, bminbytestr, argv[i]);
      break;
    case 'B':     /* browser report */
      clgenrep(&Bq, &Bsortby, Bminreqstr, Bminpagestr, Bminbytestr, argv[i]);
      break;
    case 'c':     /* status code report */
      clflag(&cq, argv[i]);
      break;
    case 'C':     /* configuration command */
      if (argv[i][2] == '\0') {
	if (warnq) {
	  fprintf(stderr, "%s: Warning: no command supplied after +C option\n",
		  commandname);
	  fprintf(stderr, "  (or space left before command)\n");
	  anywarns = ON;
	}
      }
      else
	configline(argv[i] + 2);
      break;
    case 'd':     /* daily summary */
      cldaterep(&dq, &dgraph, argv[i]);
      break;
    case 'D':     /* full daily report */
      cldaterep(&Dq, &Dgraph, argv[i]);
      break;
    case 'e':     /* error report */
      clflag(&eq, argv[i]);
      break;
    case 'f':     /* form */
      if (STREQ(argv[i] + 2, "orm"))
	formq = ON;
      else        /* referrer report */
	clgenrep(&fq, &fsortby, fminreqstr, fminpagestr, fminbytestr, argv[i]);
      break;
    case 'F':     /* FROM */
      if (argv[i][0] == '-') {
	configline("FROM OFF");
	if (argv[i][2] != '\0' && warnq) {
	  fprintf(stderr, "%s: Warning: ignoring extra text after -F option\n",
		  commandname);
	  anywarns = ON;
	}
      }
      else if (argv[i][2] == '\0' && warnq) {
	fprintf(stderr, "%s: Warning: no date supplied after +F option\n",
		commandname);
	fprintf(stderr, "  (or space left before date)\n");
	anywarns = ON;
      }
      else {
	strcpy(tempstr2, "FROM ");
	strcat(tempstr2, argv[i] + 2);
	configline(tempstr2);
      }
      break;
    case 'g':     /* configuration file */
      if (argv[i][0] == '+') {
	if (argv[i][2] == '\0') {
	  if (warnq) {
	    fprintf(stderr,
		    "%s: Warning: no filename supplied after +g option\n",
		    commandname);
	    fprintf(stderr, "  (or space left before filename)\n");
	    anywarns = ON;
	  }
	}
	else
	  config(argv[i] + 2);
      }
      else {
	fprintf(stderr, "%s: Warning: Ignoring unknown option -g:\n",
		commandname);
	fprintf(stderr, "  see Readme.html for correct usage\n");
	fprintf(stderr,
		"  or go to http://www.statslab.cam.ac.uk/~sret1/analog/\n");
      }
      break;
    case 'G':     /* default configuration file: done already */
      break;
    case 'h':     /* help */
      if (STREQ(argv[i] + 2, "elp")) {
	fprintf(stderr, "For help see Readme.html, or ");
	fprintf(stderr, "http://www.statslab.cam.ac.uk/~sret1/analog/\n");
	exit(OK);
      }
      else        /* hourly summary */
	cldaterep(&hq, &hgraph, argv[i]);
      break;
    case 'H':     /* hourly report */
      cldaterep(&Hq, &Hgraph, argv[i]);
      break;
    case 'i':     /* directory report */
      clgenrep(&iq, &isortby, iminreqstr, iminpagestr, iminbytestr, argv[i]);
      break;
    case 'l':     /* 'level' of dir report */
      dirlevel = atoi(argv[i] + 2);
      break;
    case 'm':     /* monthly report */
      cldaterep(&mq, &mgraph, argv[i]);
      break;
    case 'n':     /* our host or organisation name */
      if (argv[i][2] == '\0') {
	if (warnq) {
	  fprintf(stderr, "%s: Warning: no text supplied after %cn option\n",
		  commandname, argv[i][0]);
	  fprintf(stderr, "  (or space left before text)\n");
	  anywarns = ON;
	}
      }
      else
	strncpy(hostname, argv[i] + 2, MAXSTRINGLENGTH - 1);
      break;
    case 'o':                 /* domain report */
      clgenrep(&oq, &osortby, ominreqstr, ominpagestr, ominbytestr, argv[i]);
      break;
    case 'O':     /* outfile */
      clfile(outfile, argv[i]);
      break;
    case 'p':    /* logo? */
      clfile(logourl, argv[i]);
      break;
    case 'q':    /* warnings? */
      clflag(&warnq, argv[i]);
      break;
    case 'r':    /* request report */
      clgenrep(&rq, &rsortby, rminreqstr, rminpagestr, rminbytestr, argv[i]);
      break;
    case 's':      /* count hosts? */
      if (argv[i][0] == '-')
	sq = OFF;
      else if (argv[i][2] == 's')
	sq = APPROX;
      else
	sq = ON;
      if (argv[i][2] == 's') {
	if (argv[i][3] != '\0' && warnq) {
	  fprintf(stderr,
		  "%s: Warning: ignoring extra text after %css option\n",
		  commandname, argv[i][0]);
	  anywarns = ON;
	}
      }
      else {  /* argv[i][2] != 's' */
	if (argv[i][2] != '\0' && warnq) {
	  fprintf(stderr,
		  "%s: Warning: ignoring extra text after %cs option\n",
		  commandname, argv[i][0]);
	  anywarns = ON;
	}
      }
      break;
    case 'S':      /* full hostname report */
      clgenrep(&Sq, &Ssortby, Sminreqstr, Sminpagestr, Sminbytestr, argv[i]);
      break;
    case 't':     /* file type report */
      clgenrep(&tq, &tsortby, tminreqstr, tminpagestr, tminbytestr, argv[i]);
      break;
    case 'T':     /* TO */
      if (argv[i][0] == '-') {
	configline("TO OFF");
	if (argv[i][2] != '\0' && warnq) {
	  fprintf(stderr, "%s: Warning: ignoring extra text after -T option\n",
		  commandname);
	  anywarns = ON;
	}
      }
      else if (argv[i][2] == '\0' && warnq) {
	fprintf(stderr, "%s: Warning: no date supplied after +T option\n",
		commandname);
	fprintf(stderr, "  (or space left before date)\n");
	anywarns = ON;
      }
      else {
	strcpy(tempstr2, "TO ");
	strcat(tempstr2, argv[i] + 2);
	configline(tempstr2);
      }
      break;
    case 'u':     /* host URL */
      if (argv[i][2] == '\0') {
	if (warnq) {
	  fprintf(stderr, "%s: Warning: no URL supplied after %cu option\n",
		  commandname, argv[i][0]);
	  fprintf(stderr, "  (or space left before URL)\n");
	  anywarns = ON;
	}
      }
      else
	strncpy(hosturl, argv[i] + 2, MAXSTRINGLENGTH - 1);
      break;
    case 'U':      /* cache file */
      if(clfile(tempstr1, argv[i]) == OK) {
	strcpy(tempstr2, "CACHEFILE ");
	strncat(tempstr2, tempstr1, MAXSTRINGLENGTH - 11);
	configline(tempstr2);
      }
      break;
    case 'v':          /* print variables and exit */
      if (argv[i][2] != '\0' && warnq) {
	fprintf(stderr, "%s: Warning: ignoring extra text after %cv option\n",
		commandname, argv[i][0]);
	anywarns = ON;
      }
      vblesonly = ON;
      break;
    case 'V':          /* debugging info */
      if (argv[i][0] == '-') {
	debug = OFF;
	if (argv[i][2] != '\0' && warnq) {
	  fprintf(stderr, "%s: Warning: ignoring extra text after -V option\n",
		  commandname);
	  anywarns = ON;
	}
      }
      else {
	if (argv[i][2] == '\0')
	  debug = 1;
	else
	  debug = atoi(argv[i] + 2);
      }
      break;
    case 'w':          /* pagewidth */
      pagewidth = atoi(argv[i] + 2);
      if (pagewidth < MINPAGEWIDTH || pagewidth > MAXPAGEWIDTH) {
	if (warnq) {
	  fprintf(stderr,"%s: Warning: at option %s, page width should be between %d and %d\n", commandname, argv[i], MINPAGEWIDTH, MAXPAGEWIDTH);
	  fprintf(stderr, "  Resetting to default value of %d\n", PAGEWIDTH);
	  anywarns = ON;
	}
	pagewidth = PAGEWIDTH;
      }
      break;
    case 'W':           /* weekly report */
      cldaterep(&Wq, &Wgraph, argv[i]);
      break;
    case 'x':           /* general summary and gotos */
      clflag(&xq, argv[i]);
      break;
    default:
      fprintf(stderr, "%s: Warning: Ignoring unknown option %s:\n",
	      commandname, argv[i]);
      fprintf(stderr, "  see Readme.html for correct usage\n");
      fprintf(stderr,
      "  or go to http://www.statslab.cam.ac.uk/~sret1/analog/\n");
    }
  }

  /* Finally, the mandatory config file */
  logfilep = logfilehead;  /* reset logfile pointers for over-write */
  cachefilep = cachefilehead;
  reflogp = refloghead;
  browlogp = browloghead;
  errlogp = errloghead;

  if (config(MANDATORYCONFIGFILE)) {
    fprintf(stderr,
    "%s: Error: Cannot ignore mandatory configuration file: exiting\n",
	    commandname);
    exit(ERR);
  }

}

/*** The actual initialise() function ***/

void initialise(int argc, char **argv)
{
  FILE *outf;

  time(&starttime);
  starttimetm = localtime(&starttime);
  starttimec.year = 1900 + starttimetm -> tm_year;
  starttimec.date = starttimetm -> tm_mday;
  starttimec.monthno = starttimetm -> tm_mon;
  starttimec.hr = starttimetm -> tm_hour;
  starttimec.min = starttimetm -> tm_min;
  starttimec.code = timecode(starttimec.date, starttimec.monthno,
			     starttimec.year, starttimec.hr, starttimec.min);

  defaults();   /* enter the defaults for all the variables
		   (before possibly changing them). */

#ifdef MAC_EVENTS
  commandname = (char *)xmalloc(7);
  strcpy(commandname, "analog");
#else
  commandname = (char *)xmalloc(strlen(argv[0]) + 1);
  strcpy(commandname, argv[0]);
#endif

  init_structs();   /* initialise all the structures (and pointers to them) */

  commandline(argc, argv);  /* parse commandline. This also parses all the
			       configuration files. */

  /* correct any variables for which wrong value given */

  if (fromtime.code > totime.code) {
    fprintf(stderr, "%s: Error: FROM and TO exclude all dates.\n",
	    commandname);
    exit(ERR);
  }

  if (dirlevel == 0)
    dirlevel = 1;

  if (Sq)
    sq = ON;      /* +S implies +s */

  if (STREQ(baseurl, "none"))
    baseurl[0] = '\0';
  else if (baseurl[MAX(strlen(baseurl) - 1, 0)] == '/')
    baseurl[strlen(baseurl) - 1] = '\0';

  if (aq == CACHE) {
    configline("ALL OFF");
    xq = OFF;
    Hq = ON;
    Hback = OFF;
    Hrows = 0;
  }

  else if (aq == PREFORMATTED) {
    rawbytes = ON;
    lang = ENGLISH;  /* only affects days of the week */
    sepchar = '\0';
    repsepchar = '\0';
    decpoint = '.';  /* don't need this last one at the moment, but... */
  }

  if (rsortby == BYPAGES) {
    fprintf(stderr,
	    "%s: Warning: Cannot sort request report by page requests:\n",
	    commandname);
    fprintf(stderr, "  Sorting by requests instead.\n");
    rsortby = BYREQUESTS;
  }

  if (case_insensitive) {
    alias_to_lower(filealiashead);
    include_to_lower(wantfilehead);
    include_to_lower(wantreqhead);
    include_to_lower(linkhead);
    include_to_lower(ispagehead);
    include_to_lower(noexpandhead);
    strtolower(dirsuffix);
  }

  if (vblesonly)
    printvbles();    /* which also exits */

  if (lang == US_ENGLISH) {
    lang = ENGLISH;
    dialect = US_ENGLISH;
  }

  if (!STREQ(outfile, "stdout")) {
    if ((outf = fopen(outfile, "w")) == NULL) {
      fprintf(stderr,
	      "%s: Error: failed to open output file %s for writing.\n",
	      commandname, outfile);
      exit(ERR);   /* test now so we don't do the processing THEN find out */
    }
    else
      fclose(outf);  /* no need to hold it open */
  }

  /* if we just want a form, generate that and exit */

  if (formq) {
    formgen();
    exit(OK);
  }

  othervars();

}   /* end initialise() */


/*** Finally, a boring function just to print variables and exit ***/

void printvbles(void)
{
  FILE *filep;
  char *c;

  stdin_used = FALSE;

  printf("This is analog version %s\n", VERSION);
  printf("For more information on analog see Readme.html or\n");
  printf("http://www.statslab.cam.ac.uk/~sret1/analog/\n\n");

  if (formq) {
    printf("These options would construct form in %s; see that file for options set\n",
	   outfile);
  }
  else {
    printf("Logfiles to analyse:\n");   /* as pvfilelist(), but for logfiles */
    if (logfilehead -> name[0] == '\0')
      printf("  none\n");
    for (logfilep = logfilehead; logfilep -> name[0] != '\0';
	 logfilep = logfilep -> next) {
      printf("  %s", logfilep -> name);
      if ((filep = fopen(logfilep -> name, "r")) == NULL &&
	  !STREQ(logfilep -> name, "stdin") && !STREQ(logfilep -> name, "-"))
	printf(":   Warning: cannot open that file");
      else
	fclose(filep);  /* NB So condl 3 lines back must be in that order */
      if (STREQ(logfilep -> name, "stdin") || STREQ(logfilep -> name, "-")) {
	if (stdin_used)
	  printf("\n  Warning: attempt to use stdin twice will fail");
	stdin_used = TRUE;
      }
      printf("\n");
    }

    pvfilelist(cachefilehead, "Cache files");

    if (aq != CACHE) {
      pvfilelist(refloghead, "Referrer logs");
      pvfilelist(browloghead, "Browser logs");
      pvfilelist(errloghead, "Error logs");
    }

    if (uncompresshead -> name[0] != '\0') {
      printf("Files uncompressed by the following methods:\n");
      for (uncompressp = uncompresshead; uncompressp -> name[0] != '\0';
	   uncompressp = uncompressp -> next)
	printf("  %s by %s\n", uncompressp -> name, uncompressp -> prefix);
    }
    printf("Filenames are case %s\n",
	   (case_insensitive)?"insensitive":"sensitive");
    
    if (aq == CACHE)
      printf("\nOutputting cache file only.\n");
    else {
      printf("\nThe output will be in ");
      if (aq == HTML)
	printf("HTML\n");
      else if (aq == ASCII)
	printf("plain text\n");
      else /* aq == PREFORMATTED */
	printf("preformatted format\n");
      if (aq != PREFORMATTED) {
	printf("The output language will be ");
	if (lang == ENGLISH)
	  printf("English\n");
	else if (lang == US_ENGLISH)
	  printf("US English\n");
	else if (lang == FRENCH)
	  printf("French\n");
	else if (lang == GERMAN)
	  printf("German\n");
	else if (lang == SPANISH)
	  printf("Spanish\n");
   	else if (lang == DANISH)
	  printf("Danish\n");
        else /* lang == ITALIAN */
          printf("Italian\n");
         }
      lang = ENGLISH;   /* So that whatincluded() goes in English */
      printf("The domains file is %s\n", domainsfile);
      if (((filep = fopen(domainsfile, "r")) == NULL) &&
	  !STREQ(domainsfile, "stdin") && !STREQ(domainsfile, "-"))
	printf("  Warning: cannot open that file: cannot construct domain report\n");
      else
	fclose(filep);
      if (STREQ(domainsfile, "stdin") || STREQ(domainsfile, "-")) {
	if (stdin_used)
	  printf("\n  Warning: attempt to use stdin twice will fail");
	stdin_used = TRUE;
      }

#ifndef NODNS
      if (dnsq) {
	printf("DNS lookups performed with DNS cache file %s\n", dnsfile);
	if (((filep = fopen(dnsfile, "r")) == NULL) &&
	    !STREQ(dnsfile, "stdin") && !STREQ(dnsfile, "-"))
	  printf("  Warning: cannot open that file:\n");
	else
	  fclose(filep);
	if (STREQ(dnsfile, "stdin") || STREQ(dnsfile, "-")) {
	  if (stdin_used)
	    printf("\n  Warning: attempt to use stdin twice will fail");
	  stdin_used = TRUE;
	}
	printf("Old DNS lookups will be rechecked if they are more than %d hours old\n", dnsfreshhours);
      }
#endif

      printf("The output file is %s\n", outfile);
      /* Don't check for being able to write as it will destroy the file */

      printf("\nReport order:\n");
      printf("General summary    %s\n", xq?"ON":"OFF");
      for (c = reportorder; *c != '\0'; c++) {
	switch(*c) {
	case 'b':
	  pvgen("Browser summary", bq, bsortby, bminreqstr, bminpagestr,
		bminbytestr, bcols, "browser", "browsers");
	  if (bq)
	    pvalias("Browser summary", boutaliashead);
	  break;
	case 'B':
	  pvgen("Browser report", Bq, Bsortby, Bminreqstr, Bminpagestr,
		Bminbytestr, Bcols, "browser", "browsers");
	  break;
	case 'c':
	  printf("Status code report %s\n", cq?"ON":"OFF");
	  break;
	case 'd':
	  pvtime("Daily summary", dq, dgraph, dunit, dcols, 0);
	  break;
	case 'D':
	  pvtime("Daily report", Dq, Dgraph, Dunit, Dcols, Drows);
	  break;
	case 'e':
	  printf("Error report       %s\n", eq?"ON":"OFF");
	  if (eminreqs == 0)
	    printf("  Printing all possible errors, ");
	  else
	    printf("  Printing all errors with at least %d occurence%s,\n",
		   eminreqs, (eminreqs == 1)?"":"s");
	  printf("  sorted by number of occurrences.");
	  break;
	case 'f':
	  pvgen("Referrer report", fq, fsortby, fminreqstr, fminpagestr,
		fminbytestr, fcols, "referrer", "referrers");
	  if (fq) {
	    pvalias("Referrer report", foutaliashead);
	    pvinout("as linked items", reflinkhead);
	  }
	  break;
	case 'h':
	  pvtime("Hourly summary", hq, hgraph, hunit, hcols, 0);
	  break;
	case 'H':
	  pvtime("Hourly report", Hq, Hgraph, Hunit, Hcols, Hrows);
	  break;
	case 'i':
	  pvgen("Directory report", iq, isortby, iminreqstr, iminpagestr,
		iminbytestr, icols, "directory", "directories");
	  if (iq) {
	    printf("  Printing directories to depth %d.\n", dirlevel);
	    pvalias("Directory report", ioutaliashead);
	  }
	  break;
	case 'm':
	  pvtime("Monthly report", mq, mgraph, munit, mcols, mrows);
	  break;
	case 'o':
	  pvgen("Domain report", oq, osortby, ominreqstr, ominpagestr,
		ominbytestr, ocols, "domain", "domains");
	  if (oq) {
	    printf("  ");
	    whatincluded(stdout, osortby, Ominreqstr, Ominpagestr, Ominbytestr,
			 "requested subdomain", "requested subdomains", ON,
			 'm');
	  }
	  break;
	case 'r':
	  pvgen("Request report", rq, rsortby, rminreqstr, rminpagestr,
		rminbytestr, rcols, "requested file", "requested files");
	  if (rq) {
	    pvinout("in the report", wantreqhead);
	    pvalias("Request report", routaliashead);
	    pvinout("as linked items", linkhead);
	    if (baseurl[0] != '\0')
	      printf("  (links prepended by %s)\n", baseurl);
	  }
	  break;
	case 'S':
	  pvgen("Host report", Sq, Ssortby, Sminreqstr, Sminpagestr,
		Sminbytestr, Scols, "host", "hosts");
	  if (Sq)
	    pvalias("Host report", Soutaliashead);
	  break;
	case 't':
	  pvgen("File type report", tq, tsortby, tminreqstr, tminpagestr,
		tminbytestr, tcols, "extension", "extensions");
	  if (tq)
	    pvalias("File type report", toutaliashead);
	  break;
	case 'W':
	  pvtime("Weekly report", Wq, Wgraph, Wunit, Wcols, Wrows);
	  break;
	}  /* end switch c */
      }    /* end for c */
      

      if (sq == APPROX) {
	printf("\nApproximate hostname count ON\n");
	printf("  Space used for hostname count: ");
	int3printf(stdout, approxhostsize, sepchar, 0);
	printf(" bytes\n");
      }
      else
	printf("\nHostname count     %s\n", (sq == ON)?"ON":"OFF");

      printf("Statistics for last 7 days %s\n", q7?"ON":"OFF");

      printf("Hash sizes are:\n");
      printf("  Requests        %6d\n", rhashsize);
      printf("  Directories     %6d\n", ihashsize);
      printf("  File types      %6d\n", thashsize);
      printf("  Hosts           %6d\n", Shashsize);
      printf("  Referrers       %6d\n", fhashsize);
      printf("  Browser summary %6d\n", bhashsize);
      printf("  Browser report  %6d\n", Bhashsize);
      printf("  Subdomains      %6d\n", Ohashsize);
#ifndef NODNS
      if (dnsq)
	printf("  DNS cache       %6d\n", dnshashsize);
#endif
      
      if (oq) {
	printf("\nRequested subdomains:\n");
	if (subdomshead -> from[0] == '\0')
	  printf("  None\n");
	else for (subdomp = subdomshead; subdomp -> from[0] != '\0';
		  subdomp = subdomp -> next) {
	  if (subdomp -> from[0] == '?')  /* don't want it */
	    ;
	  else if (subdomp -> from[0] == '%')
	    printf("  Numerical subdomains\n");
	  else {
	    printf("  %s", subdomp -> from);
	    if (subdomp -> to[0] != '?')
	      printf(" (%s)", subdomp -> to);
	    printf("\n");
	  }
	}
      }
    
      if (mq || dq || hq || Hq || Dq || Wq) {
	printf("\nThe character used in the graphs will be '%c'\n", markchar);
	printf("The page width is %d\n", pagewidth);
      }

      if (sepchar != '\0')
	printf("The character used as a separator in big numbers will be '%c'\n",
	       sepchar);
      if (repsepchar != '\0')
	printf("The character used as a separator in big numbers within reports will be '%c'\n", repsepchar);
      printf("The character used as a decimal point will be '%c'\n", decpoint);

      if (Wq || Dq || dq) {
	printf("Weeks are taken to begin on ");
	switch(weekbeginson) {
	case (SUNDAY):
	  printf("Sunday.\n");
	  break;
	case (MONDAY):
	  printf("Monday.\n");
	  break;
	case (TUESDAY):
	  printf("Tuesday.\n");
	  break;
	case (WEDNESDAY):
	  printf("Wednesday.\n");
	  break;
	case (THURSDAY):
	  printf("Thursday.\n");
	  break;
	case (FRIDAY):
	  printf("Friday.\n");
	  break;
	case (SATURDAY):
	  printf("Saturday.\n");
	  break;
	}
      }

      printf("The following will be included in the output:\n");

      if (!aq)
	printf("Logo               %s\n",
	       STREQ(logourl, "none")?logourl:"OFF");
      printf("Header file        %s\n",
	     STREQ(headerfile, "none")?"OFF":headerfile);
      if (!STREQ(headerfile, "none")) {
	if (((filep = fopen(headerfile, "r")) == NULL) &&
	    !STREQ(headerfile, "stdin") && !STREQ(headerfile, "-"))
	  printf("  Warning: cannot open that file: will ignore it\n");
	else
	  fclose(filep);
      }
      printf("Footer file        %s\n",
	     STREQ(footerfile, "none")?"OFF":footerfile);
      if (!STREQ(footerfile, "none")) {
	if ((filep = fopen(footerfile, "r")) == NULL &&
	    !STREQ(footerfile, "stdin") && !STREQ(footerfile, "-"))
	  printf("  Warning: cannot open that file: will ignore it\n");
	else
	  fclose(filep);
      }
      printf("Organisation name  \"%s\"\n", hostname);
      if (!aq)
	printf("Name linked to URL %s\n\n", (hosturl[0]=='-')?"OFF":hosturl);
      
      pvinout("as pages", ispagehead);
      pvalias("File", filealiashead);
      pvalias("Host", hostaliashead);
      pvalias("Referrer", refaliashead);
      pvalias("Brower", browaliashead);
      
    }   /* end if aq != CACHE */

    if (filemaskq)
      pvinout("files", wantfilehead);
    if (hostmaskq)
      pvinout("hosts", wanthosthead);
    if (refmaskq && aq != CACHE)
      pvinout("referrers", wantrefhead);
    
    if (fromtime.code > -INFINITY || totime.code < INFINITY) {
      printf("\nExamining only times");
      if (fromtime.code > -INFINITY)
	printf(" from %02d/%s/%d", fromtime.date,
	       enmonthname[fromtime.monthno], fromtime.year);
      if (totime.code < INFINITY)
	printf(" to %02d/%s/%d", totime.date,
	       enmonthname[totime.monthno], totime.year);
      printf("\n");
    }

    printf("\nDebug level is %d\n", debug);
    printf("Warnings are %s\n", warnq?"ON":"OFF");
    if (progressfreq > 0)
      printf("Progress report every %d logfile lines\n", progressfreq);
    printf("\n");

  }  /* end not form */

  exit(OK);

}
