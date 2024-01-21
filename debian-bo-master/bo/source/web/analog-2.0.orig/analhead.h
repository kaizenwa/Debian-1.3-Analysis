/*** analhead.h: header file for analog 2.0 ***/
/* Please read Readme.html, or http://www.statslab.cam.ac.uk/~sret1/analog/  */

/*** Change any of these according to your desires ***/

#ifndef ANALHEAD
#define ANALHEAD

/*** The first few options -- up to LOGOURL -- are ones that you may want to
     change before compiling the program for the first time. ***/

#define HOSTNAME "the Statistical Laboratory"
/* the name of your organisation or WWW host. This is used for printing
   at the top of the output */

#define HOSTURL "http://www.statslab.cam.ac.uk/"
/* the URL of your host's home page, for linking to at the top of the
   output; use "-" for no linking. */

#define DOMAINSFILE "/usr/local/etc/httpd/analog/domains.tab"
/* the name of the file where the domain definitions live; see README.html
   for the format of this file */

#define LOGFILE "/usr/local/etc/httpd/logs/access_log"
/* The name of the default logfile; use "stdin" for stdin.
   Can be a list, separated by commas (but no spaces). */

#define IMAGEDIR "/images/"
/* URL of the directory where the images for the graphical reports live.
   The URL can be absolute, or relative to the output page: e.g., just the
   empty string "" for the same directory as the output page. */

#define DEFAULTCONFIGFILE "/usr/local/etc/httpd/analog/analog.cfg"
/* the name of the default configuration file; see README.html for a
   description of this file. Use "none" for no configuration file. */

#define MANDATORYCONFIGFILE "none"
/* A mandatory configuration file; read after all other arguments so
   overrides everything. Program exits if it is not found. Use "none"
   for none. */

#define LOGOURL "/images/analogo.gif"
/* the URL of the file where the analog logo lives. (One should have come
   with the program). You can put another logo in here (your organisation's
   logo, for example). Use "none" for no logo */

/*** The next few options are compiled into the program. They can't be
     changed without re-compiling the program. ***/

#define MAX_CONFIGS (50)
/* The maximum number of configuration files that are allowed. */

#define MINPAGEWIDTH (25)
#define MAXPAGEWIDTH (240)
/* the min and max allowed values of the pagewidth variable */

#define MINGRAPHWIDTH (15)
/* bar charts must be at least this many characters wide */

#define FORMPROG "/cgi-bin/analform.cgi"
/* If you make an analog form interface, what will be the URL of the
   program that processes it? (It should be wherever cgi-bin programs
   go on your server; normally in a cgi-bin directory). */

#define MAXLINELENGTH (2048)
/* the maximum length of any line in the logfile (long lines can
   result from searches), domains file or rc file. Longer lines
   will be thrown away. */

#define MAXSTRINGLENGTH (256)
/* how long is the longest string we want for a list of filenames, our
   organisation name, an internet address, a geographical location,
   an argument to a configuration command etc.?
   (Longer strings will be truncated). MUST BE AT LEAST 256. */

/*** The rest of the options can be overridden when the program is run. ***/

#define OUTFILE "stdout"
/* the default file for output. Use "stdout" for stdout. */

#define CACHEFILE "none"
/* The default file of cached time info. Use "none" for none. */

#define REFERRER_LOG "/usr/local/etc/httpd/logs/referer_log"
/* The name of the default referrer log (= referer_log). Use "none" for none.
   Can be a list, separated by commas. */

#define BROWSER_LOG "/usr/local/etc/httpd/logs/agent_log"
/* The name of the default browser log (= agent_log) similarly. */

#define ERROR_LOG "/usr/local/etc/httpd/logs/error_log"
/* The name of the default error_log similarly. */

#define NUMLOOKUP (OFF)
/* whether to try and resolve IP addresses. Can be slow. */

#define DNSFILE "/usr/local/etc/httpd/analog/dnscache"
/* where to store resolved addresses so we don't have to look them up next
   time */

#define DNSFRESHHOURS (168)
/* How many hours the information stays fresh for before we have to look it
   up again. */

#define DIRSUFFIX "index.html"
/* the default filename tried if a directory is requested. The program
   combines statistics from /dir/ and /dir/DIRSUFFIX. If you don't wish
   this combining to occur, define DIRSUFFIX to be "". The usual value
   is "index.html". (It would be nice to allow index.html or home.html,
   say, but this is not possible to do accurately without knowing more
   information than is in the logfile). */

#define REQHASHSIZE (1009)
/* the size of the hash table for requests; must be odd, should be prime.
   too small will make the program slower; too big might use a bit
   more memory and be slightly less efficient. Maybe half the number
   of URLs expected, but it shouldn't be at all critical. There is a list
   of primes in the file primes.tab */

#define DIRHASHSIZE (97)
/* ditto for directories */

#define TYPEHASHSIZE (97)
/* and for filetypes */

#define HOSTHASHSIZE (3079)
/* ditto for all hostnames that have accessed us */

#define SUBDOMHASHSIZE (503)
/* and again for all subdomains we want to analyse */

#define REFHASHSIZE (659)
/* and again for all URLs that have referenced our site */

#define BROWHASHSIZE (97)
/* and for all makes of browser */

#define FULLBROWHASHSIZE (659)
/* and for all browsers, distinguishing different versions etc. */

#define DNSHASHSIZE (3079)
/* and for numerical addresses in the DNS cache */

#ifdef UNIX
#define CASE (SENSITIVE)
#else
#define CASE (INSENSITIVE)
#endif
/* Do you have a case sensitive or case insensitive file system? */

#define REPORTORDER "mWdDhHoSitrfbBce"
/* A string like "mWdDhHoSitrfbBce" giving the order in which the reports will
   be output. You should make sure to include each of the above letters exactly
   once or you will not get some report even if you specifically request it
   later, or you may get some report twice. The letters mean
   m = monthly report   W = weekly report    d = daily summary
   D = daily report     h = hourly summary   H = hourly report
   o = domain report    S = host report      i = directory report
   r = request report   f = referrer report  b = browser summary
   B = browser report   e = error report     c = status code report
   t = filetype report */

/* whether we want each of the reports by default */
#define GENERAL (ON)      /* General statistics and GoTo's */
#define MONTHLY (ON)      /* Monthly report */
#define DAILY (ON)        /* Daily summary */
#define FULLDAILY (OFF)   /* Daily report */
/* One line for each day. Think about the poor people loading your page from
   the other side of the world before turning this on by default. */
#define WEEKLY (OFF)      /* Weekly report */
#define HOURLY (ON)       /* Hourly summary */
#define FULLHOURLY (OFF)  /* Hourly report */
/* One line for each hour ever. You really don't want this on by default. */
#define DOMAINREP (ON)    /* Domain report */
/* Not called DOMAIN because clashes with math.h */
#define DIRECTORY (ON)    /* Directory report */
#define FILETYPE (OFF)    /* File type report */
#define REQUEST (ON)      /* Request report */
#define FULLHOSTS (OFF)   /* Host report */
/* Full hostname report. Typically very long output and slow to produce
   on a full logfile, unless MIN_HOST_REQS or _BYTES is high as applicable */
#define REFERRER (OFF)    /* Referrer report */
#define BROWSER (OFF)     /* Browser summary */
#define FULLBROWSER (OFF) /* Browser report */
#define STATUS (OFF)      /* Status code report */
#define ERROR (OFF)       /* Error report */

#define DOMCOLS "Rb"
/* Which columns we want to appear in the domain report, and in which order.
   The string can contain any of the following four letters:
   R   Number of requests from each domain
   r   Percentage of the requests from each domain
   P   Number of requests for pages from each domain
   p   Percentage of the requests for pages from each domain
   B   Total number of bytes transferred to each domain
   b   The percentage of traffic to each domain */
/* And the same for the other reports */
#define HOSTCOLS "Rb"
#define DIRCOLS "Rb"
#define TYPECOLS "Rb"
#define REQCOLS "Rb"
#define MONTHCOLS "P"
#define DAYCOLS "P"
#define FULLDAYCOLS "P"
#define WEEKCOLS "P"
#define HOURCOLS "P"
#define FULLHOURCOLS "P"
#define REFCOLS "R"
#define BROWCOLS "R"
#define FULLBROWCOLS "R"

/* Should the time graphs be calculated by requests 'R', bytes 'B' or
   pages 'P'? */
#define MONTHGRAPH 'P'
#define DAYGRAPH 'P'
#define FULLDAYGRAPH 'P'
#define HOURGRAPH 'P'
#define FULLHOURGRAPH 'P'
#define WEEKGRAPH 'P'

/* Should they go forwards (oldest entries at top) or backwards?
   (NB You can mix them, but probably don't want to) */
#define MONTHLYBACK (FALSE)
#define FULLDAILYBACK (FALSE)
#define FULLHOURLYBACK (FALSE)
#define WEEKLYBACK (FALSE)

/* The maximum number of rows in each (0 for "show all time") */
#define MONTHROWS (0)
#define WEEKROWS (0)
#define FULLDAYROWS (42)
#define FULLHOURROWS (72)

#define GRAPHICAL (ON)
/* whether the graphs should use proper graphics or just ASCII art. ON will
   make it prettier but will make the output longer and harder to download */

#define MIN_URL_REQS "20"
/* the min. no. of requests a URL should have before appearing on the
   request report. Setting this to about 20 clears out a lot of junk.
   You can also use e.g. "0.1%" for any file with 0.1% of the requests.
   And setting it to a negative number gives a "top n" report. */

#define MIN_URL_BYTES "0.01%"
/* if we are sorting by bytes, use this instead. You can use
   "1000234" for 1000234 bytes
   "100k"    for 100 kilobytes  (and 100M etc. similarly)
   "0.01%"   for any file with 0.01% of the bytes
   "-50"     for the 50 files with the most bytes  */

/* now the same for directory, domain and hostname reports under requests,
   page requests, or bytes sorting. */
#define MIN_DIR_REQS "10"
#define MIN_DIR_PAGES "10"
#define MIN_DIR_BYTES "0.01%"
#define MIN_TYPE_REQS "100"
#define MIN_TYPE_PAGES "100"
#define MIN_TYPE_BYTES "0.1%"
#define MIN_DOM_REQS "1"
#define MIN_DOM_PAGES "1"
#define MIN_DOM_BYTES "0"
#define MIN_HOST_REQS "-100"
#define MIN_HOST_PAGES "-100"
#define MIN_HOST_BYTES "-100"
#define MIN_REF_REQS "20"
#define MIN_REF_PAGES "20"
#define MIN_REF_BYTES "0.01%"
#define MIN_BROW_REQS "-20"
#define MIN_BROW_PAGES "-20"
#define MIN_BROW_BYTES "-20"
#define MIN_FULLBROW_REQS "-100"
#define MIN_FULLBROW_PAGES "-100"
#define MIN_FULLBROW_BYTES "-100"
/* And the same for subdomains you have specified. These can't be negative. */
#define MIN_SUBDOM_REQS "1000"
#define MIN_SUBDOM_PAGES "300"
#define MIN_SUBDOM_BYTES "0.5%"
/* And the min number of occurences before an error gets on the error report */
#define MIN_ERR_OCCS (1)

/* how should the various reports be sorted? Legal values ar BYREQUESTS,
   BYPAGES, BYBYTES and ALPHABETICAL */
#define REQSORTBY (BYREQUESTS)
#define DOMSORTBY (BYBYTES)
#define DIRSORTBY (BYBYTES)
#define TYPESORTBY (BYBYTES)
#define HOSTSORTBY (ALPHABETICAL)
#define REFSORTBY (BYREQUESTS)
#define BROWSORTBY (BYREQUESTS)
#define FULLBROWSORTBY (BYREQUESTS)

#define WEEKBEGINSON (SUNDAY)
/* The 1st day of the week. I think it's SUNDAY, but you can choose any day. */

#define SEPCHAR ' '
/* The character to separate the thousands in a long number. I prefer ' ',
   but some people prefer '.' or ','. Use '\0' for no separator. Use '9' if
   you really want to inflate your statistics! :) */

#define REPSEPCHAR '\0'
/* The same for long numbers within the detailed output */

#define DECPOINT '.'
/* Character to use as the decimal point. */

#define PRESEP "\t"
/* A separator string between fields in preformatted output */

#define APPROXHOSTSIZE (500000)
/* The number of bytes to allocate for approximate hostname counting;
   more will make it more accurate. About 3 bytes per host should give
   you a very good estimate, though even 1 byte per host will be fair.
   If statistics for last 7 days are asked for, twice this amount of space
   will be used. */

#define BASEURL "none"
/* A string which is prepended to all URLs linked to in the request report.
   This is useful if you want to display the statistics on a different
   server from the one that generated the logfile. Use "none" for none. */

#define RAWBYTES (OFF)
/* whether bytes should be quoted in full (e.g., 1 021 453) as opposed to
   in kilobytes, megabytes etc. (e.g., 998k). */

#define HEADERFILE "none"
/* an extra file (with HTML markup if desired) placed between the page title
   and the start of the statistics. Use "none" for none. */

#define FOOTERFILE "none"
/* And one placed at the bottom of the page */

#define OUTPUT (HTML)
/* Default output type: ASCII, HTML or CACHE */

#define LANGUAGE (ENGLISH)
/* Default language for the output page:
   ENGLISH, US_ENGLISH, FRENCH, GERMAN, ITALIAN, SPANISH, DANISH */

#define DEBUG (0)
/* Level of debugging output: 0 = none, 1 = print all corrupt lines to stderr,
   2 = also print hosts with unknown domains. */

#define PROGRESSFREQ (0)
/* Frequency of progress reports. 0 for no reports. */

#define MARKCHAR '+'
/* a character for the graphical displays */

#define PAGEWIDTH (65)
/* the width of the output. Standard text screens have 80 columns, but for
   WWW browsers something like 65 is probably better. */

#define DIRLEVEL (1)
/* the level of directory report; see README */

#define COUNTHOSTS (ON)
/* whether we want to count the number of distinct hosts
   Can be ON, OFF or APPROX (for approximate count; in fact lower bound)
   FULLHOSTS ON turns COUNTHOSTS ON
   Turning this to OFF or APPROX will save a lot of memory */

#define LASTSEVEN (ON)
/* whether we want statistics for the last seven days */

#define WARNINGS (ON)
/* whether we want to see warnings */

#endif
