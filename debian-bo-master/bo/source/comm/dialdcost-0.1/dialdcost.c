/* 
   This software is FREEWARE
   It is programmed in 1997, by Ørn E. Hansen in conjunction with ISP
   connection through DIALD (DIAL on Demand) program.

   It is mainly thought of as a utility to read the logfile produced by
   diald and estimate the cost that has been accumulated with the
   internet connections made.

   The program has also been internationalized, so all messages can be
   translated through the GNU gettext program.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <stdarg.h>
#include <ctype.h>
#ifndef NO_INTL
#include <langinfo.h>
#include <libintl.h>

#define _(Text)  gettext(Text)

#define LOCALEDIR "/usr/share/locale"
#define PACKAGE   "dialdcost"
#else
#define _(Text)  Text
#endif

#define SETUP_FILE    "diald-cost.conf"
#define DEFAULT_SETUP "/etc/"SETUP_FILE

#define DELIM_CHAR    (';')

#define T_IN_SEC(h,m,s)            ((h*3600)+(m*60)+(s))
#define TIM_MIDN                   (24 * 3600)

#define DEF_LOGFILE    "/var/log/diald.log"
#define DEF_UNIT       60          /* One minute */
#define DEF_COST       0.10        /* A cent per minute */

void error_exit(char *, ...);
static unsigned char *strupr(char *);

char buf[256];

char *Days[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

char *file = DEF_LOGFILE;          /* default log file */
char *program_name;                /* the name of the program */
int unit = DEF_UNIT;               /* how many seconds in a unit */
double cost = DEF_COST;            /* what a unit costs in currency */

double cost_total = 0.0;           /* The total cost */

typedef struct tim_part {
  int start, end;                  /* the time this starts and ends */
  int unit;                        /* units in this part */
  double cost;                     /* Cost for each unit in this part */
  struct tim_part *next;           /* Pointer to next part, if any */
} TIM_PART;

static int show_help;              /* If show help only */
static int show_version;           /* If show version only */
static int verbose = 0;            /* If verbose format printing */
static int exclution = 0;          /* which days to exclude 0=none */

TIM_PART  *head = NULL;             /* pointer to first definition */

/* Some nice command line options */
static struct option const longopts[] = {
  {"file",    required_argument, NULL,         'f'},
  {"unit",    required_argument, NULL,         'u'},
  {"cost",    required_argument, NULL,         'c'},
  {"time",    required_argument, NULL,         't'},
  {"scratch", no_argument,       NULL,         's'},
  {"verbose", no_argument,       &verbose,      1},
  {"help",    no_argument,       &show_help,    1},
  {"version", no_argument,       &show_version, 1},
  {NULL,      0,                 NULL,          0}
};

void usage(status)
int status;
{
  if (status)
    printf(_("Try '%s --help' for further information\n"), program_name);
  else { 
    printf(_("Usage: %s ARGUMENTS\n"), program_name);
    printf(_("\
\n  Collects the output from DIALD log file, and calculates the estimated\n\
cost for the connection made.\n\n\
  -f, --file=FILE     Data comes from FILE\n\
  -u, --unit=UNIT     Each cost unit is UNIT seconds\n\
  -c, --cost=COST     Each unit cost COST\n\
  -s, --scratch       Scratch all setup, use built-in values\n\
  -t, --time=BEG-END  Mark cost for period BEG-END\n\
      --verbose       Be verbose, not just the calculated total\n\
      --help          Show this help, and exit\n\
      --version       Show version information, and exit\n\
\n\
  If -f argument is not used, the data is read from /var/log/diald.log as\n\
default.  If the argument given with '-f' is '-', then stdin is read.\n"));
  }
  exit(0);
}

/* Return the day of week */
int dayoweek(s)
char *s;
{
  int i;

  for(i=0;i<7;i++)
    if (strcasecmp(s, Days[i]) == 0)
      break;
  return i;
}

int nextday(d)
int d;
{
  if (++d > 6)
    return 0;
  return d;
}

/* Basically, this should read 8 has at 8:00:00 and 18:30 as such */
/* and allow specifying seconds                                   */
int tim_base(str)
char *str;
{
  int h=0,m=0,s=0;

  sscanf(str, "%d%*c%d%*c%d", &h, &m, &s);
  return T_IN_SEC(h,m,s);
}

/* Do the calculations.   First we find the period this cost belongs in
   and then we do calculations on that period.  If, there is any time
   left after the START:END period definiton, the global UNIT/COST is
   applied to that remaining duration.                                  */
void do_calculations(dow,t,dur)
int dow,t,dur;
{
  int glob_dur = 0;
  TIM_PART *x  = head;

  while (dur && x) {   /* Time unaccounted for and any period left */
    if(0 <= (t-x->start) && (t-x->start) < x->end) {  /* within period */
      int d_part = x->end-(t-x->start);  /* duration inside period */

      d_part = (d_part>dur ? dur : d_part);   /* part, inside duration */
      dur -= d_part;                          /* calculate any remains */
      t   += d_part;                          /* update the start time */
      if ((exclution & (1<<dow)) == 0)
	cost_total += (x->cost*d_part)/x->unit; /* cost of that part */
      else
	glob_dur   += d_part;                 /* this day excluded     */
      if (t >= TIM_MIDN)                      /* wrap around midnight  */
	t -= TIM_MIDN, dow=nextday(dow);
      }
    x = x->next;  /* Ok, next duration */
  }
  if (dur + glob_dur) /* remaining time, falls into global definitions */
    cost_total = cost_total + (cost*(dur+glob_dur))/unit;
}

/* Process a file, that contains diald log information */
int c_read(fp)
FILE *fp;
{
  int hour, min, sec, h=0,m=0,se=0;
  int duration=0, day, year, n, dow=0;
  char s[12], b[12], id[64];

  /* There is really only one word that interrests us inside the
     log file... i.e. for each line... the keyword               */

  /* Who knows... even DIALD might get internationalized.
     Not that I'm expecting miracles :-)                         */
  do {
    n = fscanf(fp, _(" %s %s %d %d:%d:%d %d %*s %s"),
                   s, b, &day, &hour, &min, &sec, &year, id);
    if (n == EOF) return duration;
    if (strcmp(id, _("Disconnected."))) {
      fscanf(fp, "%*[^\n]");
      if (strcmp(id, _("Connected")) == 0)
	h=hour,m=min,se=sec, dow=dayoweek(s);
    }
  } while(n==8 && strcmp(id, _("Disconnected.")));
  /* And for that time base... the duration is interesting */
  if (fscanf(fp, _(" Call duration %d seconds."), &duration) != 1) {
    printf(_("ERROR: near <%d:%s>\n"), n, id);
    exit(1);
  }
  do_calculations(dow,T_IN_SEC(h,m,se),duration);
  fscanf(fp, _(" IP transmited %*d bytes and received %*d bytes."));
  return duration;
}

int parse_file(file)
char *file;
{
  FILE *fp;
  int duration=0;

  if ((fp = fopen(file, "r")) == NULL)
    error_exit(_("Error opening %s\n"), file);
  while (!feof(fp)) duration+=c_read(fp);
  fclose(fp);
  return duration;
}

void error_exit(char *fmt, ...)
{
  va_list ap;
  char *p;

  if ((p = malloc(128)) == NULL)
    printf(_("PANIC: out of memory"));
  else {
    va_start(ap, fmt);
    vsnprintf(p, 128, fmt, ap);
    va_end(ap);
    printf(_("ERROR: %s\n"), p);
  }
  exit(1);
}

/* Recursively kill the time base */
TIM_PART *kill_time(t)
TIM_PART *t;
{
  if (t->next)
    kill_time(t->next);
  free(t);
  return NULL;
}

/* Partially recursive */
TIM_PART *tim_malloc(s, e, u, c, t)
int s,e,u;
double c;
TIM_PART *t;
{
  TIM_PART *x = malloc(sizeof(TIM_PART));

  if (x == NULL) {
    puts(_("Memory exhausted."));
    exit(1);
  }
  x->start = s;
  x->end   = e;
  x->unit  = u;
  x->cost  = c;
  if (x->end < x->start) {
    x->next = tim_malloc(0, e, u, c, t);
    x->end  = TIM_MIDN;     /* This period ends at midnight */
  } else
    x->next = t;
  x->end -= x->start;
  return x;
}

/* Convert characters in string s, to upper case */
static unsigned char *strupr(s)
char *s;
{
  for(;*s;s++)
    *s = toupper(*s);
  return s;
}

/* Read the setup file */
int SetupFile(s)
char *s;
{
  FILE *fp;
  char delim;

  if (s == NULL) s = DEFAULT_SETUP;
  if ((fp = fopen(s, "r")) == NULL)
    return 0;
  while(fscanf(fp, " %s", buf) != EOF) {
    strupr(buf);
    if (strcmp(buf, "EXCLUDE") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      strupr(buf);
      exclution|=1<<dayoweek(buf);
    } else if (strcmp(buf, "UNIT") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      unit = atoi(buf);
    } else if (strcmp(buf, "COST") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      cost = strtod(buf, NULL);
    } else if (strcmp(buf, "FILE") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      file = strdup(buf);
    } else if (strcmp(buf, "TIME") == 0) {
      char s1[24], s2[24];

      fscanf(fp, " %[^;\n]", buf);
      if (sscanf(buf, " %[^-]-%s", s1, s2) == 2)
	head = tim_malloc(tim_base(s1), tim_base(s2), unit, cost, head);
      else
	error_exit(_("Time format malformed"));
    } else
      error_exit(_("In file %s, unknown keyword %s"), s, buf);
    fscanf(fp, " %c", &delim);
    if ((char)delim != DELIM_CHAR)
      error_exit(_("Syntax near %s, expecting %c"), buf, DELIM_CHAR);
  }
  return 1;
}

void main(argc, argv)
int argc;
char **argv;
{
  int i,optc,duration = 0;
  char s1[24], s2[24], *e;

  program_name = argv[0];
#ifndef NO_INTL
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
#endif

  /* First, let us try read some default setup information, if it exists */
  /* it is either in the users home directory, or a site default inside  */
  /* /etc.                                                               */
  if ((e = getenv("HOME"))) {
    sprintf(buf, "%s/.%s", e, SETUP_FILE);
    e = strdup(buf);
    if (SetupFile(e) == 0)
      SetupFile(DEFAULT_SETUP);
    free(e);
  } else
    SetupFile(DEFAULT_SETUP);

  /* We may not have a Default Setup... or maybe we want to bypass it,   */
  /* so let the user give some nice command line options.                */
  while ((optc = getopt_long(argc,argv,"sf:c:u:",longopts, (int *)0))!=EOF) {
    switch(optc) {
    case 0:
      break;
    case 's':     /* Scratch any setup... use default settings */
      head = kill_time(head);
      unit = DEF_UNIT;
      cost = DEF_COST;
      file = DEF_LOGFILE;
      break;
    case 't':     /* new time base */
      i = sscanf(optarg, "%[^ -\n]-%s", s1, s2);
      if (i != 2)
	error_exit(_("Too few arguments"));
      head = tim_malloc(tim_base(s1), tim_base(s2), unit, cost, head);
      break;
    case 'f':     /* immediately parse the file given */
      if (strcmp(optarg, "-") == 0)
	while(!feof(stdin)) duration+=c_read(stdin);
      else
	duration += parse_file(optarg);
      file = NULL;
      break;
    case 'u':     /* set time unit */
      unit = atoi(optarg);
      break;
    case 'c':     /* and the cost for each unit */
      cost = strtod(optarg, NULL);
      break;
    default:
      usage(1);
    }
  }

  if (show_version) { puts(_("DIALD cost report (V 0.1) program")); exit(0); };
  if (show_help)    { usage(0); };
  if (file) duration += parse_file(file);
  if (verbose) {
    printf(_("Duration on calls %d:%d:%d seconds.\n"), duration/3600, (duration%3600)/60, duration%60);
    printf(_("Total amount %.2f\n"), cost_total);
  } else
    printf("%.2f", cost_total);
  exit(0);
}

