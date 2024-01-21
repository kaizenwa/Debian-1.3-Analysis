#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>

#include <X11/Xmu/Converters.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#ifndef NO_INTL
#include <langinfo.h>
#include <libintl.h>

#define _(Text)  gettext(Text)

#define LOCALEDIR "/usr/share/locale"
#define PACKAGE   "dialdcost"
#else
#define _(Text)  Text
#endif

#define MW            XtCreateManagedWidget
#define VaMW          XtVaCreateManagedWidget
#define ORIENT_HORIZ  XtNorientation, XtorientHorizontal
#define ORIENT_VERT   XtNorientation, XtorientVertical
#define F_VERT(w)     XtNfromVert, w
#define F_HOR(w)      XtNfromHoriz, w
#define LABEL(l)      XtNlabel, l

#define SETUP_FILE    "diald-cost.conf"
#define DEFAULT_SETUP "/etc/"SETUP_FILE
#define FIFO_FILE     "/var/log/diald.fifo"

#define DELIM_CHAR    (';')

#define T_IN_SEC(h,m,s)            ((h*3600)+(m*60)+(s))
#define TIM_MIDN                   (24 * 3600)

#define DEF_LOGFILE    "/var/log/diald.log"
#define DEF_FIFO       ".dialdcontrol"
#define DEF_UNIT       60          /* One minute */
#define DEF_COST       0.10        /* A cent per minute */

void error_exit(char *, ...);
static unsigned char *strupr(char *);
void Quit(), ddReset(), LineUp(), Block(), Force(), Timer(), Monitor();
Widget MakeStringBox(Widget, String, String), Labeled();
Widget MakeCommandButton(Widget, String, XtCallbackProc);
void SetLabel(Widget, String);
void MessagePrintf(Widget, String);
String GetString(Widget);

char buf[1024];

char *Days[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

float cost_total = 0.0;            /* The total cost */
int duration = 0;                  /* Duration in seconds */
int int_counter = 0;               /* Counter for monitoring */
int link_state = 0;                /* State of command link */

typedef struct tim_part {
  int start, end;                  /* the time this starts and ends */
  int unit;                        /* units in this part */
  float cost;                      /* Cost for each unit in this part */
  struct tim_part *next;           /* Pointer to next part, if any */
} TIM_PART;

TIM_PART *head = NULL;
FILE *FiFo, *FoFi;

char *ProgramName, *ff_file = NULL;
XtAppContext xtcontext;
XtIntervalId TimerID;
Widget message, CostW, TimeW, bState;
Widget State, if_lnk, if_lip, if_rip, mStatus, mLoad;
Widget mImp, mTimeot;

static int exclution = 0;

static char *WUp[]= { "<DN>", "<UP>" };
static char *WFc[]= { "<-->", "<FU>", "<FD>" };

static struct _PrivResources {
  float cost;                      /* Default cost */
  int unit;                        /* Default Unit */
  char *log_file;                  /* Default log file */
  char *fifo_file;                 /* Our fifo control file */
  char *comm_ch;                   /* Fifo Command channel */
} pres;

#define offset(field) XtOffsetOf(struct _PrivResources, field)
static XtResource resources[] = {
  { "file", "File", XtRString, sizeof (char *), 
    offset(log_file), XtRString, (XtPointer) DEF_LOGFILE },
  { "unit", "Unit", XtRInt, sizeof (int),
    offset(unit), XtRString, "0" },
  { "cost", "Cost", XtRFloat, sizeof (float),
    offset(cost), XtRString, "0.0" },
  { "fifo", "Fifo", XtRString, sizeof (char *),
    offset(fifo_file), XtRString, (XtPointer) NULL },
  { "commch", "Commch", XtRString, sizeof (char *),
    offset(comm_ch), XtRString, (XtPointer)FIFO_FILE },
};
#undef offset

static XrmOptionDescRec optionList[] =  {
  { "-file",    ".file", XrmoptionSepArg, (XPointer) NULL },
  { "-unit",    ".unit", XrmoptionSepArg, (XPointer) NULL },
  { "-cost",    ".cost", XrmoptionSepArg, (XPointer) NULL },
  { "-fifo",    ".fifo", XrmoptionSepArg, (XPointer) NULL },
  { "-commch", ".commch", XrmoptionSepArg, (XPointer) NULL },
};

static XtActionsRec priv_actions[] = {
  { "quit", Quit },
};

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
    cost_total = cost_total + (pres.cost*(dur+glob_dur))/pres.unit;
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
float c;
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
      pres.unit = atoi(buf);
    } else if (strcmp(buf, "COST") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      pres.cost = strtof(buf, NULL);
    } else if (strcmp(buf, "FILE") == 0) {
      fscanf(fp, " %[^ ;\n]", buf);
      pres.log_file = strdup(buf);
    } else if (strcmp(buf, "TIME") == 0) {
      char s1[24], s2[24];

      fscanf(fp, " %[^;\n]", buf);
      if (sscanf(buf, " %[^-]-%s", s1, s2) == 2)
        head = tim_malloc(tim_base(s1), tim_base(s2), pres.unit, pres.cost, head);
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

FILE *OpenFifo(name)
char *name;
{
  int fd;
  FILE *fp;

  if((fd = open(name, O_RDWR)) <= 0) {
    if (mkfifo(name, 0777) || (fd = open(name, O_RDWR)) <= 0) {
      fprintf(stderr, "Can't open FIFO %s\n", name);
      puts(_("\
  This program needs command channels to be aple to communicate with the\n\
diald program in a proper way.  Please create the channel, and have the\n\
proper permissions set for it, so the programs can communicate through it.\n\
Put the name of the file into your .Xdefaults file, or pass it to the\n\
program through the -fifo or -commch options."));
      exit(1);
    }
  }
  if ((fp = fdopen(fd, "r+")) == NULL) {
    fprintf(stderr, "Can't make FIFO %s into a stream\n", name);
    exit(2);
  }
  return fp;
}

main (argc, argv)
    int argc;
    char **argv;
{
    Arg args[3];
    Widget top, primary, w;
    char *message_str, *e;
    int message_len;

    ProgramName = argv[0];
#ifndef NO_INTL
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
#endif

    top = XtAppInitialize (&xtcontext, "Xdialdcost",
			   optionList, XtNumber(optionList), &argc, argv,
			   NULL, NULL, 0);

    XtVaSetValues(top, XtNinput, True, NULL);

    XtGetApplicationResources (top, (XtPointer) &pres, resources,
			       XtNumber(resources), NULL, 0);

    umask(S_IXUSR|S_IXGRP|S_IRWXO);

    if ((ff_file = pres.fifo_file) == NULL)
      ff_file = strdup(tmpnam(NULL));

    FiFo = OpenFifo(pres.comm_ch);
    FoFi = OpenFifo(ff_file);
    fprintf(FiFo, "monitor %s\n", ff_file), fflush(FiFo);

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

    if (pres.log_file)
      duration = parse_file(pres.log_file);

    primary = VaMW("primary", formWidgetClass, top, NULL);

    message = MakeStringBox(primary, "message", NULL);

    w = Labeled("box0", "timecost", boxWidgetClass, primary, message);
    XtVaSetValues(w, XtNborderWidth, 0, ORIENT_HORIZ, NULL);

    CostW = VaMW("cost", labelWidgetClass, w, LABEL("    0.00"), NULL);
    TimeW = VaMW("time", labelWidgetClass, w, LABEL("00:00:00"), NULL);

    sprintf(buf, "%5.2f", cost_total);
    SetLabel(CostW, buf);
    sprintf(buf,"%2d:%02d:%02d",duration/3600,(duration%3600)/60,duration%60);
    SetLabel(TimeW, buf);

    State  = Labeled("state", "lstate", labelWidgetClass, primary, w);
    if_lnk = Labeled("if_lnk","link", labelWidgetClass, primary, State);
    if_lip = Labeled("if_lip","local", labelWidgetClass, primary, if_lnk);
    if_rip = Labeled("if_rip", "remote", labelWidgetClass, primary, if_lip);
    mStatus= Labeled("status", "lstatus", labelWidgetClass, primary, if_rip);
    mImp   = Labeled("impuls", "limpuls", labelWidgetClass, primary, mStatus);
    mTimeot= Labeled("timeout", "ltimeout", labelWidgetClass, primary, mImp);
    mLoad  = Labeled("load", "lload", labelWidgetClass, primary, mTimeot);

    SetLabel(State,  "  STATE  ");
    SetLabel(if_lnk, "????");
    SetLabel(if_lip, "000.000.000.000");
    SetLabel(if_rip, "000.000.000.000");
    SetLabel(mStatus, "<--> <-->");
    SetLabel(mImp, "000 00 00 00");
    SetLabel(mTimeot, "00:00:00 00:00:00 00:00:00");
    SetLabel(mLoad,   "000000 000000");

    w = VaMW("box1", boxWidgetClass, primary, XtNborderWidth, 0, F_VERT(mLoad), ORIENT_HORIZ, NULL);
    MakeCommandButton(w, "cQuit", Quit);
    MakeCommandButton(w, "cMonitor", Monitor);
    MakeCommandButton(w, "cReset", ddReset);
    bState = MakeCommandButton(w, "cUp", LineUp);
    MakeCommandButton(w, "cBlock", Block);
    MakeCommandButton(w, "cForce", Force);

    TimerID=XtAppAddTimeOut(xtcontext, 1000L, Timer, NULL);

    XtAppAddActions(xtcontext, priv_actions, XtNumber(priv_actions));
    XtRealizeWidget(top);

    XtAppMainLoop(xtcontext);

    exit(0);
}

Widget Labeled(name, label, typ, parent, above)
char *name, *label;
WidgetClass typ;
Widget parent, above;
{
  Widget lw, w;

  lw = VaMW(label, labelWidgetClass, parent, F_VERT(above), XtNborderWidth, 0, NULL);
  w  = VaMW(name, typ, parent, F_HOR(lw), F_VERT(above), NULL);
  return w;
}

Widget MakeCommandButton(box, name, function)
Widget box;
char *name;
XtCallbackProc function;
{
  static Arg args[] = {
    {XtNborderWidth, (XtArgVal) 1},
    {XtNjustify, (XtArgVal)XtJustifyRight}
  };
  Widget w = XtCreateManagedWidget(name, commandWidgetClass, box,
                                   args, XtNumber(args));
  if (function != NULL)
    XtAddCallback(w, XtNcallback, function, (caddr_t)NULL);
  return w;
}

Widget MakeStringBox(parent, name, string)
Widget parent;
String name, string;
{
  Arg args[5];
  Cardinal numargs = 0;
  Widget StringW;

  XtSetArg(args[numargs], XtNeditType, XawtextEdit); numargs++;
  XtSetArg(args[numargs], XtNstring, string); numargs++;

  StringW = XtCreateManagedWidget(name, asciiTextWidgetClass,
                                  parent, args, numargs);
  return StringW;
}

void SetLabel(w, str)
Widget w;
String str;
{
  Arg arglist[1];

  XtSetArg(arglist[0], XtNlabel, str);
  XtSetValues(w, arglist, ONE);
}

String GetString(w)
Widget w;
{
  String str;
  Arg arglist[1];
  
  XtSetArg(arglist[0], XtNstring, &str);
  XtGetValues( w, arglist, ONE);
  return(str);
}

void MessagePrintf(mesg, str)
Widget mesg;
char *str;
{
  XawTextBlock text;
  static XawTextPosition pos = 0;

  text.length = strlen(str);
  text.ptr = str;
  text.firstPos = 0;
  text.format = FMT8BIT;

  XawTextReplace( mesg, pos, pos, &text);

  pos += text.length;
  XawTextSetInsertionPoint(mesg, pos);
}

void Quit()
{
  extern void exit();

  fprintf(FiFo, "monitor\n"), fflush(FiFo);
  fclose(FiFo);
  fclose(FoFi);
  if (pres.fifo_file == NULL)
    unlink(ff_file);
  XtRemoveTimeOut(TimerID);
  XtDestroyApplicationContext(xtcontext);
  exit(0);
}

void Monitor()
{
  if (int_counter >= 20) {
    MessagePrintf(message, _("\nAsking to reopen message channel"));
    fprintf(FiFo, "monitor %s\n", ff_file);
  } else {
    MessagePrintf(message, _("\nClosing message channel"));
    fprintf(FiFo, "monitor \n");
  }
  fflush(FiFo);
}

void ddReset()
{
  fprintf(FiFo, "reset\n"), fflush(FiFo);
}

ReadN(s, n)
char *s;
int n;
{
  while (n--) {
    fscanf(FoFi, " %s", s);
    s += strlen(s);
    if (n > 0) {
      *s = ' ';
      s++;
    }
  }
}

LinkCounter(status)
int status;
{
  static int seconds = 0;
  static time_t tim;

  switch(status) {
  case 0:
    if (link_state) {
      struct tm *lt = localtime(&tim);

      link_state = 0;
      seconds = time(NULL) - tim;
      do_calculations(lt->tm_wday,T_IN_SEC(lt->tm_hour,lt->tm_min,lt->tm_sec),
                      seconds);
      duration = duration + seconds;
      sprintf(buf, "%5.2f", cost_total);
      SetLabel(CostW, buf);
      sprintf(buf,"%2d:%02d:%02d",duration/3600,(duration%3600)/60,duration%60);
      SetLabel(TimeW, buf);
    }
    break;
  case 1:
    if (link_state == 0)  /* Could be a retry */
      time(&tim);
    link_state = status;
    break;
  default:
  }
}

/* Executed once every second */
void Timer()
{
  static char s[64];
  struct stat st;

  fstat(fileno(FoFi), &st);
  if (st.st_size > 8) {
    if (int_counter) {
      if (int_counter >= 50 || ++int_counter == 0)
	MessagePrintf(message, "\n"), int_counter = 0;
    }
    fscanf(FoFi, " %s", s);
    if (strcmp(s, "STATE") == 0) {
      fscanf(FoFi, " %s", s);
      if (strcmp(s, _("STOP_LINK")) == 0) LinkCounter(0);
      else if (strcmp(s, _("START_LINK")) == 0) LinkCounter(1);
      else if (strcmp(s, "UP") == 0) SetLabel(bState, "Down");
      else if (strcmp(s, "DOWN") == 0) SetLabel(bState, " Up ");
      SetLabel(State, s);
    } else if (strcmp(s, "QUEUE") == 0) {
      *s = 0;
    } else if (strcmp(s, "END") == 0) {
      fscanf(FoFi, " %[^\n]", s);            /* should be QUEUE */
    } else if (strcmp(s, "STATUS") == 0) {
      int a, b;

      fscanf(FoFi, " %d %d", &a, &b);
      ReadN(s, 4);
      SetLabel(mImp, s);
      ReadN(s, 3);
      SetLabel(mTimeot, s);
      sprintf(s, "%s %s", WUp[a], WFc[b]);
      SetLabel(mStatus, s);
    } else if (strcmp(s, "INTERFACE") == 0) {
      char a[64], b[64];
      fscanf(FoFi, " %s %s %s", s, a, b);
      SetLabel(if_lnk, s);
      SetLabel(if_lip, a);
      SetLabel(if_rip, b);
    } else if (strcmp(s, "LOAD") == 0) {
      ReadN(s, 2);
      SetLabel(mLoad, s);
    } else if (strcmp(s, "MESSAGE") == 0) {
      fscanf(FoFi, " %[^\n]", s);
      MessagePrintf(message, s);
      int_counter = -50;
    } else 
      fscanf(FoFi, " %[^\n]", s);            /* bit bucket */
  } else if (int_counter < 50) {
    if(++int_counter == 50)
      MessagePrintf(message, _("\nChannel may be closed"));
  }
  TimerID=XtAppAddTimeOut(xtcontext, 200L, Timer, NULL); /* wait a sec */
}

int FifoCommandLabel(Widget w, int State, char *On, char *Off)
{
  char *cpy, *cmd;

  if (State = !State)
    cpy = strdup(Off), cmd = On;
  else
    cpy = strdup(On), cmd = Off;
  fprintf(FiFo, "%s\n", cmd), fflush(FiFo);
  cpy[0] = toupper(cpy[0]);
  SetLabel(w, cpy);
  free(cpy);
}

void LineUp(Widget w, XtPointer client_d, XtPointer call_d)
{
  if (link_state)
    fprintf(FiFo, "down\n");
  else
    fprintf(FiFo, "up\n");
  fflush(FiFo);
}

void Block(Widget w, XtPointer client_d, XtPointer call_d)
{
  static int state = 0;

  state = FifoCommandLabel(w, state, "block", "unblock");
}

void Force(Widget w, XtPointer client_d, XtPointer call_d)
{
  static int state = 0;

  state = FifoCommandLabel(w, state, "force", "unforce");
}

