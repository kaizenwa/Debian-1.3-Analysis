/* Set the three #defines in the section below as appropriate. */
/*--------------------------------------------------------------------------*/
#define MSDOS 1     /* un-define if not MSDOS, use OS2 or UNIX instead.     */
 
#define USE_COLOR   /* un-define if not MSDOS nor OS/2, or not using colors.*/
                    /* USE_COLOR assumes that either OS2 or MSDOS is used.  */
                    /* For colors in unix, #define USE_ANSI instead.        */
 
#define USE_ANSI    /* Will work with DOS, OS/2, or Unix.  This is #undef'd */
                    /* automatically if used with USE_COLOR for MSDOS or    */
                    /* OS/2, because USE_COLOR cannot coexist with USE_ANSI.*/
/*--------------------------------------------------------------------------*/
 
#ifdef OS2
#undef MSDOS
#undef UNIX
#endif
 
/* Some self-defining unix variables */
#if defined(__unix__) || defined(UNIX)
#ifdef MSDOS
#undef MSDOS
#endif /* MSDOS */
#ifdef USE_COLOR
#undef USE_COLOR
#endif /* USE_COLOR */
#ifndef UNIX
#define UNIX 1        /* Delete if not running on UNIX */
#endif
#endif /* __unix__ */
 
/* correct non-coexisting definitions */
#if defined(OS2) || defined(MSDOS)
#if defined(USE_COLOR) && defined(USE_ANSI)
#undef USE_ANSI
#endif
#endif
 
/*===========================================================================
 * cal.c - print calendar for one month or one year
 * compatible with unix cal command
 * version 3.5
 *
 * cal [-options] [[month] year]    (numerical month argument)
 * or
 * cal [-options] [month] [year]    (verbal month argument)
 *
 * cal (C) 1992 by Unicorn Research Corporation
 * Inspired by the unix `cal' command for an Amiga, ported by Gary L. Brant.
 * Compile with MSDOS defined (above) to activate the MSDOS features.
 *
 * Borland users: PLEASE try not to Borland-ize this code.  Use
 * #ifdef __TURBOC__ on any Borland-specific sections you create.
 *===========================================================================
 
29 March 1993, Darrel Hankerson hankedr@mail.auburn.edu
  Modifications and enhancements for OS/2 (and others).
  New command line options:
    [-nodata] [-data-file=data-file]
  and color options (if compiling with -DUSE_COLOR for OS/2 or MSDOS)
    [-nocolor] [-color-file=color-file]
 
05 June 1993, Don Retzlaff donr@gab.unt.edu, University of North Texas
  Modifications and enhancements (re-done by Alex Matulich for
  ANSI compatibility):
  Variable-number of events for month display (previously only allowed 8)
  New command line option:
    [--maxappts=num]
  Allow multiple --datafile= command line parameters
 
31 August 1995, Chris Bagwell cbagwell@aud.alcatel.com
  Added ANSI color support and a few updates for compiling easier under
  Linux and SunOS.
  Added -8bit option for enabling 8-bit characters under unix.
 
11 January 1996, Dan Fandrich dan@fch.wimsey.bc.ca
  Added `reminder(1)'-style date file.
  Extracted some code into functions for readability.
 
BASIC DATA STRUCTURE:                                          specialdays()
                                                               pointers:
 char *buf ==>  line[0] -> [......LineWid (default 80).......] lln[2]
                line[1] -> [.................................] lln[3]
                line[2] -> [.................................] lln[4]
                      ...  [.................................] ...
       line[numappts-3] -> [.................................] lln[numappts-1]
        str (work area) -> [.................................]
            spcldesc[0] -> [....DAY_DESC (56).....]            lln[0]
            spcldesc[1] -> [....DAY_DESC (56).....]            lln[1]
 
           line_attr[0] -> [........LineWid (80).............]
           line_attr[1] -> [.................................] allocated
           line_attr[2] -> [.................................] only if
                      ...  [.................................] colors
  line_attr[numappts-3] -> [.................................] are used
               mon_attr -> [.................................]
                wk_attr -> [.................................]
               day_attr -> [......YEARWID (72)...........]
           spclday_attr -> [....DAY_DESC (56).....]
*/
 
#define MAXFILES 8   /* maximum number of data file paths */
#define MAXAPPTS 50  /* maximum number of appointment description lines */
 
#ifdef USE_COLOR
#ifdef OS2
#define INCL_SUB
#include <os2.h>
#else
#include <dos.h> /* for int86() */
#endif /* OS2 */
#endif /* USE_COLOR */
 
#include <stdio.h>
#ifndef UNIX
#include <io.h>  /* for isatty() */
#else
#include <unistd.h>  /* sun4/linux place for isatty() */
#endif
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
 
#ifndef max
#define max(a,b) (((a)>(b))?(a):(b))
#endif /* max */
 
/* Note: Other unix systems may require this next re-define to work. */
/* I believe that SCO is one such system.  Please send in any fixes  */
/* needed to get your system running.                                */
#ifdef __linux__          /* Make linux compatible with stricmp() */
#define stricmp(a,b) strcasecmp(a,b) /* case-insensitive string comparison */
#endif
 
#ifdef __ZTC__            /* make Zortech compatible with stricmp() */
#define stricmp(a,b) strcmpl(a,b) /* case-insensitive string comparison */
#endif /* __ZTC__ */
 
#ifdef MSDOS
#ifndef MK_FP
#define MK_FP(seg,off) (void far *)(((unsigned long)(seg)<<16)|(unsigned)(off))
#endif /* MK_FP */
#ifndef USE_ANSI
union REGS reg;
char far *video, far *tmpvideo;
#endif
#endif /* MSDOS */
 
#define FUDGE1  1               /* needed to make day of week come out right */
#define FUDGE2  6               /* for old style (Julian) calendar           */
#define MONWID  24              /* width of month                            */
#define YEARWID (3*MONWID-2)    /* width of yearly calendar                  */
#define DAY_DESC  (LineWid-MONWID)  /* width of special day descriptions     */
 
#if defined(USE_COLOR)  || defined(USE_ANSI)    /* define display attribute */
#define FG_BLACK     0x00                       /* possibilities. */
#define FG_BLUE      0x01
#define FG_GREEN     0x02
#define FG_CYAN      0x03
#define FG_RED       0x04
#define FG_VIOLET    0x05
#define FG_ORANGE    0x06
#define FG_LTGRAY    0x07
#define FG_DKGRAY    0x08
#define FG_BRTBLUE   0x09
#define FG_BRTGREEN  0x0a
#define FG_BRTCYAN   0x0b
#define FG_BRTRED    0x0c
#define FG_BRTVIOLET 0x0d
#define FG_YELLOW    0x0e
#define FG_WHITE     0x0f
#define FG_FLASH     0x80
#define BG_BLACK     0x00
#define BG_BLUE      0x10
#define BG_GREEN     0x20
#define BG_CYAN      0x30
#define BG_RED       0x40
#define BG_VIOLET    0x50
#define BG_ORANGE    0x60
#define BG_WHITE     0x70
 
unsigned
char dfmon_attr   = FG_BLACK|BG_GREEN,   /* video attribute for month name */
     dfwk_attr    = FG_BLUE|BG_CYAN,     /* attribute for weekday header   */
     dfdy_attr    = FG_BLACK|BG_WHITE,   /* attribute for days             */
     dfsun_attr   = FG_VIOLET|BG_WHITE,  /* attribute for sundays          */
     dftoday_attr = FG_YELLOW|BG_BLUE,   /* attribute for current day      */
     dfbk_attr    = FG_LTGRAY|BG_BLACK,  /* year calendar background       */
     dfspday_attr = FG_BRTCYAN|BG_BLACK, /* special day description attr.  */
     dfspdfl_attr = FG_BRTRED|FG_FLASH,  /* special day flasher            */
     dftoday_attr2;                      /* this must be initialized later */
 
char *mon_attr, *wk_attr, *day_attr, *spclday_attr, *line_attr[MAXAPPTS-2];
 
int crt;
int eightbit;
#ifndef UNIX
char *color_file = "cal.col";
#else
char *color_file = "calcol";
#endif /* UNIX */
#endif /* USE_COLOR or USE_ANSI */
 
/* LineWid may change depending on terminal (MSDOS only).             */
/* This variable determines memory allocation size and display width. */
short LineWid = 80;                /* set to 80 for non-MSDOS systems */
 
#ifndef UNIX
char *data_file = "cal.dat";
#else
char *data_file = "caldat";
#endif
 
#define REMINDER_FILE "dates"
 
short days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    mdays[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
    multi = 0, thisday, thismonth, thisyear, discard_old = 0, prog_pause = 0,
    europe = 0, numfiles = 0,
    numappts = 24;  /* default maximum number of appointments to display */
 
char *months[] = {
     "January ", "February ", "March ", "April ", "May ", "June ", "July ",
     "August " , "September ", "October ", "November ", "December " },
     monthline[] = "         ---                     ---                     ---          ",
     *monthpos[] = { monthline+9, monthline+33, monthline+57 },
     USdays[] =     " Su Mo Tu We Th Fr Sa ",
     Europedays[] = " Mo Tu We Th Fr Sa Su ",
     *dayline = USdays,
     *spcldesc[2],
     *filename[MAXFILES],
     *homedir,                  /* home directory from argv[0] or enviornment*/
     *line[MAXAPPTS-2],         /* line buffer for body of calendar          */
     *buf = NULL,       /* workspace memory into which pointers are assigned */
     *str;              /* temporary string workspace (LineWid characters)   */
 
void  addfilename(char *);
void  clearfilenames(void);
void  printmonth (short, short);
short putd(register short);
void  fixtab(register short);
short weekday(register short, register short);
void  putattr(char *, register short);
void  setcolor(char);
void  ansiputs(char *, char *);
short specialdays(short, short);
short reminderdays(short, short);
FILE *efopen(const char *, const char *);
void  usage(void);
 
/* commandline argument list */
static struct arglist {
    char *opt;         /* full argument string */
    short needsvalue;  /* 1=argument needs a value, 0=stand-alone */
} opt[] = {
    { "nodata",     0 },
    { "data-file",  1 },
    { "future",     0 },
    { "today",      0 },
    { "europe",     0 },
    { "maxappts",   1 },
    { "pause",      0 },
#if defined(USE_COLOR) || defined(USE_ANSI)
    { "nocolor",    0 },
    { "color-file", 1 },
#ifdef UNIX
    { "8bit",       0 },
#endif
#endif
    { NULL,         0 }
};
 
/*===========================================================================
 * GETARG
 * Return the index of the argument in the option array.  val is set if the
 * argument needs a value.  The passed arg to test does not have to be
 * exactly like one of the arguments in o; it just has to match one of them
 * enough to be uniquely identified.  Any arg that is passed here should
 * begin with a '-' (or also a '/' if compiling for MSDOS or OS/2).
 *=========================================================================*/
int getarg(struct arglist *o, char *arg, char **val)
{
    int len = -1, i = -1, found = -1;
    *val = NULL;
 
    while (*arg == '-'
           #if (defined(MSDOS) || defined(OS2))
           || *arg == '/'
           #endif
        ) ++arg;
    while (arg[++len]) {
        arg[len] = tolower(arg[len]);
        if (arg[len] == '=') {
            arg[len] = '\0';
            *val = &arg[len+1];
            break;
        }
    }
    while (o[++i].opt) {
        if (!memcmp(o[i].opt, arg, len)) {
            if (found >= 0) return -1;
            found = i;
        }
    }
    if (found < 0) return -1;
    if (o[found].needsvalue && !(*val)) return -1;
    return found;
}
 
/*===========================================================================
 * setopt
 * This function will set a global option based on the value passed in.
 * The value passed in must be a valid index into the options array.
 *=========================================================================*/
void setopt(int c, char *optarg)
{
    switch (c) {
    case 0:
        data_file = NULL; break;
    case 1:
        addfilename(optarg);
        break;
    case 2:
        discard_old = 1; break;   /* show future dates only */
    case 3:
        discard_old = 2; break;   /* show today's dates only */
    case 4:
        dayline = Europedays;  europe = 1;  break;
    case 5:
        numappts = atoi(optarg); break;
    case 6:
        prog_pause = 1; break;
#if defined(USE_COLOR) || defined(USE_ANSI)
    case 7:
        crt = 0; break;
    case 8:
        color_file = optarg; break;
#ifdef UNIX
    case 9:
        eightbit = 1; break;
#endif
#endif /* USE_COLOR || USE_ANSI */
    }
}
 
/*===========================================================================
 * MAIN
 * Process command line, allocate and initialize, etc.
 *=========================================================================*/
int main(int argc, char *argv[])
{
    short       bufsiz, i, m, y;
    time_t      t;
    struct tm  *lt;
    int         c;
    char       *optarg;
    char       *calopt, *s;
    int         status=0;
 
#if defined(USE_COLOR) || defined(USE_ANSI)
    FILE *colorfile;
    short fg[8], bg[8];
    crt = isatty(fileno(stdout));  /* check if stdout is being redirected */
#ifndef UNIX
    eightbit = 1;
#else
    eightbit = 0;
#endif /* UNIX */
#endif /* USE_COLOR || USE_ANSI */
 
    clearfilenames();
 
    /* First, set any options from the Env. Variable CALOPT */
    calopt = getenv("CALOPT");
    s = strtok(calopt," ");
    while (s != NULL) {
        if (*s != '-'
            #if (defined(MSDOS) || defined(OS2))
            && *s != '/'
            #endif
            ) break;
        if ((c = getarg(opt, s, &optarg)) < 0)
        {
            usage();
            status=1;   /* Let user known program didn't run */
            goto freestuff;
        }
        setopt(c, optarg);
        s = strtok(NULL," ");
    }
 
    /* Next, loop through command line options and set parameters */
    for (i = 1; i < argc; ++i) {
        if (*argv[i] != '-'
            #if (defined(MSDOS) || defined(OS2))
            && *argv[i] != '/'
            #endif
            ) break;
        if ((c = getarg(opt, argv[i], &optarg)) < 0)
        {
            usage();
            status=1;   /* Let user know program didn't run */
            goto freestuff;
        }
        setopt(c, optarg);
    }
 
    if (numappts < 8 || numappts > MAXAPPTS) {
        usage();
        status=2;  /* Let user know failed because to many datafiles */
        goto freestuff;
    }
 
    if (!numfiles && data_file)  /* add default data file if none specified */
        addfilename(data_file);
 
    /* make the argument list compatible with unix cal */
    argc = argc - i + 1;
    for (m = 1; m < argc; ++m) argv[m] = argv[i++];
 
#ifdef MSDOS
#ifndef USE_ANSI
    reg.h.ah = 0x0f;       /* function 0x0F of INT 0x10 - get display mode */
    int86(0x10, &reg, &reg);
    LineWid = max(80, reg.h.ah);  /* number of columns returned in AH */
    /* The above should work for all displays.
     * The following can be used instead, but will work in EGA and VGA only:*/
    /* Linewid = *((unsigned far *)0x004004a); */
#endif
#endif /* MSDOS */
 
    /* allocate workspace buffer */
 
    bufsiz = (numappts+1)*LineWid + 2*DAY_DESC;
#if defined(USE_COLOR) || defined(USE_ANSI)
    /* adjust workspace for color enhancements */
    bufsiz += (numappts+3)*LineWid + YEARWID + DAY_DESC;
#endif /* USE_COLOR || USE_ANSI */
    if ((buf = (char *)calloc(bufsiz,1)) == NULL) {
        puts("Memory overflow");
        goto freestuff;
    }
 
    /* assign pointers into buf */
 
    line[0] = buf;
    for (i = 1; i < numappts-2; i++)
        line[i] = line[i-1] + LineWid;               /* calendar body    */
    str = line[i-1] + LineWid;                       /* string workspace */
    *(spcldesc[0] = str + LineWid) = '\0';           /* description 0    */
    *(spcldesc[1] = spcldesc[0] + DAY_DESC) = '\0';  /* description 1    */
    /* descriptions 2,3,... point into line[] */
 
    /* find originating directory */
 
#ifndef UNIX
    homedir = argv[0];
    i = strlen(homedir);
    while (i >= 0 && homedir[i] != '/' && homedir[i] != '\\' &&
           homedir[i] != ':')
        homedir[i--] = '\0';
#else
    homedir = getenv("HOME");
#endif
 
#if defined(USE_COLOR) || defined(USE_ANSI)         /* color attributes */
    line_attr[0] = spcldesc[1] + DAY_DESC;
    for (i = 1; i < numappts-2; i++) line_attr[i] = line_attr[i-1] + LineWid;
    mon_attr = line_attr[i-1] + LineWid;
    wk_attr = mon_attr + LineWid;
    day_attr = wk_attr + LineWid;
    spclday_attr = day_attr + YEARWID;
 
    /* attempt to read in file of colors */
    if (!crt || (colorfile = efopen(color_file, "r")) == NULL) goto contsetup;
    for (i = 0; i < 8; i++) {
        if (fgets(str, LineWid, colorfile) == NULL) break;
        if ((fg[i] = atoi(str)) == (bg[i] = atoi(&str[3]))) break;
        if (fg[i] > 15 || bg[i] > 15) break;
    }
    fclose(colorfile);
    if (i < 8) goto contsetup;
    dfmon_attr   = fg[0] | (bg[0] << 4);
    dfwk_attr    = fg[1] | (bg[1] << 4);
    dfdy_attr    = fg[2] | (bg[2] << 4);
    dfsun_attr   = fg[3] | (bg[3] << 4);
    dftoday_attr = fg[4] | (bg[4] << 4);
    dfbk_attr    = fg[5] | (bg[5] << 4);
    dfspday_attr = fg[6] | (bg[6] << 4);
    dfspdfl_attr = fg[7] | (bg[7] << 4);
 
    /* initialize color attribute strings */
contsetup:
    memset (mon_attr, dfmon_attr, LineWid);
    memset (wk_attr, dfwk_attr, LineWid);
    memset (day_attr, dfdy_attr, YEARWID);
    memset (spclday_attr, dfspday_attr, DAY_DESC);
    spclday_attr[0] = dfspdfl_attr;
    for (i = MONWID-2; i <= MONWID+MONWID; i += MONWID) {
        memset (&mon_attr[i], dfbk_attr, 2);
        memset (&wk_attr[i], dfbk_attr, 2);
        memset (&day_attr[i], dfbk_attr, 2);
        memset (&day_attr[i - MONWID + 3 + (18*europe)], dfsun_attr, 2);
    }
    mon_attr[i] = dfbk_attr;  /* "termination" attr. for ansiputs() */
    memset (&day_attr[i - MONWID + 3 + (18*europe)], dfsun_attr, 2);
    dftoday_attr2 = ((dfdy_attr & '\x70')|(dftoday_attr >> 4));
 
#ifdef MSDOS
#ifndef USE_ANSI
    int86(0x11, &reg, &reg);  /* read the equipment list into reg. */
    video = (char far *)MK_FP((((reg.x.ax & 48) == 48) ? 0xb000 : 0xb800), 0);
#endif
#endif /* MSDOS */
 
#endif /* USE_COLOR || USE_ANSI */
 
    /* now that all the setup is done, we can begin with the program proper */
 
    t = time(NULL);
    lt = localtime(&t);
    thisyear = y = lt->tm_year + 1900;
    thismonth = m = lt->tm_mon + 1;
    thisday = lt->tm_mday;
    puts("");           /* first display a blank line */
 
    switch (argc) {
    case 1:             /* display current month if no arguments */
        fixtab (y);
        printmonth (m, y);
        break;
 
    case 2:
        if (isdigit(argv[1][0]) || (isdigit(argv[1][1]) &&
                                    argv[1][0] == '-')) {
            multi = 1;
            fixtab (y = atoi(argv[1]));
            fputs("\t\t\t\t ", stdout);
            putd(y);         /* put year */
            puts("\n");
            for (m = 1; m < 13; m++) printmonth (m, y);
            break;
        }
        /* drop through to next case */
 
    case 3:
        m = atoi(argv[1]);
        if (strlen(argv[1]) >= 3) {
            for (i = 0; i < 12; i++) {
                strcpy(str, months[i]);
                str[3] = argv[1][3] = '\0';
                if (!stricmp(argv[1], str)) {
                    m = i + 1;
                    break;
                }
            }
        }
        if (!m) {
            usage();
            status=3;   /* Let user know failed because invalid month */
            break;
        }
        if (argc == 3) {
            if (isdigit(argv[2][0]) || (isdigit(argv[1][1]) &&
                                        argv[2][0] == '-'))
                y = atoi(argv[2]);
            else {
                usage();
                status=4;  /* failed because invalid number parameters */
                break;
            }
        }
        fixtab(y);
        printmonth(m, y);
        break;
 
    default:
        usage();
        status=4;  /* failed because invalid number parameters */
    }
 
    if (prog_pause) {
        fputs("\nPress the RETURN key to continue.", stderr);
        fflush(stdin);
        fgets(str, 3, stdin);
    }
freestuff:
    free(buf);
    return (status);
}
 
 
/*===========================================================================
 * addfilename()
 * append a file name into the list of files to read
 * name must be a static string
 *=========================================================================*/
void addfilename(char *name)
{
    if (numfiles < MAXFILES && name) filename[numfiles++] = name;
}
 
/*===========================================================================
 * clearfilenames()
 * empty the list of file names
 * NOTE: does not free() any entries
 *=========================================================================*/
void clearfilenames(void)
{
    numfiles = 0;
}
 
 
/*===========================================================================
 * printmonth()
 * either prints an entire month at a time or multiplexes
 * a month into a buffer, dumping the buffer after the third call.
 *=========================================================================*/
void printmonth(short m, short y)
{
    register short first, last;
    register short index, p = 0;
    register char *ll;
    static short q = 0;
    short l, hilite = ((m == thismonth) && (y == thisyear)), num_appts = 0;
 
    --m;
    if (multi) {
        q++;
        if (q > 3) q = 1;
        p = MONWID * (q - 1);        /* character position of line in buffer */
        (void) memcpy(monthpos[q-1], months[m], 3);       /* copy month name */
        if (q == 3) {
            #ifdef USE_ANSI
            ansiputs(monthline,mon_attr);
            #else
            puts(monthline);
            #endif
            #ifdef USE_COLOR
            putattr(mon_attr, YEARWID);
            #endif /* USE_COLOR */
            for (index = 0; index < 2; ++index) {
            #ifdef USE_ANSI
                setcolor(dfwk_attr);
            #endif
                fputs(dayline, stdout);
                #ifdef USE_ANSI
                setcolor(dfbk_attr);
                #endif
                fputs("  ", stdout);
            }
            #ifdef USE_ANSI
            ansiputs(dayline, wk_attr);
            #else
            puts(dayline);
            #endif
            #ifdef USE_COLOR
            putattr(wk_attr, YEARWID);
            #endif /* USE_COLOR */
            #if defined(USE_COLOR) || defined(USE_ANSI)
            for (l = 0; l < 6; l++) memcpy(line_attr[l], day_attr, LineWid);
            #endif /* USE_COLOR || USE_ANSI */
        }
    }
    else {
        q = 1;         /* initialize attribute buffers for month display */
#if defined(USE_COLOR) || defined(USE_ANSI)
        for (l = 0; l < numappts-2; l++) {
            if (l<6)
                memcpy(line_attr[l], day_attr, MONWID-2);
            else
                memset(line_attr[l], 0, MONWID-2);
            line_attr[l][MONWID-2] = dfspdfl_attr;
            memcpy(&line_attr[l][MONWID-1], spclday_attr, DAY_DESC);
        }
        memcpy(&mon_attr[MONWID-1], spclday_attr, DAY_DESC);
        memcpy(&wk_attr[MONWID-1], spclday_attr, DAY_DESC);
        mon_attr[MONWID-2] = wk_attr[MONWID-2] = dfspdfl_attr;
#endif /* USE_COLOR || USE_ANSI */
    }
 
    if (!p) memset(line[0], ' ', (numappts-2)*LineWid);
 
    if (y == 1752 && m == 8) {      /* special case Sep. 1752 */
        line[0][p + 8] = '1';
        line[0][p + 11] = '2';
        first = 14;
        last = 16;
        index = 12;
    }
    else {
        short dow = weekday(m, y);   /* day of week for first day of month */
        first = 1;
        last = 7 - dow;
        index = 3 * dow;
    }
 
    for (l = 0; l < 6; ++l) {       /* loop thru month one week per line */
        ll = line[l] + p + 1;
        while (first <= last) {      /* for each day in week encode day of month */
            if (first >= 10) ll[index] = '0' + first / 10;
            ll[index+1] = '0' + first % 10;
            if (!multi && hilite && first == thisday) {  /* hilight today */
                #if defined(USE_COLOR) || defined(USE_ANSI)
                line_attr[l][index+1] = line_attr[l][index+2] = dftoday_attr;
                line_attr[l][index] = line_attr[l][index+3] = dftoday_attr2;
                ll[index-1] = (crt && eightbit) ? '\xde' : '<';
                ll[index+2] = (crt && eightbit) ? '\xdd' : '>';
                #else
                ll[index-1] = '<';
                ll[index+2] = '>';
                #endif /* USE_COLOR || USE_ANSI */
            }
            index += 3;
            ++first;
        }
        last = (last + 7) > days[m] ? days[m] : last + 7;
        index = 0;
    }
 
    if (!multi) {
        short i, yw;
        /* the next 2 lines assume that the shortest-length month name
         * in months[], such as "June ", will be 5 characters long. */
        char mtmp[] = "             ";     /* 13 spaces */
        mtmp[l = (18 - (i = strlen(months[m]))) >> 1] = '\0';
        #ifdef USE_ANSI
        setcolor(dfmon_attr);
        #endif
        fputs(mtmp, stdout);
        fputs(months[m], stdout);          /* put month name */
        yw = putd(y);                      /* put year */
        num_appts = specialdays(m+1, y);
        #ifdef USE_REMINDER
        if (num_appts < 1)
            num_appts = reminderdays(m+1, y);
        #endif
        mtmp[l] = ' ';
        mtmp[MONWID - l - i - yw - 2] = '\0';
        fputs(mtmp, stdout);
        #ifdef USE_ANSI
        setcolor(dfspday_attr);
        #endif
        if (num_appts) {
            fputs(" ", stdout);
            #ifdef USE_ANSI
            ansiputs(spcldesc[0],line_attr[0]+23);
            #else
            puts(spcldesc[0]);              /* put first description */
            #endif
        }
        else puts("");
        #ifdef USE_COLOR
        putattr(mon_attr, LineWid-1);
        #endif /* USE_COLOR */
        #ifdef USE_ANSI
        setcolor(dfwk_attr);
        #endif
        fputs(dayline, stdout);
        #ifdef USE_ANSI
        setcolor(dfspday_attr);
        #endif
        if (num_appts > 1) {
            fputs(" ", stdout);
            #ifdef USE_ANSI
            ansiputs(spcldesc[1],line_attr[1]+23);
            #else
            puts(spcldesc[1]);              /* put second description */
            #endif
        }
        else puts("");
        #ifdef USE_COLOR
        putattr(wk_attr, LineWid-1);
        #endif /* USE_COLOR */
    } /* end of if not multi */
    #ifdef USE_ANSI
    setcolor(dfdy_attr);
    #endif
    num_appts = max(6, num_appts-2);
    for (l = 0; l < num_appts; l++)
        if (!multi || q == 3) {
            index = LineWid-1;
            ll = line[l];
            while (index >= 1 && ll[index] == ' ') --index;
            if (q == 3)
                index = 69;
            else if (index < 21)  /* leave one space after last day in week */
                index = 21;
            ll[index+1] = '\0';
            #ifdef USE_ANSI
            ansiputs(line[l],line_attr[l]);
            #else
            puts(ll);
            #endif
            #ifdef USE_COLOR
            putattr(line_attr[l], multi ? YEARWID : LineWid-1);
            #endif /* USE_COLOR */
        }
    #ifdef USE_ANSI
    setcolor(0);
    #endif
}
 
 
/*===========================================================================
 * putd() - put year to standard output.
 *=========================================================================*/
short putd(register short n)
{
    register char *p = str+10;
    short yw = 0, neg = (n < 0);
    *p = '\0';
    if (neg) n = -n;
    do {
        --p;
        ++yw;
        *p = '0' + n % 10;
        n = n / 10;
    } while (n);
    if (neg) *(--p) = '-';
    fputs(p, stdout);
    return yw;
}
 
 
#define isleap(y) (!((y)%4) && (((y)%100) || !((y)%400)))
/*===========================================================================
 * fixtab() - correct for leapyears.
 *=========================================================================*/
void fixtab(register short y)
{
    register short i;
    if (!(y % 4)) {
        if ((y % 100) || !(y % 400) || (y < 1753)) {
            days[1] = 29;
            for (i = 2; i < 13; i++) mdays[i]++;
        }
    }
}
 
 
/*===========================================================================
 * weekday() - return day-of-week for first day of month.
 *=========================================================================*/
short weekday(register short m, register short y)
{
    --y;
    if (y > 1752-1 || (y == 1752-1 && m > 8))
        return (mdays[m]+y+y / 4-y / 100+y / 400+FUDGE1 - europe) % 7;
    else
        return (mdays[m] + y + y / 4           + FUDGE2 - europe) % 7;
}
 
 
#ifdef USE_COLOR
/*===========================================================================
 * putattr() - MSDOS or OS/2 only
 * modify the video memory text attribute areas with the attributes passed
 * in attr of lengh len, starting at the current cursor location.
 *=========================================================================*/
void putattr(char *attr, register short len)  /* write video attr. string */
{
#ifdef OS2
    USHORT row, col;
    int i;
#endif /* OS2 */
 
    if (!crt) return;
 
#ifdef OS2
    VioGetCurPos(&row, &col,0);
    --row;
    while (len) {        /* minimize the number of Vio calls */
        for (i = 1; i < len; i++)
            if (attr[i] != *attr) break;
        VioWrtNAttr((PBYTE) attr, i, row, col, 0);
        attr += i; col += i; len -= i;
    }
 
#else /* if not OS2 */
 
    reg.h.ah = 3;
    reg.h.bh = 0;
    int86(0x10, &reg, &reg);
    tmpvideo = video + ((LineWid << 1) * (reg.h.dh - 1) + reg.h.dl + 1);
    while (len--) {
        *tmpvideo = *(attr++);
        tmpvideo += 2;
    }
#endif /* OS2 */
}
#endif /* USE_COLOR */
 
 
#ifdef USE_ANSI
/*===========================================================================
 * ansiputs()
 * Puts a string to stdio in color as defined by line_attr.
 *=========================================================================*/
void ansiputs(char *l, char *a)
{
    char prev_color;
 
    setcolor(*a);
    while (*l) {
        fputc(*l++,stdout);
        prev_color = *a++;
        if (prev_color != *a) setcolor(*a);
    }
    fputc('\n',stdout);
}
 
 
/*===========================================================================
 * setcolor()
 * Set the color attributes to those passed in.
 *=========================================================================*/
void setcolor(char attr)
{
    char command[256];
    char dos2ansi[] = { 0, 4, 2, 6, 1, 5, 3, 7, 0, 4, 2, 6, 1, 5, 3, 7};
    if (!crt) return;
 
    /* ANSI codes
       0 = normal attributes (white one black)
       1 = high intensity     5 = blinking
       2 = Normal intensity   7 = reverse
       4 = underlined         8 = invisible
     */
    if (attr == 0)
        fputs("\033[0m",stdout);
    else {
        strcpy(command,"\033[0;");
        if (attr&0x80) strcat(command,"5;");
        if (attr&0x08) strcat(command,"1;");
        sprintf(command,"%s3%d;4%dm",command,dos2ansi[attr&0x0F],
                dos2ansi[(attr&0x70)>>4]);
        fputs(command,stdout);
    }
}
#endif /* USE_ANSI */
 
 
/*============================================================================
 * add_days()
 * function to return year, month, day of dates + n days
 * used in find periodic dates.
 * e.g. 1996 02 06 14 -- Payday! --
 *==========================================================================*/
void add_days(short *y, short *m, short *d, short n)
{
    short tmpfeb = days[1];
    days[1] = isleap(*y) ? 29 : 28;
    --(*m);
    *d += n;
    while (*d > days[*m]) {
        *d -= days[*m];
        if (++(*m) > 11) {
            ++(*y);
            days[1] = isleap(*y) ? 29 : 28;
            *m = 0;
        }
    }
    ++(*m);
    days[1] = tmpfeb;  /* restore original value */
}
 
/* Return day of month corresponding to the weekth week and dayth day of week*/
/* Requires that the line[] structure be filled in with the given month */
int monthweekday(int week, int day)
{
    int j;
 
    if (week == 9)                      /* special case to find last weekday */
        for (j = 5; j > 0; j--)
        { if (line[j][3*day+2] != ' ') break; }
    else {                              /* find dayth weekday                */
        for (j = 0; j < 6; j++)
            if (line[j][3*day+2] != ' ') if (!(--week)) break;
        if (week)
            return -1;                  /* dayth weekday didn't exist        */
        /* could probably change this to return 0 */
    }
    return atoi(&line[j][3*day+1]);     /* date of dayth weekday             */
}
 
/* make space for a new item in the right spot & return pointer to it */
char *insertitem(char **lln, int maxentry, int dy)
{
    short j,k,day;
 
    for (j = 0; j < maxentry; j++)              /* insert in sorted order */
        if (dy < (day = atoi(&lln[j][1]))) {
            k = maxentry;
            if (k == numappts) --k;
            for (; k > j; k--)                  /* move everything up     */
                strcpy(lln[k], lln[k-1]);
            break;
        }
        else if (day == 100) dy = dy-100;       /* reached new month      */
 
    if (j == numappts) return NULL;
    return lln[j];
}
 
/*===========================================================================
 * specialdays()
 * find the file CAL.DAT and attempt to read in special date descriptions.
 * e.g. -999 12 25 00 Christmas Day
 *=========================================================================*/
short specialdays(short m, short y)
{
    FILE   *fp;
    short  mo, dy, yr, w, n, filenum, wn, weeklyflag, periodflag = 0, k, j,
           hiappt = 1, hilight = (m == thismonth && y == thisyear);
    char **lln = NULL, *ll;
 
    if ((lln = (char **)calloc(numappts+1, sizeof(char *)))==NULL)
        goto spcldone;
 
    if (m != thismonth) discard_old = 0;
    lln[0] = spcldesc[0];
    lln[1] = spcldesc[1];
    for (j = 2; j < numappts; j++) lln[j] = &line[j-2][MONWID-1];
 
    strcpy(lln[0], " 100");             /* last (hiappt) entry */
 
    for (filenum = 0; filenum < numfiles; filenum++) {
        if ((fp = efopen(filename[filenum], "r")) == NULL) continue;
 
        while (fgets(str, LineWid, fp) != NULL) {  /* go until EOF is hit */
            if (!isdigit(*str) && *str != '-') continue; /* a data line? */
            if ((yr = atoi(str)) == 0) continue;         /* get year */
            if ((mo = atoi(&str[5])) == 0) continue;     /* get month */
            wn = atoi(&str[11]);                         /*nth wkdy or period*/
            if ((dy = atoi(&str[8])) == 0)               /* get day */
                if (!wn) continue;
            if (yr == y && mo > m && dy > 0 && wn > 0)
                continue; /* incalculable periodic date */
            if (!( (yr >= 1970 && mo > 0 && dy > 0 && wn > 0)
                  || ((yr == -999 || yr == y) &&
                  (mo == m || mo == m+1)) /* got it all? */
                  || ((yr == -999 || yr == y+1) &&
                      (mo == 1 && m == 12)) || mo == -9))
                continue;
            if (dy == -9)
                if (m == thismonth && y == thisyear)
                    dy = thisday;            /* set daily reminder */
                else
                    continue;
 
        periodduplicate:
            if (yr >= 1970 && mo > 0 && dy > 0 && wn > 0  /* proper format? */
                && !(yr == thisyear && mo == thismonth &&
                     dy == thisday && !periodflag)        /* not today? */
                && yr <= y && !(yr == y && mo > m)) {     /* <= displayed month
                                                               & year? */
                while (yr < y || mo <= m) {   /* find periodic dates */
                    add_days(&yr, &mo, &dy, wn);
                    if (yr == y && mo == m) break;
                }
                if (mo > m) continue;
                /* periodic dates left in month */
                periodflag = (days[m-1] - dy)/wn;
                if (periodflag && ((discard_old && dy<thisday) || (discard_old==2 && dy!=thisday)))
                    goto periodduplicate;
            }
            else periodflag = 0;
 
            weeklyflag = 5*(wn<0 && wn!=-9); /* flag to dup. weekly dates */
            if (wn < 0) wn = 50-wn;
        weeklyduplicate:
            w = weeklyflag ? weeklyflag : wn/10;
            if (!(yr > 0 && mo > 0 && dy > 0) &&
                ((wn >= 11 && wn <= 57) || w == 9) && (mo == m || mo == -9)) {
                /* find nth weekday */
                n = wn%10 - 1;           /* day of week (0=sun, 1=mon, etc.) */
                if (n < 0 || n > 6) continue;
 
                if ((dy = monthweekday(w,n)) < 0)
                    if (--weeklyflag > 0) goto weeklyduplicate;
                    else continue;        /* nth weekday didn't exist */
            }
 
            if (!dy) continue;        /* next loop if wn>57 or wn<11 or dy=0 */
            if ((mo == m+1 && (yr == y || yr == -999))
                || (mo == 1 && m == 12 && (yr == y+1 || yr == -999)))
                dy += 100;           /* make sure 'next month' days are last */
 
            if ((discard_old && dy<thisday) || (discard_old==2 && dy != thisday))
                continue;  /* next loop if old day */
 
            if ((ll = insertitem(lln,hiappt,dy)) == NULL) continue;
            ll[0] = (hilight && dy == thisday && (mo == m || mo == -9))
                ? '*' : ' ';  /* 'today' flag */
            ll[1] = (dy%100 < 10) ? ' ' : '0' + (dy%100)/10;
            ll[2] = '0' + dy%10;
            ll[3] = ':';
            j = strlen(str) - 1;         /* get rid of trailing white space */
            while (j >= 13 && isspace(str[j])) str[j--] = '\0';
 
/**************************************************************************/
/*             Check for special case of Anniversary/Birthdays             */
/*   Change added by R. Scott, and generalized a bit by Alex Matulich     */
/* A field indicated by [] will be displayed with [], but a field using {}*/
/* will be displayed without the braces.                                  */
/**************************************************************************/
/* These changes replace the next two lines: */
/*    str[j = 14+DAY_DESC-6] = '\0';         */
/*    strcpy(&ll[4], &str[13]);              */
/*********************************************/
 
            j=13; k=4;  /* init pointers into strings */
            while (k < DAY_DESC - 1 && str[j]) {
                ll[k++] = str[j];
                if (str[j] == '\\') { /* Don't treat next char as special */
                    ll[k-1] = str[++j]; /* Write next char over '\' */
                    j++;                /* & then advance to next char */
                }
                /* start of [field] */
                else if (str[j] == '[' || str[j] == '{') {
                    short suffix = -1;   /* <0 indicates no ordinal suffix */
                    long ipower = 1,
                        by = atoi(&str[++j]); /* year of aniversary */
                    if (by <= y) {             /* will year be modified? */
                        by = y - by + (dy>100 && mo==1); /* years since anniversary */
                        if (str[j-1] == '{')
                            suffix = by; /* ordinal suffix will be attached */
                    }
                    if (str[j-1] == '{' || str[j-1] == '[')
                        --k;  /* don't display brace */
                    while (by / ipower) ipower *= 10L;
                    ipower /= 10L;      /* ipower is now the magnitude of by */
                    if (!by) ll[k++] = '0';
                    else while (ipower && k < DAY_DESC - 1) {
                        ll[k++] = by / ipower + '0';
                        by -= (by / ipower) * ipower;/* remove largest digit */
                        ipower /= 10L;
                    }
                    /* optionally add the ordinal suffixes st, nd, rd and th
                     * to the end of value for better readability (for example,
                     * 31 will be 31st, 23 will be 23rd, etc.) */
                    if (suffix >= 0 && k < DAY_DESC - 3) {
                        suffix %= 100;  /* only use last two digits */
                        if (suffix >= 11 && suffix <= 13) suffix = 4;
                        else suffix %= 10; /* last digit */
                        switch (suffix) {
                        case 1:  ll[k++]='s'; ll[k++]='t';  break;
                        case 2:  ll[k++]='n'; ll[k++]='d';  break;
                        case 3:  ll[k++]='r'; ll[k++]='d';  break;
                        default: ll[k++]='t'; ll[k++]='h';
                        }
                    }
                    while (isdigit(str[j])) ++j;  /* advance to end of field */
                    if (str[j] == '}' || str[j] == ']') ++j; /* skip brace*/
                }
                else
                    ++j;      /* advance to next char */
            }
            ll[k] = '\0';   /* ensure string is terminated */
/**************************************************************************/
/* End of anniversary changes                                             */
/**************************************************************************/
 
            if (++hiappt > numappts) hiappt = numappts;
            if (--weeklyflag > 0) goto weeklyduplicate; /* dup. wkly reminders */
            if (periodflag > 0) goto periodduplicate; /* dup. periodic reminders */
        }
 
        fclose(fp);
    }
 
    for (j = 0; j < hiappt; j++)
        if (atoi(&lln[j][1]) == 100) break;     /* find last item */
    if (j == hiappt-1)
        lln[--hiappt][0] = '\0';
    else if (j < hiappt)
        strcat(strcat(strcpy(lln[j], " "), months[m%12]), "--");
 
spcldone:
    free(lln);
    return hiappt;
}
 
 
#ifdef USE_REMINDER
 
/* dayofweek()
 * convert Sun, Mon, Tues, etc. to number (honours europe flag)
 * returns 0 in case of error
*/
int dayofweek(const char *day)
{
    switch (toupper(day[0])) {
    case 'S':
        if (tolower(day[1]) == 'u')
            return europe ? 7 : 1;
        else
            return 7 - europe;
    case 'M':
        return 2 - europe;
    case 'T':
        if (tolower(day[1]) == 'u')
            return 3 - europe;
        else
            return 5 - europe;
    case 'W':
        return 4 - europe;
    case 'F':
        return 6 - europe;
    default:
        return 0;
    }
}
 
 
/*===========================================================================
 * reminderdays()
 * find the file DATES and attempt to read in reminder-style date descriptions.
 * e.g. 12/25:7:Christmas Day::N
 *=========================================================================*/
short reminderdays(short m, short y)
{
    FILE *fp;
    short mo, dy, yr, n, wn, weeklyflag, filenum, j, hiappt = 1,
        hilight = (m == thismonth && y == thisyear);
    char **lln = NULL, *ll;
    char *descrip;
    static char tempstr[256];   /* big enough for a long reminder line */
 
    if (!data_file) return 0;   /* don't read any data files */
    if ((lln = (char **)calloc(numappts+1, sizeof(char *)))==NULL) return 1;
 
/* set up pointers array for descriptions */
    lln[0] = spcldesc[0];
    lln[1] = spcldesc[1];
    for (j = 2; j < numappts; j++) lln[j] = &line[j-2][MONWID-1];
 
    strcpy(lln[0], " 100");     /* last entry */
 
    if (m != thismonth) discard_old = 0;
 
    clearfilenames();   /* ignore all the cal native format data files */
    addfilename(REMINDER_FILE);
    for (filenum = 0; filenum < numfiles; filenum++) {
        if ((fp = efopen(filename[filenum], "r")) == NULL) continue;
 
        /* go until EOF is hit */
        while (fgets(tempstr, sizeof(tempstr), fp) != NULL) {
            if (isspace(tempstr[0]))
                continue;  /* ignore blanks and empty lines */
            else if (strchr(tempstr, ':') &&
                     (isdigit(tempstr[0]) || (tempstr[1] != ':'))) {
                /* assume normal date entry & not include file (which may be
                   of form C:\NAME)  */
 
                char *field = strtok(tempstr, ":");             /* get date */
 
                if (isdigit(field[0])) {
                    mo = yr = wn = 0;
                    /* day of month only? */
                    dy = atoi(tempstr);
                    if ((field = strchr(field,'/')) != NULL) {
                        /* mm/dd or mm/dd/yy format */
                        mo = dy;
                        if (mo != m)            /* not this month */
                            continue;
                        dy = atoi(field+1);
                        yr = 0;
 
                        if ((field = strchr(field+1,'/')) != NULL) {
                            /* mm/dd/yy format */
                            yr = atoi(field+1);
                            if (yr < 1000) yr = yr + 1900;
                        }
                    }
                    if ((dy > 31))              /* bad day */
                        continue;
                    if (yr && (yr != y))        /* not this year */
                        continue;
 
                } else {
                    /* day of week */
                    dy = mo = yr = 0;
                    wn = dayofweek(field);
                }
 
                strtok(NULL, ":");              /* skip days of notice */
                descrip = strtok(NULL, ":");    /* get description */
                /* get person & status fields */
                if ((field = strtok(NULL, "\n")) == NULL)
                    continue;           /* skip if sanity check fails */
                if (field[0] != ':') {          /* person is specified */
                    char *newdescrip = malloc(LineWid);
                    field = strtok(field, ":"); /* get person */
                    strcat(strcat(strcpy(newdescrip, field), "'s "), descrip);
                    /* BUG: could overwrite end by 1 char */
                    strcpy(descrip, newdescrip);
                    field = strtok(NULL, ":");  /* get status */
                    free(newdescrip);
                } else ++field;
 
                if (!field || field[0] == 'D')  /* do not report this event */
                    continue;
 
            } else  {
                /* must be an include file--add to the list to check */
                /* NOTE: this malloced storage is never freed */
                addfilename(strtok(strcpy(malloc(strlen(tempstr)+1),tempstr),"\n"));
                continue;
            }
 
            weeklyflag = 5*(wn>0);       /* flag to dup. weekly dates */
 
            do {
                if (!(yr > 0 && mo > 0 && dy > 0) &&
                    (wn >= 1) && (mo == m || mo <= 0)) {
                    n = wn - 1;          /* day of week (0=sun, 1=mon, etc.) */
                    if ((dy = monthweekday(weeklyflag,n)) < 0)
                        if (--weeklyflag > 0) continue;
                        else continue;        /* nth weekday didn't exist */
                }
 
                if (!dy) continue;             /* ignore if dy=0 */
                if (discard_old < 2 && ((mo == m+1 && (yr == y || yr <= 0))
                    || (mo == 1 && m == 12 && (yr == y+1 || yr <= 0))))
                    dy += 100;       /* make sure 'next month' days are last */
 
                if ((discard_old && dy<thisday) || (discard_old==2 &&
                    (dy != thisday || mo != thismonth || yr != thisyear)))
                    continue;  /* next loop if old day */
 
                if ((ll = insertitem(lln,hiappt,dy)) == NULL) continue;
                ll[0] = (hilight && dy == thisday && (mo == m || mo == -9))
                    ? '*' : ' ';  /* 'today' flag */
                ll[1] = (dy%100 < 10) ? ' ' : '0' + (dy%100)/10;
                ll[2] = '0' + dy%10;
                ll[3] = ':';
                j = strlen(descrip) - 1;  /* get rid of trailing white space */
                while (j > 0 && isspace(descrip[j])) descrip[j--] = '\0';
 
                descrip[DAY_DESC-5] = '\0';     /* truncate description */
                strcpy(&ll[4], descrip);
 
                if (++hiappt > numappts) hiappt = numappts;
            } while (--weeklyflag > 0); /* dup. weekly reminders */
        }
        fclose(fp);
    }
 
    for (j = 0; j < hiappt; j++)
        if (atoi(&lln[j][1]) == 100) break;     /* find last item */
    if (j == hiappt-1)
        lln[--hiappt][0] = '\0';
    else if (j < hiappt)
        strcat(strcat(strcpy(lln[j], " "), months[m%12]), "--");
 
    free(lln);
    return hiappt;
}
#endif /* USE_REMINDER */
 
 
/*===========================================================================
 * efopen() - fopen() replacement that does path searches
 *=========================================================================*/
FILE *efopen(const char *file, const char *mode)
{
    FILE  *fp;
 
#ifdef OS2
    char  path[_MAX_PATH];
#endif /* OS2 */
 
    if ((fp = fopen(file, mode)) == NULL) {
        if (*homedir) {
            strcpy(str, homedir);
#ifdef UNIX
            if (homedir[strlen(homedir)-1] != '/') strcat(str,"/");
            if (file[0] != '.') strcat(str, ".");
#endif
            strcat(str, file);
            fp = fopen(str, mode);
        }
 
#ifdef OS2
        if (fp == NULL) {
            _searchenv(file, "PATH", path);
            if (*path == '\0')
                _searchenv(file, "DPATH", path);
            if (*path)
                fp = fopen(path, mode);
        }
#endif /* OS2 */
 
#ifdef UNIX
        /* If not found in home dir, look in current dir, then /usr/lib */
        if (fp == NULL) {
            strcpy(str, ".");
            strcpy(str, file);
            if ((fp = fopen(str, mode)) == NULL) {
                /* If still not found then look in a lib directory */
                strcpy(str, "/usr/lib/");
                strcat(str,file);
                fp = fopen(str, mode);
            }
        }
#endif
    }
    return fp;
}
 
 
/*===========================================================================
 * usage() - called from main() - display commandline usage
 *=========================================================================*/
void usage()
{
fputs("\n\
CAL 3.5 -- unix-like 'cal' by Unicorn Research Corporation & DTF\n\
Display a monthly or yearly calendar, with optional appointments\n\n\
Usages:\tcal [options] [[month (1-12)] year]\n\
\tcal [options] [month (jan-dec)] [year]\n\n\
Options:\n\
  -nod[ata]          Ignore appointment descriptions file\n\
  -d[ata-file]=file  Load appointments from `file'",stderr);
#ifndef UNIX
fputs("(CAL.DAT)\n",stderr);
#else
fputs("(caldat)\n",stderr);
#endif
fputs("\
  -f[uture]          Show only future appointments on current month\n\
  -t[oday]           Show only today's appointments on current month\n\
  -e[urope]          European format (first day is Monday)\n\
  -m[axappts]=n      Display maximum of n (8-50) appointments (24)\n\
  -p[ause]           Pause for keystroke before exiting\n", stderr);
 
#if defined(USE_COLOR) || defined(USE_ANSI)
fputs("\
  -noc[olor]         Inhibit use of colors\n\
  -c[olor-file]=file Load color definitions from `file' ",stderr);
#ifndef UNIX
fputs("(CAL.COL)\n", stderr);
#else
fputs("(calcol)\n\
  -8[bit]            Display 8-bit ascii characters\n",stderr);
#endif
#endif /* USE_COLOR */
}
