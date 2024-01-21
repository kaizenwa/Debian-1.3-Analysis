/****************************************************************************

  figlet (C) 1991, 1993, 1994 Glenn Chappell and Ian Chai
  Internet: <ggc@uiuc.edu> and <chai@uiuc.edu>
  figlet, along with the various figlet fonts and documentation, may be
    freely copied and distributed.
  If you use figlet, please send an e-mail message to <figlet@uiuc.edu>.

****************************************************************************/

#define DATE "28 Apr 1995"
#define VERSION "2.1.1"
#define VERSION_INT 20101

/* figlet (Frank, Ian & Glenn's Letters) */
/* by Glenn Chappell */
/* Apr 1991 */
/* Automatic file addition by Ian Chai May 1991 */
/* Punctuation and numbers addition by Ian Chai Jan 1993 */
/* Full ASCII by Glenn Chappell Feb 1993 */
/* Line-breaking, general rewrite by Glenn Chappell Mar 1993 */
/* Hard blanks by Glenn Chappell Apr 1993 */
/* Release 2.0 5 Aug 1993 */
/* Right-to-left printing, extended char set by Glenn Chappell Dec 1993 */
/* Control files by Glenn Chappell Feb 1994 */
/* Release 2.1 12 Aug 1994 */
/* Release 2.1.1 25 Aug 1994 */

/*---------------------------------------------------------------------------
  DEFAULTFONTDIR and DEFAULTFONTFILE should be defined in the Makefile.
  DEFAULTFONTDIR is the full path name of the directory in which figlet
    will search first for fonts (the ".flf" files).
  DEFAULTFONTFILE is the filename of the font to be used if no other
    is specified (standard.flf is recommended, but any other can be
    used). This file should reside in the directory specified by
    DEFAULTFONTDIR.
---------------------------------------------------------------------------*/
#ifndef DEFAULTFONTDIR
#define DEFAULTFONTDIR "/usr/games/lib/figlet.dir"
#endif
#ifndef DEFAULTFONTFILE
#define DEFAULTFONTFILE "standard.flf"
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <string.h>
#include <ctype.h>
#include <fcntl.h>     /* Needed for get_columns */
#include <sys/ioctl.h> /* Needed for get_columns */

#define MYSTRLEN(x) ((int)strlen(x)) /* Eliminate ANSI problem */
#define DIRSEP '/' /* E.g., make this '\\' for an MS-DOS port. */
                   /* Note: '/' also used in filename in get_columns(). */

#define FONTFILESUFFIX ".flf"
#define FONTFILEMAGICNUMBER "flf2"
#define FSUFFIXLEN MYSTRLEN(FONTFILESUFFIX)
#define CONTROLFILESUFFIX ".flc"
#define CONTROLFILEMAGICNUMBER "flc2"
#define CSUFFIXLEN MYSTRLEN(CONTROLFILESUFFIX)
#define DEFAULTCOLUMNS 80


/****************************************************************************

  Globals dealing with chars that are read

****************************************************************************/

typedef long inchr; /* "char" read from stdin */

inchr *inchrline;  /* Alloc'd inchr inchrline[inchrlinelenlimit+1]; */
                   /* Note: not null-terminated. */
int inchrlinelen,inchrlinelenlimit;


/****************************************************************************

  Globals dealing with chars that are written

****************************************************************************/

typedef struct fc {
  inchr ord;
  char **thechar;  /* Alloc'd char thechar[charheight][]; */
  struct fc *next;
  } fcharnode;

fcharnode *fcharlist;
char **currchar;
int currcharwidth;
char **outline;    /* Alloc'd char outline[charheight][outlinelenlimit+1]; */
int outlinelen;


/****************************************************************************

  Globals dealing with command file storage

****************************************************************************/

typedef struct cfn {
  char *thename;
  struct cfn *next;
  } cfnamenode;

cfnamenode *cfilelist,**cfilelistend;

typedef struct cm {
  int thecommand;
  inchr rangelo;
  inchr rangehi;
  inchr offset;
  struct cm *next;
  } comnode;

comnode *commandlist,**commandlistend;

/****************************************************************************

  Globals affected by command line options

****************************************************************************/

int deutschflag,justification,paragraphflag,right2left;
/*---------------------------------------------------------------------------
  smushmode: (given after "-m" command line switch)
   -2: Get default value from font file (default)
   -1: Do not smush
  For explanation of non-negative values, see smushem().
---------------------------------------------------------------------------*/
int smushmode;
int outputwidth;
int outlinelenlimit;
char *fontdirname,*fontname;


/****************************************************************************

  Globals read from font file

****************************************************************************/

char hardblank;
int charheight,defaultmode;


/****************************************************************************

  Name of program, used in error messages

****************************************************************************/

char *myname;


#ifdef TIOCGWINSZ
/****************************************************************************

  get_columns

  Determines the number of columns of /dev/tty.  Returns the number of
  columns, or -1 if error.  May return 0 if columns unknown.
  Requires include files <fcntl.h> and <sys/ioctl.h>.
  by Glenn Chappell & Ian Chai 14 Apr 1993

****************************************************************************/

int get_columns()
{
  struct winsize ws;
  int fd,result;

  if ((fd = open("/dev/tty",O_WRONLY))<0) return -1;
  result = ioctl(fd,TIOCGWINSZ,&ws);
  close(fd);
  return result?-1:ws.ws_col;
}
#endif /* ifdef TIOCGWINSZ */


/****************************************************************************

  myalloc

  Calls malloc.  If malloc returns error, prints error message and
  quits.

****************************************************************************/

#ifdef __STDC__
char *myalloc(size_t size)
#else
char *myalloc(size)
int size;
#endif
{
  char *ptr;
#ifndef __STDC__
  extern void *malloc();
#endif

  if ((ptr = (char*)malloc(size))==NULL) {
    fprintf(stderr,"%s: Out of memory\n",myname);
    exit(1);
    }
  else {
    return ptr;
    }
}


/****************************************************************************

  skiptoeol

  Skips to the end of a line, given a stream.

****************************************************************************/

void skiptoeol(fp)
FILE *fp;
{
  int dummy;

  while (dummy=getc(fp),dummy!='\n'&&dummy!=EOF) ;
}


/****************************************************************************

  usageerr

  Prints "Usage: ...." line to the given stream.

****************************************************************************/

void printusage(out)
FILE *out;
{
  fprintf(out,
    "Usage: %s [ -clnprtvxDELNRX ] [ -d fontdirectory ]\n",
    myname);
  fprintf(out,
    "              [ -f fontfile ] [ -m smushmode ] [ -w outputwidth ]\n");
  fprintf(out,
    "              [ -C controlfile ] [ -I infocode ]\n");
}


/****************************************************************************

  printinfo

  Prints version and copyright message, or utility information.

****************************************************************************/

void printinfo(infonum)
int infonum;
{
  switch (infonum) {
    case 0: /* Copyright message */
      printf("figlet (C) 1991, 1993, 1994 Glenn Chappell and Ian Chai\n");
      printf("Internet: <ggc@uiuc.edu> and <chai@uiuc.edu>\n");
      printf("Version: %s, date: %s\n\n",VERSION,DATE);
      printf("figlet, along with the various figlet fonts");
      printf(" and documentation, may be\n");
      printf("freely copied and distributed.\n\n");
      printf("If you use figlet, please send an");
      printf(" e-mail message to <figlet@uiuc.edu>.\n\n");
      printf("The latest version of figlet is available by anonymous");
      printf(" FTP from\n");
      printf("ftp.nicoh.com in directory \"pub/figlet\".\n\n");
      printusage(stdout);
      break;
    case 1: /* Version (integer) */
      printf("%d\n",VERSION_INT);
      break;
    case 2: /* Font directory */
      printf("%s\n",fontdirname);
      break;
    case 3: /* Font */
      printf("%s\n",fontname);
      break;
    case 4: /* Outputwidth */
      printf("%d\n",outputwidth);
    }
}


/****************************************************************************

  readTchar

  Reads a control file "T" command character specification.

****************************************************************************/

inchr readTchar(fp)
FILE *fp;
{
  inchr thechar;
  char next;

  thechar=getc(fp);
  if (thechar=='\n') { /* Handle badly-formatted file */
    ungetc(thechar,fp);
    return '\0';
    }
  if (thechar!='\\') return thechar;
  next=getc(fp);
  switch(next) {
    case 'a':
      return 7;
    case 'b':
      return 8;
    case 'e':
      return 27;
    case 'f':
      return 12;
    case 'n':
      return 10;
    case 'r':
      return 13;
    case 't':
      return 9;
    case 'v':
      return 11;
    default:
      if (next=='-' || next=='x' || (next>='0' && next<='9')) {
        ungetc(next,fp);
        fscanf(fp,"%li",&thechar);
        return thechar;
        }
      return next;
    }
}


/****************************************************************************

  readcontrol

  Allocates memory and reads in the given control file.
  Called in readcontrolfiles().

****************************************************************************/

void readcontrol(controlname)
char *controlname;
{
  inchr firstch,lastch;
  char dashcheck;
  inchr offset;
  char *controlpath,magicnum[5];
  char command;
  FILE *controlfile;
  int namelen;

  namelen = MYSTRLEN(fontdirname);
  controlpath = (char*)myalloc(sizeof(char)
    *(namelen+MYSTRLEN(controlname)+CSUFFIXLEN+2));
  controlfile = NULL;
  if (!strchr(controlname,DIRSEP)) {
    strcpy(controlpath,fontdirname);
    controlpath[namelen] = DIRSEP;
    controlpath[namelen+1] = '\0';
    strcat(controlpath,controlname);
    strcat(controlpath,CONTROLFILESUFFIX);
    controlfile = fopen(controlpath,"r");
    }
  if (controlfile==NULL) {
    strcpy(controlpath,controlname);
    strcat(controlpath,CONTROLFILESUFFIX);
    controlfile = fopen(controlpath,"r");
    if (controlfile==NULL) {
      fprintf(stderr,"%s: %s: Unable to open control file\n",myname,
        controlpath);
      exit(1);
      }
    }

  fscanf(controlfile,"%4s",magicnum);
  if (strcmp(magicnum,CONTROLFILEMAGICNUMBER)) {
    fprintf(stderr,"%s: %s: Not a figlet 2 control file\n",myname,controlpath);
    exit(1);
    }
  skiptoeol(controlfile);
  free(controlpath);

  (*commandlistend) = (comnode*)myalloc(sizeof(comnode));
  (*commandlistend)->thecommand = 0; /* Begin with a freeze command */
  commandlistend = &(*commandlistend)->next;
  (*commandlistend) = NULL;

  while(command=getc(controlfile),!feof(controlfile)) {
    switch (command) {
      case 't': /* Translate */
        fscanf(controlfile,"%*[ \t]");
        firstch=readTchar(controlfile);
        if ((dashcheck=getc(controlfile))=='-') {
          lastch=readTchar(controlfile);
          }
        else {
          ungetc(dashcheck,controlfile);
          lastch=firstch;
          }
        fscanf(controlfile,"%*[ \t]");
        offset=readTchar(controlfile)-firstch;
        skiptoeol(controlfile);
        (*commandlistend) = (comnode*)myalloc(sizeof(comnode));
        (*commandlistend)->thecommand = 1;
        (*commandlistend)->rangelo = firstch;
        (*commandlistend)->rangehi = lastch;
        (*commandlistend)->offset = offset;
        commandlistend = &(*commandlistend)->next;
        (*commandlistend) = NULL;
        break;
      case 'f': /* freeze */
        skiptoeol(controlfile);
        (*commandlistend) = (comnode*)myalloc(sizeof(comnode));
        (*commandlistend)->thecommand = 0;
        commandlistend = &(*commandlistend)->next;
        (*commandlistend) = NULL;
        break;
      case '\n': /* blank line */
        break;
      default: /* Includes '#' */
        skiptoeol(controlfile);
      }
    }
  fclose(controlfile);
}


/****************************************************************************

  readcontrolfiles

  Reads in the controlfiles names in cfilelist.  Uses readcontrol.
  Called in main().

****************************************************************************/

void readcontrolfiles()
{
  cfnamenode *cfnptr;

  for (cfnptr=cfilelist;cfnptr!=NULL;cfnptr=cfnptr->next) {
    readcontrol(cfnptr->thename);
    }
}


/****************************************************************************

  clearcfilelist

  Clears the control file list.  Assumes thename does not need freeing.

****************************************************************************/

void clearcfilelist()
{
  cfnamenode *cfnptr1,*cfnptr2;

  cfnptr1 = cfilelist;
  while (cfnptr1 != NULL) {
    cfnptr2 = cfnptr1->next;
    free(cfnptr1);
    cfnptr1 = cfnptr2;
    }
  cfilelist = NULL;
  cfilelistend = &cfilelist;
}


/****************************************************************************

  getparams

  Handles all command-line parameters.  Puts all parameters within
  bounds except smushmode.

****************************************************************************/

void getparams(argc,argv)
int argc;
char *argv[];
{
  extern char *optarg;
  extern int optind;
  int c; /* "Should" be a char -- need int for "!= -1" test*/
  int columns,firstfont,infoprint;
  char *controlname;

  if ((myname = strrchr(argv[0],DIRSEP))!=NULL) {
    myname++;
    }
  else {
    myname = argv[0];
    }
  fontdirname = DEFAULTFONTDIR;
  firstfont = 1;
  fontname = (char*)myalloc(sizeof(char)*(MYSTRLEN(DEFAULTFONTFILE)+1));
  strcpy(fontname,DEFAULTFONTFILE); /* Some systems don't have strdup() */
  if ((MYSTRLEN(fontname)>=FSUFFIXLEN)?
    !strcmp(fontname+MYSTRLEN(fontname)-FSUFFIXLEN,FONTFILESUFFIX):0) {
    fontname[MYSTRLEN(fontname)-FSUFFIXLEN]='\0';
    }
  cfilelist = NULL;
  cfilelistend = &cfilelist;
  commandlist = NULL;
  commandlistend = &commandlist;
  smushmode = -2;
  deutschflag = 0;
  justification = -1;
  right2left = -1;
  paragraphflag = 0;
  infoprint = -1;
  outputwidth = DEFAULTCOLUMNS;
  while ((c = getopt(argc,argv,"DEXLRI:xlcrpntvm:w:d:f:C:NF"))!= -1) {
      /* Note: -F is not a legal option -- prints a special err message.  */
    switch (c) {
      case 'D':
        deutschflag = 1;
        break;
      case 'E':
        deutschflag = 0;
        break;
      case 'X':
        right2left = -1;
        break;
      case 'L':
        right2left = 0;
        break;
      case 'R':
        right2left = 1;
        break;
      case 'x':
        justification = -1;
        break;
      case 'l':
        justification = 0;
        break;
      case 'c':
        justification = 1;
        break;
      case 'r':
        justification = 2;
        break;
      case 'p':
        paragraphflag = 1;
        break;
      case 'n':
        paragraphflag = 0;
        break;
      case 't':
#ifdef TIOCGWINSZ
        columns = get_columns();
        if (columns>0) {
          outputwidth = columns;
          }
#else /* ifdef TIOCGWINSZ */
        fprintf(stderr,
          "%s: \"-t\" is disabled, since ioctl is not fully implemented.\n",
          myname);
#endif /* ifdef TIOCGWINSZ */
        break;
      case 'v':
        infoprint = 0;
        break;
      case 'I':
        infoprint = atoi(optarg);
        break;
      case 'm':
        smushmode = atoi(optarg);
        /* Putting smushmode within bounds is done in readfont(). */
        break;
      case 'w':
        columns = atoi(optarg);
        if (columns>0) {
          outputwidth = columns;
          }
        break;
      case 'd':
        fontdirname = optarg;
        break;
      case 'f':
        if (firstfont) {
          free(fontname);
          firstfont = 0;
          }
        fontname = optarg;
        if ((MYSTRLEN(fontname)>=FSUFFIXLEN)?
          !strcmp(fontname+MYSTRLEN(fontname)-FSUFFIXLEN,FONTFILESUFFIX):0) {
          fontname[MYSTRLEN(fontname)-FSUFFIXLEN] = '\0';
          }
        break;
      case 'C':
        controlname = optarg;
        if ((MYSTRLEN(controlname)>=CSUFFIXLEN)?
          !strcmp(controlname+MYSTRLEN(controlname)-CSUFFIXLEN,
            CONTROLFILESUFFIX):0) {
          controlname[MYSTRLEN(controlname)-CSUFFIXLEN] = '\0';
          }
        (*cfilelistend) = (cfnamenode*)myalloc(sizeof(cfnamenode));
        (*cfilelistend)->thename = controlname;
        cfilelistend = &(*cfilelistend)->next;
        (*cfilelistend) = NULL;
        break;
      case 'N':
        clearcfilelist();
        break;
      case 'F': /* Not a legal option */
        fprintf(stderr,"%s: illegal option -- F\n",myname);
        printusage(stderr);
        fprintf(stderr,"\nBecause of numerous incompatibilities, the");
        fprintf(stderr," \"-F\" option has been\n");
        fprintf(stderr,"removed.  It has been replaced by the \"figlist\"");
        fprintf(stderr," program, which is now\n");
        fprintf(stderr,"included in the basic figlet package.  \"figlist\"");
        fprintf(stderr," is also available by\n");
        fprintf(stderr,"anonymous FTP from ftp.nicoh.com in");
        fprintf(stderr," directory \"pub/figlet/util\".\n");
        exit(1);
        break;
      default:
        printusage(stderr);
        exit(1);
      }
    }
  if (optind!=argc) {
    printusage(stderr);
    exit(1);
    }
  outlinelenlimit = outputwidth-1;
  if (infoprint>=0) {
    printinfo(infoprint);
    exit(0);
    }
}


/****************************************************************************

  clearline

  Clears both the input (inchrline) and output (outline) storage.

****************************************************************************/

void clearline()
{
  int i;

  for (i=0;i<charheight;i++) {
    outline[i][0] = '\0';
    }
  outlinelen = 0;
  inchrlinelen = 0;
}


/****************************************************************************

  readfontchar

  Reads a font character from the font file, and places it in a
  newly-allocated entry in the list.

****************************************************************************/

void readfontchar(file,theord,line,maxlen)
FILE *file;
inchr theord;
char *line;
int maxlen;
{
  int row,k;
  char endchar;
  fcharnode *fclsave;

  fclsave = fcharlist;
  fcharlist = (fcharnode*)myalloc(sizeof(fcharnode));
  fcharlist->ord = theord;
  fcharlist->thechar = (char**)myalloc(sizeof(char*)*charheight);
  fcharlist->next = fclsave;
  for (row=0;row<charheight;row++) {
    if (fgets(line,maxlen+1,file)==NULL) {
      line[0] = '\0';
      }
    k = MYSTRLEN(line)-1;
    while (k>=0 && isspace(line[k])) {
      k--;
      }
    if (k>=0) {
      endchar = line[k];
      while (k>=0 ? line[k]==endchar : 0) {
        k--;
        }
      }
    line[k+1] = '\0';
    fcharlist->thechar[row] = (char*)myalloc(sizeof(char)*(k+2));
    strcpy(fcharlist->thechar[row],line);
    }
}


/****************************************************************************

  readfont

  Allocates memory, initializes variables, and reads in the font.
  Called near beginning of main().

****************************************************************************/

void readfont()
{
#define MAXFIRSTLINELEN 1000
  int i,row,numsread;
  inchr theord;
  int maxlen,cmtlines,ffright2left;
  char *fontpath,*fileline,magicnum[5];
  FILE *fontfile;
  int namelen;

  namelen = MYSTRLEN(fontdirname);
  fontpath = (char*)myalloc(sizeof(char)
    *(namelen+MYSTRLEN(fontname)+FSUFFIXLEN+2));
  fontfile = NULL;
  if (!strchr(fontname,DIRSEP)) {
    strcpy(fontpath,fontdirname);
    fontpath[namelen] = DIRSEP;
    fontpath[namelen+1] = '\0';
    strcat(fontpath,fontname);
    strcat(fontpath,FONTFILESUFFIX);
    fontfile = fopen(fontpath,"r");
    }
  if (fontfile==NULL) {
    strcpy(fontpath,fontname);
    strcat(fontpath,FONTFILESUFFIX);
    fontfile = fopen(fontpath,"r");
    if (fontfile==NULL) {
      fprintf(stderr,"%s: %s: Unable to open font file\n",myname,fontpath);
      exit(1);
      }
    }

  fscanf(fontfile,"%4s",magicnum);
  fileline = (char*)myalloc(sizeof(char)*(MAXFIRSTLINELEN+1));
  if (fgets(fileline,MAXFIRSTLINELEN+1,fontfile)==NULL) {
    fileline[0] = '\0';
    }
  if (MYSTRLEN(fileline)>0 ? fileline[MYSTRLEN(fileline)-1]!='\n' : 0) {
    skiptoeol(stdin);
    }
  numsread = sscanf(fileline,"%*c%c %d %*d %d %d %d%*[ \t]%d",
    &hardblank,&charheight,&maxlen,&defaultmode,&cmtlines,
    &ffright2left);
  free(fileline);
  if (strcmp(magicnum,FONTFILEMAGICNUMBER) || numsread<5) {
    fprintf(stderr,"%s: %s: Not a figlet 2 font file\n",myname,fontpath);
    exit(1);
    }
  for (i=1;i<=cmtlines;i++) {
    skiptoeol(fontfile);
    }
  free(fontpath);

  if (numsread<6) {
    ffright2left = 0;
    }
  if (charheight<1) {
    charheight = 1;
    }
  if (maxlen<1) {
    maxlen = 1;
    }
  maxlen += 100; /* Give ourselves some extra room */
  if (smushmode<-1) {
    smushmode = defaultmode;
    }
  if (smushmode<-1) {
    smushmode = -1;
    }
  if (right2left<0) {
    right2left = ffright2left;
    }
  if (justification<0) {
    justification = 2*right2left;
    }

  fileline = (char*)myalloc(sizeof(char)*(maxlen+1));
  /* Allocate "missing" character */
  fcharlist = (fcharnode*)myalloc(sizeof(fcharnode));
  fcharlist->ord = 0;
  fcharlist->thechar = (char**)myalloc(sizeof(char*)*charheight);
  fcharlist->next = NULL;
  for (row=0;row<charheight;row++) {
    fcharlist->thechar[row] = (char*)myalloc(sizeof(char));
    fcharlist->thechar[row][0] = '\0';
    }
  for (theord=' ';theord<='~';theord++) {
    readfontchar(fontfile,theord,fileline,maxlen);
    }
  for (theord= -255;theord<= -249;theord++) {
    readfontchar(fontfile,theord,fileline,maxlen);
    }
  while (fgets(fileline,maxlen+1,fontfile)==NULL?0:
    sscanf(fileline,"%li",&theord)==1) {
    readfontchar(fontfile,theord,fileline,maxlen);
    }
  fclose(fontfile);
  free(fileline);
}


/****************************************************************************

  linealloc

  Allocates & clears outline, inchrline. Sets inchrlinelenlimit.
  Called near beginning of main().

****************************************************************************/

void linealloc()
{
  int row; 

  outline = (char**)myalloc(sizeof(char*)*charheight);
  for (row=0;row<charheight;row++) {
    outline[row] = (char*)myalloc(sizeof(char)*(outlinelenlimit+1));
    }
  inchrlinelenlimit = outputwidth*4+100;
  inchrline = (inchr*)myalloc(sizeof(inchr)*(inchrlinelenlimit+1));
  clearline();
}


/****************************************************************************

  getletter

  Sets currchar to point to the font entry for the given character.
  Sets currcharwidth to the width of this character.

****************************************************************************/

void getletter(c)
inchr c;
{
  fcharnode *charptr;

  for (charptr=fcharlist;charptr==NULL?0:charptr->ord!=c;
    charptr=charptr->next) ;
  if (charptr!=NULL) {
    currchar = charptr->thechar;
    }
  else {
    for (charptr=fcharlist;charptr==NULL?0:charptr->ord!=0;
      charptr=charptr->next) ;
    currchar = charptr->thechar;
    }
  currcharwidth = MYSTRLEN(currchar[0]);
}


/****************************************************************************

  smushem

  Given 2 characters, attempts to smush them into 1, according to
  smushmode.  Returns smushed character or '\0' if no smushing can be
  done.  Assumes smushmode >= 0.

  smushmode values are sum of following (all values smush blanks):
    1: Smush equal chars (not hardblanks)
    2: Smush '_' with any char in hierarchy below
    4: hierarchy: "|", "/\", "[]", "{}", "()", "<>"
       Each class in hier. can be replaced by later class.
    8: [ + ] -> |, { + } -> |, ( + ) -> |
   16: / + \ -> X, > + < -> X (only in that order)
   32: hardblank + hardblank -> hardblank

****************************************************************************/

char smushem(lch,rch)
char lch,rch;
{
  if (lch==' ') return rch;
  if (rch==' ') return lch;

  if (smushmode & 32) {
    if (lch==hardblank && rch==hardblank) return lch;
    }

  if (lch==hardblank || rch==hardblank) return '\0';

  if (smushmode & 1) {
    if (lch==rch) return lch;
    }

  if (smushmode & 2) {
    if (lch=='_' && strchr("|/\\[]{}()<>",rch)) return rch;
    if (rch=='_' && strchr("|/\\[]{}()<>",lch)) return lch;
    }

  if (smushmode & 4) {
    if (lch=='|' && strchr("/\\[]{}()<>",rch)) return rch;
    if (rch=='|' && strchr("/\\[]{}()<>",lch)) return lch;
    if (strchr("/\\",lch) && strchr("[]{}()<>",rch)) return rch;
    if (strchr("/\\",rch) && strchr("[]{}()<>",lch)) return lch;
    if (strchr("[]",lch) && strchr("{}()<>",rch)) return rch;
    if (strchr("[]",rch) && strchr("{}()<>",lch)) return lch;
    if (strchr("{}",lch) && strchr("()<>",rch)) return rch;
    if (strchr("{}",rch) && strchr("()<>",lch)) return lch;
    if (strchr("()",lch) && strchr("<>",rch)) return rch;
    if (strchr("()",rch) && strchr("<>",lch)) return lch;
    }

  if (smushmode & 8) {
    if (lch=='[' && rch==']') return '|';
    if (rch=='[' && lch==']') return '|';
    if (lch=='{' && rch=='}') return '|';
    if (rch=='{' && lch=='}') return '|';
    if (lch=='(' && rch==')') return '|';
    if (rch=='(' && lch==')') return '|';
    }

  if (smushmode & 16) {
    if (lch=='/' && rch=='\\') return 'X';
    if (rch=='/' && lch=='\\') return 'X';
    if (lch=='>' && rch=='<') return 'X';
      /* Don't want the reverse of above to give 'X'. */
    }

  return '\0';
}


/****************************************************************************

  smushamt

  Returns the maximum amount that the current character can be smushed
  into the current line.

****************************************************************************/

int smushamt()
{
  int maxsmush,amt;
  int row,linebd,charbd;
  char ch1,ch2;

  if (smushmode== -1) {
    return 0;
    }
  maxsmush = currcharwidth;
  for (row=0;row<charheight;row++) {
    if (right2left) {
      for (charbd=MYSTRLEN(currchar[row]);
        ch1=currchar[row][charbd],(charbd>0&&(!ch1||ch1==' '));charbd--) ;
      for (linebd=0;ch2=outline[row][linebd],ch2==' ';linebd++) ;
      amt = linebd+currcharwidth-1-charbd;
      }
    else {
      for (linebd=MYSTRLEN(outline[row]);
        ch1 = outline[row][linebd],(linebd>0&&(!ch1||ch1==' '));linebd--) ;
      for (charbd=0;ch2=currchar[row][charbd],ch2==' ';charbd++) ;
      amt = charbd+outlinelen-1-linebd;
      }
    if (!ch1||ch1==' ') {
      amt++;
      }
    else if (ch2) {
      if (smushem(ch1,ch2)!='\0') {
        amt++;
        }
      }
    if (amt<maxsmush) {
      maxsmush = amt;
      }
    }
  return maxsmush;
}


/****************************************************************************

  addchar

  Attempts to add the given character onto the end of the current line.
  Returns 1 if this can be done, 0 otherwise.

****************************************************************************/

int addchar(c)
inchr c;
{
  int smushamount,row,k;
  char *templine;

  getletter(c);
  smushamount = smushamt();
  if (outlinelen+currcharwidth-smushamount>outlinelenlimit
      ||inchrlinelen+1>inchrlinelenlimit) {
    return 0;
    }

  templine = (char*)myalloc(sizeof(char)*(outlinelenlimit+1));
  for (row=0;row<charheight;row++) {
    if (right2left) {
      strcpy(templine,currchar[row]);
      for (k=0;k<smushamount;k++) {
        templine[currcharwidth-smushamount+k] =
          smushem(templine[currcharwidth-smushamount+k],outline[row][k]);
        }
      strcat(templine,outline[row]+smushamount);
      strcpy(outline[row],templine);
      }
    else {
      for (k=0;k<smushamount;k++) {
        outline[row][outlinelen-smushamount+k] =
          smushem(outline[row][outlinelen-smushamount+k],currchar[row][k]);
        }
      strcat(outline[row],currchar[row]+smushamount);
      }
    }
  free(templine);
  outlinelen = MYSTRLEN(outline[0]);
  inchrline[inchrlinelen++] = c;
  return 1;
}


/****************************************************************************

  putstring

  Prints out the given null-terminated string, substituting blanks
  for hardblanks.  If outputwidth is 1, prints the entire string;
  otherwise prints at most outputwidth-1 characters.  Prints a newline
  at the end of the string.  The string is left-justified, centered or
  right-justified (taking outputwidth as the screen width) if
  justification is 0, 1 or 2, respectively.

****************************************************************************/

void putstring(string)
char *string;
{
  int i,len;

  len = MYSTRLEN(string);
  if (outputwidth>1) {
    if (len>outputwidth-1) {
      len = outputwidth-1;
      }
    if (justification>0) {
      for (i=1;(3-justification)*i+len+justification-2<outputwidth;i++) {
        putchar(' ');
        }
      }
    }
  for (i=0;i<len;i++) {
    putchar(string[i]==hardblank?' ':string[i]);
    }
  putchar('\n');
}


/****************************************************************************

  printline

  Prints outline using putstring, then clears the current line.

****************************************************************************/

void printline()
{
  int i;

  for (i=0;i<charheight;i++) {
    putstring(outline[i]);
    }
  clearline();
}


/****************************************************************************

  splitline

  Splits inchrline at the last word break (bunch of consecutive blanks).
  Makes a new line out of the first part and prints it using
  printline.  Makes a new line out of the second part and returns.

****************************************************************************/

void splitline()
{
  int i,gotspace,lastspace,len1,len2;
  inchr *part1,*part2;

  part1 = (inchr*)myalloc(sizeof(inchr)*(inchrlinelen+1));
  part2 = (inchr*)myalloc(sizeof(inchr)*(inchrlinelen+1));
  gotspace = 0;
  for (i=inchrlinelen-1;i>=0;i--) {
    if (!gotspace && inchrline[i]==' ') {
      gotspace = 1;
      lastspace = i;
      }
    if (gotspace && inchrline[i]!=' ') {
      break;
      }
    }
  len1 = i+1;
  len2 = inchrlinelen-lastspace-1;
  for (i=0;i<len1;i++) {
    part1[i] = inchrline[i];
    }
  for (i=0;i<len2;i++) {
    part2[i] = inchrline[lastspace+1+i];
    }
  clearline();
  for (i=0;i<len1;i++) {
    addchar(part1[i]);
    }
  printline();
  for (i=0;i<len2;i++) {
    addchar(part2[i]);
    }
  free(part1);
  free(part2);
}


/****************************************************************************

  handlemapping

  Given an input character (type inchr), executes re-mapping commands
  read from control files.  Returns re-mapped character (inchr).

****************************************************************************/

inchr handlemapping(c)
inchr c;
{
  comnode *cmptr;

  cmptr=commandlist;
  while (cmptr!=NULL) {
    if (cmptr->thecommand ?
      (c >= cmptr->rangelo && c <= cmptr->rangehi) : 0) {
      c += cmptr->offset;
      while(cmptr!=NULL ? cmptr->thecommand : 0) {
        cmptr=cmptr->next;
        }
      }
    else {
      cmptr=cmptr->next;
      }
    }
  return c;
}


/****************************************************************************

  main

  The main program, of course.
  Reads characters 1 by 1 from stdin, and makes lines out of them using
  addchar. Handles line breaking, (which accounts for most of the
  complexity in this function).

****************************************************************************/

int main(argc,argv)
int argc;
char *argv[];
{
  inchr c,c2;
  int i;
  int last_was_eol_flag;
/*---------------------------------------------------------------------------
  wordbreakmode:
    -1: /^$/ and blanks are to be absorbed (when line break was forced
      by a blank or character larger than outlinelenlimit)
    0: /^ *$/ and blanks are not to be absorbed
    1: /[^ ]$/ no word break yet
    2: /[^ ]  *$/
    3: /[^ ]$/ had a word break
---------------------------------------------------------------------------*/
  int wordbreakmode;
  int char_not_added;

  getparams(argc,argv);
  readcontrolfiles();
  readfont();
  linealloc();

  wordbreakmode = 0;
  last_was_eol_flag = 0;

  while ((c = getchar())!=EOF) {

    if (c=='\n'&&paragraphflag&&!last_was_eol_flag) {
      ungetc(c2 = getchar(),stdin);
      c = ((isascii(c2)&&isspace(c2))?'\n':' ');
      }
    last_was_eol_flag = (isascii(c)&&isspace(c)&&c!='\t'&&c!=' ');

    if (deutschflag) {
      if (c>='[' && c<=']') {
        c += -255-'[';
        }
      else if (c >='{' && c <= '~') {
        c += -252-'{';
        }
      }

    c = handlemapping(c);

    if (isascii(c)&&isspace(c)) {
      c = (c=='\t'||c==' ') ? ' ' : '\n';
      }

    if ((c>'\0' && c<' ' && c!='\n') || c==127) continue;

/*
  Note: The following code is complex and thoroughly tested.
  Be careful when modifying!
*/

    do {
      char_not_added = 0;

      if (wordbreakmode== -1) {
        if (c==' ') {
          break;
          }
        else if (c=='\n') {
          wordbreakmode = 0;
          break;
          }
        wordbreakmode = 0;
        }

      if (c=='\n') {
        printline();
        wordbreakmode = 0;
        }

      else if (addchar(c)) {
        if (c!=' ') {
          wordbreakmode = (wordbreakmode>=2)?3:1;
          }
        else {
          wordbreakmode = (wordbreakmode>0)?2:0;
          }
        }

      else if (outlinelen==0) {
        for (i=0;i<charheight;i++) {
          if (right2left && outputwidth>1) {
            putstring(currchar[i]+MYSTRLEN(currchar[i])-outlinelenlimit);
            }
          else {
            putstring(currchar[i]);
            }
          }
        wordbreakmode = -1;
        }

      else if (c==' ') {
        if (wordbreakmode==2) {
          splitline();
          }
        else {
          printline();
          }
        wordbreakmode = -1;
        }

      else {
        if (wordbreakmode>=2) {
          splitline();
          }
        else {
          printline();
          }
        wordbreakmode = (wordbreakmode==3)?1:0;
        char_not_added = 1;
        }

      } while (char_not_added);
    }

  if (outlinelen!=0) {
    printline();
    }
  exit(0);
}
