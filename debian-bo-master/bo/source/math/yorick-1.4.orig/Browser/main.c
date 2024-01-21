/*
 * MAIN.C
 *
 * $Id: main.c,v 1.1 1993/08/27 17:19:10 munro Exp $
 *
 * Main for GIST CGM viewer
 *
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include <cgm.h>
#ifndef NO_XLIB
#include <dispas.h>
#include <xbasic.h>
#endif

/* OpenCGM, ReadCGM, CGMRelative, cgmLandscape, bg0fg1 in cgmin.h */
#include "cgmin.h"
/* GpEPSEngine declared in eps.h */
#include "eps.h"

extern void *malloc(long);  /* really size_t which is unsigned */
extern void free(void *);
extern void exit(int);

#ifdef STDC_HEADERS
#include <string.h>
#else
#ifndef SIZE_T_TYPE
#define SIZE_T_TYPE unsigned long
#endif
extern int strcmp(const char *, const char *);
extern char *strtok(char *, const char *);
extern SIZE_T_TYPE strlen(const char *);
extern char *strcpy(char *, const char *);
#endif

extern long strtol(const char *, char **, int);

extern int cgmScaleToFit;
extern int gtDoEscapes;

/* List of commands in alphabetical order, 0 terminated */
static char *commandList[]= {
  "cgm", "display", "draw", "eps", "free", "help", "info", "open",
  "ps", "quit", "send", 0 };

/* Corresponding functions */
static int MakeCGM(int help);
static int MakeX(int help);
static int Draw(int help);
static int EPS(int help);
static int FreeAny(int help);
static int Help(int help);
static int Info(int help);
static int OpenIn(int help);
static int MakePS(int help);
static int Quit(int help);
static int Send(int help);
static int Special(int help);  /* must correspond to 0 in commandList */
static int nPrefix, cSuffix;   /* arguments for Special */
typedef int (*Command)(int help);
static Command CommandList[]= {
  &MakeCGM, &MakeX, &Draw, &EPS, &FreeAny, &Help, &Info, &OpenIn,
  &MakePS, &Quit, &Send, &Special };

int amBatch= 0;

int warningCount= 0;

static int CreateCGM(int device, char *name);
static int CreatePS(int device, char *name);
static int CreateX(int device, char *name);

static int GetPageGroup(const char *token);
static int CheckEOL(const char *command);
static void DrawSend(int ds, char *token);
static int GetDeviceList(int ds, const char *command);
static int FindDevice(void);
static int SaveName(int device, const char *name);
static void DoSpecial(int nPrefix, int cSuffix);

static int HandleInput(FILE *file, void *context);
static int xPrefix= 0;
#ifndef NO_XLIB
static void HandleExpose(Engine *engine, Drawing *drawing, XEvent *event);
static void HandleOther(Engine *engine, Drawing *drawing, XEvent *event);
#endif
static void HelpAndExit(void);
static void MessageAndExit(const char *msg);

int defaultDPI;

char *outNames[8];
int outLengths[8];
int outDraw[8], outSend[8], outMark[8];

int nPageGroups= 0;
int mPage[32], nPage[32], sPage[32];

int main(int argc, char *argv[])
{
  int i, j, noDisplay, nOut, x_only;
  char *arg, *inName;

  nOut= 0;
  for (i=0 ; i<8 ; i++) {
    outNames[i]= 0;
    outEngines[i]= 0;
    outDraw[i]= outSend[i]= outLengths[i]= 0;
  }
  noDisplay= amBatch= x_only= 0;
  defaultDPI= 100;
  inName= 0;

  for (i=1 ; i<argc ; i++) {
    arg= argv[i];

    if (arg[0]=='-') {
      int fileType= -1;
      arg++;
      if (strcmp(arg, "cgm")==0) {
	fileType= 0;
	i++;
	if (i>=argc) MessageAndExit("Missing file or display name");
	arg= argv[i];
      } else if (strcmp(arg, "ps")==0) {
	fileType= 1;
	i++;
	if (i>=argc) MessageAndExit("Missing file or display name");
	arg= argv[i];
      } else if (strcmp(arg, "in")==0) {
	i++;
	if (i>=argc) MessageAndExit("Missing file or display name");
	if (inName) HelpAndExit();
	else inName= argv[i];
#ifndef NO_XLIB
      } else if (strcmp(arg, "display")==0 || strcmp(arg, "d")==0) {
	fileType= 2;
	i++;
	if (i>=argc) MessageAndExit("Missing file or display name");
	arg= argv[i];
#endif
      } else if (strcmp(arg, "f")==0) {
	amBatch= 1;
	fileType= 1;
	arg= "*stdout*";
      } else if (strcmp(arg, "nd")==0) noDisplay= 1;
      else if (strcmp(arg, "b")==0) amBatch= 1;
#ifndef NO_XLIB
      else if (strcmp(arg, "75")==0) defaultDPI= 75;
      else if (strcmp(arg, "100")==0) defaultDPI= 100;
      else if (strcmp(arg, "gks")==0) {
	gx75width= gx75height= 600;     /* 8x8 X window size */
	gx100width= gx100height= 800;   /* 8x8 X window size */
	cgmScaleToFit= 1;               /* 8x8 PostScript plotting area */
	gtDoEscapes= 0;
      } else if (strcmp(arg, "x")==0) x_only= 1;
#endif
#ifdef NO_XLIB
      else if (strcmp(arg, "gks")==0) {
	cgmScaleToFit= 1;
	gtDoEscapes= 0;
      }
#endif
      else if (strcmp(arg, "fmbug")==0) epsFMbug= 1;
      else if (strcmp(arg, "bg0fg1")==0) bg0fg1= 1;
      else if (strcmp(arg, "esc0")==0) gtDoEscapes= 0;
      else if (strcmp(arg, "esc1")==0) gtDoEscapes= 1;
      else HelpAndExit();

      if (fileType>=0) {
	if (nOut>=8)
	  MessageAndExit("At most 8 output files/displays allowed");
	for (j=0 ; j<nOut ; j++) if (strcmp(outNames[j], arg)==0)
	  MessageAndExit("Duplicate output filenames not allowed");
	outNames[nOut]= arg;
	outTypes[nOut]= fileType;
	nOut++;
      }

    } else if (arg[0]<='9' && arg[0]>='0') {
      if (GetPageGroup(arg)) MessageAndExit("Try again");

    } else if (!inName) {
      inName= arg;

    } else {
      HelpAndExit();
    }
  }

  if (inName && OpenCGM(inName)) inName= 0;

  if (amBatch) {
    if (!inName)
      MessageAndExit("Must specify an input CGM file to use -b or -f");
    if (!nOut)
      MessageAndExit("Must specify some output file to use -b");
    noDisplay= 1;
#ifndef NO_XLIB
  } else {
    DispatchWorker= CatalogCGM;
    if (!x_only)
      AddFDispatcher(stdin, &HandleInput, 0);  /* interactive input */
    else if (!inName)
      MessageAndExit("Must specify an input CGM file to use -x");
#endif
  }

  /* Create CGM and PostScript engines */
  for (i=0 ; i<nOut ; i++) {
    if (outTypes[i]==0) CreateCGM(i, outNames[i]);
    else if (outTypes[i]==1) CreatePS(i, outNames[i]);
  }

  /* If page list specified, do implied send command */
  if (amBatch && nPageGroups<=0) {
    mPage[0]= 1;
    nPage[0]= 32767;
    sPage[0]= 1;
    nPageGroups= 1;
  }
  if (nPageGroups>0) {
    int n= 0;
    for (i=0 ; i<8 ; i++) {
      if (!outSend[i]) GpDeactivate(outEngines[i]);
      if (outSend[i] && !GpActivate(outEngines[i])) n++;
    }
    if (n) ReadCGM(mPage, nPage, sPage, nPageGroups);
  }

#ifndef NO_XLIB
  /* Connect to X server(s) by creating X engines */
  GxInitialize(&argc, argv);
  if (!noDisplay) {
    int noX= 1;
    for (i=0 ; i<nOut ; i++) if (outTypes[i]==2) {
      if (!CreateX(i, outNames[i])) noX= 0;
    }
    if (noX && nOut<8) {
      if (!CreateX(nOut, 0)) {
	nOut++;
	noX= 0;
      }
    }
    noDisplay= noX;
  }
  if (noDisplay && x_only) MessageAndExit("Must be an X display to use -x");
#endif

  /* If interactive, enter event loop */
  if (!amBatch) {
    if (!x_only) {
      fputs("gist> ", stdout);  /* print first prompt */
      fflush(stdout);
    }
#ifndef NO_XLIB
    DispatchEvents();
#else
    while (!HandleInput(stdin, (void *)0));
#endif
  }

  for (i=0 ; i<8 ; i++) {
    if (outEngines[i]) {
      GpDeactivate(outEngines[i]);
      GpKillEngine(outEngines[i]);
    }
  }

  return 0;
}

/* ------------------------------------------------------------------------ */

static void HelpAndExit(void)
{
  fputs("Usage:   gist [cgminput] [options] [page list]\n\n", stderr);
  fputs("   options:  -in cgminput  (open cgminput for input)\n", stderr);
  fputs("             -cgm cgmout   (open cgmout for output)\n", stderr);
  fputs("             -ps psout     (open psout for output)\n", stderr);
#ifndef NO_XLIB
  fputs("             -display host:n.m  (connect to X server)\n", stderr);
  fputs("             -75  (X windows default to 75 dpi)\n", stderr);
  fputs("             -100 (X windows default to 100 dpi)\n", stderr);
  fputs("             -gks (X windows default to 8x8 inches)\n", stderr);
  fputs("             -nd  (not even default X server)\n", stderr);
  fputs("             -b   (batch mode, implies -nd)\n", stderr);
  fputs("             -x   (X window only, no keyboard)\n", stderr);
#else
  fputs("             -b   (batch mode)\n", stderr);
#endif
  fputs("             -f   (PostScript to stdout, implies -b)\n", stderr);
  fputs("             -fmbug (if EPS files are for FrameMaker)\n", stderr);
  fputs("             -bg0fg1 (force color 0 to bg, 1 to fg)\n", stderr);
  fputs("             -esc0 (skip ^_! escapes-assumed if -gks)\n", stderr);
  fputs("             -esc1 (handle ^_! text escapes-default)\n", stderr);
  fputs("   page list:  [page group] [page group] ...\n", stderr);
  fputs("   page group:  n     (output page n)\n", stderr);
  fputs("   page group:  n-m   (output pages n->m inclusive)\n", stderr);
  fputs("   page group:  n-m-s (output every s-th page, n-m)\n", stderr);
  exit(0);
}

static void MessageAndExit(const char *msg)
{
  fputs("gist: ", stderr);
  fputs(msg, stderr);
  fputs("\ngist: try    gist -help    for usage\n", stderr);
  exit(0);
}

void Warning(const char *general, const char *particular)
{
  if (++warningCount > 5) return;
  fputs("gist: (WARNING) ", stderr);
  fputs(general, stderr);
  fputs(particular, stderr);
  fputs("\n", stderr);
}

/* ------------------------------------------------------------------------ */

static int GetCommand(const char *token)
{
  char **command= commandList;
  char *cmd;
  const char *now= token;
  int quitOK= 0;

  if (!now) return -1;

  /* Quit has two synonyms, ? is a synonym for help, and f, b, and g
     are special command forms.  */
  if (strcmp(token, "exit")==0 || strcmp(token, "end")==0 ||
      strcmp(token, "quit")==0) {
    /* Quit and its synonyms cannot be abbreviated */
    now= "quit";
    quitOK= 1;
  } else if (strcmp(token, "?")==0) now= "help";
  else if (strcmp(token, "f")==0) now= "1f";
  else if (strcmp(token, "b")==0) now= "1b";
  else if (strcmp(token, "g")==0) now= "1g";
  else if (strcmp(token, "s")==0) now= "send";

  /* Check for nf, nb, ng */
  if (*now<='9' && *now>='0') {
    char *suffix;
    nPrefix= (int)strtol(now, &suffix, 10);
    cSuffix= *suffix;
    if ((cSuffix=='f' || cSuffix=='b' || cSuffix=='g') &&
	*(suffix+1)=='\0') {
      while (*command) command++;
      return command-commandList;
    }
  }

  cmd= *command;
  while (cmd && *now>*cmd) cmd= *(++command);
  while (cmd && *now==*cmd) {
    while (*cmd && *now==*cmd) { now++;  cmd++; }
    if (!*now) break;  /* token matches cmd */
    now= token;
    cmd= *(++command);
  }
  if (!cmd || *now) {
    fputs("gist: Unknown command: ", stderr);
    fputs(token, stderr);
    fputs("\n      Type help for help.\n", stderr);
    return -1;
  }

  if (*cmd) {
    now= token;
    cmd= *(command+1);
    if (cmd && *now++==*cmd++) {
      while (*cmd && *now==*cmd) { now++;  cmd++; }
      if (!*now) {
	fputs("gist: Ambiguous command: ", stderr);
	fputs(token, stderr);
	fputs("\n      Type help for help.\n", stderr);
	return -1;
      }
    }
  }

  if (strcmp(*command, "quit")==0 && !quitOK) {
    fputs("gist: The quit command cannot be abbreviated.\n", stderr);
    return -1;
  }

  return command-commandList;
}

static int GetPageGroup(const char *token)
{
  int n, m, s;
  char *suffix, *tok;

  s= 1;
  n= m= (int)strtol(token, &suffix, 10);
  if (suffix!=token && *suffix=='-') {
    tok= suffix+1;
    n= (int)strtol(tok, &suffix, 10);
    if (suffix!=tok && *suffix=='-') {
      tok= suffix+1;
      s= (int)strtol(tok, &suffix, 10);
      if (suffix==tok) suffix= tok-1;  /* this is an error */
    }
  }

  if (*suffix) {
    fputs("gist: (SYNTAX) ", stderr);
    fputs(token, stderr);
    fputs(" is not a legal page number group\n", stderr);
    return 1;
  }

  if (nPageGroups>=32) {
    fputs("gist: (SYNTAX) too many page number groups (32 max)\n", stderr);
    return 1;
  }

  mPage[nPageGroups]= m;
  nPage[nPageGroups]= n;
  sPage[nPageGroups]= s;
  nPageGroups++;
  return 0;
}

static int CheckEOL(const char *command)
{
  if (strtok(0, " \t\n")) {
    fputs("gist: (SYNTAX) garbage after ", stderr);
    fputs(command, stderr);
    fputs(" command\n", stderr);
    return 1;
  } else {
    return 0;
  }
}

static int Help(int help)
{
  int cmd;

  if (help) {
    fputs("gist: help command syntax:\n     help [command]\n", stderr);
    fputs("  Print help message (for given command).\n", stderr);
    return 0;
  }

  cmd= GetCommand(strtok(0, " \t\n"));

  if (cmd<0) {
    int len;
    char line[80], **command= commandList;
    fputs("gist: Input Syntax:\n     command [arg1] [arg2] ...\n", stderr);
    strcpy(line, "  Available commands are:  ");
    len= 27;
    for (;;) {
      for (;;) {
	if (len+strlen(*command) > 72) {
	  strcpy(line+len, "\n");
	  fputs(line, stderr);
	  break;
	}
	strcpy(line+len, *command);
	len+= strlen(*command++);
	if (!*command) {
	  strcpy(line+len, "\n");
	  fputs(line, stderr);
	  break;
	}
	strcpy(line+len, ", ");
	len+= 2;
      }
      if (!*command) break;
      strcpy(line, "     ");
      len= 5;
    }
    fputs("  The quit command has two synonyms:  exit, end\n", stderr);
    fputs("  Any command except quit may be abbreviated by\n", stderr);
    fputs("  omitting characters from the right.\n", stderr);
    fputs("  The help command explains specific syntax, e.g.:\n", stderr);
    fputs("     help cgm\n", stderr);
    fputs("  describes the syntax of the cgm command.\n", stderr);
    fputs("  Five commands can be typed to a gist X window:\n", stderr);
    fputs("     nf   - forward n pages and draw (default 1)\n", stderr);
    fputs("     nb   - backward n pages and draw (default 1)\n", stderr);
    fputs("     ng   - go to page n and draw (default 1)\n", stderr);
    fputs("     s    - send current page\n", stderr);
    fputs("     q    - quit\n", stderr);

  } else {
    CommandList[cmd](1);
  }

  return 0;
}

static char line[256];

/* ARGSUSED */
static int HandleInput(FILE *file, void *context)
{
  int cmd;

  fgets(line, 256, file);
  if (ferror(file)) {
    fputs("gist: ***FATAL*** error reading stdin\n", stderr);
    exit(1);
  }
  if (feof(file)) {
    fputs("gist: ***FATAL*** eof reading stdin\n", stderr);
    exit(1);
  }
  cmd= GetCommand(strtok(line, " \t\n"));

  warningCount= xPrefix= 0;
  if (cmd>=0 && CommandList[cmd](0)) return 1;

  fputs("gist> ", stdout);
  fflush(stdout);
  return 0;
}

static int Quit(int help)
{
  if (help) {
    fputs("gist: quit command syntax:\n     quit\n", stderr);
    fputs("  Finish and close any output files, then exit.\n", stderr);
    fputs("  Synonyms:  exit, end   (no abbreviations allowed)\n", stderr);
    return 0;
  }
  return 1;
}

/* ------------------------------------------------------------------------ */

static int SaveName(int device, const char *name)
{
  int len;

  if (name == (const char *)(outNames+device)) return 0;

  len= name? (int)strlen(name) : 0;
  if (len>outLengths[device]) {
    if (outLengths[device]>0) free(outNames[device]);
    outNames[device]= (char *)malloc(len+1);
    if (!outNames[device]) {
      fputs("gist: (SEVERE) memory manager failed in SaveName\n", stderr);
      outLengths[device]= 0;
      return 1;
    }
    outLengths[device]= len;
  }

  if (name) strcpy(outNames[device], name);
  else if (outLengths[device]>0) outNames[device][0]= '\0';
  else outNames[device]= 0;
  return 0;
}

static int CreateCGM(int device, char *name)
{
  if (SaveName(device, name)) return 1;

  outEngines[device]=
    GpCGMEngine("CGM Viewer", cgmLandscape, 0, name);
  if (!outEngines[device]) {
    Warning(gistError, "");
    Warning("Unable to create CGM engine ", name);
    return 1;
  }

  outTypes[device]= 0;
#ifndef NO_XLIB
  outDraw[device]= 0;
#else
  outDraw[device]= 1;
#endif
  outSend[device]= 1;

  return 0;
}

static int CreatePS(int device, char *name)
{
  if (SaveName(device, name)) return 1;

  outEngines[device]=
    GpPSEngine("CGM Viewer", cgmLandscape, 0, name);
  if (!outEngines[device]) {
    Warning(gistError, "");
    Warning("Unable to create PostScript engine ", name);
    return 1;
  }

  outTypes[device]= 1;
#ifndef NO_XLIB
  outDraw[device]= 0;
#else
  outDraw[device]= 1;
#endif
  outSend[device]= 1;

  return 0;
}

#ifndef NO_XLIB
static int CreateX(int device, char *name)
{
  if (SaveName(device, name)) return 1;

  outEngines[device]=
    GpBXEngine("CGM Viewer", cgmLandscape, defaultDPI, name);
  if (!outEngines[device]) {
    Warning(gistError, "");
    Warning("Unable to open X server ", name? name : "(default)");
    return 1;
  }

  outTypes[device]= 2;
  outDraw[device]= 1;
  outSend[device]= 0;

  GxInput(outEngines[device], &HandleExpose, 0, &HandleOther, KeyPressMask);

  return 0;
}
#endif

/* ------------------------------------------------------------------------ */

static int OpenIn(int help)
{
  char *token;

  if (help) {
    fputs("gist: open command syntax:\n     open cgminput\n", stderr);
    fputs("  Closes the current CGM input file, then opens cgminput.\n",
	  stderr);
    fputs("  Only a Gist-compliant binary CGM file is legal.\n", stderr);
    fputs("  The cgminput may be the first file of a family.\n", stderr);
    fputs("  Subsequent page numbers refer to this input file.\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (!token) {
    fputs("gist: (SYNTAX) cgminput name missing in open command\n", stderr);
    return 0;
  }
  if (CheckEOL("open")) return 0;

  OpenCGM(token);
  return 0;
}

static int FindDevice(void)
{
  int i;
  for (i=0 ; i<8 ; i++) if (!outEngines[i]) break;
  if (i>=8)
    Warning("8 devices already open for output, command ignored", "");
  return i;
}

static int MakeCGM(int help)
{
  char *token, *cgmout;
  long size= 0;
  int device;

  if (help) {
    fputs("gist: cgm command syntax:\n     cgm cgmout [size]\n", stderr);
    fputs("  Opens a CGM file cgmout for output.\n", stderr);
    fputs("  The size (default 1000000) is the maximum size of a\n", stderr);
    fputs("  single file in the output family, in bytes.\n", stderr);
    fputs("  Subsequent send commands will write to cgmout,\n", stderr);
    fputs("  unless the send to list is modified (see send).\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (!token) {
    fputs("gist: (SYNTAX) cgmout name missing in cgm command\n", stderr);
    return 0;
  }
  cgmout= token;
  token= strtok(0, " \t\n");
  if (token) {
    char *suffix;
    size= strtol(token, &suffix, 0);
    if (*suffix) {
      fputs("gist: (SYNTAX) size unintelligble in cgm command\n", stderr);
      return 0;
    }
    if (CheckEOL("cgm")) return 0;
  }

  device= FindDevice();
  if (device>=8) return 0;

  if (!CreateCGM(device, cgmout) &&
      size>0) ((CGMEngine *)outEngines[device])->fileSize= size;

  return 0;
}

static int MakePS(int help)
{
  char *token;
  int device;

  if (help) {
    fputs("gist: ps command syntax:\n     ps psout\n", stderr);
    fputs("  Opens a PostScript file psout for output.\n", stderr);
    fputs("  Subsequent send commands will write to psout,\n", stderr);
    fputs("  unless the send to list is modified (see send).\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (!token) {
    fputs("gist: (SYNTAX) psout name missing in ps command\n", stderr);
    return 0;
  }
  if (CheckEOL("ps")) return 0;

  device= FindDevice();
  if (device>=8) return 0;

  CreatePS(device, token);

  return 0;
}

static int MakeX(int help)
{
#ifndef NO_XLIB
  char *token, *server;
  int dpi, device, defDPI;

  if (help) {
    fputs("gist: display command syntax:\n     ", stderr);
    fputs("display host:server.screen [dpi]\n", stderr);
    fputs("  Connects to the specified X server.\n", stderr);
    fputs("  Subsequent draw commands will write to server,\n", stderr);
    fputs("  unless the draw to list is modified (see draw).\n", stderr);
    fputs("  If specified, dpi must be 75 or 100 (default 100).\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (!token) {
    fputs("gist: (SYNTAX) cgmoutput name missing in cgm command\n", stderr);
    return 0;
  }
  server= token;
  token= strtok(0, " \t\n");
  if (token) {
    char *suffix;
    dpi= (int)strtol(token, &suffix, 0);
    if (*suffix) {
      fputs("gist: (SYNTAX) dpi unintelligble in display command\n", stderr);
      return 0;
    }
    if (dpi!=75 && dpi!=100) {
      fputs("gist: (SYNTAX) dpi must be 75 or 100 in display command\n",
	    stderr);
      return 0;
    }
    if (CheckEOL("display")) return 0;
  } else {
    dpi= 100;
  }

  device= FindDevice();
  if (device>=8) return 0;

  defDPI= defaultDPI;
  defaultDPI= dpi;
  CreateX(device, server);
  defaultDPI= defDPI;

#else
  if (help) {
    fputs("gist: display command disabled (no X windows)\n", stderr);
    return 0;
  }
  Warning("No X windows in this version of gist, command ignored.", "");
#endif
  return 0;
}

static int EPS(int help)
{
  char *token;
  int device;

  if (help) {
    fputs("gist: eps command syntax:\n     eps epsout\n", stderr);
    fputs("  Open an Encapsulated PostScript file epsout, write\n", stderr);
    fputs("  the current page to it, then close epsout.\n", stderr);
    fputs("  (Note that an EPS file can have only a single page.)\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (!token) {
    fputs("gist: (SYNTAX) epsout name missing in eps command\n", stderr);
    return 0;
  }
  if (CheckEOL("eps")) return 0;

  device= FindDevice();
  if (device>=8) return 0;

  device= FindDevice();
  if (device>=8) return 0;

  outEngines[device]=
    GpPSEngine("CGM Viewer", cgmLandscape, 0, "_tmp.eps");
  if (!outEngines[device]) {
    Warning(gistError, "");
    Warning("Unable to create PostScript engine ", token);
    return 0;
  }

  GpPreempt(outEngines[device]);

  nPage[0]= mPage[0]= CGMRelative(0);
  sPage[0]= 1;
  nPageGroups= 1;

  /* First read does PS part, second computes EPS preview */
  if (!ReadCGM(mPage, nPage, sPage, nPageGroups)) {
    GpPreempt(0);
    outEngines[device]= EPSPreview(outEngines[device], token);
    if (outEngines[device]) {
      GpPreempt(outEngines[device]);
      ReadCGM(mPage, nPage, sPage, nPageGroups);
    } else {
      Warning("memory manager failed creating EPS engine ", token);
      return 0;
    }
  }

  if (outEngines[device]) {
    GpPreempt(0);

    GpKillEngine(outEngines[device]);
    outEngines[device]= 0;
  }

  return 0;
}

static int FreeAny(int help)
{
  int i;

  if (help) {
    fputs("gist: free command syntax:\n     free [device# ...]\n", stderr);
    fputs("  Finish and close the device#(s).  If none given,\n", stderr);
    fputs("  frees all send devices,\n", stderr);
    fputs("  (Use the info command to describe current device numbers.)\n",
	  stderr);
    return 0;
  }

  if (GetDeviceList(1, "free")) return 0;

  for (i=0 ; i<8 ; i++) {
    if (outMark[i]) {
      GpKillEngine(outEngines[i]);
      outEngines[i]= 0;
    }
  }

  return 0;
}

static char *yorn[2]= { "No  ", "Yes " };
static char *tname[3]= { "CGM", "PS ", "X  " };

extern void CGMinfo(void);  /* cgmin.c */

static int Info(int help)
{
  int i;

  if (help) {
    fputs("gist: info command syntax:\n     info\n", stderr);
    fputs("  Print descriptions of all current output files.\n", stderr);
    return 0;
  }

  fputs("\n", stdout);
  CGMinfo();
  fputs("Number Draw Send Type   Name\n", stdout);
  for (i=0 ; i<8 ; i++) {
    if (outEngines[i]) {
      char msg[80];
      sprintf(msg, "%3d    %s %s %s  ", i, yorn[outDraw[i]], yorn[outSend[i]],
	     tname[outTypes[i]]);
      fputs(msg, stdout);
      fputs(outNames[i]? outNames[i] : "", stdout);
      fputs("\n", stdout);
    }
  }
  fputs("\n", stdout);
  fflush(stdout);

  return 0;
}

/* ------------------------------------------------------------------------ */

static int Draw(int help)
{
  int i, n;
  char *token;

  if (help) {
    fputs("gist: draw command syntax:\n     draw [page list]\n", stderr);
    fputs(
    "  Copy the page(s) (default current page) from the current CGM input\n",
	  stderr);
    fputs("  to all display output devices.\n", stderr);
    fputs("  By default, these are all X windows.\n", stderr);
    fputs("  Use alternate syntax:\n     draw to [device#1 ...]\n", stderr);
    fputs("  to specify a particular list of devices to be used\n", stderr);
    fputs("  by the draw command.  Without any device numbers,\n", stderr);
    fputs("  draw to restores the default list of devices.\n", stderr);
    fputs("  (Use the info command to describe current device numbers.)\n",
	  stderr);
    fputs("  Page list syntax:   group1 [group2 ...]\n", stderr);
    fputs("  Page group syntax:   n   - just page n\n", stderr);
    fputs("                     m-n   - pages n thru m\n", stderr);
    fputs("                   m-n-s   - pages n thru m, step s\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (token && strcmp(token, "to")==0) {
    DrawSend(1, token);
    return 0;
  }

  n= 0;
  for (i=0 ; i<8 ; i++) {
    if (!outDraw[i]) GpDeactivate(outEngines[i]);
    if (outDraw[i] && !GpActivate(outEngines[i])) n++;
  }

#ifndef NO_XLIB
  if (!n && (i= FindDevice())<8) {
    if (!CreateX(i, 0) && !GpActivate(outEngines[i])) n++;
  }
#endif

  if (n) DrawSend(0, token);
  else Warning("no devices active for draw command", "");
  return 0;
}

static int Send(int help)
{
  int i, n;
  char *token;

  if (help) {
    fputs("gist: send command syntax:\n     send [page list]\n", stderr);
    fputs(
    "  Copy the page(s) (default current page) from the current CGM input\n",
	  stderr);
    fputs("  to all display output devices.\n", stderr);
    fputs("  By default, these are all X windows.\n", stderr);
    fputs("  Use alternate syntax:\n     send to [device#1] ...\n", stderr);
    fputs("  to specify a particular list of devices to be used\n", stderr);
    fputs("  by the send command.  Without any device numbers,\n", stderr);
    fputs("  send to restores the default list of devices.\n", stderr);
    fputs("  (Use the info command to describe current device numbers.)\n",
	  stderr);
    fputs("  Page list syntax:   group1 [group2 ...]\n", stderr);
    fputs("  Page group syntax:   n   - just page n\n", stderr);
    fputs("                     m-n   - pages n thru m\n", stderr);
    fputs("                   m-n-s   - pages n thru m, step s\n", stderr);
    return 0;
  }

  token= strtok(0, " \t\n");
  if (token && strcmp(token, "to")==0) {
    DrawSend(1, token);
    return 0;
  }

  n= 0;
  for (i=0 ; i<8 ; i++) {
    if (!outSend[i]) GpDeactivate(outEngines[i]);
    if (outSend[i] && !GpActivate(outEngines[i])) n++;
  }

  if (n) DrawSend(1, token);
  else Warning("no devices active for send command", "");
  return 0;
}

static int GetDeviceList(int ds, const char *command)
{
  int device;
  char *token= strtok(0, " \t\n");

  if (token) {
    char *suffix;
    for (device=0 ; device<8 ; device++) outMark[device]= 0;
    do {
      device= (int)strtol(token, &suffix, 10);
      if (*suffix || device<0 || device>7) {
	fputs("gist: (SYNTAX) ", stderr);
	fputs(token, stderr);
	fputs(" not a legal device# in ", stderr);
	fputs(command, stderr);
	fputs(" command\n", stderr);
	return 1;
      }
      if (!outEngines[device])
	Warning("ignoring non-existent device# ", token);
      else
	outMark[device]= 1;
    } while ((token= strtok(0, " \t\n")));
    if (ds==0) {
      for (device=0 ; device<8 ; device++) if (outMark[device]) break;
      if (device>=8) {
	Warning(command, " command with no legal devices, no action taken");
	return 1;
      }
    }

  } else if (ds==0) {
    for (device=0 ; device<8 ; device++) outMark[device]= 0;
  } else {
    for (device=0 ; device<8 ; device++) outMark[device]= outSend[device];
  }

  return 0;
}

static char *dsName[]= { "draw to", "send to" };

static void DrawSend(int ds, char *token)
{
  nPageGroups= 0;

  if (token && strcmp(token, "to")==0) {
    /* draw to  or send to  merely resets outDraw or outSend list */
    int i, n= 0;
    int *outDS= ds? outSend : outDraw;
    if (GetDeviceList(0, dsName[ds])) return;
    for (i=0 ; i<8 ; i++) if ((outDS[i]= outMark[i])) n++;
    if (!n) for (i=0 ; i<8 ; i++)
      outDS[i]= outEngines[i]? (ds? outTypes[i]<2 : outTypes[i]==2) : 0;
    return;

  } else if (token) {
    do {
      if (GetPageGroup(token)) return;
    } while ((token= strtok(0, " \t\n")));
  } else {
    nPage[0]= mPage[0]= CGMRelative(0);
    sPage[0]= 1;
    nPageGroups= 1;
  }

  ReadCGM(mPage, nPage, sPage, nPageGroups);
}

static int Special(int help)
{
  if (help) {
    char msg[80];
    sprintf(msg, "gist: n%c command syntax:\n     n%c\n",
	    cSuffix, cSuffix);
    fputs(msg, stderr);
    if (cSuffix=='f')
      fputs("  Forward n (default 1) pages, then draw\n", stderr);
    else if (cSuffix=='b')
      fputs("  Backward n (default 1) pages, then draw\n", stderr);
    else
      fputs("  Go to page n (default 1), then draw\n", stderr);
    return 0;
  }

  if (CheckEOL("nf, nb, or ng")) return 0;

  DoSpecial(nPrefix, cSuffix);
  return 0;
}

static void DoSpecial(int nPrefix, int cSuffix)
{
  int i, n;

  n= 0;
  for (i=0 ; i<8 ; i++) {
    if (!outDraw[i]) GpDeactivate(outEngines[i]);
    if (outDraw[i] && !GpActivate(outEngines[i])) n++;
  }

  if (cSuffix=='f') mPage[0]= CGMRelative(nPrefix);
  else if (cSuffix=='b') mPage[0]= CGMRelative(-nPrefix);
  else mPage[0]= nPrefix;
  nPage[0]= mPage[0];
  sPage[0]= 1;
  nPageGroups= 1;

  if (n) ReadCGM(mPage, nPage, sPage, nPageGroups);
  else Warning("no devices active for nf, nb, or ng command", "");
}

/* ------------------------------------------------------------------------ */

#ifndef NO_XLIB
/* ARGSUSED */
static void HandleExpose(Engine *engine, Drawing *drawing, XEvent *event)
{
  XEngine *xEngine= GisXEngine(engine);

  if (!xEngine) return;

  /* Redraw current picture on this engine only */

  GpPreempt(engine);

  nPage[0]= mPage[0]= CGMRelative(0);
  sPage[0]= 1;
  nPageGroups= 1;

  ReadCGM(mPage, nPage, sPage, nPageGroups);

  GpPreempt(0);
}

static int cSuffices[]= { 'f', 'b', 'g' };

#include <X11/keysym.h>

/* ARGSUSED */
static void HandleOther(Engine *engine, Drawing *drawing, XEvent *event)
{
  XEngine *xEngine= GisXEngine(engine);
  Display *dpy= event->xany.display;
  char buffer[8];
  int nChars, go, bad;
  KeySym keysym;
  XComposeStatus compose;

  if (!xEngine) return;

  switch (event->type) {
  case KeyPress:

    go= bad= 0;
    nChars= XLookupString(&event->xkey, buffer, 8, &keysym, &compose);
    if (keysym==XK_0 || keysym==XK_KP_0) xPrefix= 10*xPrefix;
    else if (keysym==XK_1 || keysym==XK_KP_1) xPrefix= 10*xPrefix+1;
    else if (keysym==XK_2 || keysym==XK_KP_2) xPrefix= 10*xPrefix+2;
    else if (keysym==XK_3 || keysym==XK_KP_3) xPrefix= 10*xPrefix+3;
    else if (keysym==XK_4 || keysym==XK_KP_4) xPrefix= 10*xPrefix+4;
    else if (keysym==XK_5 || keysym==XK_KP_5) xPrefix= 10*xPrefix+5;
    else if (keysym==XK_6 || keysym==XK_KP_6) xPrefix= 10*xPrefix+6;
    else if (keysym==XK_7 || keysym==XK_KP_7) xPrefix= 10*xPrefix+7;
    else if (keysym==XK_8 || keysym==XK_KP_8) xPrefix= 10*xPrefix+8;
    else if (keysym==XK_9 || keysym==XK_KP_9) xPrefix= 10*xPrefix+9;
    else if (keysym==XK_f || keysym==XK_F || keysym==XK_KP_Add) go= 1;
    else if (keysym==XK_b || keysym==XK_B || keysym==XK_KP_Subtract) go= 2;
    else if (keysym==XK_g || keysym==XK_G || keysym==XK_KP_Enter) go= 3;
    else if (keysym==XK_s || keysym==XK_S || keysym==XK_KP_Equal) go= 4;
    else if (keysym==XK_q || keysym==XK_Q || keysym==XK_KP_Equal) go= 5;
    else if (keysym<XK_Shift_L || keysym>XK_Hyper_R) bad= 1;

    if ((go==4||go==5) && xPrefix!=0) bad= 1;
    if (go && !bad) {
      if (go<4) {
	if (xPrefix==0) xPrefix= 1;
	DoSpecial(xPrefix, cSuffices[go-1]);
      } else if (go==4) {
	int i, n= 0;
	for (i=0 ; i<8 ; i++) {
	  if (!outSend[i]) GpDeactivate(outEngines[i]);
	  if (outSend[i] && !GpActivate(outEngines[i])) n++;
	}

	nPage[0]= mPage[0]= CGMRelative(0);
	sPage[0]= 1;
	nPageGroups= 1;

	if (n) ReadCGM(mPage, nPage, sPage, nPageGroups);
	else Warning("no devices active for send command", "");
      } else if (go==5) {
	int i;
	for (i=0 ; i<8 ; i++) {
	  if (outEngines[i]) {
	    GpDeactivate(outEngines[i]);
	    GpKillEngine(outEngines[i]);
	  }
	}
	exit(0);
      }
      xPrefix= 0;
      warningCount= 0;
    } else if (bad) {
      XBell(dpy, 100);
      xPrefix= 0;
    }
    break;

  default:
    /* ignore anything else */
    break;
  }
}
#endif

/* ------------------------------------------------------------------------ */
