/* cfengine for GNU
 
        Copyright (C) 1995/6
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 

/*******************************************************************/
/*                                                                 */
/*  GLOBAL variables for cfengine                                  */
/*                                                                 */
/*******************************************************************/

#include "cf.defs.h"

char VERSION[maxvarsize];
char *COPYRIGHT = "(C) Free Software Foundation 1995/6\nDonated by Mark Burgess, Centre of Science and Technology\nFaculty of Engineering, Oslo College, 0254 Oslo, Norway";

char *VPRECONFIG = "cf.preconf";
char *VRCFILE = "cfrc";

/*******************************************************************/
/* FLAGS and Semaphores                                            */
/*******************************************************************/

short DEBUG = false;
short D1 = false;
short D2 = false;

short VERBOSE = false;
short TIDYDIRS = false;
short TRAVLINKS = false;
short PTRAVLINKS = false;
short DONTDO = false;
short IFCONF = true;
short PARSEONLY = false;
short GOTMOUNTINFO = true;
short NOMOUNTS = false;
short NOFILECHECK = false;
short NOTIDY = false;
short NOSCRIPTS = false;
short PRSYSADM = false;
short MOUNTCHECK = false;
short NOPROCS = false;
short NOEDITS = false;
short KILLOLDLINKS = false;
short IGNORELOCK = false;
short NOPRECONFIG = false;
short WARNINGS = true;
short MINUSF = false;
short NOLINKS = false;
short ENFORCELINKS = false;
short FORCELINK;
short NOCOPY = false;
short SILENT=false;
short EDITVERBOSE=false;
short LINKSILENT;
short FORCECOPY=false;
short IMAGEBACKUP=true;
short ROTATE=0;
short TIDYSIZE=0;
short USEENVIRON=false;
short PROMATCHES=-1;
short EDABORTMODE=false;
short UNDERSCORE_CLASSES=false;
short NOHARDCLASSES=false;

char LINKTYPE = 's';
char AGETYPE = 'a';
char COPYTYPE = 't';
char REPOSCHAR = '_';
char LISTSEPARATOR = ':';
char LINKDIRS = 'k';

int LINENUMBER = 1;
int HAVEUID = 0;
int NUMBEROFEDITS = 0;


/*******************************************************************/
/* Strings and Buffers                                             */
/*******************************************************************/

int ERRORCOUNT = 0;

char VBUFF[bufsize];           /* General workspace, contents not guaranteed */
char VUIDNAME[maxvarsize];
char VGIDNAME[maxvarsize];
char SERVER[maxvarsize];

char VINPUTFILE[bufsize];
char VCURRENTFILE[bufsize];
char VLOGFILE[bufsize];
char VSETUIDLOG[bufsize];
char VARCH[maxvarsize];
char VREPOSITORY[bufsize];

char VEDITABORT[maxlinksize];
FILE *VLOGFP;


char    CURRENTITEM[bufsize];
char      GROUPBUFF[bufsize];
char     ACTIONBUFF[bufsize];
char    CURRENTPATH[bufsize];
char      CLASSBUFF[bufsize];
char ALLCLASSBUFFER[bufsize];
char       LINKFROM[bufsize];
char         LINKTO[bufsize];
char          ERROR[bufsize];
char      MOUNTFROM[bufsize];
char      MOUNTONTO[bufsize];
char    DESTINATION[bufsize];
char    IMAGEACTION[bufsize];

char *HASH[hashtablesize];

/*******************************************************************/
/* VARIOUS/DEFAULTS                                                */
/*******************************************************************/

struct Item VDEFAULTBINSERVER =      /* see GetNameInfo(), main.c */
   {
   NULL,
   NULL
   };

struct utsname VSYSNAME;                           /* For uname (2) */
time_t TICKSPERDAY;

mode_t PLUSMASK;
mode_t MINUSMASK;

mode_t DEFAULTMODE = (mode_t) 0755;
mode_t DEFAULTSYSTEMMODE = (mode_t) 0644;

uid_t VUID;
gid_t VGID;

int VRECURSE;
int VAGE;

enum classes VSYSTEMHARDCLASS;

/*******************************************************************/
/* Data structures - root pointers                                 */
/*******************************************************************/
struct Item *VMOUNTLIST = NULL;
struct Item *VEXCLUDECOPY = NULL;
struct Item *VEXCLUDELINK = NULL;
struct Item *VCOPYLINKS = NULL;
struct Item *VLINKCOPIES = NULL;
struct Item *VEXCLUDEPARSE = NULL;
struct Item *VCPLNPARSE = NULL;
struct Item *VINCLUDEPARSE = NULL;

struct Item *VHEAP = NULL;      /* Points to the base of the attribute heap */
struct Item *VNEGHEAP = NULL;
struct Item *VMOUNTABLES = NULL;         /* Points to the list of mountables */
struct Item *VMOUNTED = NULL;
struct Tidy *VTIDY = NULL;               /* Points to the list of tidy specs */
struct Tidy *VTIDYTOP = NULL;
struct Item *VPROCESSES = NULL;                       /* Points to proc list */
struct Item *VREQUIRED = NULL;              /* List of required file systems */
struct Item *VSCRIPT = NULL;                   /* List of scripts to execute */
struct Mounted *MOUNTED = NULL;             /* Files systems already mounted */
struct MiscMount *VMISCMOUNT = NULL;
struct MiscMount *VMISCMOUNTTOP = NULL;
struct Item *VBINSERVERS = &VDEFAULTBINSERVER;
struct Link *VLINK = NULL;
struct Link *VLINKTOP = NULL;
struct File *VFILE = NULL;
struct File *VFILETOP = NULL;
struct Image *VIMAGE = NULL;
struct Image *VIMAGETOP=NULL;
struct Item *VHOMESERVERS = NULL;
struct Item *VSETUIDLIST = NULL;
struct Disable *VDISABLELIST = NULL;
struct Disable *VDISABLETOP = NULL;
struct File *VMAKEPATH = NULL;
struct File *VMAKEPATHTOP = NULL;
struct Link *VCHLINK = NULL;
struct Link *VCHLINKTOP = NULL;
struct Item *VIGNORE = NULL;
struct Item *VHOMEPATLIST = NULL;
struct Item *VRESOLVE = NULL;
struct Item *VIMPORT=NULL;
struct Item *VACTIONSEQ=NULL;
struct Item *VACCESSLIST=NULL;
struct Item *VADDCLASSES=NULL;
struct Item *VALLADDCLASSES=NULL;
struct Item *VUNMOUNT=NULL;
struct Edit *VEDITLIST=NULL;
struct Edit *VEDITLISTTOP=NULL;
struct Item *VCLASSDEFINE=NULL;
struct Process *VPROCLIST=NULL;
struct Process *VPROCTOP=NULL;


int SENSIBLEFILECOUNT = 2;
int SENSIBLEFSSIZE = 1000;
int EDITFILESIZE = 1000;

/*******************************************************************/
/* CFENGINE special variables                                      */
/*******************************************************************/

char      VFACULTY[maxvarsize];
char       VDOMAIN[maxvarsize];
char       VSYSADM[maxvarsize];
char      VNETMASK[maxvarsize];
char    VBROADCAST[maxvarsize];
char   VMAILSERVER[bufsize];
char     VTIMEZONE[maxvarsize];
char VDEFAULTROUTE[maxvarsize];
char      VNFSTYPE[maxvarsize];
char       VFQNAME[maxvarsize];
char       VUQNAME[maxvarsize];
char       LOGFILE[maxvarsize];
char      LOCKFILE[maxvarsize];
char      LASTFILE[maxvarsize];

/* Parsing flags etc */

enum actions ACTION = none;
enum classes CLASS = empty;
enum fileactions FILEACTION = warnall;

flag ACTION_IS_LINK = false;
flag ACTION_IS_LINKCHILDREN = false;
flag MOUNT_ONTO = false;
flag MOUNT_FROM = false;
flag HAVE_RESTART = false;
flag ACTIONPENDING = false;
flag HOMECOPY=false;

/*********************************************************************/
/* TOOLKIT : actions                                                 */
/*********************************************************************/


char *ACTIONTEXT[] =
   {
   "",
   "Control Defintions:",
   "Groups:",
   "File Imaging:",
   "Resolve:",
   "Processes:",
   "Files:",
   "Tidy:",
   "Home Servers:",
   "Binary Servers:",
   "Mail Server:",
   "Required Filesystems",
   "Reading Mountables",
   "Links:",
   "Import files:",
   "User Shell Commands:",
   "Disable Files:",
   "Make Directory Path:",
   "Ignore File Paths:",
   "Broadcast Mode:",
   "Default Packet Route:",
   "Miscellaneous Mountables:",
   "Edit Simple Text File:",
   "Unmount filesystems:",
   NULL
   };


char *ACTIONID[] =    /* The actions which may be specified as indexed */
   {                    /* macros in the "special" section of the file   */
   "",
   "control",
   "groups",
   "copy",
   "resolve",
   "processes",
   "files",
   "tidy",
   "homeservers",
   "binservers",
   "mailserver",
   "required",
   "mountables",
   "links",
   "import",
   "shellcommands",
   "disable",
   "makepath",
   "ignore",
   "broadcast",
   "defaultroute",
   "miscmounts",
   "editfiles",
   NULL
   };


/*********************************************************************/
/* Toolkit: classes                                                  */
/*********************************************************************/

char *DAYTEXT[] =
   {
   "Monday",
   "Tuesday",
   "Wednesday",
   "Thursday",
   "Friday",
   "Saturday",
   "Sunday"
   };

/*********************************************************************/
/* Toolkit: file/image actions                                       */
/*********************************************************************/

char *FILEACTIONTEXT[] = 
   {
   "warnall",
   "warnplain",
   "warndirs",
   "fixall",
   "fixplain",
   "fixdirs",
   "touch",
   "linkchildren",
   NULL
   };

char *COMMATTRIBUTES[] =
   {
   "recurse",
   "mode",
   "owner",
   "group",
   "age",
   "action",
   "pattern",
   "links",
   "type",
   "destination",
   "force",
   "backup",
   "rotate",
   "size",
   "matches",
   "signal",
   "exclude",
   "copy",
   "symlink",
   "copytype",
   "linktype",
   "include",
   "dirlinks",
   "rmdirs",
   "server",
   NULL
   };

/*********************************************************************/

char *ACTIONSEQTEXT[] =
   {
   "directories",
   "links",
   "simplelinks",
   "childlinks",
   "mailcheck",
   "required",
   "tidy",
   "shellcommands",
   "files",
   "disable",
   "addmounts",
   "editfiles",
   "mountall",
   "unmount",
   "resolve",
   "copy",
   "netconfig",
   "checktimezone",
   "mountinfo",
   "processes",
   "none",
   NULL
   };

/*********************************************************************/
/* Varstring: variable names                                         */
/*********************************************************************/

char *VVNAMES[] =
   {
   "faculty",
   "site",
   "host",
   "fqhost",
   "binserver",
   "sysadm",
   "domain",
   "timezone",
   "netmask",
   "nfstype",
   "sensiblesize",
   "sensiblecount",
   "editfilesize",
   "actionsequence",
   "mountpattern",
   "homepattern",
   "addclasses",
   "access",
   "class",
   "arch",
   "allclasses",
   "excludecopy",
   "excludelink",
   "copylinks",
   "linkcopies",
   "repository",
   "spc",
   "tab",
   "lf",
   "cr",
   "repchar",
   "split",
   "underscoreclasses",
   NULL
   };

/*********************************************************************/
/* Edit action names / flags etc                                     */
/*********************************************************************/

char *VEDITNAMES[] =
   {
   "NoEdit",
   "DeleteLinesStarting",
   "DeleteLinesContaining",
   "DeleteLinesMatching",
   "AppendIfNoSuchLine",
   "PrependIfNoSuchLine",
   "WarnIfNoSuchLine",
   "WarnIfLineMatching",
   "WarnIfNoLineMatching",
   "WarnIfLineStarting",
   "WarnIfLineContaining",
   "WarnIfNoLineStarting",
   "WarnIfNoLineContaining",
   "HashCommentLinesContaining",
   "HashCommentLinesStarting",
   "HashCommentLinesMatching",
   "SlashCommentLinesContaining",
   "SlashCommentLinesStarting",
   "SlashCommentLinesMatching",
   "PercentCommentLinesContaining",
   "PercentCommentLinesStarting",
   "PercentCommentLinesMatching",
   "ResetSearch",
   "SetSearchRegExp",
   "LocateLineMatching",
   "InsertLine",
   "IncrementPointer",
   "ReplaceLineWith",
   "DeleteToLineMatching",
   "HashCommentToLineMatching",
   "PercentCommentToLineMatching",
   "SetScript",
   "RunScript",
   "RunScriptIfNoLineMatching",
   "RunScriptIfLineMatching",
   "AppendIfNoLineMatching",
   "PrependIfNoLineMatching",
   "DeleteNLines",
   "EmptyEntireFilePlease",
   "GotoLastLine",
   "BreakIfLineMatches",
   "BeginGroupIfNoMatch",
   "BeginGroupIfNoLineMatching",
   "BeginGroupIfNoSuchLine",
   "EndGroup",
   "Append",
   "Prepend",
   "SetCommentStart",
   "SetCommentEnd",
   "CommentLinesMatching",
   "CommentLinesStarting",
   "CommentToLineMatching",
   "CommentNLines",
   "UnCommentNLines",
   "ReplaceAll",
   "With",
   "SetLine",
   "FixEndOfLine",
   "AbortAtLineMatching",
   "UnsetAbort",
   "AutomountDirectResources",
   "UnCommentLinesContaining",
   "UnCommentLinesMatching",
   "InsertFile",
   NULL
   };

int CURRENTLINENUMBER = 1;           /* current line number in file */

struct Item *CURRENTLINEPTR = NULL;  /* Ptr to current line */

struct re_pattern_buffer *SEARCHPATTBUFF;
struct re_pattern_buffer *PATTBUFFER;

int EDITGROUPLEVEL=0;
int SEARCHREPLACELEVEL=0;

char *COMMENTSTART;
char *COMMENTEND;

/*********************************************************************/
/* Signal names                                                      */
/*********************************************************************/

char *SIGNALS[] =
   {
   "NOSIG",
   "SIGHUP",        /* hangup */
   "SIGINT",        /* interrupt */
   "SIGQUIT",       /* quit */
   "SIGILL",        /* illegal instruction (not reset when caught) */
   "SIGTRAP",       /* trace trap (not reset when caught) */
   "SIGIOT",        /* IOT instruction */
   "SIGEMT",        /* EMT instruction */
   "SIGFPE",        /* floating point exception */
   "SIGKILL",       /* kill (cannot be caught or ignored) */
   "SIGBUS",        /* bus error */
   "SIGSEGV",       /* segmentation violation */
   "SIGSYS",        /* bad argument to system call */
   "SIGPIPE",       /* write on a pipe with no one to read it */
   "SIGALRM",       /* alarm clock */
   "SIGTERM",       /* software termination signal from kill */
   "SIGURG",        /* urgent condition on IO channel */
   "SIGSTOP",       /* sendable stop signal not from tty */
   "SIGTSTP",       /* stop signal from tty */
   "SIGCONT",       /* continue a stopped process */
   "SIGCHLD",       /* to parent on child stop or exit */
   "SIGTTIN",       /* to readers pgrp upon background tty read */
   "SIGTTOU",       /* like TTIN for output if (tp->t_local&LTOSTOP) */
   "SIGIO",         /* input/output possible signal */
   "SIGXCPU",       /* exceeded CPU time limit */
   "SIGXFSZ",       /* exceeded file size limit */
   "SIGVTALRM",     /* virtual time alarm */
   "SIGPROF",       /* profiling time alarm */
   "SIGWINCH",      /* window changed */
   "SIGLOST",       /* resource lost (eg, record-lock lost) */
   "SIGUSR1",       /* user defined signal 1 */
   "SIGUSR2"
   };



/*********************************************************************/
/* Resource names                                                    */
/*********************************************************************/

char *VRESOURCES[] = /* one for each major variable in class.c */
   {
   "mountcomm",
   "unmountcomm",
   "ethernet",
   "mountopts",
   "unused",
   "fstab",
   "maildir",
   "netstat",
   "pscomm",
   "psopts",
   NULL
   };


/* GNU STUFF FOR LATER #include "getopt.h" */
 
#include "../pub/getopt.h"
 
struct option OPTIONS[] =
   {
   { "help",no_argument,0,'h' },
   { "debug",optional_argument,0,'d' }, 
   { "verbose",no_argument,0,'v' },
   { "traverse-links",no_argument,0,'l' },
   { "recon",no_argument,0,'n' },
   { "dry-run",no_argument,0,'n'},
   { "just-print",no_argument,0,'n'},
   { "no-ifconfig",no_argument,0,'i' },
   { "file",required_argument,0,'f' },
   { "parse-only",no_argument,0,'p' },
   { "no-mount",no_argument,0,'m' },
   { "no-check-files",no_argument,0,'c' },
   { "no-check-mounts",no_argument,0,'C' },
   { "no-tidy",no_argument,0,'t' },
   { "no-commands",no_argument,0,'s' },
   { "sysadm",no_argument,0,'a' },
   { "version",no_argument,0,'V' },
   { "define",required_argument,0,'D' },
   { "negate",required_argument,0,'N' },
   { "undefine",required_argument,0,'N' },
   { "delete-stale-links",no_argument,0,'L' },
   { "no-warn",no_argument,0,'w' },
   { "silent",no_argument,0,'S' },
   { "quiet",no_argument,0,'w' },
   { "no-preconf",no_argument,0,'x' },
   { "no-links",no_argument,0,'X'},
   { "no-edits",no_argument,0,'e'},
   { "enforce-links",no_argument,0,'E'},
   { "no-copy",no_argument,0,'k'},
   { "use-env",no_argument,0,'u'},
   { "no-processes",no_argument,0,'P'},
   { "underscore-classes",no_argument,0,'U'},
   { "no-hard-classes",no_argument,0,'H'},
   { NULL,0,0,0 }
   };



/* EOF */

