/* cfengine for GNU
 
        Copyright (C) 1995
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
   Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
 

/*******************************************************************/
/*                                                                 */
/*  HEADER for cfengine                                            */
/*                                                                 */
/*******************************************************************/

#include "conf.h"

#include <stdio.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include <string.h>
#include <ctype.h>

#ifdef HAVE_UNAME
#include <sys/utsname.h>
#else
#define _SYS_NMLN       257

struct utsname
   {
   char    sysname[_SYS_NMLN];
   char    nodename[_SYS_NMLN];
   char    release[_SYS_NMLN];
   char    version[_SYS_NMLN];
   char    machine[_SYS_NMLN];
   };

#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#ifdef HAVE_MOUNT_H
#include <sys/mount.h>
#endif

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(s) ((unsigned)(s) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(s) (((s) & 255) == 0)
#endif
#ifndef WIFSIGNALED
# define WIFSIGNALED(s) ((s) & 0)  /* Can't use for BSD */
#endif
#ifndef WTERMSIG
#define WTERMSIG(s) ((s) & 0)
#endif


#include <errno.h>

#ifdef HAVE_DIRENT_H
# include <dirent.h>
#else
# define dirent direct
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#   include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

#include <signal.h>

extern int errno;

/* Do this for ease of configuration from the Makefile */

#ifdef HPuUX
#define HPUX
#endif

#ifdef SunOS
#define SUN4
#endif

/* end of patch */

#ifdef AIX
#ifndef ps2
#include <sys/statfs.h>
#endif
#endif

#ifdef SOLARIS
#include <sys/statvfs.h>
#undef nfstype
#endif

#ifndef HAVE_BCOPY
#define bcopy(fr,to,n)  memcpy(to,fr,n)  /* Eliminate ucblib */
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n)     memset ((s), 0, (n))
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <fcntl.h>

#ifdef HAVE_VFS_H
# include <sys/vfs.h>
#endif

#ifdef HPUX
# include <sys/dirent.h>
#endif

#ifdef HAVE_UTIME_H
# include <utime.h>      /* use utime not utimes for portability */
#elif TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
#elif HAVE_SYS_TIME_H
#  include <sys/time.h>
#elif ! defined(AOS)
#  include <time.h>
#endif

#ifdef HAVE_TIME_H
# include <time.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#include <pwd.h>
#include <grp.h>

#ifdef HAVE_SYS_SOCKIO_H
# include <sys/sockio.h>
#endif

#ifdef INET
# include <sys/socket.h>
# include <sys/ioctl.h>
# include <net/if.h>
# include <netinet/in.h>
# ifndef AOS
#  include <arpa/inet.h>
# endif
# include <netdb.h>
# ifndef LINUX
# include <sys/protosw.h>
# include <net/route.h>
# endif
#endif

#ifdef LINUX
# include <linux/route.h>
# include <linux/in.h>
#endif

#include "../pub/regex.h"

/*******************************************************************/
/* Various defines                                                 */
/*******************************************************************/

#define true  1
#define false 0
#define bufsize 2048
#define buffer_margin 10
#define maxvarsize 128
#define maxlinksize 256
#define maxlinklevel 4
#define maxargs 30
#define hashtablesize 119   /* prime number */
#define macroalphabet 60    /* a-z, A-Z plus a bit */
#define maxshellargs 11
#define samemode 0
#define sameowner -1
#define extra_space 8      /* pads items during AppendItem for eol handling in editfiles */


   /* Class array limits */

#define clssattr 49         /* increase this for each new class added */
                            /* It defines the array size for class data */
#define attr 3              /* Only used in CLASSATTRUBUTES[][] defn */

   /* end class array limits */

#ifdef DEBIAN
# define LOCKFILEDIR "/var/run"
# define LOGFILEDIR "/var/log"
#else
# define LOCKFILEDIR "/etc"
# define LOGFILEDIR  "/etc"
#endif

#define CFINPUTSVAR "CFINPUTS"          /* default name for file path var */
#define CFALLCLASSESVAR "CFALLCLASSES"  /* default name for CFALLCLASSES env */
#define INFINITERECURSE -99             /* code used to signify inf in recursion */
#define CF_TRUNCATE -1
#define CF_EMPTYFILE -2
#define CF_USELOGFILE true              /* synonyms for tidy.c */
#define CF_NOLOGFILE  false
#define CF_SAVED ".cfsaved"

#define Verbose if (VERBOSE || DEBUG || D2) printf
#define EditVerbose  if (EDITVERBOSE || DEBUG) printf
#define Debug2  if (DEBUG || D2) printf
#define Debug1  if (DEBUG || D1) printf
#define Debug   if (DEBUG || D1 || D2) printf
#define DebugVoid if (false) printf
#define Silent if (! SILENT || VERBOSE || DEBUG || D2) printf

/* GNU REGEX */

#define BYTEWIDTH 8

/*******************************************************************/
/* These are to reuse some of the GLOBAL buffers used in parsing   */
/*******************************************************************/

#define EXPR       LINKFROM     /* buff */
#define RESTART    LINKTO       /* buff */
#define PROSIGNAL  ROTATE       /* short*/
#define PROACTION  LINKTYPE     /* char starts as 's' */
#define PROCOMP    COPYTYPE     /* char */

/*******************************************************************/

enum actions
   {
   none,
   control,
   groups,
   image,
   resolve,
   processes,
   files,
   tidy,
   homeservers,
   binservers,
   mailserver,
   required,
   mountables,
   links,
   import,
   shellcommands,
   disable,
   makepath,
   ignore,
   broadcast,
   defaultroute,
   misc_mounts,
   editfiles,
   unmounta
   };

/*******************************************************************/

enum classes
   {
   empty,
   soft,
   faculty,
   site,
   domain,
   sysadm,
   netmask,
   mountpath,
   homepat,
   addclasses,
   timezon,
   sensiblesize,
   sensiblecount,
   editfilesize,
   actionsequence,
   accesss,
   nfstypec,
   excludecopy,
   excludelink,
   copylinks,
   linkcopies,
   repository,
   reposchar,
   listseparator,
   underscoreclasses,
   sun4,
   ultrx,
   hp10,
   hp,
   aix,
   linuxx,
   solaris,
   osf,
   sun3,
   irix4,
   irix,
   irix64,
   freebsd,
   solarisx86,
   bsd4_3,
   newsos,
   netbsd,
   aos,
   bsd_i,
   nextstep,
   unused1,
   unused2,
   unused3
   };


/*******************************************************************/

enum fileactions
   {
   warnall,
   warnplain,
   warndirs,
   fixall,
   fixplain,
   fixdirs,
   touch,
   linkchildren
   };

/*******************************************************************/

enum fileattr
   {
   cfrecurse,
   cfmode,
   cfowner,
   cfgroup,
   cfage,
   cfaction,
   cfpattern,
   cflinks,
   cftype,
   cfdest,
   cfforce,
   cfbackup,
   cfrotate,
   cfsize,
   cfmatches,
   cfsignal,
   cfexclude,
   cfcopy,
   cfsymlink,
   cfcptype,
   cflntype,
   cfinclude,
   cfdirlinks,
   cfrmdirs,
   cfserver,
   cfbad
   };


/*******************************************************************/

enum itemtypes
   {
   simple,
   netgroup,
   classscript,
   deletion,
   groupdeletion
   };


/*********************************************************************/

enum vnames 
   {
   fac,
   sit,
   host,
   fqhost,
   binserv,
   sysad,
   dom,
   timez,
   netmsk,
   nfstp,
   ssize,
   scount,
   esize,
   actseq,
   mpat,
   hpat,
   adclass,
   acess,
   clss,
   archt,
   allclass,
   excludecp,
   excludeln,
   cplinks,
   lncopies,
   repos,
   cfspc,
   cftab,
   cflf,
   cfcr,
   repchar,
   listsep,
   underscore
   };

/*******************************************************************/

enum resc
   {
   rmountcom,
   runmountcom,
   rethernet,
   rmountopts,
   runused,
   rfstab,
   rmaildir,
   rnetstat,
   rpscomm,
   rpsopts
   };

/*******************************************************************/

enum aseq
   {
   mkpaths,
   lnks,
   simplelnks,
   childlnks,
   chkmail,
   requir,
   tidyf,
   shellcom,
   chkfiles,
   disabl,
   mountresc,
   edfil,
   mountall,
   umnt,
   resolv,
   imag,
   netconfig,
   tzone,
   mountinfo,
   procs,
   non
   };

enum aseq EvaluateAction();

/*******************************************************************/

enum editnames
   {
   NoEdit,
   DeleteLinesStarting,
   DeleteLinesContaining,
   DeleteLinesMatching,
   AppendIfNoSuchLine,
   PrependIfNoSuchLine,
   WarnIfNoSuchLine,
   WarnIfLineMatching,
   WarnIfNoLineMatching,
   WarnIfLineStarting,
   WarnIfLineContaining,
   WarnIfNoLineStarting,
   WarnIfNoLineContaining,
   HashCommentLinesContaining,
   HashCommentLinesStarting,
   HashCommentLinesMatching,
   SlashCommentLinesContaining,
   SlashCommentLinesStarting,
   SlashCommentLinesMatching,
   PercentCommentLinesContaining,
   PercentCommentLinesStarting,
   PercentCommentLinesMatching,
   ResetSearch,
   SetSearchRegExp,
   LocateLineMatching,
   InsertLine,
   IncrementPointer,
   ReplaceLineWith,
   DeleteToLineMatching,
   HashCommentToLineMatching,
   PercentCommentToLineMatching,
   SetScript,
   RunScript,
   RunScriptIfNoLineMatching,
   RunScriptIfLineMatching,
   AppendIfNoLineMatching,
   PrependIfNoLineMatching,
   DeleteNLines,
   EmptyEntireFilePlease,
   GotoLastLine,
   BreakIfLineMatches,
   BeginGroupIfNoMatch,
   BeginGroupIfNoLineMatching,
   BeginGroupIfNoSuchLine,
   EndGroup,
   Append,
   Prepend,
   SetCommentStart,
   SetCommentEnd,
   CommentLinesMatching,
   CommentLinesStarting,
   CommentToLineMatching,
   CommentNLines,
   UnCommentNLines,
   ReplaceAll,
   With,
   SetLine,
   FixEndOfLine,
   AbortAtLineMatching,
   UnsetAbort,
   AutoMountDirectResources,
   UnCommentLinesContaining,
   UnCommentLinesMatching,
   InsertFile,
   };

enum editnames EditActionsToCode();

enum RegExpTypes
   {
   posix,
   gnu,
   bsd
   };

/*******************************************************************/

enum SignalNames
   {
   cfnosignal,
   cfhup,
   cfint,
   cfquit,
   cfill,
   cftrap,
   cfiot,
   cfemt,
   cffpr,
   cfkill,
   cfbus,
   cfsegv,
   cfsys,
   cfpipe,
   cfalrm,
   cfterm,
   cfurg,
   cfstop,
   cftstp,
   cfcont,
   cfchld,
   cfgttin,
   cfgttou,
   cfio,
   cfxcpu,
   cfxfsz,
   cfvtalrm,
   cfprof,
   cfwinch,
   cflost,
   cfusr1,
   cfusr2
   };

#define highest_signal 31

/*******************************************************************/

enum actions ActionStringToCode();
enum classes ClassStringToCode();
enum vnames ScanVariable();

typedef char flag;
char ToLower();

/*******************************************************************/

struct Item
   {
   char *name;
   char *classes;
   struct Item *next;
   };

struct Item *LocateNextItemContaining();
struct Item *LocateItemMatchingRegExp();
struct Item *LocateNextItemStarting();
struct Item *LocateNextItemMatching();
struct Item *GotoLastItem();
struct Item *SplitVarstring();

/*******************************************************************/

struct TwoDimList
   {
   short is2d;                  /* true if list > 1 */
   short hasslash;              /* true if string began with / */
   short rounds;
   struct Item *ilist;          /* Each node contains a list */
   struct Item *current;        /* A static working pointer */
   struct TwoDimList *next;
   };

/*******************************************************************/

struct Process
   {
   char           *expr;          /* search regex */
   char           *restart;       /* shell comm to be done after */
   short          matches;
   char           comp;
   short          signal;
   char           action;
   char           *classes;
   struct Process *next;
   };

/*******************************************************************/

struct Mountables
   {
   char *filesystem;
   struct Mountables *next;
   };

/*******************************************************************/

struct Tidy
   {
   int                recurse;
   char               *path;
   struct TidyPattern *tidylist;
   struct Tidy        *next;   
   };

   /**** SUB CLASS *********************************************/

struct TidyPattern
   {
   int                recurse;
   short              age;
   short              size;              /* in kB */
   char               *pattern;
   char               *classes;
   char               travlinks;
   char               dirlinks;          /* k=keep, t=tidy */
   char               rmdirs;            /* t=true, f=false */
   char               searchtype;        /* a, m, c time */
   struct TidyPattern *next;
   };


/*******************************************************************/

struct Mounted
   {
   char *name;
   char *on;
   char *options;
   char *type;
   };

/*******************************************************************/

struct MiscMount
   {
   char *from;
   char *onto;
   char *options;
   char *classes;
   struct MiscMount *next;
   };

/*******************************************************************/

struct File
   {
   char   *path;
   enum   fileactions action;
   mode_t plus;
   mode_t minus;
   int    recurse;
   char   travlinks;
   char   *classes;
   struct UidList *uid;
   struct GidList *gid;
   struct File *next;
   };

/*******************************************************************/

struct Disable
   {
   char *name;
   char *classes;
   char *type;
   short rotate;
   struct Disable *next;
   };

/*******************************************************************/

struct Image
   {
   char   *path;
   char   *destination;
   char   *server;
   mode_t plus;
   mode_t minus;
   struct UidList *uid;
   struct GidList *gid;
   char   *action;      /* fix / warn */
   char   *classes;
   char   force;        /* true false */
   char   type;         /* checksum, ctime */
   char   linktype;     /* if file is linked instead of copied */
   short  backup;
   int    recurse;
   struct Item *exclusions;
   struct Item *inclusions;
   struct Item *symlink;
   struct Image *next;
   };

/*******************************************************************/

struct UidList
   {
   int uid;
   struct UidList *next;
   };

struct UidList *MakeUidList();

/*******************************************************************/

struct GidList
   {
   int gid;
   struct GidList *next;
   };

struct GidList *MakeGidList();

/*******************************************************************/

struct Link
   {
   char   *from;
   char   *to;
   char   *classes;
   short  force;
   short  silent;
   char   type;
   char   copytype;
   int    recurse;
   struct Item *exclusions;
   struct Item *inclusions;
   struct Item *copy;
   struct Link *next;
   };

/*******************************************************************/

struct Edit
   {
   char *fname;
   struct Edlist *actions;
   struct Edit *next;
   };

   /**** SUB-CLASS ********************************************/

struct Edlist
   {
   enum editnames code;
   char *data;
   struct Edlist *next;
   char *classes;
   };

/*******************************************************************/
/* Ultrix/BSD don't have all these from sys/stat.h                 */
/*******************************************************************/

# ifndef S_IFBLK
#  define S_IFBLK 0060000
# endif
# ifndef S_IFCHR
#  define S_IFCHR 0020000
# endif
# ifndef S_IFDIR
#  define S_IFDIR 0040000
# endif
# ifndef S_IFIFO
#  define S_IFIFO 0010000
# endif
# ifndef S_IFREG
#  define S_IFREG 0100000
# endif
# ifndef S_IFLNK
#  define S_IFLNK 0120000
# endif
# ifndef S_IFSOCK
#  define S_IFSOCK 0140000
# endif
# ifndef S_IFMT
#  define S_IFMT  00170000
# endif


#ifndef S_ISREG
# define S_ISREG(m)      (((m) & S_IFMT) == S_IFREG)
#endif
#ifndef S_ISDIR
# define S_ISDIR(m)      (((m) & S_IFMT) == S_IFDIR)
#endif
#ifndef S_ISLNK
# define S_ISLNK(m)      (((m)&S_IFMT) == S_IFLNK)
#endif
#ifndef S_ISFIFO
# define S_ISFIFO(m)     (((m) & S_IFMT) == S_IFIFO)
#endif
#ifndef S_ISCHR
# define S_ISCHR(m)      (((m) & S_IFMT) == S_IFCHR)
#endif
#ifndef S_ISBLK
# define S_ISBLK(m)      (((m) & S_IFMT) == S_IFBLK)
#endif
#ifndef S_ISSOCK
# define S_ISSOCK(m)     (((m) & S_IFMT) == S_IFSOCK)
#endif

#ifndef S_IRUSR
#define S_IRWXU 00700
#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100
 
#define S_IRWXG 00070
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010
 
#define S_IRWXO 00007
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001
#endif

/********************************************************************/
/* Misc. prototypes                                                 */
/********************************************************************/

char *GetMacroValue();
char *Get2DListEnt();


