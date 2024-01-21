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
/*  extern HEADER for cfengine                                     */
/*                                                                 */
/*******************************************************************/

extern char *tzname[2];
extern char *optarg;
extern int optind;
extern struct option OPTIONS[];

extern char *VPRECONFIG;
extern char *VRCFILE;

extern char VARCH[];
extern char *ACTIONTEXT[]; 
extern char *ACTIONID[];
extern char *CLASSTEXT[];
extern char *DAYTEXT[];
extern char *CLASSATTRIBUTES[clssattr][attr];
extern char *FILEACTIONTEXT[];
extern char *COMMATTRIBUTES[];
extern char VINPUTFILE[];
extern char VCURRENTFILE[];
extern char VLOGFILE[];
extern char VSETUIDLOG[];
extern FILE *VLOGFP;
extern char VEDITABORT[];
extern char LISTSEPARATOR;
extern char REPOSCHAR;

extern struct tm TM1;
extern struct tm TM2;
extern time_t TICKSPERDAY;
extern int ERRORCOUNT;
extern int NUMBEROFEDITS;

extern struct utsname VSYSNAME;

extern int LINENUMBER;
extern mode_t DEFAULTMODE;
extern mode_t DEFAULTSYSTEMMODE;
extern int HAVEUID;
extern char VUIDNAME[maxvarsize];
extern char VGIDNAME[maxvarsize];
extern char SERVER[maxvarsize];
extern uid_t VUID;
extern gid_t VGID;
extern int VRECURSE;
extern int VAGE;


extern short LINKSILENT;
extern char  LINKTYPE;
extern char  AGETYPE;
extern char  COPYTYPE;
extern char  LINKDIRS;

extern char   CURRENTITEM[bufsize];
extern char   CURRENTPATH[bufsize];
extern char     GROUPBUFF[bufsize];
extern char    ACTIONBUFF[bufsize];
extern char     CLASSBUFF[bufsize];
extern char ALLCLASSBUFFER[bufsize];
extern char      LINKFROM[bufsize];
extern char        LINKTO[bufsize];
extern char         ERROR[bufsize];
extern char     MOUNTFROM[bufsize];
extern char     MOUNTONTO[bufsize];
extern char   DESTINATION[bufsize];
extern char   IMAGEACTION[bufsize];

extern char *HASH[hashtablesize];

extern char VBUFF[bufsize];

extern char VFACULTY[maxvarsize];
extern char VDOMAIN[maxvarsize];
extern char VSYSADM[maxvarsize];
extern char VNETMASK[maxvarsize];
extern char VBROADCAST[maxvarsize];
extern char VMAILSERVER[maxvarsize];
extern char VTIMEZONE[maxvarsize];
extern char VDEFAULTROUTE[maxvarsize];
extern char VNFSTYPE[maxvarsize];
extern char VREPOSITORY[bufsize];
extern enum classes VSYSTEMHARDCLASS;
extern char *VVNAMES[];
extern char VFQNAME[];
extern char VUQNAME[];
extern char LOGFILE[];
extern char LOCKFILE[];
extern char LASTFILE[];


extern struct Item *VEXCLUDECOPY;
extern struct Item *VEXCLUDELINK;
extern struct Item *VCOPYLINKS;
extern struct Item *VLINKCOPIES;
extern struct Item *VEXCLUDEPARSE;
extern struct Item *VCPLNPARSE;
extern struct Item *VINCLUDEPARSE;

extern struct Item *VMOUNTLIST;
extern struct Item *VHEAP;      /* Points to the base of the attribute heap */
extern struct Item *VNEGHEAP;
extern struct Item *VMOUNTABLES;         /* Points to the list of mountables */
extern struct Item *VMOUNTED;
extern struct Tidy *VTIDY;               /* Points to the list of tidy specs */
extern struct Item *VREQUIRED;              /* List of required file systems */
extern struct Item *VSCRIPT;                   /* List of scripts to execute */
extern struct Mounted *MOUNTED;             /* Files systems already mounted */
extern struct Item VDEFAULTBINSERVER;
extern struct Item *VBINSERVERS;
extern struct Link *VLINK;
extern struct File *VFILE;
extern struct Item *VHOMESERVERS;
extern struct Item *VSETUIDLIST;
extern struct Disable *VDISABLELIST;
extern struct Disable *VDISABLETOP;
extern struct File *VMAKEPATH;
extern struct File *VMAKEPATHTOP;
extern struct Link *VCHLINK;
extern struct Item *VIGNORE;
extern struct Item *VHOMEPATLIST;
extern struct Item *VRESOLVE;
extern struct MiscMount *VMISCMOUNT;
extern struct MiscMount *VMISCMOUNTTOP;
extern struct Item *VIMPORT;
extern struct Item *VACTIONSEQ;
extern struct Item *VACCESSLIST;
extern struct Item *VADDCLASSES;
extern struct Item *VALLADDCLASSES;
extern struct Edit *VEDITLIST;
extern struct Edit *VEDITLISTTOP;
extern struct Item *VUNMOUNT;
extern struct Item *VCLASSDEFINE;
extern struct Image *VIMAGE;
extern struct Image *VIMAGETOP;
extern struct Process *VPROCLIST;
extern struct Process *VPROCTOP;

/* Associated variables which simplify logic */

extern struct Link *VLINKTOP;
extern struct Link *VCHLINKTOP;
extern struct Tidy *VTIDYTOP;
extern struct File *VFILETOP;

extern char VERSION[];
extern char *COPYRIGHT;

extern short DEBUG;
extern short D1;
extern short D2;

extern short VERBOSE;
extern short TIDYDIRS;
extern short TRAVLINKS;
extern short PTRAVLINKS;
extern short DONTDO;
extern short IFCONF;
extern short PARSEONLY;
extern short GOTMOUNTINFO;
extern short NOMOUNTS;
extern short NOPROCS;
extern short NOFILECHECK;
extern short NOTIDY;
extern short NOSCRIPTS;
extern short PRSYSADM;
extern short MOUNTCHECK;
extern short NOEDITS;
extern short KILLOLDLINKS;
extern short IGNORELOCK;
extern short NOPRECONFIG;
extern short WARNINGS;
extern short MINUSF;
extern short NOLINKS;
extern short ENFORCELINKS;
extern short FORCELINK;
extern short NOCOPY;
extern short SILENT;
extern short EDITVERBOSE;
extern short FORCECOPY;
extern short IMAGEBACKUP;
extern short ROTATE;
extern short TIDYSIZE;
extern short USEENVIRON;
extern short PROMATCHES;
extern short EDABORTMODE;
extern short NOPROCS;
extern short UNDERSCORE_CLASSES;
extern short NOHARDCLASSES;

extern enum actions ACTION;
extern enum classes CLASS;

extern mode_t PLUSMASK;
extern mode_t MINUSMASK;

extern flag  ACTION_IS_LINK;
extern flag  ACTION_IS_LINKCHILDREN;
extern flag  MOUNT_ONTO;
extern flag  MOUNT_FROM;
extern flag  HAVE_RESTART;
extern flag  ACTIONPENDING;
extern flag  HOMECOPY;

extern char *VPSCOMM[];
extern char *VPSOPTS[];
extern char *VMOUNTCOMM[];
extern char *VMOUNTOPTS[];
extern char *VIFDEV[];
extern char *VETCSHELLS[];
extern char *VRESOLVCONF[];
extern char *VHOSTEQUIV[];
extern char *VFSTAB[];
extern char *VMAILDIR[];
extern char *VNETSTAT[];
extern char *ACTIONSEQTEXT[];
extern char *VEDITNAMES[];
extern char *VUNMOUNTCOMM[];
extern char *VRESOURCES[];

extern char *SIGNALS[];

extern char *tzname[2]; /* see man ctime */

extern int SENSIBLEFILECOUNT;
extern int SENSIBLEFSSIZE;
extern int EDITFILESIZE;

extern enum fileactions FILEACTION;

extern int CURRENTLINENUMBER;
extern struct Item *CURRENTLINEPTR;

extern int EDITGROUPLEVEL;
extern int SEARCHREPLACELEVEL;

extern char *COMMENTSTART, *COMMENTEND;

/* GNU REGEXP */

extern struct re_pattern_buffer *SEARCHPATTBUFF;
extern struct re_pattern_buffer *PATTBUFFER;
