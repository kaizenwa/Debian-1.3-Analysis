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
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 


/*******************************************************************/
/*                                                                 */
/*  GLOBAL class default variables for cfengine                    */
/*  These variables are what needs to be modified if you add or    */
/*  modify class definitions...                                    */
/*                                                                 */
/*******************************************************************/

#include "cf.defs.h"

/*********************************************************************/

                      /* See also "enum classes" in cf.defs.h        */
char *CLASSTEXT[] =   /* If you change here change enum classes too! */
   {
   "",
   "<soft>",
   "faculty",
   "site",
   "domain",
   "sysadm",
   "netmask",
   "mountpath",
   "homepat",
   "addclasses",
   "timezone",
   "sensiblesize",
   "sensiblecount",
   "editfilesize",
   "actionsequence",               /* All classes must use LOWER CASE */
   "access",                       /* due to ClassStringToCode()      */
   "nfstype",
   "excludecopy",
   "excludelink",
   "copylinks",
   "linkcopies",
   "repository",
   "repchar",
   "split",
   "underscoreclasses",
   "sun4",
   "ultrix",
   "hpux10",
   "hpux",
   "aix",
   "linux",
   "solaris",
   "osf",
   "sun3",
   "irix4",
   "irix",
   "irix64",
   "freebsd",
   "solarisx86",
   "bsd4_3",
   "newsos",
   "netbsd",
   "aos",
   "bsdos",
   "nextstep",
   "unused1",
   "unused2",
   "unused3",
   NULL
   };

/*********************************************************************/

char *CLASSATTRIBUTES[clssattr][attr] = /* Used to match the softclass */
   {                            /* from uname. Only as many characters */
   "-","-","-",                 /* as appear here are matched. The     */
   "-","-","-",                 /* fields are sysname and machine which*/
   "-","-","-",                 /* actually gives the architecture!    */
   "-","-","-",                 /* * and ? are wildcards - is blank    */
   "-","-","-",
   "-","-","-",
   "-","-","-",                    /* remember to change cf.defs.h !!  */
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",
   "-","-","-",   
   "sunos","*","4*",           /* sun 4  */
   "ultrix","risc","4*",       /* ultrix */
   "hp-ux","9000*","?.1?.*",   /* hpux10 */
   "hp-ux","9000*","*",        /* hpux */
   "aix","*","?",              /* aix */
   "linux","*","*",            /* linux */
   "sunos","sun4?","5*",       /* solaris */
   "osf1","alpha","*",         /* osf1 */
   "sunos","sun3","4*",        /* sun3 */
   "irix4","ip*","4*",         /* irix4 */
   "irix", "ip*","*",          /* irix */
   "irix64","ip*","*",         /* irix64 */
   "freebsd","i?86","*",       /* freebsd */
   "sunos","i86pc","5*",       /* solarisx86 */
   "bsd","*","*",              /* bsd 4.3 */
   "newsos","*","*",           /* newsos4 */
   "netbsd","*","*",           /* NetBSD */
   "aos","*","*",              /* AOS */
   "bsd/os","*","*",             /* BSDI */
   "nextstep","*","*",         /* nextstep */
   "unused1","blah","blah",
   "unused2","blah","blah",
   "unused3","blah","blah",
   NULL,NULL,NULL
   };

/*********************************************************************/

char *VPSCOMM[clssattr] =
   {
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",   
   "/bin/ps",       /* sun 4  */
   "/bin/ps",       /* ultrix */
   "/bin/ps",       /* hpux10 */
   "/bin/ps",       /* hpux */
   "/bin/ps",       /* aix */
   "/bin/ps",       /* linux */
   "/bin/ps",       /* solaris */
   "/bin/ps",       /* osf1 */
   "/bin/ps",       /* sun3 */
   "/bin/ps",       /* irix4 */
   "/bin/ps",       /* irix */
   "/bin/ps",       /* irix64 */
   "/bin/ps",       /* freebsd */
   "/bin/ps",       /* solarisx86 */
   "/bin/ps",       /* bsd 4.3 */
   "/bin/ps",       /* newos4 */
   "/bin/ps",       /* netbsd */
   "/bin/ps",       /* AOS */
   "/bin/ps",       /* BSDI */
   "/bin/ps",       /* nextstep */
   "/bin/ps",
   "/bin/ps",
   "/bin/ps",
   NULL
   };

/*********************************************************************/

char *VPSOPTS[clssattr] =
   {
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-ax",    /* sun4 */
   "-ax",    /* ultrix */
   "-ef",    /* hp10 */
   "-ef",    /* hpux */
   "-ef",    /* aix */
   "-ax",    /* linux */
   "-ef",    /* solaris */
   "-ax",    /* osf1 */
   "-ax",    /* sun3 */
   "-ef",    /* irix4 */
   "-ef",    /* irix */
   "-ef",    /* irix64 */
   "-ax",    /* freebsd */
   "-ef",    /* solarisx86 */
   "-ax",    /* bsd 4.3 */
   "-ax",    /* newsos4 */
   "-ax",    /* netbsd */
   "-ax",    /* AOS */
   "-ax",    /* BSDI */
   "-ax",    /* nextstep */
   "-",
   "-",
   "-",
   NULL
   };

/*********************************************************************/

char *VMOUNTCOMM[clssattr] =
   {
   "",                                              /* see cf.defs.h */
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "/etc/mount -va 2<&1",      /* sun4 */
   "/etc/mount -va 2<&1",      /* ultrix */
   "/sbin/mount -va 2<&1",     /* hpux10 */
   "/etc/mount -va 2<&1",      /* hpux */
   "/etc/mount -t nfs 2<&1",   /* aix */
   "/bin/mount -va 2<&1",      /* linux */
   "/usr/sbin/mount -a 2>&1",  /* solaris */
   "/usr/sbin/mount -va 2<&1", /* osf1 */
   "/etc/mount -va 2<&1",      /* sun3 */
   "/sbin/mount -va 2<&1",     /* irix4 */
   "/sbin/mount -va 2<&1",     /* irix */
   "/sbin/mount -va 2<&1",     /* irix64 */
   "/sbin/mount -va 2<&1" ,    /* freebsd */
   "/usr/sbin/mount -a 2>&1",  /* solarisx86 */
   "/etc/mount -a 2>&1",       /* bsd 4.3 */
   "/etc/mount -a 2>&1",       /* newsos4 */
   "/sbin/mount -a 2>&1",      /* netbsd */
   "/etc/mount -a 2>&1",       /* AOS */
   "/sbin/mount -a 2>&1",      /* BSDI */
   "/usr/etc/mount -a 2>&1",   /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };

/*********************************************************************/

char *VUNMOUNTCOMM[clssattr] =
   {
   "",                                              /* see cf.defs.h */
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "/etc/umount",      /* sun4 */
   "/etc/umount",      /* ultrix */
   "/sbin/umount",     /* hpux10 */
   "/etc/umount",      /* hpux */
   "/etc/umount",      /* aix */
   "/sbin/umount",     /* linux */
   "/etc/umount",      /* solaris */
   "/usr/sbin/umount", /* osf1 */
   "/etc/umount",      /* sun3 */
   "/sbin/umount",     /* irix4 */
   "/sbin/umount",     /* irix */
   "/sbin/umount",     /* irix64 */
   "/sbin/umount",     /* freebsd */
   "/etc/umount",      /* solarisx86 */
   "/etc/umount",      /* bsd4.3 */
   "/etc/umount",      /* newsos4 */
   "/sbin/umount",     /* netbsd */
   "/etc/umount",      /* AOS */
   "/sbin/umount",     /* BSDI */
   "/usr/etc/umount",  /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };



/*********************************************************************/

char *VMOUNTOPTS[clssattr] =
   {
   "",                                              /* see cf.defs.h */
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "bg,hard,intr",    /* sun4 */
   "bg,hard,intr",    /* ultrix */
   "bg,hard,intr",    /* hpux10 */
   "bg,hard,intr",    /* hpux */
   "bg,hard,intr",    /* aix */
   "defaults",        /* linux */
   "bg,hard,intr",    /* solaris */
   "bg,hard,intr",    /* osf1 */
   "bg,hard,intr",    /* sun3 */
   "bg,hard,intr",    /* irix4 */
   "bg,hard,intr",    /* irix */
   "bg,hard,intr",    /* irix64 */
   "bg,intr",         /* freebsd */
   "bg,hard,intr",    /* solarisx86 */
   "bg,hard,intr",    /* bsd4.3 */
   "bg,hard,intr",    /* newsos4 */
   "-i,-b",           /* netbsd */
   "bg,hard,intr",    /* AOS */
   "bg,intr",         /* BSDI */
   "bg,hard,intr",    /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };

/*********************************************************************/

char *VIFDEV[clssattr] =
   {
   "-",
   "-",                                              /* see cf.defs.h */
   "-",
   "-",
   "-",
   "-",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "le0",    /* sun4 */
   "ln0",    /* ultrix */
   "lan0",   /* hpux10 */
   "lan0",   /* hpux */
   "en0",    /* aix */
   "eth0",   /* linux */ 
   "le0",    /* solaris */
   "ln0",    /* osf1 */
   "le0",    /* sun3 */
   "ec0",    /* irix4 */
   "ec0",    /* irix */
   "et0",    /* irix64 */
   "ep0",    /* freebsd */
   "dnet0",  /* solarisx86 */
   "le0",    /* bsd4.3 */
   "en0",    /* newsos4 */
   "le0",    /* netbsd */
   "un0",    /* AOS */
   "le0",    /* BSDI */
   "en0",    /* nextstep -default */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };


/*********************************************************************/

char *VRESOLVCONF[clssattr] =
   {
   "-",
   "-",                                              /* see cf.defs.h */
   "-",
   "-",
   "-",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "/etc/resolv.conf",     /* sun4 */
   "/etc/resolv.conf",     /* ultrix */
   "/etc/resolv.conf",     /* hpux10 */
   "/etc/resolv.conf",     /* hpux */
   "/etc/resolv.conf",     /* aix */
   "/etc/resolv.conf",     /* linux */   
   "/etc/resolv.conf",     /* solaris */
   "/etc/resolv.conf",     /* osf1 */
   "/etc/resolv.conf",     /* sun3 */
   "/usr/etc/resolv.conf", /* irix4 */
   "/etc/resolv.conf",     /* irix */
   "/etc/resolv.conf",     /* irix64 */
   "/etc/resolv.conf",     /* freebsd */
   "/etc/resolv.conf",     /* solarisx86 */
   "/etc/resolv.conf",     /* bsd4.3 */
   "/etc/resolv.conf",     /* newsos4 */
   "/etc/resolv.conf",     /* netbsd */
   "/etc/resolv.conf",     /* AOS */
   "/etc/resolv.conf",     /* BSDI */
   "/etc/resolv.conf",     /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };



/*********************************************************************/

char *VFSTAB[clssattr] =
   {
   "-",
   "-",                                              /* see cf.defs.h */
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "/etc/fstab",       /* sun4 */
   "/etc/fstab",       /* ultrix */
   "/etc/fstab",       /* hpux10 */
   "/etc/checklist",   /* hpux */
   "/etc/filesystems", /* aix */
   "/etc/fstab",       /* linux */
   "/etc/vfstab",      /* solaris */
   "/etc/fstab",       /* osf1 */
   "/etc/fstab",       /* sun3 */
   "/etc/fstab",       /* irix4 */
   "/etc/fstab",       /* irix */
   "/etc/fstab",       /* irix64 */
   "/etc/fstab",       /* freebsd */
   "/etc/vfstab",      /* solarisx86 */
   "/etc/fstab",       /* bsd4.3 */
   "/etc/fstab",       /* newsos4 */
   "/etc/fstab",       /* netbsd */
   "/etc/fstab",       /* AOS */
   "/etc/fstab",       /* BSDI */
   "/etc/fstab",       /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };

/*********************************************************************/

char *VMAILDIR[clssattr] =
   {
   "-",
   "-",                                              /* see cf.defs.h */
   "-",
   "-",
   "-",
   "-",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "/var/spool/mail",    /* sun4 */
   "/usr/spool/mail",    /* ultrix */
   "/var/mail",          /* hpux10 */
   "/usr/mail",          /* hpux */
   "/var/spool/mail",    /* aix */
   "/var/spool/mail",    /* linux */  
   "/var/mail",          /* solaris */
   "/usr/spool/mail",    /* osf1 */
   "/var/spool/mail",    /* sun3 */
   "/usr/mail",          /* irix4 */
   "/usr/mail",          /* irix */
   "/usr/var/mail",      /* irix64 */
   "/var/mail",          /* freebsd */
   "/var/mail",          /* solarisx86 */
   "/usr/spool/mail",    /* bsd4.3 */
   "/usr/spool/mail",    /* newsos4 */
   "/var/mail",          /* netbsd */
   "/usr/spool/mail",    /* AOS */
   "/var/mail",          /* BSDI */
   "/usr/spool/mail",    /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };

/*********************************************************************/

char *VNETSTAT[clssattr] =
   {
   "-",
   "-",                                              /* see cf.defs.h */
   "-",
   "-",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "-",
   "/usr/ucb/netstat -rn",   /* sun4 */
   "/usr/ucb/netstat -rn",   /* ultrix */
   "/usr/bin/netstat -rn",   /* hpux10 */
   "/usr/bin/netstat -rn",   /* hpux */
   "/usr/bin/netstat -rn",   /* aix */
   "/bin/netstat -rn",       /* linux */
   "/usr/ucb/netstat -rn",   /* solaris */
   "/usr/sbin/netstat -rn",  /* osf1 */
   "/usr/ucb/netstat -rn",   /* sun3 */
   "/usr/etc/netstat -rn",   /* irix4 */
   "/usr/etc/netstat -rn",   /* irix */
   "/usr/etc/netstat -rn",   /* irix64 */
   "/usr/bin/netstat -rn",   /* freebsd */
   "/bin/netstat -rn",       /* solarisx86 */
   "/usr/ucb/netstat -rn",   /* bsd4.3 */
   "/usr/ucb/netstat -rn",   /* newsos4 */
   "/usr/bin/netstat -rn",   /* netbsd */
   "/usr/ucb/netstat -rn",   /* AOS */
   "/usr/sbin/netstat -rn",  /* BSDI */
   "/usr/ucb/netstat -rn",   /* nextstep */
   "unused-blah",
   "unused-blah",
   "unused-blah",
   NULL
   };


