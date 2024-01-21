
/*
 * libdpkg - Debian packaging suite library routines
 * dpkg.h - general header for Debian package handling
 *
 * Copyright (C) 1994,1995 Ian Jackson <iwj10@cus.cam.ac.uk>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with dpkg; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef DPKG_H
#define DPKG_H

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/sysinfo.h>

#define ARCHIVEVERSION     "2.0"
#define SPLITVERSION       "2.1"
#define OLDARCHIVEVERSION  "0.939000"
#define SPLITPARTDEFMAX    (450*1024)
#define MAXFIELDNAME        200
#define MAXCONFFILENAME     1000
#define MAXDIVERTFILENAME   1024
#define MAXCONTROLFILENAME  100
#define BUILDCONTROLDIR    "DEBIAN"
#define EXTRACTCONTROLDIR   BUILDCONTROLDIR
#define DEBEXT             ".deb"
#define OLDDBEXT           "-old"
#define NEWDBEXT           "-new"
#define OLDOLDDEBDIR       ".DEBIAN"
#define OLDDEBDIR          "DEBIAN"
#define REMOVECONFFEXTS    "~", ".bak", "%", \
                           DPKGTEMPEXT, DPKGNEWEXT, DPKGOLDEXT, DPKGDISTEXT

#ifndef ARCHBINFMT
#define ARCHBINFMT
#endif
#define DPKG_VERSION_ARCH  DPKG_VERSION " (" ARCHITECTURE ARCHBINFMT ")"

#define NEWCONFFILEFLAG    "newconffile"
#define NONEXISTENTFLAG    "nonexistent"

#define DPKGTEMPEXT        ".dpkg-tmp"
#define DPKGNEWEXT         ".dpkg-new"
#define DPKGOLDEXT         ".dpkg-old"
#define DPKGDISTEXT        ".dpkg-dist"

#define CONTROLFILE        "control"
#define CONFFILESFILE      "conffiles"
#define PREINSTFILE        "preinst"
#define POSTINSTFILE       "postinst"
#define PRERMFILE          "prerm"
#define POSTRMFILE         "postrm"
#define LISTFILE           "list"

#define ADMINDIR        "/var/lib/dpkg"
#define STATUSFILE      "status"
#define AVAILFILE       "available"
#define LOCKFILE        "lock"
#define CMETHOPTFILE    "cmethopt"
#define METHLOCKFILE    "methlock"
#define DIVERSIONSFILE  "diversions"
#define UPDATESDIR      "updates/"
#define INFODIR         "info/"
#define PARTSDIR        "parts/"
#define CONTROLDIRTMP   "tmp.ci/"
#define IMPORTANTTMP    "tmp.i"
#define REASSEMBLETMP   "reassemble" DEBEXT
#define IMPORTANTMAXLEN  10
#define IMPORTANTFMT    "%04d" /* change => also change lib/database.c:cleanup_updates */
#define MAXUPDATES       50

#define LIBDIR              "/usr/lib/dpkg"
#define LOCALLIBDIR         "/usr/local/lib/dpkg"
#define METHODSDIR          "methods"

#define NOJOBCTRLSTOPENV    "DPKG_NO_TSTP"
#define SHELLENV            "SHELL"
#define DEFAULTSHELL        "sh"

#define IMETHODMAXLEN        50
#define IOPTIONMAXLEN        IMETHODMAXLEN
#define METHODOPTIONSFILE   "names"
#define METHODSETUPSCRIPT   "setup"
#define METHODUPDATESCRIPT  "update"
#define METHODINSTALLSCRIPT "install"
#define OPTIONSDESCPFX      "desc."
#define OPTIONINDEXMAXLEN    5

#define PKGSCRIPTMAXARGS     10
#define MD5HASHLEN           32

#define CONFFOPTCELLS  /* int conffoptcells[2] {* 1= user edited *}              \
                                           [2] {* 1= distributor edited *} = */  \
                                  /* dist not */     /* dist edited */           \
   /* user did not edit */    {     cfo_keep,           cfo_install    },        \
   /* user did edit     */    {     cfo_keep,         cfo_prompt_keep  }

#define ARCHIVE_FILENAME_PATTERN "*.deb"

#define BACKEND      "dpkg-deb"
#define SPLITTER     "dpkg-split"
#define MD5SUM       "md5sum"
#define DSELECT      "dselect"
#define DPKG         "dpkg"

#define TAR          "tar"
#define GZIP         "gzip"
#define CAT          "cat"
#define RM           "rm"
#define FIND         "find"
#define SHELL        "sh"

#define SHELLENVIR   "SHELL"

#define FIND_EXPRSTARTCHARS "-(),!"

#define TARBLKSZ     512

extern const char thisname[]; /* defined separately in each program */
extern const char printforhelp[];

/*** from ehandle.c ***/

void push_error_handler(jmp_buf *jbufp,
                        void (*printerror)(const char *, const char *),
                        const char *contextstring);
void set_error_display(void (*printerror)(const char *, const char *),
                       const char *contextstring);
void print_error_fatal(const char *emsg, const char *contextstring);
void error_unwind(int flagset);
void push_cleanup(void (*f1)(int argc, void **argv), int flagmask1,
                  void (*f2)(int argc, void **argv), int flagmask2,
                  int nargs, ...);
void push_checkpoint(int mask, int value);
void pop_cleanup(int flagset);
enum { ehflag_normaltidy=01, ehflag_bombout=02, ehflag_recursiveerror=04 };

void do_internerr(const char *string, int line, const char *file) NONRETURNING;
#define internerr(s) do_internerr(s,__LINE__,__FILE__)

struct varbuf;
void ohshit(const char *fmt, ...) NONRETURNPRINTFFORMAT(1,2);
void ohshitv(const char *fmt, va_list al) NONRETURNING;
void ohshite(const char *fmt, ...) NONRETURNPRINTFFORMAT(1,2);
void ohshitvb(struct varbuf*) NONRETURNING;
void badusage(const char *fmt, ...) NONRETURNPRINTFFORMAT(1,2);
void werr(const char *what) NONRETURNING;

/*** from mlib.c ***/

void *m_malloc(size_t);
void *m_realloc(void*, size_t);
int m_fork(void);
void m_dup2(int oldfd, int newfd);
void m_pipe(int fds[2]);

void checksubprocerr(int status, const char *description, int sigpipeok);
void waitsubproc(pid_t pid, const char *description, int sigpipeok);

extern volatile int onerr_abort;

/*** from showcright.c ***/

struct cmdinfo;
void showcopyright(const struct cmdinfo*, const char*);

#endif /* DPKG_H */
