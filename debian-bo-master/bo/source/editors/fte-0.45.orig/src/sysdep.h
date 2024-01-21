/*    sysdep.h
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#ifndef __SYSDEP_H
#define __SYSDEP_H

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#if !defined(OS2) && \
    !defined(NT) && \
    !defined(DOSP32) && \
    !defined(LINUX) && \
    !defined(HPUX) && \
    !defined(AIX) && \
    !defined(IRIX) && \
    !defined(SUNOS)
#    error Target not supported.
#endif

#if defined(UNIX)
#    define USE_DIRENT
#endif

#if defined(USE_DIRENT) // also needs fnmatch
#    include <dirent.h>
#endif

#if defined(UNIX)
#    include <unistd.h>
#    include <pwd.h>
#    if defined(HPUX) || defined(AIX)
#        include <fnmatch.h>             // IRIX seems to be missing this ??
#    endif
#    define strnicmp strncasecmp
#    define stricmp strcasecmp
#    define filecmp strcmp
#    define memicmp strncasecmp   // FIX, fails for nulls
#endif

#if defined(OS2)
#    include <malloc.h>
#    include <dos.h>
#    include <io.h>
#    include <process.h>
#    if defined(BCPP) || defined(WATCOM)
#        include <direct.h>
#    endif
#    if defined(BCPP)
#        include <dir.h>
#    endif
#    define filecmp stricmp
#endif

#if defined(DOS) || defined(DOSP32)
#    include <malloc.h>
#    include <dos.h>
#    include <io.h>
#    include <process.h>
#    if defined(BCPP)
#        include <dir.h>
#    endif
#    if defined(WATCOM)
#        include <direct.h>
#    endif
#    define filecmp stricmp
#endif

#if defined(NT)
#    include <malloc.h>
#    include <dos.h>
#    include <io.h>
#    include <process.h>
#    if defined(MSVC)
#        include <direct.h>
#    endif
#    if defined(WATCOM)
#        include <direct.h>
#    endif
#    if defined(BCPP)
#        include <dir.h>
#    endif
#    define filecmp stricmp
#endif

#ifndef MAXPATH
#    define MAXPATH 260
#endif

#ifndef O_BINARY
#    define O_BINARY 0   /* defined on OS/2, no difference on unix */
#endif

#if defined(OS2) || defined(NT)
#if defined(EMX) || defined(WATCOM)
#define FAKE_BEGINTHREAD_NULL NULL,
#else
#define FAKE_BEGINTHREAD_NULL
#endif
#endif

#define PT_UNIXISH   0
#define PT_DOSISH    1

#ifndef S_ISDIR  // NT, DOS, DOSP32
#    ifdef S_IFDIR
#        define S_ISDIR(mode)  ((mode) & S_IFDIR)
#    else
#        define S_ISDIR(mode)  ((mode) & _S_IFDIR)
#    endif
#endif

#if defined(OS2) || defined(NT) || defined(DOSP32) || defined(DOS)
#define PATHTYPE   PT_DOSISH
#else
#define PATHTYPE   PT_UNIXISH
#endif

#endif
