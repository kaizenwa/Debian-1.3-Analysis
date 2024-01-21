/*
 * sys.h -- system include files.
 */

/*
 * The const storage-class qualifier causes conflicts between some of
 *  our prototypes and those from system include files on some platforms.
 *  In addition, some RTL code uses casts to const pointers to keep rtt
 *  from printing warnings about possible indirect modifications. However,
 *  these casts do not pass non-ANSI-standard C compilers.
 */
#ifndef AllowConst
#define const  /* make const disapear */
#endif					/* StandardLib */

/*
 * The following code is operating-system dependent [@sys.01].  Include files
 *  that are system-dependent.
 */

#if PORT
#include <signal.h>
   /* probably needs something more */
Deliberate Syntax Error
#endif					/* PORT */

#if AMIGA
#include <signal.h>
#include <fcntl.h>
#include <ios1.h>
#include <string.h>
#include <libraries/dosextens.h>

#if AZTEC_C
#include <ctype.h>
#endif					/* AZTEC_C */
#endif					/* AMIGA */

#if ARM
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#endif					/* ARM */

#if ATARI_ST
#include <fcntl.h>
#include <osbind.h>
#endif					/* ATARI_ST */

#if MACINTOSH
#include <signal.h>
#if LSC
#include <unix.h>
#endif					/* LSC */
#if MPW
#define create xx_create	/* prevent duplicate definition of create() */
#include <stdlib.h>
#include <stdio.h>
#include <Types.h>
#include <Events.h>
#include <Files.h>
#include <FCntl.h>
#include <Files.h>
#include <IOCtl.h>
#include <SANE.h>
#include <OSUtils.h>
#include <Memory.h>
#include <Errors.h>
#include "time.h"
#include <QuickDraw.h>
#include <ToolUtils.h>
#include <CursorCtl.h>
#endif					/* MPW */
#if THINK_C
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#endif
#ifdef MacGraph         /* MacGraph */
#include <console.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <AppleEvents.h>
#include <GestaltEqu.h>
#include <Values.h>
#include <sane.h>
#include <QDOffscreen.h>
#include <Palettes.h>
#endif                  /* MacGraph */
#endif					/* MACINTOSH */

#if MSDOS
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>

#ifdef MSWindows
#define int_PASCAL int PASCAL
#define LRESULT_CALLBACK LRESULT CALLBACK
#if BORLAND_286
#include <time.h>
#include <dir.h>
#endif
#include <windows.h>
#include "../wincap/dibutil.h"
#endif					/* MSWindows */
#include <setjmp.h>
#endif					/* MSDOS */

#if MVS || VM
#ifdef RecordIO
#if SASC
#include <lcio.h>
#endif					/* SASC */
#endif					/* RecordIO */
#if SASC
#include <lcsignal.h>
#else					/* SASC */
#include <signal.h>
#endif					/* SASC */
#endif					/* MVS || VM */

#if OS2
#define INCL_DOS
#define INCL_ERRORS
#define INCL_RESOURCES
#define INCL_DOSMODULEMGR

#ifdef PresentationManager
#define INCL_PM
#endif					/* PresentationManager */

#include <os2.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
/* Pipe support for OS/2 */
#include <stddef.h>
#include <process.h>
#include <fcntl.h>
#include <errno.h>

#if CSET2V2
#include <io.h>
#include <direct.h>
#define IN_SYS_H
#include "../h/local.h"                 /* Include #pragmas */
#undef IN_SYS_H
#endif					/* CSet/2 version 2 */

#endif					/* OS2 */
#ifdef ConsoleWindow
/*
 * pull in variable arg lists
 */
#include <stdarg.h>
#endif					/* ConsoleWindow */

#if UNIX

#ifdef ATT3B
#include <sys/types.h>
#include <sys/stat.h>
#endif					/* ATT3B */

#ifdef XENIX_386

#ifdef StandardLib
#include <unistd.h>
#endif					/* StandardLib */

#endif					/* XENIX_386 */

#ifdef SuppressAlignmentMsg
#include <sys/sysinfo.h>
#include <sys/proc.h>
#endif					/* SuppressAlignmentMsg */

#include <signal.h>

#ifndef XWindows
#include <sys/types.h>
#endif					/* XWindows */
#include <sys/time.h>

#ifdef UtsName
#include <sys/utsname.h>
#endif					/* UtsName */

#ifdef KeyboardFncs
#ifdef HaveTermio

#ifndef Linux
#include <sys/termio.h>
#else					/* Linux */
#include <termio.h>
#endif					/* Linux */

#include <sys/ioctl.h>
#include <sys/errno.h>
#include <sys/signal.h>
#endif					/* HaveTermio */

#ifdef HaveTioc
#include <sys/ioctl.h>
#include <errno.h>
#include <sys/signal.h>
#endif					/* HaveTioc */

#endif					/* KeyboardFncs */

#ifdef LoadFunc
#include <dlfcn.h>
#endif					/* LoadFunc */

#endif					/* UNIX */

#if VMS
#include <signal.h>
#include <types.h>
#include <dvidef>
#include <iodef>
#include <stsdef.h>
#endif					/* VMS */

#ifdef ConsoleWindow
#undef printf
#undef fprintf
#undef fflush
#endif

#include <stdio.h>

#ifdef ConsoleWindow
#define printf Consoleprintf
#define fprintf Consolefprintf
#define fflush Consolefflush
#endif

#ifdef XWindows
/*
 * DECstation X Window #includes check for #ifdef VMS, so if it's 0, undef it.
 * DECwindows #includes check for #ifdef UNIX, so if VMS is 1, undef UNIX.
 */

#if !VMS
#undef VMS
#endif					/* VMS */

#if VMS
#undef UNIX
#include "decw$include:Xlib.h"
#include "decw$include:Xutil.h"
#include "decw$include:Xos.h"
#include "decw$include:Xatom.h"

#ifdef HaveXpmFormat
#include "../xpm/xpm.h"
#endif					/* HaveXpmFormat */

#undef UNIX
#define UNIX 0
#else					/* VMS */

#ifdef HaveXpmFormat
#include "../xpm/xpm.h"
#else					/* HaveXpmFormat */
#include <X11/Xlib.h>
#endif					/* HaveXpmFormat */

#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#ifndef VMS
#define VMS 0
#endif					/* VMS */

#endif					/* VMS */

#endif					/* XWindows */

/*
 * End of operating-system specific code.
 */

#include <ctype.h>
#include <errno.h>
#include <math.h>


/*
 *  The following symbols should be in <stdio.h>; define them if they aren't.
 */
#ifndef SEEK_SET
#define SEEK_SET 0
#endif					/* SEEK_SET */
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif					/* SEEK_CUR */
#ifndef SEEK_END
#define SEEK_END 2
#endif					/* SEEK_END */

/*
 * The following depends on <sys/types.h>, which is included at different
 *  points above depending on XWindows.  It's unneeded but harmless if
 *  not Unix.
 */
#ifdef FD_SET
#define FD_NULL ((fd_set *) 0)
#else                                   /* FD_SET */
#define FD_NULL ((long *) 0)
#endif                                  /* FD_SET */
