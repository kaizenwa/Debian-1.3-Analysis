/*
 * config.h -- customize p3nfsd here, if you really need to.
 */
#define PTY	/* Enable vt100 support parallel with nfsd */

 /* HP-UX 10 */
#ifdef __hpux
#  ifdef HPUX10
#    define DDEV "/dev/tty0p0"
#  else
#    define DDEV "/dev/tty00"
#  endif
#endif
#ifdef linux
#  define DDEV "/dev/ttyS0"
#endif
#ifdef sun
#  if defined(__svr4__)
#    define DDEV "/dev/term/a"
#  else
#    define DDEV "/dev/ttya"
#  endif
#endif
#ifdef _IBMR2
#  define DONT_UPDATE_MTAB /*The mount table is obtained from the kernel (!?) */
#  define DDEV "/dev/tty0"
#endif
#ifdef __sgi
#  define DDEV "/dev/ttyf1"
#endif

#define DUSER "root"
#ifndef DDEV
ERROR: p3nfsd is not yet ported to this system
#endif

#ifndef DDIR
# define DDIR "/psion.stand/mnt"
#endif

#define DSPEED 19200

#if defined(sun) && defined(__svr4__)
#define MTAB_PATH "/etc/mnttab" /*gec*/
#else
#define MTAB_PATH "/etc/mtab"
#endif
#ifdef linux
# define MTAB_TMP  "/etc/mtab~"
#else
# define MTAB_TMP  "/etc/mtab.p3nfsd"
#endif

/* See CHANGES for comment */
#ifdef linux
#define NO_WRITE_SELECT
#endif
