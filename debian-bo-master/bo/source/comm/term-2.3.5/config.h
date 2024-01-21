/*
 *
 * USE_IOCTL use ioctl() to set non-blocked io instead of fcntl()
 *
 * USE_SETSID use setsid() to become process leader
 *
 * USE_TIOCSCTTY use ioctl() to acquire a controlling terminal
 *
 * USE_TERMIOS uses the termios struct to control terminals.
 *   Comment this out if you want to use sgtty instead.
 * 
 * ERR_BLOCK sets the errno that is returned for an operation
 * that blocked.
 *
 * USE_VHANGUP says to run vhangup() on the new ptys.
 *
 * USE_TCATTR uses tc{get/set}attr() instead of ioctl's to set
 * termios.
 *
 * USE_SIGWINCH tells trsh to watch for SIGWINCH. Only define it if
 * your system supports it.
 *
 * USE_WINCHKILL if you sigwinch is broken, and doesn't send the
 * signal when the window size gets changed.
 *
 * USE_HERROR specifies that herror() should be used, not term_herror().
 *
 * USE_CONNBLOCK this specifies that term may block waiting for a
 * a connection.
 *
 * USE_WAITPID use waitpid() instead of wait3()
 *
 * USE_ONEXIT use onexit instead of atexit()
 *
 * USE_NOEOF when a client is closing, don't wait for an EOF in the stream.
 *
 * NO_ATEXIT don't use either
 *
 * NO_VFORK  doesn't have vfork()
 *
 * NO_STRERROR  don't allow term to define it's own strerror.
 *
 * NO_TTYNAME does not have isatty() and/or ttyname()
 *
 * Any combination of the following may be defined for txconn:
 *    X_TCP_SOCKET   - use a tcp style of socket for txconn (default)
 *    X_UNIX_SOCKET  - use a plain unix socket for txconn
 *    X_STREAMS_PIPE - use a streams pipe for txconn
 *
 * To use X11 sockets, you may need to change:
 *    LOCAL_X_DIR    - defaults to "/tmp/.X11-unix"
 *    LOCAL_X_NAME   - defaults to "X"
 */

	/* First we do simple, OS specific rules */

#ifdef linux
#define USE_SIGWINCH
#define USE_SETSID
#define USE_TERMIOS
#define USE_VHANGUP
#define USE_HERROR 
#define ERR_BLOCK  EAGAIN
#endif

#ifdef DGUX
#ifndef SYSV
#define SYSV
#endif
#define USE_IOCTL
#define USE_SETSID
#define USE_TERMIOS
#define USE_TCATTR
#define USE_WINCHKILL
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#define ERR_BLOCK EAGAIN
#endif

#ifdef EPIX
/* this works for the EP/IX Version 2.1.1AC */
#define USE_SIGWINCH
#define USE_VHANGUP
#define ERR_BLOCK  EWOULDBLOCK
#define USE_SETPGRP
#define USE_TIOCNOTTY
#define NO_ATEXIT
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#ifdef BSD
#undef BSD
#endif
#endif
 
#ifdef ISC
#ifndef SYSV
# define SYSV
#endif
#define USE_SETSID
#define USE_TERMIOS
#define USE_TCATTR
#define USE_WINCHKILL
#define USE_WAITPID
#define ERR_BLOCK EAGAIN
#define NO_VFORK
#define NO_UNIX_DOMAIN
#define NO_ATEXIT
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#define _XOPEN_SOURCE
#endif /* ISC */

#ifdef DYNIXPTX
#ifndef SYSV
#  define SYSV
#endif
#define HAS_PSEUDO
#define USE_WAITPID
#define USE_SIGWINCH
#define USE_TERMIOS
#define USE_TCATTR
#define USE_STIME
#define ERR_BLOCK EAGAIN
#define USE_SETPGRP
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#ifndef I_TIME
# define I_TIME
#endif
#ifndef I_TYPES
#define I_TYPES
#endif
#endif

#ifdef SVR4
#ifndef SYSV
# define SYSV
#endif
#define NO_STRERROR
#define USE_WAITPID
#define USE_SIGWINCH
#define USE_SETSID
#define USE_TERMIOS
#define USE_TCATTR
#define ERR_BLOCK  EAGAIN
#endif

#ifdef SCO
#ifndef SYSV
#define SYSV
#endif
#define X_DEFAULT_DISPLAY_NUM 7
#define USE_IOCTL
#define USE_SETSID
#define USE_TERMIOS
#define USE_TCATTR
#define USE_WINCHKILL
#define ERR_BLOCK EAGAIN
#define STREAMS_PIPE
#define X_STREAMS_PIPE
#define NO_UNIX_DOMAIN
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#define NO_VFORK
#endif

#ifdef ultrix
#define USE_IOCTL
#define USE_SIGWINCH
#define USE_TIOCNOTTY
#define USE_VHANGUP
#define USE_TERMIOS
#define USE_HERROR
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#define ERR_BLOCK EWOULDBLOCK
#endif

#ifdef NeXT
#define USE_IOCTL
#define USE_VHANGUP
#define USE_TCATTR
#define USE_SIGWINCH
#define USE_TIOCNOTTY
#define ERR_BLOCK EWOULDBLOCK
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#ifndef I_TYPES
#define I_TYPES
#endif
#endif

#ifdef BSDI
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_TIOCNOTTY
#endif

#ifdef __MACHTEN__
#define USE_VHANGUP
#define USE_HERROR
#define USE_SIGWINCH
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#define ERR_BLOCK  EAGAIN
#endif
 
#if defined(__hpux)
#define X_UNIX_SOCKET
#define X_TCP_SOCKET
#define USE_SIGWINCH
#define USE_TERMIOS
#define USE_TCATTR
#define USE_NOEOF
#define USE_SETSID
#define ERR_BLOCK EAGAIN
#endif

#ifdef _AIX
#define USE_SIGWINCH
#define USE_TERMIOS
#define USE_TCATTR
#define ERR_BLOCK EAGAIN
#define USE_SETPGRP
#define USE_TIOCNOTTY
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#endif

#ifdef sgi
#define	USE_SETSID
#define	USE_VHANGUP
#define	USE_TERMIOS
#define	USE_TCATTR
#define ERR_BLOCK EAGAIN
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#endif

#ifdef __QNX__
#define NO_STRERROR
#define USE_WAITPID
#define USE_HERROR 
#define USE_SIGWINCH
#define USE_TCATTR
#define USE_TERMIOS
#define USE_TTYNAME
#define USE_SPAWN
#define USE_NOEOF
#define ERR_BLOCK      EAGAIN
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#endif

#ifdef titan
#define NO_PTYEXEC
#define USE_VARARGS
#define	USE_SETIDS
#define NO_SIGWINCH
#define NO_ATEXIT
#endif

#ifdef IRIX
#define NO_SIGWINCH
#define NO_VFORK
#define NO_TTYNAME
#endif

#ifdef __FreeBSD__
#define NO_STRERROR
#define USE_TERMIOS
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SETPGRP
#endif

#ifdef __NetBSD__
#define NO_STRERROR
#define USE_TERMIOS
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SETPGRP
#endif

#if defined(___386BSD___) || defined(__386BSD__)
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SETPGRP
#endif

#if defined(__OSF1__)  || defined(__OSF__) || defined(__osf__)
#ifndef SYSV
#define SYSV
#endif
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SETSID
#define USE_TIOCSCTTY
#define USE_SIGWINCH
#define NO_VHANGUP
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#endif

	/* Now the more complicated stuff */

#if defined(__convex__) && !defined(convex)
#define convex
#endif

#ifndef SVR4
#if defined(sun) || defined(convex)
#define USE_TERMIOS
#define USE_TCATTR
#define ERR_BLOCK EAGAIN
#ifndef NO_VHANGUP
#define USE_VHANGUP
#endif
#ifndef NO_SIGWINCH
#define USE_SIGWINCH
#endif
#if defined(sun)
#define HAS_SETSID
#define USE_WINCHKILL
#define USE_ONEXIT
#define USE_SETPGRP
#else
#define USE_SETSID
#endif
#endif
#endif /* SVR4 */

#if defined(BSD) && !defined(BSDI)
#define USE_TCATTR
#define USE_SETPGRP
#define ERR_BLOCK EWOULDBLOCK
#endif

#if defined(BSD) && !defined(NO_SIGWINCH)
#define USE_SIGWINCH
#endif

#ifdef hcx
#ifdef att_universe
#define USE_VHANGUP
#define USE_TERMIOS
#define USE_TCATTR
#else /* ucb_universe */
#define NO_VSPRINTF
#endif
#define USE_SETSID
#define ERR_BLOCK EAGAIN
#define USE_HERROR
#define USE_SIGWINCH
	/* NO_TTYNAME might not be needed... */
#define NO_TTYNAME
#endif


#if !defined(ERR_BLOCK) /* if no OS defined */
/* 
#error "Need to define an OS" 
Ultrix MIPS compiler chokes on this, even though ERR_BLOCK is defined!
*/
"Compilation directive: You need to define an OS";
/* (Note: We do need the semicolon above for the Mips-CC , argh ;-() */
/* If your OS isn't defined you need to work out which of the above defines */
/* you need and build and entry for it. Please send me the diff if you do so */
/* I am oreillym@tartarus.uwa.edu.au */
#endif
