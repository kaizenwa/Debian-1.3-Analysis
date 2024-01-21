
/*
**
*/

#include "config.h"
#include "termnet.h"

	/* This is just to help track what is always being included */
#ifndef I_ALWAYS
# define I_ALWAYS
#endif

#if defined(I_IDENT)
#ifndef I_LIMITS
#define I_LIMITS
#endif
#ifndef I_PWD
#define I_PWD
#endif
#ifndef I_TYPE
#define I_TYPE
#endif
#ifndef I_CTYPE
#define I_CTYPE
#endif
#endif

#ifndef I_TIME
# ifdef I_SYS
#  define I_TIME
# endif
#endif

	/* This helps avoid conflicts when using libtermnet.a */
#if defined(I_ERRNO)
# include <errno.h>
#endif

#ifdef I_CTYPE
# include <ctype.h>
#endif

#ifdef I_ALWAYS
# ifndef titan
#  include <sys/types.h>
#  ifdef ISC
#   include <sys/bsdtypes.h>
#   include <net/errno.h>
#  endif /* ISC */
# else
#  include <types.h>
# endif
#endif

#ifdef I_IOCTL
# if !defined(I_TTY) || !defined(sun) || defined(SVR4)
#  include <sys/ioctl.h>
# endif
# if defined(SVR4)
#  include <sys/filio.h>		/* FIONREAD */
# endif
# include <fcntl.h>
# ifndef FD_CLOEXEC
#  define FD_CLOEXEC 1
# endif
#endif

#ifdef I_STRING
# include <string.h>
#endif

#ifdef I_ALWAYS
#ifndef NO_UNIX_DOMAIN
# include <sys/un.h>
#endif
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# ifdef SYSV
#  include <sys/utsname.h>
# endif
# if defined(SVR4) || defined(_LIBC)
#  ifdef SVR4
    int socketpair(int, int, int, int[]);
#  endif
   int x__accept(int, struct sockaddr *, int *);
   int x__bind(int, struct sockaddr *, int);
   int x__connect(int, struct sockaddr *, int);
   int x__listen(int, int);
   int x__socket(int, int, int);
   int x__shutdown(int, int);
   int x__gethostname(char *, size_t);
#  if defined(UNIX_SV) || defined(_LIBC)
    int x__recvfrom(int,char *,int,unsigned int, struct sockaddr *,int *);
    int x__sendto(int,char *,int,unsigned int, struct sockaddr *,int);
    int x__getpeername(int, struct sockaddr *, int *);
    int x__getsockname(int, struct sockaddr *, int *);
    int x__rcmd(char **,unsigned short, char *, char *, char *, int *);
#  endif
#  ifdef _LIBC
    int x__recv(int,char *,int,unsigned int);
    int x__send(int,char *,int,unsigned int);
    struct hostent *x__gethostbyname(char *);
    struct hostent *x__gethostbyaddr(char *,int,int);
#  endif
# endif
# ifdef SVR3
#  define socketpair term_socketpair
# endif
# ifndef NO_UNIX_DOMAIN 
#  define S_Pipe(soc) socketpair(AF_UNIX, SOCK_STREAM, 0, soc) 
# else 
#  ifndef SCO
#   define S_Pipe(soc) socketpair(AF_INET, SOCK_STREAM, 0, soc)
#  else
#   define S_Pipe(soc) s_pipe(soc)
#  endif
# endif
#endif

#ifdef I_GETOPT
  int term_getopt(int argc, char *argv[], char *optstring);
  extern char *term_optarg;
  extern int term_optind, term_opterr, term_optopt;
#endif

#ifdef I_TTY
# ifdef USE_TERMIOS
#   include <termios.h>
# else
#  include <sgtty.h>
# endif
#endif

#ifdef I_ALWAYS
# ifdef NO_VFORK
#  define vfork fork
# endif
# include <stdio.h>
# ifdef _LIBC
   void x__perror(char *);
   int x__fcntl(int, int, ...);
   char *x__strerror(int);
   int x__close(int);
   int x__fork(void); 
#ifndef NO_VFORK
   int x__vfork(void);
#endif 
   int x__chroot(char *);
   int x__dup(int);
   int x__dup2(int,int);
# endif
# ifndef titan
#  include <stdlib.h>
# else
   extern char *getenv(char *name);
#  include <malloc.h>
# endif /* titan */
# ifndef NeXT
#  include <unistd.h>
# endif
# if defined(ultrix) || defined(NeXT)
#  include <sys/file.h>
# endif
# ifndef R_OK
#  define R_OK 4
# endif
# ifndef W_OK
#  define W_OK 2
# endif
# ifndef X_OK
#  define X_OK 1
# endif
# ifndef IXANY
#  define IXANY 0
# endif
# if !defined(ONLCR) && defined(__QNX__)
#  define ONLCR OPOST
# endif
# ifndef STDIN_FILENO
#  define STDIN_FILENO 0
# endif
# ifndef USE_HERROR
#  define herror term_herror
void term_herror(char *);
# endif
# ifdef USE_ONEXIT
#  define atexit(procp) on_exit(procp, 0)
# endif
# ifdef NO_ATEXIT
#  define atexit(procp) while(0)
# endif

/* Establish a default single TCP connection if none were specified */

#if !defined(X_TCP_SOCKET) && !defined(X_STREAMS_PIPE) && !defined(X_UNIX_SOCKET)
#define X_TCP_SOCKET
#endif

/* Establish the default X unix sockets */

#ifndef LOCAL_X_DIR
#define LOCAL_X_DIR  "/tmp/.X11-unix"
#endif

#ifndef LOCAL_X_NAME
#define LOCAL_X_NAME "X"
#endif

#ifndef REMOTE_X
#define REMOTE_X "/tmp/.X11-unix/X0"
#endif

#endif

#ifdef I_SIGNAL
# include <signal.h>
# ifndef SIGWINCH
#  define SIGWINCH SIGWINDOW 
# endif
#endif

#ifdef I_TIMES
# include <sys/times.h>
#endif

#ifdef I_TIME
# include <sys/time.h>
# ifdef USE_STIME
#  define gettime(a) get_process_stats(a,getpid(),0,0)
#  define settime(a) stime(a)
# else
#  ifdef SVR4
    int gettimeofday(struct timeval *);
    int settimeofday(struct timeval *);
#   define gettime gettimeofday
#   define settime settimeofday
#  else
#   define gettime(a) gettimeofday((a), (struct timezone *)0)
#   define settime(a) settimeofday((a), (struct timezone *)0)
#  endif
# endif
#endif

#ifdef I_UTIME
# if !defined(titan) && !defined(NeXT)
#  include <utime.h>
# else
          struct    utimbuf        {
               time_t      actime; /* access time */
               time_t      modtime;/* modification time */
          };
# endif
#endif


#ifdef I_ARGS
# ifndef NO_VSPRINTF
#  ifndef USE_VARARGS
#   include <stdarg.h>
#  else /* USE_VARARGS */
#   include <varargs.h>
#  endif /* USE_VARARGS */
# else /* NO_VSPRINTF */
#  define va_alist a1,a2,a3,a4,a5,a6,a7,a8,a9
#  define va_dcl long a1,a2,a3,a4,a5,a6,a7,a8,a9;
#  define vsprintf(buf,fmt,v) sprintf((buf),(fmt),a1,a2,a3,a4,a5,a6,a7,a8,a9)
#  define va_list int
#  define va_start(v)
#  define va_end(v)
#  define USE_VARARGS
# endif /* NO_VSPRINTF */
#endif /* I_ARGS */

#ifdef I_SYS
# if defined (_AIX) || defined(__QNX__) || defined(DYNIXPTX)
#  include <sys/select.h>
# endif
# if defined(DYNIXPTX) || defined(SVR4)
   int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
# endif
#endif

#ifdef I_INET
# include <arpa/inet.h>
#endif

#ifdef I_PARAM
# include <sys/param.h>
#endif

#ifdef I_MEMORY
# ifndef convex
#  include <memory.h>
# endif
#endif

#ifdef I_PWD
# include <pwd.h>
#endif

#ifdef I_PROCESS
# ifdef __QNX__
#  include <process.h>
# endif
#endif

#ifdef I_WAIT
# include <sys/wait.h>
#endif

#ifdef I_STROPT
# if defined(SCO) || defined(SYSV) || defined(SVR3)
#  include <stropts.h>
# endif
#endif

#ifdef I_POLL
# ifdef SVR3
#   include <poll.h>
# endif
#endif

#ifdef I_STREAM
# ifdef STREAMS_PIPE
#  include <sys/stream.h>
# endif
#endif

#ifdef I_STAT
# include <sys/stat.h>
# ifndef S_ISREG
#  define S_ISREG(a) (((a) & S_IFMT) == S_IFREG)
#  define S_ISDIR(a) (((a) & S_IFMT) == S_IFDIR)
# endif
#endif

#ifdef I_LIMITS
# include <limits.h> 
# include <sys/param.h> 
# ifndef _POSIX_PIPE_BUF
#  define _POSIX_PIPE_BUF 512
# endif
# ifndef PIPE_BUF
#  define PIPE_BUF 4096
# endif
# define PIPE_BUFFER ((PIPE_BUF<30720&&PIPE_BUF>0)?PIPE_BUF:30720)
# ifndef PATH_MAX
#  define PATH_MAX 1024
# endif
#endif

# ifndef CLK_TCK
#  ifdef HZ
#   define CLK_TCK HZ
#  else
#   define CLK_TCK 50
#  endif
# endif



#include "term_main.h"

