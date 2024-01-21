/*************************************************************************
**
** Network version of syslog (syslog,openlog,closelog)
**
**
**************************************************************************
**/

/*
 * This code from Jean-luc Szpyrka's modified super. (jls@sophia.inria.fr)

 * Modification history:
 *
 * Will Deich (will@nfra.nl), 20 Dec 1994.
 *    - don't use prototypes for ropenlog(), rcloselog().
 *    - rsyslog() changed to use stdargs if __STDC__, varargs otherwise.
 *    - change ropenlog() to be closer to openlog() in its arguments.
 *    - change rsyslog() to be closer to syslog() in what it prints.
 * Will Deich (will@nfra.nl), 20 Jan 1996.
 *    - replaced most of the #include's with #include "localsys.h"
 */

#include "localsys.h"

/* Just in case there is no errno.h, we provide our own useless errno
 * variable, so that the code below always compiles.
 */
#ifndef HAVE_ERRNO_H
static int errno;
#endif

struct socketd {
    short	        f_socket;			
    struct sockaddr_in	f_addr;
};

static struct socketd LogSocket;

#define MAXIDENT 128	/* keep MAXIDENT << MAXLINE */
#define MAXLINE 1024

static struct {
    char ident[MAXIDENT];
    int log_pid;
    int def_facility;
} loginfo = { "", 0, LOG_USER };

/*
 * This procedure sends a message to and already opened networked syslog
 * It uses varargs to provide a useful interface (a la printf)
 *  
 */
/*VARARGS0*/

/**
 ** Warning -- awful code style here.  Depending on whether you are
 ** compiling with HAVE_STDARG_H set or not, we compile different code for
 ** the function declaration and first few lines of the body.
 **/

#ifdef HAVE_STDARG_H
    /** ** ** ** STD C beginning ** ** ** **/
    void
    rsyslog(unsigned int level, char *fmt, ...)
    {
	va_list args;

#else
    /** ** ** ** K&R C beginning ** ** ** **/

    void
    rsyslog( va_alist )
    va_dcl
    {
	va_list args;
	unsigned int level;
	char *fmt;
#endif
    /** ** ** ** Body of function ** ** ** **/

    char msg[2*MAXLINE];   /* MAXLINE is the maximum size of the message,
			    * but  <%d> ident:   or  <%d> (pid) ident:
			    * is added at the beginning of it before
			    * sending it on the net.
			    */
    int l;

    /*
     * Formats the output 
     */

#ifdef HAVE_STDARG_H
    va_start(args,fmt);
#else
    va_start(args);
    level = va_arg(args, unsigned long);
    fmt = va_arg(args, char *);
#endif

    (void) sprintf(msg,"<%d>", level);
    if (loginfo.log_pid)
	(void) sprintf(msg,"(%d) ", getpid());
    if (*loginfo.ident)
	(void) sprintf(msg+strlen(msg),"%s: ", loginfo.ident);
    (void) vsprintf(msg+strlen(msg), fmt, args);
    va_end(args);

    /*
     * Eventually truncate the results (I hope 2*MAXLINE is enough !!!!)
     */
    l = strlen(msg);
    if (l > MAXLINE)
	l = MAXLINE;

    /*
     * Sends to the socket
     */
    if (sendto(LogSocket.f_socket, msg, l, 0,
	       (struct sockaddr *)&LogSocket.f_addr,
	       sizeof LogSocket.f_addr) != l) {
	int e = errno;
	(void) close(LogSocket.f_socket);
	errno = e;
	perror("rsyslog: sendto");
    }
    /*
     * That's it !
     */
}


/*
 *  This procedure opens a UDP connection to the syslog service of the
 * collecting machine. 
 *
 */
void
ropenlog(ident, logopt, facility, host)
char *ident;	/* same as in ordinary openlog() */
int logopt;	/* same as in openlog(), but only LOG_PID is used.
		 * All other options are quietly ignored.
		 */
int facility;	/* same as in openlog() -- but don't rely on using #define'd
		 * values here (e.g. LOG_USER or LOG_LOCAL1) because the
		 * loghost may have a different set of #define's for the
		 * facilities than the machine running ropenlog(), and thus
		 * may interpret this differently than you think.
		 */
char *host;	/* host is the name of the collecting machine */
{
    struct hostent *pHost;
    struct servent *pService;
    char   msg[MAXLINE];

    strncpy(loginfo.ident, ident, sizeof(loginfo.ident)-1);
    loginfo.ident[sizeof(loginfo.ident)-1] = '\0';
    loginfo.log_pid = logopt & LOG_PID;
    loginfo.def_facility = facility;

    /*
     * if no argument is given (this test is necessary to avoid
     * future possible core dumps)
     */
    if ( host==NULL ) {
      perror("ropenlog(): host not defined (was passed NULL ptr!)");
      errno = 0;
      exit(-1);
    }

    /*
     *  the hostent structure has to be used later
     */ 
    pHost = gethostbyname(host);

    /*
     * also verify that the host does exists
     */
    if (pHost == NULL) {
	(void) sprintf(msg, "ropenlog: unknown host %s", host);
	errno = 0;
	perror(msg);
	exit(-1);
    }

    /*
     * Network stuff
     */
    memset((char *) &LogSocket.f_addr,0,sizeof LogSocket.f_addr);

    LogSocket.f_addr.sin_family = AF_INET;

    /*
     * Use the service routine to figure what the syslog/udp port is
     * (If it doesn't exists on a machine (you never now !), it could be useful 
     * to force it to 514, but I never had this problem.)
     */
    pService = getservbyname("syslog", "udp");
    if (pService == NULL) {
	errno = 0;
	perror("ropenlog: syslog/udp is an unknown service");
	exit(-1);
    }
    LogSocket.f_addr.sin_port = pService->s_port;

    /*
     * Opens the connection
     */
    memcpy((char *) &LogSocket.f_addr.sin_addr,
	pHost->h_addr, pHost->h_length);
    LogSocket.f_socket = socket(AF_INET, SOCK_DGRAM, 0);
    if (LogSocket.f_socket < 0) {
	perror("ropenlog: socket");
	exit(-1);
    }

}

/*
 * This procedure closes the opened syslog connection.
 */
void
rcloselog()
{
    (void) close(LogSocket.f_socket);
}

