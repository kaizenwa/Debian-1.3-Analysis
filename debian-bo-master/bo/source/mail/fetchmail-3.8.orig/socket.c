/*
 * socket.c -- socket library functions
 *
 * For license terms, see the file COPYING in this directory.
 */

#include <config.h>

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#if defined(STDC_HEADERS)
#include <stdlib.h>
#endif
#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#if defined(HAVE_STDARG_H)
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "socket.h"

#ifndef  INADDR_NONE
#ifdef   INADDR_BROADCAST
#define  INADDR_NONE	INADDR_BROADCAST
#else
#define	 INADDR_NONE	-1
#endif
#endif

FILE *SockOpen(char *host, int clientPort)
{
    int sock;
    unsigned long inaddr;
    struct sockaddr_in ad;
    struct hostent *hp;

    memset(&ad, 0, sizeof(ad));
    ad.sin_family = AF_INET;

    inaddr = inet_addr(host);
    if (inaddr != INADDR_NONE)
        memcpy(&ad.sin_addr, &inaddr, sizeof(inaddr));
    else
    {
        hp = gethostbyname(host);
        if (hp == NULL)
            return (FILE *)NULL;
        memcpy(&ad.sin_addr, hp->h_addr, hp->h_length);
    }
    ad.sin_port = htons(clientPort);
    
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
        return (FILE *)NULL;
    if (connect(sock, (struct sockaddr *) &ad, sizeof(ad)) < 0)
    {
	close(sock);
        return (FILE *)NULL;
    }

    return fdopen(sock, "r+");
}


#if defined(HAVE_STDARG_H)
int SockPrintf(FILE *sockfp, char* format, ...)
{
#else
int SockPrintf(sockfp,format,va_alist)
FILE *sockfp;
char *format;
va_dcl {
#endif

    va_list ap;
    char buf[8192];

#if defined(HAVE_STDARG_H)
    va_start(ap, format) ;
#else
    va_start(ap);
#endif
    vsprintf(buf, format, ap);
    va_end(ap);
    return SockWrite(buf, 1, strlen(buf), sockfp);

}

int SockWrite(char *buf, int size, int len, FILE *sockfp)
{
    int n, wrlen = 0;

    len *= size;
    while (len)
    {
        n = write(fileno(sockfp), buf, len);
        if (n <= 0)
            return -1;
        len -= n;
	wrlen += n;
	buf += n;
    }
    return wrlen;
}

char *SockGets(char *buf, int len, FILE *sockfp)
{
    char *p, *bp = buf;
    int n;

    if (--len < 1)
	return NULL;
    do {
	/* return value of 0 is EOF, < 0 is error */
	if ((n = recv(fileno(sockfp), bp, len, MSG_PEEK)) <= 0)
	    return NULL;
	if ((p = memchr(bp, '\n', n)) != NULL)
	{
	    if (read(fileno(sockfp), bp, ++p - bp) == -1)
		return NULL;
	    *p = '\0';
	    return buf;
	}
	if ((n = read(fileno(sockfp), bp, n)) == -1)
	    return NULL;
	bp += n;
	len -= n;
    } while (len);
    *bp = '\0';
    return buf;
}

int SockPeek(FILE *sockfp)
/* peek at the next socket character without actually reading it */
{
    int n;
    char ch;

    if ((n = recv(fileno(sockfp), &ch, 1, MSG_PEEK)) == -1)
	return -1;
    else
	return(ch);
}

#ifdef MAIN
/*
 * Use the chargen service to test input beuffering directly.
 * You may have to uncomment the `chargen' service description in your
 * inetd.conf (and then SIGHUP inetd) for this to work. 
 */
main()
{
    FILE	*fp = SockOpen("localhost", 19);
    char	buf[80];

    while (SockGets(buf, sizeof(buf)-1, fp))
	SockWrite(buf, 1, strlen(buf), stdout);
}
#endif /* MAIN */

/* socket.c ends here */
