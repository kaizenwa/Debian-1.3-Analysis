/*  VER 101  TAB P   $Id: socket.c,v 1.13 1996/11/22 12:31:52 src Exp $
 *
 *  open a socket connection and read/write to nntp server
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

#include <errno.h>
#include <signal.h>
#include <setjmp.h>

#if !(HAVE_SOCKET)
#error program requires socket functionality
#endif

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifndef INADDR_NONE
  #define INADDR_NONE 0xffffffff
#endif

/*
 * tcp_open - Open a tcp connection to 'host' for service 'service',
 * returning a file descriptor for the socket.
 */
int tcp_open(char *host, char *service)
{
    int on = 1;
    int sockfd;
    unsigned long inaddr;
    struct servent *sp;
    struct hostent *hp;

    struct sockaddr_in serv_addr;
    struct servent serv_info;
    struct hostent host_info;

    memset(&serv_addr, 0, sizeof (serv_addr));
    serv_addr.sin_family = AF_INET;

    /* get service information */
    if ((sp = getservbyname(service, "tcp")) == NULL) {
	log_msg(L_ERR,"unknown service %s/tcp", service);
	return -1;
    }
    serv_info = *sp;
    serv_addr.sin_port = sp->s_port;

    /* try to convert host name as dotted decimal */
    if ((inaddr = inet_addr(host)) != INADDR_NONE) {
	memcpy (&serv_addr.sin_addr, &inaddr, sizeof (inaddr));
	host_info.h_name = NULL;
    } else {
	/* if that failed, then look up the host name */
	if ((hp = gethostbyname(host)) == NULL) {
	    log_msg(L_ERR,"host name error: '%s'", host);
	    return -1;
	}
	host_info = *hp;
	memcpy (&serv_addr.sin_addr, hp->h_addr, hp->h_length);
    }
    log_msg(L_DEBUG3,"connecting to %ld.%ld.%ld.%ld port %d",
			      *(long*)&(serv_addr.sin_addr) & 0xff,
			     (*(long*)&(serv_addr.sin_addr) >> 8) & 0xff,
			     (*(long*)&(serv_addr.sin_addr) >> 12) & 0xff,
			     (*(long*)&(serv_addr.sin_addr) >> 24) & 0xff,
				      ((serv_addr.sin_port & 0xff) << 8)
				    + ((serv_addr.sin_port >> 8) & 0xff));

    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	log_msg(L_ERRno,"can't create TCP socket");
	return -1;
    }

    if (connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) {
	log_msg(L_ERRno,"can't connect to server '%s'", host);
	close(sockfd);
	return -1;
    }

    if (setsockopt(sockfd, SOL_SOCKET, SO_KEEPALIVE,
					(char *) &on, sizeof (on)) < 0)
	log_msg(L_ERRno,"can't set KEEPALIVE on socket");

    return sockfd;
}

/*
 *  open a TCP/IP socket 
 */
int socket_open(char *hostname, char *service, SOCKET_D *sock)
{
    /* make the connection */
    if ((sock->r_fd = tcp_open(hostname, service)) < 0)
	return -1;
    sock->w_fd = dup(sock->r_fd);

    sock->is_echo = 0;
    sock->f_chat = 0;
    sock->chat_mode = 0;
    sock->is_telnet = (strcmp(service,"telnet")==0);
    if (chat_file) {
	if (!(sock->f_chat = fopen(chat_file,"r"))) {
	    log_msg(L_ERRno,"can't open chat file: %s",chat_file);
	    close(sock->r_fd);
	    close(sock->w_fd);
	    return -1;
	}
    }

    /* enable buffering of data */
    if ((sock->r_str = fdopen(sock->r_fd, "r")) == NULL) {
	log_msg(L_ERRno,"can't fdopen socket for read");
	close(sock->r_fd);
	close(sock->w_fd);
	return -1;
    }

    if ((sock->w_str = fdopen(sock->w_fd, "w")) == NULL) {
	log_msg(L_ERRno,"can't fdopen socket for write");
	fclose(sock->r_str);
	close(sock->w_fd);
	return -1;
    }
	
    return 0;
}

/*
 *  close a socket 
 */
void socket_close(SOCKET_D *sock)
{
    fclose(sock->r_str);
    fclose(sock->w_str);
}

/*
 *  alarm handler
 */
static jmp_buf env_alrm;
int alarm_active; /* externally visible.. */

RETSIGTYPE sig_alrm(int signo)
{
    alarm_active = 0;
    longjmp(env_alrm, 1);
}

/*
 *  get line from socket
 *  do not strip CRLF
 *  return false on timeout
 */
int get_socket(char *line, int size, SOCKET_D *sock)
{
    int e;
    int ok = 1;
    static int tcp_time; 

    /* set up an alarm to handle socket timeout */
    if (setjmp(env_alrm)) {
	/* here if alarm happened... */
	alarm(0);			/* reset alarm clock */
	signal(SIGALRM, SIG_DFL);
	progtitle("alarm");
	errno = EPIPE;
	log_msg(L_ERR,"timeout on server socket");
	return 0;
    }

    signal(SIGALRM, sig_alrm);
    alarm_active = 1; /* catch other interrupts too */
    tcp_time = timeout ? timeout : TIMEOUT;
    alarm(tcp_time);

    /* read line */
    if (sock->is_telnet) {
	if (read_telnet(line, size, sock) < 0) ok = 0;
    } else {
	fgets(line, size, sock->r_str);
    }

    /* reset the alarm */
    alarm_active = 0; 
    e = errno;
    alarm(0);
    signal(SIGALRM, SIG_DFL);
    errno = e;

    /* report any error */
    if (ferror(sock->r_str)) {
	log_msg(L_ERRno,"read error on server socket");
	return 0;
    }
    return ok;
}

/*
 *  write to a socket
 *  possibly with CRLF
 */
int put_socket(char *line, SOCKET_D *sock)
{
    if (sock->is_telnet) {
	return write_telnet(line, sock);
    } else {
	fputs(line,sock->w_str);
	if (fflush(sock->w_str) == EOF
	 || ferror(sock->w_str)) {
	    log_msg(L_ERRno,"write error on server socket");
	    return 0;
	}
	return 1;
    }
}

