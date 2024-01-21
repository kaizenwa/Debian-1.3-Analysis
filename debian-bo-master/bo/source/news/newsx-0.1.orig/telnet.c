/*  VER 125  TAB P   $Id: telnet.c,v 1.5 1996/11/24 14:10:24 src Exp $
 *
 *  implement a very simple telnet protocol
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

#define TELOPTS
#define TELCMDS
#include <arpa/telnet.h>

#ifndef IAC
#define IAC  255
#define DONT 254 
#define DO   253
#define WONT 252
#define WILL 251
#define TELOPT_BINARY 0
#define TELOPT_ECHO 1
#define TELOPT_SGA 3 
#define TELOPT_LINEMODE 34
#endif	

/*
 *  write line with telnet protocol
 *  suppress echo
 */
int write_telnet(char *line, SOCKET_D *sock)
{
    int len;
    int n;
    char c1,c2;
    char echo[NNTP_STRLEN+3];

    len = strlen(line);
    n = send(sock->w_fd,line,len,0);
    if (n != len) {
	log_msg(L_ERRno,"write error on telnet socket");
	return 0;
    }
    if (sock->is_echo) {
      again:
	/* await echo */
	echo[NNTP_STRLEN+2] = '\0';
	read_telnet(echo,NNTP_STRLEN+2,sock);
	for (n=0; n<len; ++n) {
	    c1 = line[n];  
	    c2 = echo[n];  
	    if ((c1=='\r' || c1=='\n')
	     && (c2=='\r' || c2=='\n' || !c2)) break;

	    /* NOTE: echo may be filtered through 7bit mask */
	    if ((line[n] & 0x7f) != (echo[n] & 0x7f)) {
		log_msg(L_DEBUG,"wrong echo '%s'",echo);
		goto again;
	    }
	}
    }
    return 1;
}

/*
 *  read line with telnet protocol
 *  NOTE: this is really a primitive implementation!
 */
static void send_iac3(SOCKET_D *sock,char how,char what)
{
    char sbuf[3];
    sbuf[0] = IAC;
    sbuf[1] = how;
    sbuf[2] = what;
    send(sock->w_fd,sbuf,3,0);
}

/*
 *  return string for telnet command
 */
static char *str_telcmd(int cmd)
{
    static char str[10];
#ifdef TELCMD
    if (TELCMD_OK(cmd)) {
	return TELCMD(cmd);
    }
#endif
    sprintf(str,"%d",cmd);
    return str;
}

/*
 *  return string for telnet option
 */
static char *str_telopt(int opt)
{
    static char str[10];
#ifdef TELOPT
    if (TELOPT_OK(opt)) {
	return TELOPT(opt);
    }
#endif
    sprintf(str,"%d",opt);
    return str;
}

/*
 *  read line with telnet protocol
 *  NOTE: this is really a primitive implementation!
 */
int read_telnet(char *line, int size, SOCKET_D *sock)
{
    int ix;
    int n;
    int state;
    int c;
    int d;
    static unsigned char buf[NNTP_STRLEN];
    static int buf_n = 0;
    static int buf_m = 0;
    static int not_first_time = 0;

    if (!not_first_time) {
	not_first_time = 1;
	/* IAC DO TELOPT_BINARY */
	/* IAC DO TELOPT_SGA = suppress goahead */
	/* IAC DONT TELOPT_ECHO (which is default) */
	/*
	log_msg(L_DEBUG3,"telnet send: IAC DO BINARY, IAC DO SGA");
	send_iac3(sock,DO,  TELOPT_BINARY);
	send_iac3(sock,DO,  TELOPT_SGA);
	 */
	/*
	send_iac3(sock,DONT,TELOPT_ECHO);
	send_iac3(sock,DONT,TELOPT_LINEMODE);
	 */
    }
    ix = 0;
    state = 0;
    while (ix < size) {
	/* read a packet */
	while (buf_n >= buf_m) {
	    /* need to refill local buffer */
	    buf_n = 0;
	    buf_m = recv(sock->r_fd,&buf,NNTP_STRLEN,0);
	    if (buf_m < 0) {
		log_msg(L_ERRno,"read error on telnet socket");
		return -1;
	    }
	}
	c = buf[buf_n++];

	switch (state) {
	default:
	    if (c == IAC) {
		state = IAC;
	    } else if (c == '\0') {
		/* ignore */
	    } else if (ix == 0 && c == '\r') {
		/* ignore */
	    } else {
		line[ix++] = c;
		if (ix >= size) return ix;
		line[ix] = '\0';

		if (sock->f_chat) 
		    chat_update(line,sock);

		if (c == '\n') {
		    return ix; /* end of line */
		}
	    }
	    break;
	case IAC:
	    switch (c) {
	    case DO:
	    case DONT:
	    case WILL:
	    case WONT:
		state = c;
		break;
	    case GA: /* go-ahead */
	    default:
		/* ignore two characters... */
		log_msg(L_DEBUG3,"telnet ignore: IAC %d",
						str_telcmd(c));
		state = 0;
		break;
	    }
	    break;
	case DO:
	    switch (c) {
	    case TELOPT_SGA:
	    case TELOPT_BINARY:
		send_iac3(sock,d=WILL,c);
		break;
	    case TELOPT_ECHO:
	    default:
		send_iac3(sock,d=WONT,c);
		break;
	    }
	    log_msg(L_DEBUG3,"telnet %s: IAC DO %s",
				     d==WILL ? "will":"wont",
						str_telopt(c));
	    state = 0;
	    break;
	case DONT:
	    log_msg(L_DEBUG3,"telnet ignore: IAC DONT %s",
						str_telopt(c));
	    state = 0;
	    break;
	case WILL:
	case WONT:
	    log_msg(L_DEBUG3,"telnet ignore: IAC %s %s",
						str_telcmd(state),
						str_telopt(c));
	    /* ignore three characters... */
	    state = 0;
	    break;
	}
    }
    return ix;
}
