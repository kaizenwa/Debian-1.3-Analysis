/*  VER 067  TAB P   $Id: server.c,v 1.15 1996/11/22 12:31:52 src Exp $
 *
 *  NNTP server interface
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

#include <errno.h>

/*
 *  currently open socket pair, for read and write
 */
SOCKET_D nntp_sock;

/*
 *  open an NNTP server connection
 *  returns servers initial response code  
 */
int open_server(char *host,char *port)
{
    int sts;
    char line[NNTP_STRLEN];

    /* first try and make the connection */
    if (via_exec) {
	progtitle("connecting pgm");

	program_open(via_exec, &nntp_sock);
    } else {
	progtitle("connecting");

	if ((socket_open(host, port, &nntp_sock)) < 0)
	    return -1;
	log_msg(L_INFO,"connecting to %s server at %s", port, host);
    }

    if (connect_exec) {
	/* execute command when initiating */
	progtitle("exec cmd");
					       
	log_msg(L_DEBUG3,"running '%s'", connect_exec);
	fflush(nntp_sock.w_str);		       
	if ((sts = script(connect_exec,        
		       /* in */  fileno(nntp_sock.r_str),
		       /* out */ fileno(nntp_sock.w_str))) != 0) {
	    /* connect failed */
	    log_msg(L_ERR,"connect script failed, status is %d",sts);
	    unlock_exit(11);
	}
	log_msg(L_DEBUG,"connect script finished");
    }

    progtitle("await greeting");
    do {
	/* greeting */
	if (!get_server_nntp(line,sizeof (line))) {
	    /* timeout */
	    unlock_exit(9);
	}
    
    } while (!isdigit(line[0]));
    /* banner code */
    return atoi(line);
}

/*
 *  close NNTP connection
 */
void close_server()
{
    char line[NNTP_STRLEN];

    progtitle("closing server");
    put_server("QUIT\r\n");
    if (!get_server_nntp(line,sizeof (line))) {
	/* timeout */
	unlock_exit(9);
    }

    socket_close(&nntp_sock);
}

/*
 *  read a line from server
 *  return 0 on timeout and other problems
 */
int get_server_nntp(char *line, int size)
{
    int len;

    if (noaction_flag) {
	line[0] = '\0';
	return 1;
    }
    if (!get_socket(line, size, &nntp_sock)) {
	return 0;
    }
    len = strlen(line);
    gross_bytecount += len;
    log_msg(L_GET,"%s", line);

    /* no CRLF */
    while (len > 0 && (line[len-1]=='\r' || line[len-1]=='\n')) {
	line[--len] = '\0';
    }

    return 1;
}

/*
 *  read message part from server
 *  any CRLF will not be removed
 *  do not keep gross_bytecount up to date
 *  return 0 on timeout and other problems
 */
int get_server_msg(char *line, int size)
{
    if (noaction_flag) {
	line[0] = '\0';
	return 1;
    }
    if (!get_socket(line, size, &nntp_sock)) {
	return 0;
    }
    if (debug_flag >= 4) {
	log_msg(L_GET,"%s", line);
    }
    return 1;
}

/*
 *  write an NNTP command line to server
 *  line includes CRLF
 */
void put_server(char *line)
{
    log_msg(L_PUT,"%s", line);
    if (noaction_flag) return;

    gross_bytecount += strlen(line)+2;
    if (!put_socket(line, &nntp_sock)) {
	unlock_exit(9);
    }
}

/*
 *  write message part to server, 
 *  a newline may or may not be added
 *  return 0 on timeout and other problems
 */
int put_server_msg(char *line)
{
    if (debug_flag >= 4) {
	log_msg(L_PUT,"%s",line);
    }
    if (noaction_flag) return 1;

    gross_bytecount += strlen(line);
    return put_socket(line, &nntp_sock);
}
