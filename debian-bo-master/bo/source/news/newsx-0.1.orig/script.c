/*  VER 112  TAB P   $Id: script.c,v 1.5 1996/11/22 12:31:52 src Exp $
 *
 *  implement a very simple telnet protocol
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

static char chat_buf[NNTP_STRLEN+3];
static char *chat_match;
static char *chat_send;

static int chat_line(SOCKET_D *sock);

/*
 *  update chat mode
 */
void chat_update(char *line,SOCKET_D *sock)
{
    char *p;

    if (sock->chat_mode == 0) {
	/* fill buffer */
	if (!chat_line(sock)) return;
	sock->chat_mode = 1; 
    }
    /* found string? */
    if (strstr(line,chat_match)) {
	log_msg(L_DEBUG,"chat found '%s'",line);
	log_msg(L_DEBUG3,"chat sends '%s'",chat_send);
	strcat(chat_send,"\r");
	/* BUG: error... */
	send(sock->w_fd,chat_send,strlen(chat_send),0);
	sock->chat_mode = 0; 
    }
}

/*
 *  line from chat file
 */
static int chat_line(SOCKET_D *sock)
{
    char *p;
    char qt;

    if (!(sock->f_chat)) return 0;
  again:
    chat_buf[NNTP_STRLEN] = '\0';
    if (!fgets(chat_buf,NNTP_STRLEN,sock->f_chat)) {
	fclose(sock->f_chat);
	sock->f_chat = 0;
	return 0;
    }		    
    if (p = strchr(chat_buf,'\n')) *p = '\0';

    /* analyze line */
    p = chat_buf;
    while (isspace(*p)) ++p;

    if (!*p || *p=='#') goto again;

    if (strcmp(chat_buf,"ECHO")==0) {
	sock->is_echo = 1;
	goto again;
    }

    if (strncmp(chat_buf,"ABORT ",6)==0) {
	/* BUG: ignore... */
	goto again;
    }

    qt = (*p=='\'' || *p =='"') ? *p++ : 0;
    chat_match = p;
    while (qt ? (*p!=qt) : !isspace(*p)) {
	if (!*p++) {
	    /* nothing */
	    log_msg(L_ERRno,"chat not pair: %s",chat_buf);
	    goto again;
	}
    }
    *p++ = '\0';

    while (isspace(*p)) ++p;
    qt = (*p=='\'' || *p =='"') ? *p++ : 0;
    chat_send = p;
    while (*p) {
	if (qt ? (*p==qt) : isspace(*p)) {
	    *p++ = '\0';
	    while (isspace(*p)) ++p;
	    if (*p) log_msg(L_ERRno,"chat excess ignored: %s",p);
	    break;
	}
	++p;
    }
    return 1;
}

