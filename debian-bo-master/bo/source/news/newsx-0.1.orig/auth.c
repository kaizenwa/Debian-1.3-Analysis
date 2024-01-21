/*  VER 010   TAB P   $Id: auth.c,v 1.7 1996/11/22 12:31:52 src Exp $
 *
 *  authorization related stuff
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"
#include "nntp.h"

/*
 *  pick authorisation info
 */
void get_authinfo(char *file)
{
    FILE *f;
    char *p;
    static char buf[BUFSIZ];

    if (!(f = fopen(file,"r"))) {
	log_msg(L_ERRno,"cannot open authorization file '%s'",file);
	unlock_exit(10);
    }
    buf[BUFSIZ-1] = '\0';
    while (fgets(buf,BUFSIZ-1,f)) {
	p = buf;
	while (isspace(*p)) ++p;
	if (p[0] && p[0] != '#') {
	    ai_username = p;
	    while (*p && !isspace(*p)) ++p;
	    if (!*p) {
	      bad:
		log_msg(L_ERRno,"bad syntax in authinfo '%s'",buf);
		unlock_exit(10);
	    }
	    *p++ = '\0';
	    log_msg(L_DEBUG3,"username %s",ai_username);
	    while (isspace(*p)) ++p;
	    if (!*p) goto bad;
	    ai_password = p;
	    while (*p && !isspace(*p)) ++p;
	    *p = '\0';
	    break;
	}
    }
    fclose(f);
}

/*
 *  check in the authinfo username and password with the
 *  server 
 */
void do_authinfo(char *username,char *password)
{
    char buf[NNTP_STRLEN];

    progtitle("do authinfo");

    /* send the username to the server */
    sprintf(buf, "AUTHINFO USER %s\r\n", username);
    put_server(buf);
    if (noaction_flag) return;

    /* get the response and check it's okay */
    if (!get_server_nntp(buf, sizeof(buf))) unlock_exit(9);
    if (atoi(buf) != NEED_AUTHDATA) {
	log_msg(L_ERR,"NNTP authinfo protocol error: got '%s'", buf);
	unlock_exit(4);
    }
		    
    /* send the password to the server */
    sprintf(buf, "AUTHINFO PASS %s\r\n", password);
    put_server(buf);

    /* get the response and check it's okay */
    if (!get_server_nntp(buf, sizeof(buf))) unlock_exit(9);
    if (atoi(buf) != OK_AUTH) {
	log_msg(L_ERR,"NNTP authorization failed: got '%s'", buf);
	unlock_exit(4);
    }
}

/*
 *  send mode reader command to INN to switch to nnrpd
 *  so we can do a NEWNEWS.
 */
void do_mode_reader()
{
    char buf [NNTP_STRLEN];

    progtitle("mode reader");
    /* send the command to the server */
    put_server("MODE READER\r\n");
    if (noaction_flag) return;

    /* get the response and check it's okay */
    if (!get_server_nntp(buf,sizeof(buf))) unlock_exit(9);

    switch (atoi(buf)) {
    case OK_CANPOST :
    case OK_NOPOST :
	break;
    default :
	log_msg(L_ERR,"NNTP mode reader protocol error: got '%s'", buf); 
	unlock_exit(4);
    }
}
