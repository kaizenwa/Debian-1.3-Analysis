/* $Id: nntpauth.c,v 3.6 1994/10/28 11:11:11 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "nntp.h"
#include "util2.h"
#include "INTERN.h"
#include "nntpauth.h"

#ifdef USE_NNTP

char
nntp_handle_auth_err(strict)
bool_int strict;
{
#ifdef USE_GENAUTH
    extern char *loginName;
    char last_command_save[NNTP_STRLEN];
    char *s, *cmd = "XAUTHINFO GENERIC" + (CompliantServer? 0 : 1);
    char ch;

    /* save previous command */
    strcpy(last_command_save, last_command);

    if (s = getenv("NNTPAUTH"))
	sprintf(ser_line, "%s %s", cmd, s);
    else
	sprintf(ser_line, "%s any %s", cmd, loginName);

    /* issue authentication request */
    nntp_command(ser_line);

    ch = (s? nntp_auth(s) : nntp_check(strict));

    if (ch == NNTP_CLASS_FATAL) {
	char tmpbuf[LBUFLEN];
	sprintf(tmpbuf,"\n%s\n",ser_line);
	nntp_error(tmpbuf);
	finalize(1);
    }

    nntp_command(last_command_save);
    return nntp_check(strict);
#else /* !USE_GENAUTH */
    if (strict) {
	char tmpbuf[LBUFLEN];
	sprintf(tmpbuf,"\n%s\n",ser_line);
	nntp_error(tmpbuf);
	finalize(1);
    }
    return *ser_line;
#endif /* !USE_GENAUTH */
}

#ifdef USE_GENAUTH

char
nntp_auth(authc)
char *authc;
{
    char *s;

    if (cookiefd == -1 && (s = getenv("NNTP_AUTH_FDS")) != Nullch) {
	sscanf(s, "%*d.%*d.%d", &cookiefd);
    }

    if (cookiefd == -1) {
	FILE *f = tmpfile();
	if (f)
	    cookiefd = fileno(f);
    }

    sprintf(ser_line,"%d.%d.%d",fileno(ser_rd_fp),fileno(ser_wr_fp),cookiefd);
    export("NNTP_AUTH_FDS", ser_line);

    if (system(authc))
	strcpy(ser_line, "502 Authentication failed");
    else
	strcpy(ser_line, "281 Ok");
    return *ser_line;
}
#endif /* USE_GENAUTH */

#endif /* USE_NNTP */
