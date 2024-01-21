/* $Id: nntpclient.c,v 3.0 1991/11/22 04:12:21 davison Trn $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"

#ifdef USE_NNTP

#include "INTERN.h"
#include "nntpclient.h"

void finalize _((int));
#ifdef NNTP_HANDLE_TIMEOUT
char nntp_handle_timeout _((bool_int));
#endif
#ifdef NNTP_HANDLE_AUTH_ERR
char nntp_handle_auth_err _((bool_int));
#endif

#define CANTPOST	\
	"NOTE:  This machine does not have permission to post articles.\n\n"
#define CANTUSE		\
	"This machine does not have permission to use the %s news server.\n\n"

int
nntp_connect(verbose)
bool_int verbose;
{
    char *server, filebuf[128];
    int response;

    if ((server = getenv("NNTPSERVER")) == Nullch)
	server = SERVER_NAME;
    if (server[0] == '/') {
	register FILE *fp;
	if ((fp = fopen(server, "r")) != Nullfp) {
	    server = Nullch;
	    while (fgets(filebuf, sizeof filebuf, fp) != Nullch) {
		if (*filebuf == '\n' || *filebuf == '#')
		    continue;
		if ((server = index(filebuf, '\n')) != Nullch)
		    *server = '\0';
		server = filebuf;
		break;
	    }
	    fclose(fp);
	} else
	    server = Nullch;
	if (server == Nullch) {
	    sprintf(ser_line, "\
Couldn't get name of news server from %s\n\
Either fix this file, or put NNTPSERVER in your environment.\n", SERVER_NAME);
	    nntp_init_error(ser_line);
	    return 0;
	}
    }

    switch (response = server_init(server)) {
    case NNTP_GOODBYE_VAL:
	if (atoi(ser_line) == response) {
	    char tmpbuf[LBUFLEN];
	    sprintf(tmpbuf,"News server %s unavailable: %s\n",server,&ser_line[4]);
	    nntp_init_error(tmpbuf);
	    return 0;
	}
    case -1:
	sprintf(ser_line,"News server %s unavailable, try again later.\n",server);
	nntp_init_error(ser_line);
	return 0;
    case NNTP_ACCESS_VAL:
	sprintf(ser_line,CANTUSE,server);
	nntp_init_error(ser_line);
	return 0;
    case NNTP_NOPOSTOK_VAL:
	if (verbose) {
	    nntp_advise(CANTPOST);
	}
	/* FALL THROUGH */
    case NNTP_POSTOK_VAL:
	break;
    default:
	sprintf(ser_line,"Unknown response code %d from %s.\n", response, server);
	nntp_init_error(ser_line);
	return 0;
    }
    return 1;
}

void
nntp_command(buf)
char *buf;
{
#if defined(DEBUG) && defined(FLUSH)
    if (debug & DEB_NNTP)
	printf(">%s\n", buf) FLUSH;
#endif
#if defined(NNTP_HANDLE_TIMEOUT) || defined(NNTP_HANDLE_AUTH_ERR)
    strcpy(last_command, buf);
#endif
    fprintf(ser_wr_fp, "%s\r\n", buf);
    fflush(ser_wr_fp);
}

#ifdef NNTP_HANDLE_TIMEOUT
extern char *instr();
#endif

char
nntp_check(strict)
bool_int strict;
{
    int n;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    n = (fgets(ser_line, sizeof ser_line, ser_rd_fp) == NULL)? -1 : 0;
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    if (n < 0) {
	nntp_close(FALSE);
	nntp_error("\nUnexpected close of server socket.\n");
#ifdef NNTP_ERROR_IS_FATAL
	finalize(1);
#else
	return NNTP_CLASS_FATAL;
#endif
    }
    n = strlen(ser_line);
    if (n >= 2 && ser_line[n-1] == '\n' && ser_line[n-2] == '\r')
	ser_line[n-2] = '\0';
#if defined(DEBUG) && defined(FLUSH)
    if (debug & DEB_NNTP)
	printf("<%s\n", ser_line) FLUSH;
#endif
#ifdef NNTP_HANDLE_AUTH_ERR
    if (atoi(ser_line) == NNTP_AUTH_NEEDED_VAL) {
	return nntp_handle_auth_err(strict);
    }
#endif
#ifdef NNTP_HANDLE_TIMEOUT
    if (atoi(ser_line) == NNTP_TMPERR_VAL && instr(ser_line,"timeout",FALSE)) {
	/* See if this was really a timeout */
	return nntp_handle_timeout(strict);
    }
#endif
    if (strict && *ser_line == NNTP_CLASS_FATAL) {	/* Fatal error */
	char tmpbuf[LBUFLEN];
	sprintf(tmpbuf,"\n%s\n",ser_line);
	nntp_error(tmpbuf);
#ifdef NNTP_ERROR_IS_FATAL
	finalize(1);
#endif
    }
    return *ser_line;
}

int
nntp_gets(buf, len)
char *buf;
int  len;
{
    int n;

#ifdef HAS_SIGHOLD
    sighold(SIGINT);
#endif
    n = (fgets(buf, len, ser_rd_fp) == NULL)? -1 : 0;
#ifdef HAS_SIGHOLD
    sigrelse(SIGINT);
#endif
    if (n < 0) {
	nntp_close(FALSE);
	nntp_error("\nUnexpected close of server socket.\n");
#ifdef NNTP_ERROR_IS_FATAL
	finalize(1);
#else
	return -1;
#endif
    }
    n = strlen(buf);
    if (n >= 2 && buf[n-1] == '\n' && buf[n-2] == '\r') {
	buf[n-2] = '\0';
	return 1;
    }
    return 0;
}

void
nntp_close(send_quit)
bool_int send_quit;
{
    if (send_quit && ser_wr_fp != NULL && ser_rd_fp != NULL) {
	nntp_command("QUIT");
	nntp_check(FALSE);
    }
    /* the nntp_check() above might have closed these already. */
    if (ser_wr_fp != NULL && ser_rd_fp != NULL) {
	fclose(ser_wr_fp);
	ser_wr_fp = NULL;
	fclose(ser_rd_fp);
	ser_rd_fp = NULL;
    }
}

#endif /* USE_NNTP */
