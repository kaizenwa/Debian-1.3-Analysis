/*-
 * ENIGMA.C - routines for Enigma Logic tokens, specifically the
 *  Multisync challenge/response token and the DES Silver one-time
 *  password token.  The tokens are supported in the following modes:
 *    - dynamic password enabled or disabled
 *    - no fixed password enabled
 * contributed by John Wack, NIST
*/

static	char	RcsId[] = "Header: enigma.c,v 1.2 94/11/02 22:42:40 mjr rel ";
#include	<stdio.h>
#include	<ctype.h>
#include	<string.h>
#include	<sys/types.h>
#include	"firewall.h"
#include	"auth.h"

#ifdef	AUTHPROTO_ENIGMA

#include	"custpb.h"
#include	"custf.h"

extern 	void	pbmain();
extern	void	uninitenv();

static	struct	pblk	pblock;
static	int	pass_mode = 0;
static	int	challenged = 0;
static	char	save_challenge[20];
static	char	save_response[20];


enigchallng(user,buf,bs)
char	*user;
char	*buf;
int	bs;
{
	struct	pblk	*pb;

	challenged = 1;
	pb = &pblock;
	bzero(&pblock, sizeof(pblock));
	bzero(save_challenge,sizeof(save_challenge));
	bzero(save_response,sizeof(save_response));
	pb = &pblock;
	strcpy(pb->uport, "authsrv");
	pb->status = NO_STATUS;
	pb->mode = CHALLENGE;	 /* set the mode to Challenge */
	strcpy(pb->id, user);  /* copy the user name into the block */

	pbmain(pb);

	if(pb->status == PASS) {
		pass_mode = 1;
		sprintf(buf,"No password required - press RETURN to continue: ");
		return(0);
	}
	if(pb->dynpwdf == ENABLED && pb->status != GOOD_NO_CHAL) {
		if(pb->chal[0] != '\0') {
			strcpy(save_challenge, pb->chal);
			sprintf(buf,"Challenge: <%s>  Response: ",pb->chal);
		} else
			sprintf(buf, "Dynamic Password: ");
		return(0);
	}

	sprintf(buf,"Failed: %s - %s", pb->msg1, pb->msg2);
	return(1);
}


enigverify(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	struct	pblk	*pb;
	int	status;

	strcpy(rbuf,"Permission Denied.");
	if(!challenged)
		return(1);
	challenged = 0;

	pb = &pblock;
	if(pass_mode == 1) {
		sprintf(rbuf,"ok %s  %s",pb->msg1,pb->msg2);
		status = 0;
	} else {
		pb->mode = EVALUATE_ALL;
		strcpy(pb->dynpwd, pass);
		strcpy(save_response,pass);
		pbmain(pb);

		if(pb->status == PASS || pb->status == PASS_MASTER) {
			sprintf(rbuf,"ok %s  %s",pb->msg1,pb->msg2);
			status = 0;
		} else {
			sprintf(rbuf,"%s  %s",pb->msg1,pb->msg2);
			status = 1;
		}
	}

	strcpy(pb->chal,save_challenge);
	strcpy(pb->dynpwd,save_response);
	pb->mode = UPDATE_LOGS;
	pbmain(pb);

	return(status);
}


enigset(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	return(0);
}
#endif
