/*-
 * Copyright (c) 1993, Trusted Information Systems, Incorporated
 * All rights reserved.
 *
 * Redistribution and use are governed by the terms detailed in the
 * license document ("LICENSE") included with the toolkit.
 */

/*
 *	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
 *	fixes for newer securid from Alastair Young <alastair@cadence.com>
 */
static	char	RcsId[] = "Header: securid.c,v 1.3 94/11/01 11:52:30 mjr rel ";
#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <sys/types.h>

#include	"firewall.h"
#include	"auth.h"

#ifdef	AUTHPROTO_SECURID

#include "sdi_athd.h"
#include "sdi_size.h"
#include "sdi_type.h"
#include "sdacmvls.h"
#include "sdconf.h"

union config_record configure;


static	int			challenged = 0;
static	int			sd_initted = 0;
static	struct SD_CLIENT	sdbuf;


secichallng(user,buf,bs)
char	*user;
char	*buf;
int	bs;
{
	challenged = 1;
	strcpy(buf,"SecurID Passcode: ");
	return(0);
}



seciverify(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	int	result;
	char	debug;

	strcpy(rbuf,"Permission Denied.");
	if(!challenged)
		return(1);
	challenged = 0;

	if(strncmp(pass,"next",4)) {
		if(!sd_initted) {
			bzero(&sdbuf,sizeof(sdbuf));
			creadcfg(); 
			if(sd_init(&sdbuf)) {
				strcpy(rbuf,"Cannot talk to ACE server");
				return(1);
			}	
			sd_initted = 1;
		}
		result = sd_check(pass,user,&sdbuf);
	} else {
		result = sd_next(&pass[4],&sdbuf);
	}

	switch(result) {
	case ACM_OK:
		strcpy(rbuf,"ok");
		return(0);
	case ACM_ACCESS_DENIED:
		return(1);
	case ACM_NEXT_CODE_REQUIRED:
		strcpy(rbuf,"Interim code. Retry");
		return(1);
	case ACM_NEW_PIN_REQUIRED:
		if(sd_pin(sdbuf.system_pin,0,&sdbuf) != ACM_NEW_PIN_ACCEPTED) {
			strcpy(rbuf,"Could not assign new card a new PIN");
			return(1);
		}
		sprintf(rbuf,"ok new PIN is %s",sdbuf.system_pin);
		return(0);
	}
	/* default. huh? */
	return(1);
}



seciset(user,pass,ap,rbuf)
char	*user;
char	*pass;
Auth	*ap;
char	*rbuf;
{
	if(pass == (char *)0) {
		strcpy(rbuf,"NULL PIN not supported with SecurID");
		return(0);
	}
	if(!sd_initted) {
		creadcfg(); 
		if(sd_init(&sdbuf)) {
			strcpy(rbuf,"Cannot talk to ACE server");
			return(1);
		}	
		sd_initted = 1;
	}

	if(sd_pin(pass,0,&sdbuf) != ACM_NEW_PIN_ACCEPTED) {
		strcpy(rbuf,"New PIN Rejected");
		return(1);
	}
	strcpy(rbuf,"New PIN Accepted");
	return(0);
}
#endif
