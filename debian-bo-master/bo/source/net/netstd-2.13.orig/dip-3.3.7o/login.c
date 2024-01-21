/*
 * dip		A program for handling dialup IP connecions.
 *		Incoming connection handling module.
 *
 * Version:	@(#)login.c	3.3.6	07/15/94
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal, <uri@watson.ibm.com>
 *              (C) 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"

#define MIN(a, b) ((a < b)? a : b)

static int verify_inet_addr(char *p);

static struct dip *
getdipnam(char *who)
{
  static struct dip dip;
  char buff[1024];
  register FILE *fp;
  register char *sp;
  char *bp;

  if ((fp = fopen(_PATH_ETC_DIPHOSTS, "r")) == (FILE *)NULL) {
	fprintf(stderr, "dip: cannot open %s\n", _PATH_ETC_DIPHOSTS);
	syslog(LOG_ERR, "cannot open %s", _PATH_ETC_DIPHOSTS);
	return((struct dip *)NULL);
  }

  while(fgets(buff, 1024, fp) != (char *)NULL) {
	if ((sp = strchr(buff, '\n')) != (char *)NULL) *sp = '\0';
	if (buff[0] == '#' || buff[0] == '\0') continue;

	sp = buff;

	memcpy ((char *) &dip, (char *) &mydip, sizeof(struct dip));
	strcpy(dip.name, "nobody");

	/* Make bp pointing at dip-user login name in diphosts entry */
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';

	/* accept this diphosts record if either id or */
	/* ttyname is in first field                   */
	if (strcmp(bp, who) && strcmp(bp, ttyname(0)) &&
	    strcmp(bp, (rindex(ttyname(0), '/')+ 1))
           ) 
	  continue; /* no, this record doesn't match */

	strncpy(dip.name, bp, sizeof(dip.name));

	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.passwd, bp, sizeof(dip.passwd));
	if (*bp == 0x00) memset(dip.passwd, 0x00, sizeof(dip.passwd));

	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.remote, bp, sizeof(dip.remote));

	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	if (*bp)
	   strncpy(dip.local, bp, sizeof(dip.local));

	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	if (*bp != 0) /* if netmask is given - use it */
	  strncpy(dip.netmask, bp, sizeof(dip.netmask));
        /* otherwise use the default netmask */
	
	bp = sp;
	while (*sp && *sp != ':') sp++;
	*sp++ = '\0';
	strncpy(dip.comment, bp, sizeof(dip.comment));

	bp = sp;
	while (*sp && *sp != ':' && *sp != ',') sp++;
	*sp++ = '\0';
	strncpy(dip.protocol, bp, sizeof(dip.protocol));

	bp = sp;
	while (*sp && *sp != ',') sp++;
	*sp++ = '\0';
	dip.mtu = atoi(bp);

	(void) fclose(fp);
	return(&dip);
  }
  (void) fclose(fp);
  return((struct dip *)NULL);
}


void
do_login(char *name)
{
  struct dip *dip;
  struct hostent *hp;
  char   lip[20];
  int i;
  extern char *h_errlist[];
  extern int h_errno;

#if 0
  (void) openlog("dip", LOG_PID, LOG_DAEMON);
#endif

  if (name) { /* Is User Login Name given? */
    char *p = (char *)0;
    
    dip = getdipnam(name);
    if (dip == (struct dip *)NULL) {
      fprintf(stderr, "You do not have DIP access.  Go away.\n");
      syslog(LOG_WARNING, "%s tried to access DIP: no access!", 
	     name);
      exit(-1);
    } /* endif "No User entry in diphosts file" */
    
#ifdef SKEY
    /* If it's S/Key password, don't mess with it here */
    /* Go to where the main S/Key stuff is done...     */
    if (strcmp(dip->passwd, SKEY_TOKEN) == 0)
      goto skey_check;
#endif /* SKEY */
    
#if 0
    if (strlen(dip->passwd) == 0)
      goto skey_check;
#endif
    
    /* Check whether the password check is required   */
    /* A sign of it would be non-empty password field */
    if (strlen(dip->passwd) > 0) {
	/* There is a password in the diphosts file. */
	p = getpass("password: ");    /* Get passwd from the User */  
	if ((strcmp(dip->passwd,p) != 0)    /* Is password wrong? */
	    &&
	    ( strlen(dip->passwd) > 0) /* and the passwd is there */
	    ) 
	  { 
	    fprintf(stderr, "You do not have DIP access. Go away.\n");
	    syslog(LOG_WARNING, "%s tried to access DIP: no access!", 
		   dip->name);
	    exit(-1);
	  } /* endif - wrong password */
      } else { /* dip->passwd is empty */
	  goto no_further_auth;  /* OK, skip authentication */
    } /* endif - is there a password, or it's empty? */
  } else { /* No, no User Login Name - let's prompt for it */
    char *p = getpass("slip-login: ");
    dip = getdipnam(p);
#ifdef SKEY
    /* If it's S/Key password, don't mess with it here */
    /* Go to where the main S/Key stuff is done...     */
    if (strcmp(dip->passwd, SKEY_TOKEN) == 0)
      goto skey_check;
#endif /* SKEY */
    p = getpass("password: ");
    if ((dip == (struct dip *)NULL) || (strcmp(dip->passwd,p) != 0)) {
      fprintf(stderr, "You do not have DIP access.  Go away.\n");
      syslog(LOG_WARNING, "%s tried to access DIP: no access!", 
	     dip->name);
      exit(-1);
    } /* endif - either no diphosts entry, or wrong password */
  } /* endif - was user name explicitely given? */

#ifdef SKEY
 skey_check:;
  /* check if the user has S/Key authentication */
  if (strcmp(dip->passwd, SKEY_TOKEN) == 0) {
    int	i;
    
    /* Make sure that the user has an S/Key entry - if not
       log a warning, and kick them off */
    if ((i = skey_haskey(name)) != SKEY_SUCCESS) {
      fprintf(stderr, "Cannot find your S/Key entry.\n");
      syslog(LOG_WARNING, "S/Key lookup failure for %s (%d)", 
	     name, i);
      exit(1);
    }
    
    if (skey_authenticate(name) != SKEY_SUCCESS) {
      fprintf(stderr, "S/Key authentication failure.\n");
      syslog(LOG_WARNING, "S/Key authentication failure for %s", name);
      exit(-1);
    }
  } 
#endif /* SKEY */

  /* At this point all the DIP user authentication is completed */
  /* and user is believed to be authentic and authorized.       */
 no_further_auth:;

  /* Resolve this caller's host name to an IP address. */
  if ((hp = gethostbyname(dip->remote)) == (struct hostent *)NULL) {
    syslog(LOG_ERR, "unknown host %s: %s\n",
	   dip->remote, h_errlist[h_errno]);
    exit(-1);
  }
  strncpy(dip->remote, hp->h_name, 128);
  memcpy((char *) &dip->rmt_ip, (char *) hp->h_addr_list[0], hp->h_length);

  /* Resolve the local name to what IP address server should have */
  if ((hp = gethostbyname(dip->local)) == (struct hostent *)NULL) {
	syslog(LOG_ERR, "unknown local host %s: %s\n",
			dip->local, h_errlist[h_errno]);
	exit(-1);
  }
  strncpy(dip->local, hp->h_name, 128);
  memcpy((char *) &dip->loc_ip, (char *) hp->h_addr_list[0], hp->h_length);

  /* Make sure the netmask is reasonably valid. And if not given, */
  /* make it 255.255.255.0, just like what I have in my subnet.   */
  if ((dip->netmask[0] == '\0') || (verify_inet_addr(dip->netmask) != 0))
    strcpy(dip->netmask, "255.255.255.0");
  
  /* Find out which protocol we have to use. */
  if ((i = get_prot(dip->protocol)) == 0) {
	fprintf(stderr, "dip: unknown protocol %s\n", dip->protocol);
	syslog(LOG_ERR, "%s wants unknown protocol %s",
					dip->remote, dip->protocol);
	exit(-1);
  }
  dip->protonr = i;

  /* Show some info. */
  if (opt_v == 1) {
	printf("Hostname: \"%s\" [%s]\n", dip->remote, inet_ntoa(dip->rmt_ip));
	printf("Local   : \"%s\" [%s]\n", dip->local,  inet_ntoa(dip->loc_ip));
	printf("Netmask : \"%s\"\n",      dip->netmask);
	printf("Comments: \"%s\"\n",      dip->comment);
	printf("Protocol: \"%s\" (%d)\n", dip->protocol, dip->protonr);
	printf("MTU     : %d\n", dip->mtu);
#if 0
	printf("Proxy   : %d\n", dip->proxyarp);
#endif
  }

  memset (lip, '\0', sizeof(lip));
  strcpy(lip, inet_ntoa(dip->loc_ip));
  syslog(LOG_INFO, "%s connecting %s/%s to local server %s/%s  with %s/%d",
	dip->name, 
        dip->remote, inet_ntoa(dip->rmt_ip), 
        dip->local,  lip,
        dip->protocol, dip->mtu);

  printf("Your IP address is %s ",inet_ntoa(dip->rmt_ip));
  sleep(1); (void) fflush(stdout);
  printf("Server address is %s\n",inet_ntoa(dip->loc_ip));
  sleep(1); (void) fflush(stdout);
  printf("Netmask is %s MTU is %d  Starting %s\n", 
         dip->netmask, dip->mtu, dip->protocol);
  sleep(1); (void) fflush(stdout);

  /* Initialize this terminal line for 8-bit clean operation. */
  (void) tty_open(NULL);

  /* Make tty the control terminal, detect DCD loss from now. */
  if (tty_login() < 0)
    exit(-1);
  
  /* Set up for "no messages". We do not want to be disturbed anymore. */
  (void) tty_nomesg();

  /* Enter background mode here. */
  (void) dip_login_setup(dip);

  exit(0);
}

static
int verify_inet_addr(char *p)
{
  int  i    = 0;
  int  dots = 0;
  char ch;

  for (ch = *p++; ch; ch = *p++) {
    switch (ch) {
    case '.':
      dots++;
      if (dots > 3)   return (-1);
      if ((i > 255) || (i < 0)) return (-1);
      i = 0; 
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      i = (i * 10) + (int) ch - '0';
      break;
    case '\0':
      goto end_of_loop;
    default:
      return (-1);
    }
  }
  
 end_of_loop:
  if ((dots != 3) || (i > 255) || (i < 0))
    return (-1);
  
  return 0;
}
