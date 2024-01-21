/*
 * dip		A program for handling dialup IP connecions.
 *		Handle the process of going into the background.
 *
 * Version:	@(#)daemon.c	3.3.3	08/16/93
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1993 MicroWalt Corporation
 *
 * Modified:    Uri Blumenthal, <uri@watson.ibm.com>
 *              Copyright 1994
 *
 *		Paul Cadach, <paul@paul.east.alma-ata.su>
 *		(C) 1994
 *
 *		This program is free software; you can redistribute it
 *		and/or  modify it under  the terms of  the GNU General
 *		Public  License as  published  by  the  Free  Software
 *		Foundation;  either  version 2 of the License, or  (at
 *		your option) any later version.
 */
#include "dip.h"

static int forked = 0;
static int catched = 0;
static int do_exit = 0;

extern int tty_lock(char *path, int mode);
static void (*oldalrmsig)(int);

/* Catch any signals. */
static void
sig_catcher(int sig)
{
  /* Don't let SIGHUP from terminal bother us here */
  (void) signal(SIGHUP, SIG_IGN); 

  (void) signal (SIGALRM, oldalrmsig);
  alarm (0);

#ifdef NE_PAUL
  (void) tty_close();
#else
  (void) cleanup();
#endif
  exit(0);
}


static void sig_login_catcher(int sig)
{
  (void) signal (SIGALRM, oldalrmsig);
  alarm (0);

  /* Don't let SIGHUP from terminal bother us here */
  (void) signal(SIGHUP, SIG_IGN); 

#ifdef NE_PAUL
  (void) tty_login_close();
#else
  if (do_exit == 1) {
    syslog(LOG_ERR, ">>> Line disconnected\n");
    if (opt_v) {
      fprintf(stderr, "!!! Line disconnected !!!\n");
    }
    /* Don't let SIGHUP from terminal bother us here */
    (void) signal(SIGHUP, SIG_IGN); 
    tty_setmode(1);
    log_cleanup();
    do_exit = 0;
    return;
  }
  tty_setmode(1);
  log_cleanup();
#endif
  exit(0);
}


/* Record the current processID in a file. */
static void
dip_record(void)
{
  FILE *fp;

  fp = fopen(_PATH_DIP_PID, "w");
  if (fp == NULL) {
    syslog(LOG_ERR, "DIP: cannot create %s: %s\n",
	   _PATH_DIP_PID, strerror(errno));
    fprintf(stderr, "DIP: cannot create %s: %s\n",
	    _PATH_DIP_PID, strerror(errno));
    return;
  }
  fprintf(fp, "%d", getpid());
  (void) fclose(fp);
}


static
void readifc(char *ifname, int *TX, int *RX)
{
  FILE *fp = fopen("/proc/net/dev","r");
  char buf[255];
  while (fgets(buf,sizeof(buf),fp)) {
    char *bp = buf;

    while (*bp && isspace(*bp)) bp++;

    if ((strncmp(bp,ifname,strlen(ifname)) == 0) && 
	(bp[strlen(ifname)]==':')) {
      sscanf(bp,"%*s %d %*d %*d %*d %*d %d",TX,RX);
      break;
    }
  }
  
  fclose(fp);
}


int
dip_setup(struct dip *dp)
{
  register int i;

  if (!catched) {
    catched = 1;

    oldalrmsig = signal (SIGALRM, SIG_IGN);
    for(i = 1; i < 32; i++) 
      (void) signal(i, SIG_IGN);
    (void) signal(SIGHUP,  sig_login_catcher);
    (void) signal(SIGINT,  sig_catcher);
    (void) signal(SIGTERM, sig_catcher);
    (void) signal(SIGQUIT, sig_catcher);
  }

  /* Standard BSD behaviour: change to the root dir. */
  (void) chdir(dp->home);

  /* Fire up the protocol here. */
  protosw[dp->protonr-1].func(dp);

  /* Wait forever to terminate. */
  do_exit = 1;
  while(do_exit) {
    if (dp->timeout) {
      static int oldTX=-1, 
                 oldRX=-1;
      int	 valTX,  
                 valRX;

      /* What are the current values of packets recv'd and tx'd? */
      readifc(dp->ifname,&valTX,&valRX);

      /* If line was inactive - simulate signal and suicide */
      if ((valTX == oldTX) && (valRX == oldRX)) {
	syslog(LOG_INFO, 
	       "Line inactive for %d seconds, disconnecting...\n",
		dp->timeout);
	sig_catcher(0);
      }

      oldTX = valTX;
      oldRX = valRX;
      sleep(dp->timeout);

    } else (void) sleep(30);
  }

  /* 
   * Something inside somewhat reset "do_exit" to 0. Very
   * funny, but how to deal with it? And why does it happen?
   */
#if 0
  /* Suddently I'm not sure about this any more... */
  syslog(LOG_ERR, "DIP must never be here!\n");
#endif
  return(0);
}


int dip_login_setup(struct dip *dp)
{
  int i;

  oldalrmsig = signal (SIGALRM, SIG_IGN);
  for(i = 1; i < 32; i++) 
    (void) signal(i, SIG_IGN);
  (void) signal(SIGHUP,  sig_login_catcher);
  (void) signal(SIGINT,  sig_catcher);
  (void) signal(SIGTERM, sig_catcher);
  (void) signal(SIGQUIT, sig_catcher);

  (void) chdir(dp->home);

  /* Fire up the protocol here. */
  protosw[dp->protonr-1].func(dp);

  /* Make sure "ps" command shows correct IP address for DIP */
  setproctitle("-dip (%s)", dp->remote);

  /* Wait forever to terminate. */
  while(1) {
	(void) sleep(30);
  }

  return(0);
}



int
dip_daemon(struct dip *dip)
{
  int i;

  /* First of all, if not forked yet - fork off a sub-process. */
  if (!forked) {
    if ((i = fork()) < 0) return(-1);
    if (i != 0) exit(0);
  } 

  /* Make tty the control terminal, detect DCD loss from now. */
  if ((i = tty_notlocal()) < 0)
  	return i;

  if (!forked) {

    /* Record our PID. */
    dip_record();
 
    for (i = 0; i < 3; i++)
	close(i);

    /* Disable screen output... */
    (void) open("/dev/null", O_RDONLY);  /* stdin  */
    (void) open("/dev/null", O_WRONLY);  /* stdout */
    (void) dup(1);                       /* stderr */

    /* Make the IP address visible via "ps" command */
    setproctitle("-dip (%s)",inet_ntoa(dip->loc_ip));

    /* Re-acquire the lock */
    (void)tty_lock("no_matter", 2);
    forked = 1;
  }

  return(dip_setup(dip));
}
