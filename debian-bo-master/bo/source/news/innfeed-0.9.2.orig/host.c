/* -*- c -*-
 *
 * Author:      James Brister <brister@vix.com> -- berkeley-unix --
 * Start Date:  Thu Dec 28 17:29:05 1995
 * Project:     INN (innfeed)
 * File:        host.c
 * RCSId:       $Id: host.c,v 1.22 1996/12/07 01:33:06 brister Exp $
 *
 * Copyright:   Copyright (c) 1996 by Internet Software Consortium
 *
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose with or without fee is hereby
 *              granted, provided that the above copyright notice and this
 *              permission notice appear in all copies.
 *
 *              THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE
 *              CONSORTIUM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *              SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *              MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET
 *              SOFTWARE CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT,
 *              INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *              WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *              WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 *              TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 *              USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Description: Implementation of the Host class.
 * 
 */

#if ! defined (lint)
static const char *rcsid = "$Id: host.c,v 1.22 1996/12/07 01:33:06 brister Exp $" ;
static void use_rcsid (const char *rid) {   /* Never called */
  use_rcsid (rcsid) ; use_rcsid (rid) ;
}
#endif


#include "config.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <sys/param.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <limits.h> /* LONG_MAX */

#include "host.h"
#include "tape.h"
#include "connection.h"
#include "article.h"
#include "buffer.h"
#include "endpoint.h"
#include "innlistener.h"
#include "msgs.h"

extern char *configFile ;

/* the host keeps a couple lists of these */
typedef struct proc_q_elem 
{
    Article article ;
    struct proc_q_elem *next ;
    struct proc_q_elem *prev ;
} *ProcQElem ;

struct host_s 
{
    InnListener listener ;      /* who created me. */
    char *peerName ;            /* name INN calls the remote */
    char *ipName ;              /* IP name/address from config file. */
    char **ipAddrs ;		/* the ip addresses of the remote */
    char **nextIpAddr ;		/* the next ip address to hand out */

    Connection *connections ;   /* NULL-terminated list of all connections */
    bool *cxnActive ;           /* true if the corresponding cxn is active */
    bool *cxnSleeping ;         /* true if the connection is sleeping */
    u_int maxConnections ;      /* max # of connections to set up */
    u_int activeCxns ;          /* number of connections currently active */
    u_int sleepingCxns ;        /* number of connections currently sleeping */
    u_int initialCxns ;         /* number of connections to create at start */
    Connection blockedCxn ;     /* the first connection to get the 400 banner*/
    double lowPassLow ;		/* value of low-pass filter off threshold */
    double lowPassHigh ;	/* value of low-pass filter on threshold */

    bool wantStreaming ;        /* true if config file says to stream. */
    bool remoteStreams ;        /* true if remote supports streaming */
    
    u_int maxChecks ;           /* max number of CHECK commands to send */
    u_int articleTimeout ;      /* max time to wait for new article */
    u_int responseTimeout ;     /* max time to wait for response from remote */
    u_short port ;              /* port the remote listens on */

    ProcQElem queued ;          /* articles done nothing with yet. */
    ProcQElem queuedTail ;

    ProcQElem processed ;       /* articles given to a Connection */
    ProcQElem processedTail ;
    
    TimeoutId statsId ;         /* timeout id for stats logging. */

    Tape myTape ;
    
    bool backedUp ;             /* set to true when all cxns are full */
    u_int backlog ;             /* number of arts in `queued' queue */
    u_int backlogSpooled ;      /* count of arts given to tape */

    bool loggedModeOn ;         /* true if we logged going into no-CHECK mode */
    bool loggedModeOff ;        /* true if we logged going out of no-CHECK mode */

    bool loggedBacklog ;        /* true if we already logged the fact */
    bool notifiedChangedRemBlckd ; /* true if we logged a new response 400 */
    bool removeOnReload ;	/* set to false when host is in config */

    /* these numbers get reset periodically (after a 'final' logging). */
    u_int artsOffered ;         /* # of articles we offered to remote. */
    u_int artsAccepted ;        /* # of articles succesfully transferred */
    u_int artsNotWanted ;       /* # of articles remote already had */
    u_int artsRejected ;        /* # of articles remote rejected */
    u_int artsDeferred ;        /* # of articles remote asked us to retry */
    u_int artsMissing ;         /* # of articles whose file was missing. */
    u_int artsFromTape ;        /* # of articles we pulled off tape */

    /* These numbers are as above, but for the life of the process. */
    u_int gArtsOffered ;        
    u_int gArtsAccepted ;
    u_int gArtsNotWanted ;
    u_int gArtsRejected ;
    u_int gArtsDeferred ;
    u_int gArtsMissing ;
    u_int gArtsFromTape ;

    time_t firstConnectTime ;   /* time of first connect. */
    time_t connectTime ;        /* the time the first connection was fully
                                   set up (MODE STREAM and everything
                                   else). */
    time_t spoolTime ;          /* the time the Host had to revert to
                                   spooling articles to tape. */
    time_t lastSpoolTime ;      /* the time the last time the Host had to
                                   revert to spooling articles to tape. */
    time_t nextIpLookup ;	/* time of last IP name resolution */

    Host next ;                 /* for global list of hosts. */
};




static u_int defaultArticleTimeout ;
static u_int defaultResponseTimeout ;
static u_int defaultInitialConnections ;
static u_int defaultMaxConnections ;
static u_int defaultMaxChecks ;
static bool defaultStreaming ;
static double defaultLowFilter ;
static double defaultHighFilter ;
static u_short defaultPortNumber ;

  /*
   * Host object private methods.
   */
static void articleGone (Host host, Connection cxn, Article article) ;
static void hostStopSpooling (Host host) ;
static void hostStartSpooling (Host host) ;
static void hostLogStats (Host host, bool final) ;
static void hostStatsTimeoutCbk (TimeoutId tid, void *data) ;
static void backlogToTape (Host host) ;
static void queuesToTape (Host host) ;
static bool amClosing (Host host) ;
static void hostLogStatus (void) ;
static void hostPrintStatus (Host host, FILE *fp) ;

static bool getHostInfo (FILE *fp,
                         char **name,
                         char **ipname,
                         u_int *articleTimeout,
                         u_int *responseTimeout,
                         u_int *initialConnections,
                         u_int *maxConnections,
                         u_int *maxChecks,
                         bool *streaming,
                         double *lowFilter,
                         double *highFilter,
                         u_short *portNumber) ;
static Host findHostByName (char *name) ;
static bool getOptUInt (u_int *field, char *line, u_int fNum, u_int defVal) ;
static bool getOptBool (bool *field, char *line, u_int fNum, bool defVal) ;
static bool getOptDouble (double *field, char *line, u_int fieldNum,
                          double defVal) ;

/* article queue management functions */
static Article remHead (ProcQElem *head, ProcQElem *tail) ;
static void queueArticle (Article article, ProcQElem *head, ProcQElem *tail) ;
static bool remArticle (Article article, ProcQElem *head, ProcQElem *tail) ;





/*
 * Host class data
 */

/* if true then when a Host logs its stats, it has all its connections
   log theirs too. */
static bool logConnectionStats = (bool) LOG_CONNECTION_STATS ;

/* The frequency in seconds with which a Host will log its stats. */
static u_int statsPeriod = STATS_PERIOD ;

static Host gHostList = NULL ;

static u_int gHostCount = 0 ;

static u_int maxIpNameLen = 0 ;
static u_int maxPeerNameLen = 0 ;

static time_t start ;
static char startTime [30] ;    /* for ctime(3) */
static pid_t myPid ;

/*******************************************************************/
/*                  PUBLIC FUNCTIONS                               */
/*******************************************************************/







void configHosts (bool talkSelf)
{
  FILE *fp ;
  Host nHost, h ;
  char *name, *ipName ;
  u_int artTout, respTout, initCxns, maxCxns, maxChecks ;
  double lowFilter, highFilter ;
  bool streaming ;
  u_short portNum ;

  name = ipName = NULL ;

  if ( strlen (configFile) == 0 || !fileExistsP (configFile) )
    {
      syslog (LOG_ERR, NOSUCH_CONFIG, configFile) ;
      dprintf (1,"No such config file: %s\n", configFile) ;
      exit (1) ;
    }

  if ((fp = fopen (configFile,"r")) == NULL)
    {
      syslog (LOG_ERR,FOPEN_FAILURE, configFile) ;
      exit (1) ;
    }

  syslog(LOG_NOTICE, "loading %s", configFile) ;

  openfds++ ;

  while (getHostInfo (fp, &name, &ipName, &artTout, &respTout, &initCxns,
                      &maxCxns, &maxChecks, &streaming, &lowFilter,
                      &highFilter,&portNum))
    {
      h = findHostByName (name) ;
      if ( h != NULL )
        {
	  int i ;

	  if (strcmp(h->ipName, ipName) != 0)
	    {
	      FREE (h->ipName) ;
	      h->ipName = strdup (ipName) ;
	      h->nextIpLookup = theTime () ;
	    }

	  h->articleTimeout = artTout ;
	  h->responseTimeout = respTout ;
	  h->maxChecks = maxChecks ;
	  h->wantStreaming = streaming ;
	  h->port = portNum ;
	  h->removeOnReload = false ;
 	  h->lowPassLow = lowFilter ;   /* the off threshold */
 	  h->lowPassHigh = highFilter ; /* the on threshold */

	  if (maxCxns != h->maxConnections)
	    {
	      if (maxCxns < h->maxConnections)
		{
		  for ( i = h->maxConnections ; i > maxCxns ; i-- )
		    {
		      /* XXX this is harsh, and arguably there could be a
			 cleaner way of doing it.  the problem being addressed
			 by doing it this way is that eventually a connection
			 closed cleanly via cxnClose can end up ultimately
			 calling hostCxnDead after h->maxConnections has
			 been lowered and the relevant arrays downsized.
			 If trashing the old, unallocated space in
			 hostCxnDead doesn't kill the process, the
			 ASSERT against h->maxConnections surely will.
		       */
		      cxnNuke (h->connections[i-1]) ;
		    }
		  h->maxConnections = maxCxns ;
		}

	      h->connections =
		REALLOC (h->connections, Connection, maxCxns + 1);
	      ASSERT (h->connections != NULL) ;
	      h->cxnActive = REALLOC (h->cxnActive, bool, maxCxns) ;
	      ASSERT (h->cxnActive != NULL) ;
	      h->cxnSleeping = REALLOC (h->cxnSleeping, bool, maxCxns) ;
	      ASSERT (h->cxnSleeping != NULL) ;

	      /* if maximum was raised, establish the new connexions
		 (but don't start using them).
		 XXX maybe open them based on initCxns? */
	      if (maxCxns > h->maxConnections)
		{
		  i = h->maxConnections ;
		  /* need to set h->maxConnections before cxnWait() */
		  h->maxConnections = maxCxns ;
		  while ( i < maxCxns )
		    {
		      /* XXX this does essentially the same thing that happens
			 in newHost, so they should probably be combined
			 to one new function */
		      h->connections [i] =
			newConnection (h, i,
				       h->ipName,
				       h->articleTimeout,
				       h->port,
				       h->responseTimeout,
				       CLOSE_PERIOD, lowFilter, highFilter) ;
		      h->cxnActive [i] = false ;
		      h->cxnSleeping [i] = false ;
		      cxnWait (h->connections [i]) ;
		      i++ ;
		    }
		}
	    }

	  for ( i = 0 ; i < maxCxns ; i++ )
	    cxnSetCheckThresholds (h->connections[i], lowFilter, highFilter) ;

	  /* XXX how to handle initCxns change? */
        }
      else
        {
	  nHost = newHost (mainListener,name,ipName,artTout,respTout,initCxns,
			   maxCxns,maxChecks,portNum,CLOSE_PERIOD,
			   streaming,lowFilter,highFilter) ;
	  
	  /* this is where we'll die if the locks are still held. */
	  if (nHost == 0)
	    {
	      syslog (LOG_ERR,NO_HOST,name) ;
	    }
	  else 
	    {
	      if (initCxns == 0 && talkSelf)
		syslog (LOG_NOTICE,BATCH_AND_NO_CXNS,name) ;
	      
	      dprintf (1,"Adding %s %s article (%d) response (%d) initial (%d) max con (%d) max checks (%d) portnumber (%d) streaming (%s) lowFilter (%.2f) highFilter (%.2f)\n",
		       name, ipName, artTout, respTout, initCxns, maxCxns,
		       maxChecks, portNum, streaming ? "true" : "false",
		       lowFilter, highFilter) ;
	      
	      if ( !listenerAddPeer (mainListener,nHost) )
		die ("failed to add a new peer\n") ;
	    }
	}
    }
  

  if ( !feof (fp) )
    {
      syslog (LOG_ERR,PARSE_FAILURE) ;
      exit (1) ;
    }

  fclose (fp) ;
  openfds-- ;

  for (h = gHostList; h != NULL; h = h->next)
    if (h->removeOnReload)
      hostClose (h) ;
    else
      /* prime it for the next config file read */
      h->removeOnReload = true ;
}

/*
 * Create a new Host object. Called by the InnListener.
 */

Host newHost (InnListener listener,
              const char *name, 
              const char *ipName,
              u_int artTimeout, 
              u_int respTimeout,
              u_int initialCxns,
              u_int maxCxns,
              u_int maxCheck,
              u_short portNum,
              u_int closePeriod,
              bool streaming,
              double lowPassLow,
              double lowPassHigh) 
{
  u_int i ;
  Host nh ; 

  ASSERT (maxCxns > 0 && maxCxns < MAX_CONNECTION_COUNT) ;
  ASSERT (maxCheck > 0) ;

  nh =  CALLOC (struct host_s, 1) ;
  ASSERT (nh != NULL) ;

  nh->listener = listener ;
  nh->peerName = strdup (name) ;
  nh->ipName = strdup (ipName) ;

  nh->connections = CALLOC (Connection, maxCxns + 1) ;
  ASSERT (nh->connections != NULL) ;

  nh->cxnActive = CALLOC (bool, maxCxns) ;
  ASSERT (nh->cxnActive != NULL) ;

  nh->cxnSleeping = CALLOC (bool, maxCxns) ;
  ASSERT (nh->cxnSleeping != NULL) ;

  nh->maxConnections = maxCxns ;
  nh->activeCxns = 0 ;
  nh->sleepingCxns = 0 ;
  nh->initialCxns = initialCxns ;
  nh->lowPassLow = lowPassLow ;
  nh->lowPassHigh = lowPassHigh ;

  nh->blockedCxn = NULL ;
  nh->maxChecks = maxCheck ;
  nh->articleTimeout = artTimeout ;
  nh->responseTimeout = respTimeout ;
  nh->port = portNum ;
  nh->wantStreaming = streaming ;

  nh->queued = NULL ;
  nh->queuedTail = NULL ;

  nh->processed = NULL ;
  nh->processedTail = NULL ;
  
  nh->statsId = 0 ;

  /* Create the Host's tape. If we're only working off backup (listener is
     a dummy InnListener), then any articles we spool to tape don't get
     read back in. */
  nh->myTape = newTape (name,listenerIsDummy (listener)) ;
  if (nh->myTape == NULL)
    {                           /* tape couldn't be locked, probably */
      FREE (nh) ;
      return NULL ;
    }

  nh->backedUp = false ;
  nh->backlog = 0 ;
  nh->backlogSpooled = 0 ;

  nh->loggedBacklog = false ;
  nh->loggedModeOn = false ;
  nh->loggedModeOff = false ;
  nh->notifiedChangedRemBlckd = false ;
  nh->removeOnReload = false ;

  nh->artsOffered = 0 ;
  nh->artsAccepted = 0 ;
  nh->artsNotWanted = 0 ;
  nh->artsRejected = 0 ;
  nh->artsDeferred = 0 ;
  nh->artsMissing = 0 ;

  nh->gArtsOffered = 0 ;
  nh->gArtsAccepted = 0 ;
  nh->gArtsNotWanted = 0 ;
  nh->gArtsRejected = 0 ;
  nh->gArtsDeferred = 0 ;
  nh->gArtsMissing = 0 ;
  
  nh->firstConnectTime = 0 ;
  nh->connectTime = 0 ;
  
  nh->spoolTime = 0 ;

  /* Create all the connections, but only the initial ones connect
     immediately */
  for (i = 0 ; i < maxCxns ; i++)
    {
      nh->cxnActive [i] = false ;
      nh->cxnSleeping [i] = false ;
      nh->connections [i] = newConnection (nh,
                                           i,
                                           nh->ipName,
                                           nh->articleTimeout,
                                           nh->port,
                                           nh->responseTimeout,
                                           closePeriod,
                                           nh->lowPassLow,
                                           nh->lowPassHigh) ;
      if (i < initialCxns)
        cxnConnect (nh->connections [i]) ;
      else
        cxnWait (nh->connections [i]) ;
    }

  nh->connections [maxCxns] = NULL ;

  nh->next = gHostList ;
  gHostList = nh ;
  gHostCount++ ;

  if (maxIpNameLen == 0)
    {
      start = theTime() ;
      strcpy (startTime,ctime (&start)) ;
      myPid = getpid() ;
    }
  
  if (strlen (nh->ipName) > maxIpNameLen)
    maxIpNameLen = strlen (nh->ipName) ;
  if (strlen (nh->peerName) > maxPeerNameLen)
    maxPeerNameLen = strlen (nh->peerName) ;
  
  return nh ;
}

struct in_addr *hostIpAddr (Host host)
{
  int i ;
  char *p ;
  char **newIpAddrs = NULL;
  struct in_addr ipAddr, *returnAddr ;
  struct hostent *hostEnt ;

  /* check to see if need to look up the host name */
  if (host->nextIpLookup <= theTime () )
    {
      /* see if the ipName we're given is a dotted quad */
      if ( !inet_aton (host->ipName,&ipAddr) )
	{
	  if ((hostEnt = gethostbyname (host->ipName)) == NULL)
	    syslog (LOG_ERR, HOST_RESOLV_ERROR, host->peerName, host->ipName,
		    host_err_str ()) ;
	  else
	    {
	      /* figure number of pointers that need space */
	      for (i = 0 ; hostEnt->h_addr_list[i] ; i++)
		;
	      i++;		

	      newIpAddrs =
		(char **) MALLOC ( (i * sizeof(char *)) +
				  ( (i - 1) * hostEnt->h_length) ) ;
	      ASSERT (newIpAddrs != NULL) ;

	      /* copy the addresses from gethostbyname() static space */
	      p = (char *)&newIpAddrs [ i ] ;
	      i = 0;
	      for (i = 0 ; hostEnt->h_addr_list[i] ; i++)
		{
		  newIpAddrs[i] = p;
		  memcpy (p, hostEnt->h_addr_list[i], hostEnt->h_length) ;
		  p += hostEnt->h_length ;
		}
	      newIpAddrs[i] = NULL ;
	    }
	}
      else
	{
	  newIpAddrs = (char **) MALLOC (2 * sizeof(char *) + sizeof(ipAddr)) ;
	  ASSERT (newIpAddrs != NULL) ;
	  p = (char *)&newIpAddrs [ 2 ];
	  newIpAddrs[0] = p;
	  memcpy (p, (char *)&ipAddr, sizeof(ipAddr)) ;
	  newIpAddrs[1] = NULL;
	}

      if (newIpAddrs)
	{
	  if (host->ipAddrs)
	    FREE (host->ipAddrs) ;
	  host->ipAddrs = newIpAddrs ;
	  host->nextIpAddr = host->ipAddrs ;
	  /* XXX should be configurable */
	  host->nextIpLookup = theTime () + 86400 ;
	}
      else
	{
	  /* failed to setup new addresses */
	  /* XXX should be configurable */
	  host->nextIpLookup = theTime () + 900 ;
	}
    }

  if (host->ipAddrs)
    {
      returnAddr = (struct in_addr *)(host->nextIpAddr[0]) ;
      if (*(++host->nextIpAddr) == NULL)
	host->nextIpAddr = host->ipAddrs ;
    }
  else
    returnAddr = NULL ;

  return returnAddr ;
}


void gPrintHostInfo (FILE *fp, u_int indentAmt)
{
  Host h ;
  char indent [INDENT_BUFFER_SIZE] ;
  u_int i ;
  
  for (i = 0 ; i < MIN(INDENT_BUFFER_SIZE - 1,indentAmt) ; i++)
    indent [i] = ' ' ;
  indent [i] = '\0' ;
  
  fprintf (fp,"%sGlobal Host list : (count %d) {\n",indent,gHostCount) ;
  
  for (h = gHostList ; h != NULL ; h = h->next)
    printHostInfo (h,fp,indentAmt + INDENT_INCR) ;
  
  fprintf (fp,"%s}\n",indent) ;
}


void printHostInfo (Host host, FILE *fp, u_int indentAmt)
{
  char indent [INDENT_BUFFER_SIZE] ;
  u_int i ;
  Connection *cxn ;
  ProcQElem qe ;
  
  for (i = 0 ; i < MIN(INDENT_BUFFER_SIZE - 1,indentAmt) ; i++)
    indent [i] = ' ' ;
  indent [i] = '\0' ;

  fprintf (fp,"%sHost : %p {\n",indent,host) ;

  if (host == NULL)
    {
      fprintf (fp,"%s}\n",indent) ;
      return ;
    }
  
  fprintf (fp,"%s    peer-name : %s\n",indent,host->peerName) ;
  fprintf (fp,"%s    ip-name : %s\n",indent,host->ipName) ;
  fprintf (fp,"%s    max-connections : %d\n",indent,host->maxConnections) ;
  fprintf (fp,"%s    active-connections : %d\n",indent,host->activeCxns) ;
  fprintf (fp,"%s    sleeping-connections : %d\n",indent,host->sleepingCxns) ;
  fprintf (fp,"%s    remote-streams : %s\n",indent,
           boolToString (host->remoteStreams)) ;
  fprintf (fp,"%s    max-checks : %d\n",indent,host->maxChecks) ;
  fprintf (fp,"%s    article-timeout : %d\n",indent,host->articleTimeout) ;
  fprintf (fp,"%s    response-timeout : %d\n",indent,host->responseTimeout) ;
  fprintf (fp,"%s    port : %d\n",indent,host->port) ;
  fprintf (fp,"%s    statistics-id : %d\n",indent,host->statsId) ;
  fprintf (fp,"%s    backed-up : %s\n",indent,boolToString (host->backedUp));
  fprintf (fp,"%s    backlog : %d\n",indent,host->backlog) ;
  fprintf (fp,"%s    backlogSpooled : %d\n",indent,host->backlogSpooled) ;
  fprintf (fp,"%s    loggedModeOn : %s\n",indent,
           boolToString (host->loggedModeOn)) ;
  fprintf (fp,"%s    loggedModeOff : %s\n",indent,
           boolToString (host->loggedModeOff)) ;
  fprintf (fp,"%s    logged-backlog : %s\n",indent,
           boolToString (host->loggedBacklog)) ;
  fprintf (fp,"%s    streaming-type changed : %s\n",indent,
           boolToString (host->notifiedChangedRemBlckd)) ;
  fprintf (fp,"%s    articles offered : %d\n",indent,host->artsOffered) ;
  fprintf (fp,"%s    articles accepted : %d\n",indent,host->artsAccepted) ;
  fprintf (fp,"%s    articles not wanted : %d\n",indent,
           host->artsNotWanted) ;
  fprintf (fp,"%s    articles rejected : %d\n",indent,host->artsRejected);
  fprintf (fp,"%s    articles deferred : %d\n",indent,host->artsDeferred) ;
  fprintf (fp,"%s    articles missing : %d\n",indent,host->artsMissing) ;

  fprintf (fp,"%s    process articles offered : %d\n",indent,
           host->gArtsOffered) ;
  fprintf (fp,"%s    process articles accepted : %d\n",indent,
           host->gArtsAccepted) ;
  fprintf (fp,"%s    process articles not wanted : %d\n",indent,
           host->gArtsNotWanted) ;
  fprintf (fp,"%s    process articles rejected : %d\n",indent,
           host->gArtsRejected);
  fprintf (fp,"%s    process articles deferred : %d\n",indent,
           host->gArtsDeferred) ;
  fprintf (fp,"%s    process articles missing : %d\n",indent,
           host->gArtsMissing) ;

  fprintf (fp,"%s    firstConnectTime : %s",indent,
           ctime (&host->firstConnectTime));
  fprintf (fp,"%s    connectTime : %s",indent,ctime (&host->connectTime));
  fprintf (fp,"%s    spoolTime : %s",indent,ctime (&host->spoolTime)) ;
  fprintf (fp,"%s    last-spool-time : %s",indent,
           ctime (&host->lastSpoolTime)) ;
  
#if 0
  fprintf (fp,"%s    tape {\n",indent) ;
  printTapeInfo (host->myTape,fp,indentAmt + INDENT_INCR) ;
  fprintf (fp,"%s    }\n",indent) ;
#else
  fprintf (fp,"%s    tape : %p\n",indent,host->myTape) ;
#endif
  
  fprintf (fp,"%s    QUEUED articles {\n",indent) ;
  for (qe = host->queued ; qe != NULL ; qe = qe->next)
    {
#if 0
      printArticleInfo (qe->article,fp,indentAmt + INDENT_INCR) ;
#else
      fprintf (fp,"%s    %p\n",indent,qe->article) ;
#endif
    }
  
  fprintf (fp,"%s    }\n",indent) ;
  
  fprintf (fp,"%s    IN PROCESS articles {\n",indent) ;
  for (qe = host->processed ; qe != NULL ; qe = qe->next)
    {
#if 0
      printArticleInfo (qe->article,fp,indentAmt + INDENT_INCR) ;
#else
      fprintf (fp,"%s    %p\n",indent,qe->article) ;
#endif
    }
  
  fprintf (fp,"%s    }\n",indent) ;

  
  
  fprintf (fp,"%s    Connections {\n",indent) ;
  for (cxn = host->connections ; cxn != NULL && *cxn != NULL ; cxn++)
    {
#if 0
    printCxnInfo (*cxn,fp,indentAmt + INDENT_INCR) ;
#else
    fprintf (fp,"%s        %p\n",indent,*cxn) ;
#endif
    }
  fprintf (fp,"%s    }\n",indent) ;

  fprintf (fp,"%s    Active Connections {\n%s        ",indent,indent) ;
  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->cxnActive[i])
      fprintf (fp," [%d:%p]",i,host->connections[i]) ;
  fprintf (fp,"\n%s    }\n",indent) ;

  fprintf (fp,"%s    Sleeping Connections {\n%s        ",indent,indent) ;
  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->cxnSleeping[i])
      fprintf (fp," [%d:%p]",i,host->connections[i]) ;
  fprintf (fp,"\n%s    }\n",indent) ;

  fprintf (fp,"%s}\n",indent) ;
}







/* close down all the connections of the Host. All articles that are in
 * processes are still pushed out and then a QUIT is issued. The Host will
 * also spool all inprocess articles to tape incase the process is about to
 * be killed (they'll be refused next time around). When all Connections
 * report that they're gone, then the Host will delete itself.
 */
void hostClose (Host host)
{
  u_int i ;

  dprintf (1,"Closing host %s\n",host->peerName) ;
  
  queuesToTape (host) ;
  delTape (host->myTape) ;
  host->myTape = NULL ;
  
  hostLogStats (host,true) ;

  clearTimer (host->statsId) ;
  
  host->connectTime = 0 ;

  for (i = 0 ; i < host->maxConnections ; i++)
    cxnTerminate (host->connections [i]) ;
}







/*
 * have the Host transmit the Article if possible.
 */
void hostSendArticle (Host host, Article article)
{
  if (host->spoolTime > 0)
    {                           /* all connections are asleep */
      host->backlogSpooled++ ;
      tapeTakeArticle (host->myTape, article) ;
    }
  else                 /* at least one connection is feeding or waiting */
    {
      u_int idx ;
      Article extraRef ;
      bool taken = false ;
      
      extraRef = artTakeRef (article) ; /* the referrence we give away */
      
      /* stick on the queue of articles we've handed off--we're hopeful. */
      queueArticle (article,&host->processed,&host->processedTail) ;

      /* first we try to give it to one of our active connections. We
         simply start at the bottom and work our way up. This way
         connections near the end of the list will get closed sooner from
         idleness. */
      for (idx = 0 ; !taken && idx < host->maxConnections ; idx++)
        {
          if (host->cxnActive [idx])
            taken = cxnTakeArticle (host->connections [idx],extraRef) ;
        }

      if ( !taken )
        {
          /* Wasn't taken so try to give it to one of the waiting
             connections. */
          for (idx = 0 ; idx < host->maxConnections ; idx++)
            if (!host->cxnActive [idx] && !host->cxnSleeping [idx])
              {
                if (cxnTakeArticle (host->connections [idx], extraRef))
                  break ;
                else
                  dprintf (1,"%s Inactive connection %d refused an article\n",
                           host->peerName,idx) ;
              }

          if (idx == host->maxConnections)
            {
              /* this'll happen if all connections are feeding and all
                 their queues are full, or if those not feeding are asleep. */
              dprintf (1, "Couldn't give the article to a connection\n") ;
              
              delArticle (extraRef) ;
                  
              remArticle (article,&host->processed,&host->processedTail) ;
              queueArticle (article,&host->queued,&host->queuedTail) ;
              
              host->backlog++ ;
              if (host->backlog > HOST_HIGHWATER)
                backlogToTape (host) ;
            }
        }
    }
}







/*
 * called by the Host's connection when the remote is refusing postings
 * from us becasue we're not allowed (banner code 400).
 */
void hostCxnBlocked (Host host, Connection cxn, char *reason)
{
#ifndef NDEBUG
  {
    u_int i ;
    
    for (i = 0 ; i < host->maxConnections ; i++)
      if (host->connections [i] == cxn)
        ASSERT (host->cxnActive [i] == false) ;
  }
#endif

  if (host->activeCxns == 0 && host->spoolTime == 0)
    {
      host->blockedCxn = cxn ;  /* to limit log notices */
      syslog (LOG_NOTICE,REMOTE_BLOCKED, host->peerName, reason) ;
    }
  else if (host->activeCxns > 0 && !host->notifiedChangedRemBlckd)
    {
      syslog (LOG_NOTICE,CHANGED_REMOTE_BLOCKED, host->peerName,reason) ;
      host->notifiedChangedRemBlckd = true ;
    }
  else if (host->spoolTime != 0 && host->blockedCxn == cxn)
    {
      syslog (LOG_NOTICE,REMOTE_STILL_BLOCKED, host->peerName, reason) ;
    }
  
}







/*
 * Called by the Connection when it gets a response back to the MODE
 * STREAM command. It's now that we consider the connection usable.
 */
void hostRemoteStreams (Host host, Connection cxn, bool doesStreaming)
{
  u_int i ;

  host->blockedCxn = NULL ;
  
  /* we may have told the connection to quit while it was in the middle
     of connecting */
  if (amClosing (host))
    return ;
  
  if (host->connectTime == 0)   /* first connection for this cycle. */
    {
      if (doesStreaming && host->wantStreaming)
        syslog (LOG_NOTICE, REMOTE_DOES_STREAMING, host->peerName);
      else if (doesStreaming)
        syslog (LOG_NOTICE, REMOTE_STREAMING_OFF, host->peerName);
      else
        syslog (LOG_NOTICE, REMOTE_NO_STREAMING, host->peerName);

      if (host->spoolTime > 0)
        hostStopSpooling (host) ;

      /* set up the callback for statistics logging. */
      if (host->statsId != 0)
        clearTimer (host->statsId) ;
      host->statsId = prepareSleep (hostStatsTimeoutCbk, statsPeriod, host) ;

      host->remoteStreams = (host->wantStreaming ? doesStreaming : false) ;

      host->connectTime = theTime() ;
      if (host->firstConnectTime == 0)
        host->firstConnectTime = host->connectTime ;
    }
  else if (host->remoteStreams != doesStreaming && host->wantStreaming)
    syslog (LOG_NOTICE,STREAMING_CHANGE,host->peerName) ;

  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->connections [i] == cxn)
      {
        host->cxnActive [i] = true ;
        if (host->cxnSleeping [i])
          host->sleepingCxns-- ;
        host->cxnSleeping [i] = false ;
        break ;
      }

  ASSERT (i != host->maxConnections) ;

  host->activeCxns++ ;

  hostLogStatus () ;
}







/*
 * Called by the connection when it is no longer connected to the
 * remote. Perhaps due to getting a code 400 to an IHAVE, or due to a
 * periodic close.
 */
void hostCxnDead (Host host, Connection cxn)
{
  u_int i ;
    
  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->connections [i] == cxn)
      {
        if (host->cxnActive [i]) /* won't be active if got 400 on banner */
          {
            host->cxnActive [i] = false ;
            host->activeCxns-- ;

            if (!amClosing (host) && host->activeCxns == 0)
              {
                clearTimer (host->statsId) ;
                hostLogStats (host,true) ;
                host->connectTime = 0 ;
              }
          }

        break ;
      }

  ASSERT (i < host->maxConnections) ;
  hostLogStatus () ;
}







/*
 * Called by the Connection when it is going to sleep so the Host won't
 * bother trying to give it Articles
 */
void hostCxnSleeping (Host host, Connection cxn)
{
  u_int i ;

  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->connections [i] == cxn)
      {
        if (!host->cxnSleeping [i]) 
          {
            host->cxnSleeping [i] = true ;
            host->sleepingCxns++ ;
          }

        if (host->spoolTime == 0 && host->sleepingCxns >= host->maxConnections)
          hostStartSpooling (host) ;

        break ;
      }

  ASSERT (i < host->maxConnections) ;

  hostLogStatus () ;
}







/*
 * Called by the Connection when it goes into the waiting state.
 */
void hostCxnWaiting (Host host, Connection cxn)
{
  u_int i ;

  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->connections [i] == cxn)
      {
        if (host->cxnSleeping [i])
          host->sleepingCxns-- ;
        host->cxnSleeping [i] = false ;
        break ;
      }

  ASSERT (i < host->maxConnections) ;

  if (host->spoolTime > 0)
    hostStopSpooling (host) ;

  hostLogStatus () ;
}







/*
 * Called by the Connection when it is about to delete itself.
 */
bool hostCxnGone (Host host, Connection cxn)
{
  u_int i;
  bool oneThere = false ;

  /* forget about the Connection and see if we are still holding any live
     connections still. */
  for (i = 0 ; i < host->maxConnections ; i++)
    if (host->connections [i] == cxn)
      {
        if (!amClosing (host))
          syslog (LOG_ERR,CONNECTION_DISAPPEARING,host->peerName,i) ;
        host->connections [i] = NULL ;
        host->cxnActive [i] = false ;
        host->activeCxns-- ;
      }
    else if (host->connections [i] != NULL)
      oneThere = true ;

  /* remove the host if it has no connexions */
  if ( !oneThere )
    {
      time_t now = theTime() ;
      u_int hostsLeft ;

      if (host->firstConnectTime > 0)
        syslog (LOG_NOTICE,REALLY_FINAL_STATS,
                host->peerName,
                (long) (now - host->firstConnectTime),
                host->gArtsOffered, host->gArtsAccepted,
                host->gArtsNotWanted, host->gArtsRejected,
                host->gArtsMissing) ;

      hostsLeft = listenerHostGone (host->listener, host) ;
      delHost (host) ;

      /* return true if that was the last host */
      return hostsLeft == 0 ;
    }

  /* return false because there is still at least one host (this one) */
  return false ;
}







/*
 * The connections has offered an article to the remote.
 */
void hostArticleOffered (Host host, Connection cxn) 
{
  (void) cxn ;                  /* keep lint happy. */
  
  host->artsOffered++ ;
  host->gArtsOffered ++ ;
}







/*
 * Article was succesfully transferred.
 */
void hostArticleAccepted (Host host, Connection cxn, Article article)
{
  const char *filename = artFileName (article) ;
  const char *msgid = artMsgId (article) ;

  dprintf (5,"Article %s (%s) was transferred\n", msgid, filename) ;
  
  host->artsAccepted++ ;
  host->gArtsAccepted++ ;

  /* host has two references to the article here... the parameter `article'
     and the queue */

  delArticle (article) ;        /* drop the parameter reference */

  if (!amClosing (host))
    articleGone (host,cxn,article) ; /* and the one in the queue */
}







/*
 * remote said no thanks to an article.
 */
void hostArticleNotWanted (Host host, Connection cxn, Article article)
{
  const char *filename = artFileName (article) ;
  const char *msgid = artMsgId (article) ;

  dprintf (5,"Article %s (%s) was not wanted\n", msgid, filename) ;
  
  host->artsNotWanted++ ;
  host->gArtsNotWanted++ ;
  
  
  /* host has two references to the article here... `article' and the
     queue */

  delArticle (article) ;        /* drop the `article' reference */
  
  if (!amClosing (host)) 
    articleGone (host,cxn,article) ; /* and the one in the queue */
}







/*
 * remote rejected the article after it was was transferred
 */
void hostArticleRejected (Host host, Connection cxn, Article article) 
{
  const char *filename = artFileName (article) ;
  const char *msgid = artMsgId (article) ;

  dprintf (5,"Article %s (%s) was rejected\n", msgid, filename) ;
  
  host->artsRejected++ ;
  host->gArtsRejected++ ;

  /* host has two references to the article here... `article' and the queue */

  delArticle (article) ;        /* drop the `article' reference */

  if (!amClosing (host))
    articleGone (host,cxn,article) ;
}







/*
 * The remote wants us to retry the article later.
 */
void hostArticleDeferred (Host host, Connection cxn, Article article) 
{
  host->artsDeferred++ ;
  host->gArtsDeferred++ ;

  host->backlogSpooled++ ;

  if (!amClosing (host))
    {
      tapeTakeArticle (host->myTape,article) ;
      articleGone (host,cxn,article) ; /* drop from the queue */
    }
  else
    delArticle(article); /*drop parameter reference if not sent to tape*/
}







/*
 * The Connection is giving the article back to the Host, but it doesn't
 * want a new one in return.
 */
void hostTakeBackArticle (Host host, Connection cxn, Article article) 
{
  (void) cxn ;                  /* keep lint happy */
  
  if (!amClosing (host)) 
    {
      tapeTakeArticle (host->myTape,article) ;
      articleGone (host,NULL,article) ; /* drop from the queue */
    }
  else
    delArticle(article); /*drop parameter reference if not sent to tape*/

}







/*
 * The disk file for the article is no longer valid
 */
void hostArticleIsMissing (Host host, Connection cxn, Article article)
{
  const char *filename = artFileName (article) ;
  const char *msgid = artMsgId (article) ;

  dprintf (5, "%s article is missing %s %s\n", host->peerName, msgid, filename) ;
    
  host->artsMissing++ ;
  host->gArtsMissing++ ;

  /* host has two references to the article here... `article' and the
     queue */

  delArticle (article) ;        /* drop the `article' reference */

  if (!amClosing (host))
    articleGone (host,cxn,article) ; /* and the one in the queue */
}







/* The Connection wants something to do. This is called by the Connection
 * after it has transferred an article. This is what keeps the pipes full
 * of data off the tapes if the input from inn is idle.
 */
bool hostGimmeArticle (Host host, Connection cxn)
{
  Article article = NULL ;
  bool gaveSomething = false ;
  size_t amtToGive = cxnQueueSpace (cxn) ; /* may be more than one */

  if (amClosing (host))
    {
      dprintf (5,"%s no article to give due to closing\n",host->peerName) ;

      return false ;
    }

  if (amtToGive == 0)
    dprintf (5,"%s Queue space is zero....\n",host->peerName) ;
  
  while (amtToGive > 0)
    {
      bool tookIt ;
      
      if ((article = remHead (&host->queued,&host->queuedTail)) != NULL)
        {
          host->backlog-- ;
          tookIt = cxnQueueArticle (cxn,artTakeRef (article)) ;

          ASSERT (tookIt == true) ;

          queueArticle (article,&host->processed,&host->processedTail) ;
          amtToGive-- ;

          gaveSomething = true ;
        }
      else if ((article = getArticle (host->myTape)) != NULL) 
        {                       /* go to the tapes */
          tookIt = cxnQueueArticle (cxn,artTakeRef (article)) ;

          ASSERT (tookIt == true) ;

          host->artsFromTape++ ;
          host->gArtsFromTape++ ;
          queueArticle (article,&host->processed,&host->processedTail) ;
          amtToGive-- ;

          gaveSomething = true ;
        }
      else
        {
          /* we had nothing left to give... */
          
          if (host->processed == NULL) /* and if nothing outstanding... */
            listenerHostIsIdle (host->listener,host) ; /* tell our owner */
  
          amtToGive = 0 ;
        }
    }

  return gaveSomething ;
}







/*
 * get the name that INN uses for this host
 */
const char *hostPeerName (Host host)
{
  ASSERT (host != NULL) ;
    
  return host->peerName ;
}


/* return true if the Connections for this host should attempt to do
   streaming. */
bool hostWantsStreaming (Host host)
{
  return host->wantStreaming ;
}

u_int hostMaxChecks (Host host)
{
  return host->maxChecks ;
}







/**********************************************************************/
/**                       CLASS FUNCTIONS                            **/
/**********************************************************************/

/*
 * Set the state of whether each Connection is told to log its stats when
 * its controlling Host logs its stats.
 */
void hostLogConnectionStats (bool val)
{
  logConnectionStats = val ;
}


bool hostLogConnectionStatsP (void)
{
  return logConnectionStats ;
}


/*
 * Set the number of seconds between each logging done by the Hosts.
 */
void hostSetStatsPeriod (u_int period) 
{
  statsPeriod = period ;
}


/*
 * Called by one of the Host's Connection's when it (the Connection)
 * switches into or out of no-CHECK mode.
 */
void hostLogNoCheckMode (Host host, bool on)
{
  if (on && host->loggedModeOn == false)
    {
      syslog (LOG_NOTICE, STREAMING_MODE_SWITCH, host->peerName) ;
      host->loggedModeOn = true ;
    }
  else if (!on && host->loggedModeOff == false) 
    {
      syslog (LOG_NOTICE, STREAMING_MODE_UNDO, host->peerName) ;
      host->loggedModeOff = true ;
    }
}




/**********************************************************************/
/**                      PRIVATE FUNCTIONS                           **/
/**********************************************************************/







  /* parse lines of the form used in the config file:

     name:fqdn:article-timeout:response-timeout:initial-connections:
     max-connections:max-q-size:streaming:low-filter:high-filter:portnum

     XXX i'm not handling continuation lines yet.
     */


static bool getHostInfo (FILE *fp,
                         char **name,
                         char **ipName,
                         u_int *articleTimeout,
                         u_int *responseTimeout,
                         u_int *initialConnections,
                         u_int *maxConnections,
                         u_int *maxChecks,
                         bool *streaming,
                         double *lowFilter,
                         double *highFilter,
                         u_short *portNumber) 
{
  static bool seenDefault ;
  static char line [1024] ;
  char *p, *l, copyLine [1024] ;
  char *field1 = 0;
  char *field2 = 0;
  u_int field3, field4, field5, field6, field7 ;
  bool field8 ;
  double field9, field10 ;
  u_int field11 ;
  bool gotSomething = false ;

  while (fgets (line, sizeof (line), fp) != NULL)
    {
      gotSomething = false ;
      dprintf (1,"Doing config line: %s",line) ;
      
      strcpy (copyLine,line) ;  /* copyLine is used for syslog only */
      
      for (l = line ; *l != '\0' && isspace (*l) ; l++)
          /* nada */ ;
      
      if ((p = strchr (l,'#')) == NULL)
        p = l + strlen (l) - 1 ;
      else
        p-- ;

      while (p > l && isspace (*p))
        p-- ;

      *++p = '\0' ;

      if (strlen (l) == 0) 
        continue ;

        /* field 1 the system name */
      if (((field1 = mystrtok (l,":")) == NULL) || (strlen (field1) == 0))
        {
          dprintf (1,"Bad system name in config file line: %s\n", copyLine) ;
          return false ;
        }
      trim_ws (field1) ;

        /* field 2 the fqdn */
      if ((field2 = mystrtok (NULL,":")) == NULL)
        {
          dprintf (1,"Bad system IP name in config file line: %s\n", copyLine) ;
          return false ;
        }
      trim_ws (field2) ;

      if ((strcmp (field1, "default") != 0) && (strlen (field2) == 0))
        {
          dprintf (1,"f2 - 1\n") ;
          return false ;
        }
      else if ((strcmp (field1, "default") == 0) && (strlen (field2) != 0))
        {
          dprintf (1,"f2 - 2\n") ;
          return false ;
        }


        /* field 3 the article timeout */
      if ( !getOptUInt (&field3,copyLine,3,defaultArticleTimeout) )
        return false ;

        /* field 4 the response timeout */
      if ( !getOptUInt (&field4,copyLine,4,defaultResponseTimeout) )
        return false ;

        /* field 5 the number of initial connections to create */
      if ( !getOptUInt (&field5,copyLine,5,defaultInitialConnections) )
        return false ;
      
        /* field 6 the max # of connections */
      if ( !getOptUInt (&field6,copyLine,6,defaultMaxConnections) )
        return false ;

        /* field 7 the max number of checks */
      if ( !getOptUInt (&field7,copyLine,7,defaultMaxChecks) )
        return false ;

      /* field 8 the streaming flag */
      if ( !getOptBool (&field8,copyLine,8,defaultStreaming) )
        return false ;

      /* field 9 the low-pass filter low value. */
      if ( !getOptDouble (&field9,copyLine,9,defaultLowFilter) )
        return false ;

      /* field 10 the low-pass filter high value */
      if ( !getOptDouble (&field10,copyLine,10,defaultHighFilter) )
        return false ;

      /* field 11 the port number */
      if ( !getOptUInt (&field11,copyLine,11,defaultPortNumber) )
        return false ;

      if (field9 > field10)
        {
          syslog (LOG_ERR,LOW_GREATER,field1) ;
          exit (1) ;
        }
      
      if (strcmp (field1,"default") != 0)
        {
          gotSomething = true ;
          break ;
        }

      seenDefault = true ;
      
      defaultArticleTimeout = field3 ;
      defaultResponseTimeout = field4 ;
      defaultInitialConnections = field5 ;
      defaultMaxConnections = field6 ;
      defaultMaxChecks = field7 ;
      defaultStreaming = field8 ;
      defaultLowFilter = field9 ;
      defaultHighFilter = field10 ;
      defaultPortNumber = field11 ;

      dprintf (1,"defaults are set %d %d %d %d %d %s %.2f %.2f %d\n",
               defaultArticleTimeout,
               defaultResponseTimeout,
               defaultInitialConnections,
               defaultMaxConnections,
               defaultMaxChecks,
               defaultStreaming ? "true" : "false",
               defaultLowFilter,
               defaultHighFilter,
               defaultPortNumber
               ) ;
      
    }

  if ( !gotSomething )
    return false ;

  if ( !seenDefault )
    {
      syslog (LOG_ERR,NO_DEFAULT) ;
      return false ;
    }

  *name = field1 ;
  *ipName = field2 ;
  *articleTimeout = field3 ;
  *responseTimeout = field4 ;
  *initialConnections = field5 ;
  *maxConnections = field6 ;
  *maxChecks = field7 ;
  *streaming = field8 ;
  *lowFilter = field9 ;
  *highFilter = field10 ;
  *portNumber = field11 ;

  return true ;
}



void getHostDefaults (u_int *articleTout,
                      u_int *respTout,
                      u_int *initialCxns,
                      u_int *maxCxns,
                      u_int *maxChecks,
                      bool *streaming,
                      double *lowFilter,
                      double *highFilter,
                      u_short *portNum)
{
  ASSERT (defaultMaxConnections > 0) ;
  
  ASSERT (articleTout != NULL) ;
  ASSERT (respTout != NULL) ;
  ASSERT (initialCxns != NULL) ;
  ASSERT (maxCxns != NULL) ;
  ASSERT (maxChecks != NULL) ;
  ASSERT (portNum != NULL) ;
  ASSERT (streaming != NULL) ;
  ASSERT (lowFilter != NULL) ;
  ASSERT (highFilter != NULL) ;
  
  
  *articleTout = defaultArticleTimeout ;
  *respTout = defaultResponseTimeout ;
  *initialCxns = defaultInitialConnections ;
  *maxCxns = defaultMaxConnections ;
  *maxChecks = defaultMaxChecks ;
  *streaming = defaultStreaming ;
  *lowFilter = defaultLowFilter ;
  *highFilter = defaultHighFilter ;
  *portNum = defaultPortNumber ;
}


/*
 * fully delete and clean up the Host object.
 */
void delHost (Host host)
{
  Host h,q ;

  for (h = gHostList, q = NULL ; h != NULL ; q = h, h = h->next)
    if (h == host)
      {
        if (gHostList == h)
          gHostList = gHostList->next ;
        else
          q->next = h->next ;
        break ;
      }

  ASSERT (h != NULL) ;
        
  delTape (host->myTape) ;
  
  FREE (host->connections) ;
  FREE (host->cxnActive) ;
  FREE (host->cxnSleeping) ;
  FREE (host->peerName) ;
  FREE (host->ipName) ;

  if (host->ipAddrs)
    FREE (host->ipAddrs) ;

  FREE (host) ;
  gHostCount-- ;
}



static Host findHostByName (char *name) 
{
  Host h;

  for (h = gHostList; h != NULL; h = h->next)
    if ( strcmp(h->peerName, name) == 0 )
      return h;

  return NULL;
}



/* 
 * the article can be dropped from the process queue and the connection can
 * take a new article if there are any to be had.
 */
static void articleGone (Host host, Connection cxn, Article article)
{
  if ( !remArticle (article,&host->processed,&host->processedTail) )
    die ("remArticle in hostArticleDeferred failed") ;

  delArticle (article) ;

  if (cxn != NULL)
    hostGimmeArticle (host,cxn) ; /* may not give anything over */
}







/*
 * One of the Connections for this Host has reestablished itself, so stop
 * spooling article info to disk.
 */
static void hostStopSpooling (Host host)
{
  ASSERT (host->spoolTime != 0) ;
  
  clearTimer (host->statsId) ;
  hostLogStats (host,true) ;
 
  tapeClose (host->myTape) ;
  host->spoolTime = 0 ;
}







/*
 * No connections are active and we're getting response 201 or 400 (or some
 * such) so that we need to start spooling article info to disk.
 */
static void hostStartSpooling (Host host)
{
  ASSERT (host->spoolTime == 0) ;

  hostLogStats (host,true) ;
  
  host->spoolTime = theTime() ;
  if (host->firstConnectTime == 0)
    host->firstConnectTime = host->spoolTime ;

  /* don't want to log too frequently */
  if (SPOOL_LOG_PERIOD > 0 &&
      (host->spoolTime - host->lastSpoolTime) > SPOOL_LOG_PERIOD)
    {
      syslog (LOG_NOTICE,SPOOLING,host->peerName) ;
      host->lastSpoolTime = host->spoolTime ;
    }
  
  host->connectTime = 0 ;

  host->notifiedChangedRemBlckd = false ;

  clearTimer (host->statsId) ;
  host->statsId = prepareSleep (hostStatsTimeoutCbk, statsPeriod, host) ;

  /* while we're spooling everything goes straight to disk */
  tapeOpen (host->myTape) ;
}







/*
 * Time to log the statistics for the Host. If FINAL is true then the
 * counters will be reset.
 */
static void hostLogStats (Host host, bool final)
{
  time_t now = theTime() ;
  time_t *startPeriod ;

  if (host->spoolTime == 0 && host->connectTime == 0)
    return ;        /* host has never connected and never started spooling*/

  startPeriod = (host->spoolTime != 0 ? &host->spoolTime : &host->connectTime);

  if (now - *startPeriod >= STATS_RESET_PERIOD)
    final = true ;
  
  if (host->spoolTime != 0)
    syslog (LOG_NOTICE, HOST_SPOOL_STATS, host->peerName,
            (final ? "final" : "checkpoint"),
            (long) (now - host->spoolTime), host->backlogSpooled) ;
  else
    syslog (LOG_NOTICE, HOST_STATS_MSG, host->peerName, 
            (final ? "final" : "checkpoint"),
            (long) (now - host->connectTime),
            host->artsOffered, host->artsAccepted,
            host->artsNotWanted, host->artsRejected,
            host->artsMissing,host->backlogSpooled) ;

  if (logConnectionStats) 
    {
      u_int i ;
      
      for (i = 0 ; i < host->maxConnections ; i++)
        if (host->connections [i] != NULL && host->cxnActive [i])
          cxnLogStats (host->connections [i],final) ;
    }

  /* one 'spooling backlog' message per stats logging period */
  host->loggedBacklog = false ;
  host->loggedModeOn = host->loggedModeOff = false ;

  if (final)
    {
      host->artsOffered = 0 ;
      host->artsAccepted = 0 ;
      host->artsNotWanted = 0 ;
      host->artsRejected = 0 ;
      host->artsDeferred = 0 ;
      host->artsMissing = 0 ;
      host->artsFromTape = 0 ;
      host->backlogSpooled = 0 ;
      
      *startPeriod = theTime () ; /* in of case STATS_RESET_PERIOD */
    }

#if 0
  /* XXX turn this section on to get a snapshot at each log period. */
  if (gPrintInfo != NULL)
    gPrintInfo () ;
#endif
}







/*
 * Log the status of the Hosts.
 */
extern char *versionInfo ;
static void hostLogStatus (void)
{
  char path [MAXPATHLEN] ;
  FILE *fp = NULL ;
  Host h ;
  bool anyToLog = false ;
  u_int peerNum = 0, actConn = 0, slpConn = 0 ;

  for (h = gHostList ; h != NULL ; h = h->next)
    if (h->myTape != NULL)   /* the host deletes its tape when it's closing */
      {
        anyToLog = true ;
        peerNum++ ;
        actConn += h->activeCxns ;
        slpConn += h->sleepingCxns ;
      }

  if (!anyToLog)
    return ;
  
  sprintf (path,"%s/%s",getTapeDirectory (),"innfeed.status") ;
  if ((fp = fopen (path,"w")) == NULL)
    {
      static bool logged ;

      if ( !logged )
        syslog (LOG_ERR,NO_STATUS,path) ;
      logged = true ;
    }
  else
    {
      char timeString [30] ;
      time_t now ;

      now = time (NULL) ;
      strcpy (timeString,ctime (&now)) ;

#if GEN_HTML
      fprintf (fp,"<HTML><META HTTP-EQUIV=\"Refresh\" CONTENT=\"20;\"><PRE>\n\n") ;
#endif

      fprintf (fp,"%s\npid %d started %s\nUpdated: %s",
               versionInfo,(int) myPid,startTime,timeString) ;
      fprintf (fp," (peers: %d active: %d sleeping: %d)\n\n",
               peerNum, actConn, slpConn) ;
               
      fprintf (fp,"Default configuration parameters:\n") ;

      fprintf (fp,"    article timeout: %-5d     initial connections: %d\n",
	       defaultArticleTimeout, defaultInitialConnections) ;

      fprintf (fp,"   response timeout: %-5d         max connections: %d\n",
	       defaultResponseTimeout, defaultMaxConnections) ;

      fprintf (fp,"         max checks: %-5d               streaming: %s\n",
	       defaultMaxChecks, (defaultStreaming ? "true" : "false")) ;

      fprintf (fp,"       on threshold: %-2.1f%%                port num: %d\n",
	       (defaultHighFilter * 10), defaultPortNumber) ;

      fprintf (fp,"      off threshold: %-2.1f%%\n\n",
	       (defaultLowFilter * 10)) ;

      for (h = gHostList ; h != NULL ; h = h->next)
        hostPrintStatus (h,fp) ;

#if GEN_HTML
      fprintf (fp,"</PRE></HTML>\n") ;
#endif
      
      fclose (fp) ;
    }
}


/*
 * This prints status information for each host.  An example of the
 * format of the output is:
 *
 * sitename
 *   seconds: 351       art. timeout: 400          ip name: foo.bar
 *   offered: 1194     resp. timeout: 240             port: 119
 *  accepted: 178     want streaming: yes      active cxns: 6
 *   refused: 948       is streaming: yes    sleeping cxns: 0
 *  rejected: 31          max checks: 25      initial cxns: 5
 *   missing: 0         on threshold: 95.0%       max cxns: 6
 *  deferred: 0        off threshold: 95.0%
 *
 */
static void hostPrintStatus (Host host, FILE *fp)
{
  time_t now = theTime() ;

  ASSERT (host != NULL) ;
  ASSERT (fp != NULL) ;

  fprintf (fp,"%s\n",
	   host->peerName);
           
  fprintf (fp, "   seconds: %-7ld   art. timeout: %-5d        ip name: %s\n",
	   host->firstConnectTime > 0 ? (long) (now - host->firstConnectTime) : 0,
	   host->articleTimeout, host->ipName) ;
           
  fprintf (fp, "   offered: %-7ld  resp. timeout: %-5d           port: %d\n",
	   (long) host->gArtsOffered, host->responseTimeout, host->port);

  fprintf (fp, "  accepted: %-7ld want streaming: %s      active cxns: %d\n",
	   (long) host->gArtsAccepted, 
           (host->wantStreaming ? "yes" : "no "),
	   host->activeCxns) ;

  fprintf (fp, "   refused: %-7ld   is streaming: %s    sleeping cxns: %d\n",
	   (long) host->gArtsNotWanted,
           (host->remoteStreams ? "yes" : "no "),
	   host->sleepingCxns) ;

  fprintf (fp, "  rejected: %-7ld     max checks: %-5d   initial cxns: %d\n",
	   (long) host->gArtsRejected, host->maxChecks,
	   host->initialCxns) ;

  fprintf (fp, "   missing: %-7ld   on threshold: %-2.1f%%       max cxns: %d\n",
	   (long) host->gArtsMissing, (host->lowPassHigh * 10),
	   host->maxConnections) ;

  fprintf (fp, "  deferred: %-7ld  off threshold: %-2.1f%%\n",
	   (long) host->gArtsDeferred, (host->lowPassLow * 10)) ;

  fprintf (fp, "\n");
}







/*
 * The callback function for the statistics timer to call.
 */
static void hostStatsTimeoutCbk (TimeoutId tid, void *data)
{
  Host host = (Host) data ;

  (void) tid ;                  /* keep lint happy */

  ASSERT (tid == host->statsId) ;
  
  if (!amClosing (host))
    hostLogStats (host, false) ;
  
  host->statsId = prepareSleep (hostStatsTimeoutCbk, statsPeriod, host) ;
}


/* the host has too many unprocessed articles so we send them to the tape. */
static void backlogToTape (Host host)
{
  Article article ;

  if (!host->loggedBacklog)
    {
#if 0               /* this message is pretty useless and confuses people */
      syslog (LOG_NOTICE,BACKLOG_TO_TAPE,host->peerName /* ,host->backlog */) ;
#endif
      host->loggedBacklog = true ;
    }
  
  while ((article = remHead (&host->queued,&host->queuedTail)) != NULL)
    {
      host->backlogSpooled++ ;
      tapeTakeArticle (host->myTape,article) ;
    }

  host->backlog = 0 ;
}







/*
 * Returns true of the Host is in the middle of closing down.
 */
static bool amClosing (Host host)
{
  return (host->myTape == NULL ? true : false) ;
}







/*
 * flush all queued articles all the way out to disk.
 */
static void queuesToTape (Host host)
{
  Article art ;
  
  tapeOpen (host->myTape) ;

  while ((art = remHead (&host->processed,&host->processedTail)) != NULL)
    {
      host->backlogSpooled++ ;
      tapeTakeArticle (host->myTape,art) ;
    }
  
  while ((art = remHead (&host->queued,&host->queuedTail)) != NULL)
    {
      host->backlog-- ;
      host->backlogSpooled++ ;
      tapeTakeArticle (host->myTape,art) ;
    }

  tapeClose (host->myTape) ;
}







/*
 * Add an article to the given queue.
 */
static void queueArticle (Article article, ProcQElem *head, ProcQElem *tail)
{
  ProcQElem elem ;
  
  elem = ALLOC (struct proc_q_elem, 1) ;
  ASSERT (elem != NULL) ;

  elem->article = article ;
  elem->next = NULL ;
  elem->prev = *tail ;
  if (*tail != NULL)
    (*tail)->next = elem ;
  else
    *head = elem ;
  *tail = elem ;  
}







/*
 * remove the article from the queue
 */
static bool remArticle (Article article, ProcQElem *head, ProcQElem *tail)
{
  ProcQElem elem ;

  ASSERT (head != NULL) ;
  ASSERT (tail != NULL) ;

  /* we go backwards down the list--probably faster */
  elem = *tail ;
  while (elem != NULL && elem->article != article)
    elem = elem->prev ;

  if (elem != NULL)
    {
      if (elem->prev != NULL)
        elem->prev->next = elem->next ;
      if (elem->next != NULL)
        elem->next->prev = elem->prev ;
      if (*head == elem)
        *head = elem->next ;
      if (*tail == elem)
        *tail = elem->prev ;

      FREE (elem) ;
      
      return true ;
    }
  else
    return false ;
}







/*
 * remove the article that's at the head of the queue and return
 * it. Returns NULL if the queue is empty.
 */
static Article remHead (ProcQElem *head, ProcQElem *tail)
{
  ProcQElem elem ;
  Article art ;

  ASSERT (head != NULL) ;
  ASSERT (tail != NULL) ;
  ASSERT ((*head == NULL && *tail == NULL) ||
          (*head != NULL && *tail != NULL)) ;

  if (*head == NULL)
    return NULL ;

  elem = *head ;
  art = elem->article ;
  *head = elem->next ;
  if (elem->next != NULL)
    elem->next->prev = NULL ;

  if (*tail == elem)
    *tail = NULL ;

  FREE (elem) ;

  return art ;
}

static bool getOptDouble (double *field, char *line, u_int fieldNum,
                          double defVal)
{
  char *tok, *ptr ;

  if ((tok = mystrtok (NULL,":")) == NULL)
    *field = defVal ;
  else if (strlen (tok) > 0)
    {
      errno = 0 ;

      /* yeah, I know I could collapse these all into one giant if. */
      if (((*field = strtod (tok, &ptr)) == 0) && (ptr == tok))
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
      else if (*ptr != '\0')
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
      else if ((*field == -(HUGE_VAL) || *field == HUGE_VAL) &&
               errno == ERANGE)
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
      else if (*field == 0 && errno == ERANGE)
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
    }
  else
    *field = defVal ;

  return true ;
}

static bool getOptBool (bool *field, char *line, u_int fieldNum, bool defVal)
{
  char *tok, *ptr, *p ;
  u_int intVal ;

  if ((tok = mystrtok (NULL,":")) == NULL)
    *field = defVal ;
  else if (strlen (tok) > 0)
    {
      intVal = strtol (tok, &ptr, 10) ;
      if (ptr == tok)
        {
          for (p = tok ; *p && isspace (*p) ; p++)
              /* nada */ ;
          if ( *p == 't' || *p == 'T' )
            *field = true ;
          else if ( *p == 'f' || *p == 'F' )
            *field = false ;
          else
            {
              dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
              return false ;
            }
        }
      else if (ptr != '\0')
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
      else if (intVal == 0)
        *field = false ;
      else if (intVal == 1)
        *field = true ;
      else
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
    }
  else
    *field = defVal ;

  return true ;
}

static bool getOptUInt (u_int *field, char *line, u_int fieldNum, u_int defVal)
{
  char *tok, *ptr ;

  if ((tok = mystrtok (NULL,":")) == NULL)
    *field = defVal ;
  else if (strlen (tok) > 0)
    {
      if (((*field = strtol (tok, &ptr, 10)) == LONG_MAX) || (*ptr != '\0'))
        {
          dprintf (1,"Bad field %d in line: %s\n", fieldNum, line) ;
          return false ;
        }
    }
  else
    *field = defVal ;

  return true ;
}
