/*  itstime.c is part of Statnet */
/* Statnet is protected under the GNU Public License (GPL2). */
/* Author: Jeroen Baekelandt (jeroenb@igwe.vub.ac.be)       */
/* 27DEC95: Scot E. Wilcoxon (sewilco@fieldday.mn.org)      */

#include "netwatch.h"
#include "curs.h"
#include <sys/time.h>
#include <ncurses.h>
#include <signal.h>
#include <unistd.h>

#define MAXREFRESH 15
#define ROUTERSTATSMAX 60
extern int scrmid;
extern struct itimerval oldtm, newtm;

void makeaddr (char naddr[], char ascii[]);

void
dispdata (int errnum)
{

  int xleft = 1;		/* Offset for Local Info */
  int xright;
  int y;
  static int refreshscr = 0;
  static int routerstats = 0;
  extern double maxburst;
  static time_t bursttime;
  static char burst[80];
  static time_t routertime;
  static unsigned long int oldrouteruse=0;
  static unsigned long int oldrouterto=0;
  static unsigned long int oldrouterfrom=0;
  unsigned long int tfuse;
  unsigned long int ttuse;
  unsigned long int ttotal;
  static unsigned long int minfuse;
  static unsigned long int mintuse;
  static unsigned long int sec;
  static unsigned long int use;
  double fuse;
  extern unsigned long int routeruse;
  
  int lcnt = 0;
  int rcnt = 0;
  static char ttt[260];
  static char thost[60];
  static int statsonce = FALSE;
  static int disableburst = FALSE;
  int yact;
  unsigned char *pc;
  extern int help;
  extern int watch;
  extern int wai;
  extern int llockout;
  extern int rlockout;
  extern int lydisp;
  extern int rydisp;
  extern int localkey;
  extern int localupdate;
  extern int remoteupdate;
  extern int refreshloc;
  extern int refreshrem;
  extern int poschange;
  extern int disprouterstats;
  extern unsigned long routerto;
  extern unsigned long routerfrom;
  extern unsigned long routeruse;
  static unsigned long orouterto=0;
  static unsigned long orouterfrom=0;
  static unsigned long orouteruse=0;
  unsigned long wrouteto, wroutefrom, wrouteuse;
  int localbypass = FALSE;
  int remotebypass = FALSE;
  time_t mnew;
  extern int dispopt;		/* TXRC IPPROTO OTHER */
  int updaterest = 0;
  int xoff;
  time_t colred, colyel, colgreen, testt;
  extern HOSTINFO *lfirst;
  extern HOSTINFO *rfirst;
  HOSTINFO *current;

  mnew = time (0);
  localbypass =  (!localkey && (watch || help));  
  remotebypass = (localkey && (watch || help));
  colred = mnew - 60;		/* 1 min */
  colyel = mnew - 300;		/* 5 min */
  colgreen = mnew - 1800;	/* 30 min */
  scrmid = COLS / 2;
  xright = scrmid + 1;
  if (rewrite_labels)
    {				/* if rewrite_labels */

      clrportion (1, 1, 4, COLS - 1);
      mvprintw (0, (COLS / 2) - 6, "NETWORK WATCH");
      mvprintw (1, COLS - (COLS) / 3, "REMOTE NETWORK");
      mvprintw (1, (COLS - 22) / 4, "LOCAL NETWORK");
      switch (dispopt)
	{
	case DISP_TX_RC:
	  mvprintw (2, (xright + 5), "HOST          (PKTS)     X      R");
	  mvprintw (2, (xleft + 5), "HOST          (PKTS)     X      R");
	  break;
	case DISP_IPPROTO:
	  mvprintw (2, (xright + 5), "HOST             IP      SERVICE");
	  mvprintw (2, (xleft + 5), "HOST             IP      SERVICE");
	  break;
	case DISP_DEST2:
	case DISP_DEST1:
	  mvprintw (2, (xright + 5), "HOST                TALKING TO");
	  mvprintw (2, (xleft + 5), "HOST                TALKING TO");
	  break;
	case DISP_ACCT:
	  mvprintw (2, (xright + 5), "HOST     (BYTES)    X          R ");
	  mvprintw (2, (xleft + 5), "HOST      (BYTES)    X          R ");
	  break;
	}
      refresh ();
      rewrite_labels = 0;
    }
  if (localkey)
    {
      attron (COLOR_PAIR (2));
      mvprintw (1, 1, "KEY");
      attron (COLOR_PAIR (4));
      mvprintw (1, 76, "   ");
    }
  else
    {
      attron (COLOR_PAIR (4));
      mvprintw (1, 1, "   ");
      attron (COLOR_PAIR (2));
      mvprintw (1, 76, "KEY");
      attron (COLOR_PAIR (4));
    }
  if (llockout)
    mvprintw (0, 62, "LX");
  else
    {
      mvaddch (0, 62, ACS_HLINE);
      addch (ACS_HLINE);
    }
  if (rlockout)
    mvprintw (0, 65, "RX");
  else
    {
      mvaddch (0, 65, ACS_HLINE);
      addch (ACS_HLINE);
    }
  if (poschange)
    mvprintw (0, 68, "P");
  else
    mvaddch (0, 68, ACS_HLINE);
  mvprintw (0, 70, "LINE %3d ", rydisp);
  mvprintw (0, 2, "LINE %3d ", lydisp);
/*  mvprintw (24, 2, "WAI=%1d",wai); */
  if (poschange)
    lcnt = 0;
  routerstats++;
  if (routerstats==ROUTERSTATSMAX)
  {
  	statsonce++;
  	sec = mnew - routertime;
  	use = routeruse - oldrouteruse;
  	mintuse = routerto - oldrouterto;
  	minfuse = routerfrom - oldrouterfrom;
  	routertime = mnew;
  	oldrouteruse = routeruse;
  	oldrouterto = routerto;
  	oldrouterfrom = routerfrom;
  	/* CALC STATS for ROUTER 
  		use = # of bytes thru router in "sec" seconds
  	*/
  	fuse = (double)use/sec* 0.008;
  	if (fuse>1.0)
	  	mvprintw(LINES-1,scrmid-15," ROUTER %10.2g kbits/sec ",fuse);
	else
	  	mvprintw(LINES-1,scrmid-15," ROUTER %10.2g bits/sec ",fuse*1000.0);
  	routerstats = 0;
  }
  xoff = 1;
  tfuse = routerfrom - orouterfrom;
  ttuse = routerto - orouterto;
  ttotal = tfuse + ttuse;
  fuse = (double)ttotal*0.008;
  if (!disableburst && fuse>maxburst)
  {
    maxburst = fuse;
    bursttime = mnew;
    strcpy(burst,ctime(&bursttime));
    burst[strlen(burst)-1]=0;
  }
  if (disableburst) disableburst = FALSE;
  if (watch && disprouterstats)
  {
  	if (localkey) xoff = scrmid+1;
	mvprintw(6,xoff+10,"ROUTER STATS BYTES");
	mvprintw(8,xoff+3,"Over 1 second period:");
	mvprintw(13,xoff+3,"Over 1 minute period:");
  	mvprintw(9,xoff+3, " Incoming: %8lu",tfuse);
  	mvprintw(10,xoff+3," Outgoing: %8lu",ttuse);
  	mvprintw(11,xoff+3," Total:    %8lu",ttotal);
  	if (fuse>=1.0)
	  	mvprintw(11,xoff+22,"(%5.1f kbps)",fuse);
	else
	  	mvprintw(11,xoff+22,"(%5.1f bps )",fuse*1000);
	
  	if (statsonce)
  	{
  	mvprintw(14,xoff+3," Incoming: %8lu",minfuse);
  	mvprintw(15,xoff+3," Outgoing: %8lu",mintuse); 
  	mvprintw(16,xoff+3," Total:    %8lu",use); 
  	}
  	else
	  	mvprintw(15,xoff+3," N/A");
	if (maxburst>1.0)
	{
	  	mvprintw(18,xoff+3," MAX. BURST:    %5.1f kbps",maxburst); 
	  	mvprintw(19,xoff+3," at %s",burst); 
	 }
  }
orouterfrom = routerfrom;
orouterto = routerto;
  if (wai==CURSESBUG)
  {
  	ungetch(' ');
  	disableburst = TRUE;
  }
  	
  refreshscr++;
  if (refreshscr > MAXREFRESH)
    {
      poschange = 1;
      refreshscr = 0;
    }
  if (poschange)
    {
      refreshloc = refreshrem = TRUE;
    }
     newtm.it_value.tv_sec = 1;
 newtm.it_value.tv_usec = 0;
setitimer(ITIMER_REAL,&newtm,&oldtm); /* first screen update in 1 sec (exact)*/
  signal (SIGALRM, dispdata);

  if (refreshloc && !localbypass)
    clrportion (4, 1, LINES - 1, scrmid);
  if (refreshrem && !remotebypass)
    clrportion (4, scrmid+1, LINES - 1, COLS - 1);
 
  if (!localbypass && !llockout && (localupdate || refreshloc))
    {
      mvaddch (3,0, ACS_VLINE);
      lcnt = 0;
      current = lfirst->flink;
      while (current != lfirst)
	{
/*              y = current->disprow; */
	  y = lcnt;
	  lcnt++;
	  yact = y - lydisp;
	  if (yact >= 0 && yact < LINES - 5)
	    {
	      yact += 4;
	      if (refreshloc || updaterest || current->update)
		{
		  strncpy (thost, current->name, 23);
		  thost[23] = 0;
		  testt = current->tstamp;
		  if (testt > colred)
		    attron (COLOR_PAIR (1));
		  else if (testt > colyel)
		    attron (COLOR_PAIR (2));
		  else if (testt > colgreen)
		    attron (COLOR_PAIR (3));
		  else
		    attron (COLOR_PAIR (4));
		  switch (dispopt)
		    {
		    case DISP_TX_RC:
		      sprintf (ttt, "%-24s %6d %6d", thost,
			       current->pktcntsend, current->pktcntrec);
		      break;
		    case DISP_IPPROTO:
		      thost[16] = 0;
		      sprintf (ttt, "%-16s %8s %10s   ", thost,
			       current->ip_pr, current->servicename);
		      break;
		    case DISP_DEST1:
		      pc = (unsigned char *) current->othaddr;
		      thost[21] = 0;
		      sprintf (ttt, "%-21s %u.%u.%u.%u        ", thost,
			       pc[0], pc[1], pc[2], pc[3]);
		      break;
		    case DISP_DEST2:
		      pc = (unsigned char *) current->othaddr;
		      makeaddr (current->addr, thost);
		      thost[21] = 0;
		      sprintf (ttt, "%-21s %u.%u.%u.%u        ", thost,
			       pc[0], pc[1], pc[2], pc[3]);
		      break;
		    case DISP_ACCT:
		      thost[16] = 0;
		      sprintf (ttt, "%-16s %10lu %10lu", thost,
			current->sendbytes,current->recbytes);
		      break;
		    }
		  ttt[38] = 0;
		  mvprintw (yact, xleft, ttt);
		  attron (COLOR_PAIR (4));
		  if (current->update)
		    {
		      if (current->update == 1)
			updaterest = 1;
		      current->update = 0;
		    }
		}
	    }
	  else
	    {
	      if (yact < 0)
		      mvaddch (3,0, ACS_UARROW);
	      else
		      mvaddch (3,0, ACS_DARROW);
	    }
	  current = current->flink;
	}

      localupdate = 0;
    }
  if (!remotebypass && !rlockout && (remoteupdate || refreshrem))
    {
/*   rcnt++;
   mvprintw(11,10,"%2d",rcnt);  */
      mvaddch (3,79, ACS_VLINE);
      rcnt = 0;
      current = rfirst->flink;
      while (current != rfirst)
	{
/*              y = current->disprow; */
	  y = rcnt;
	  rcnt++;
	  yact = y - rydisp;
	  if (yact >= 0 && yact < LINES - 5)
	    {
	      yact += 4;
	      if (refreshrem || updaterest || current->update)
		{
		  strncpy (thost, current->name, 23);
		  thost[23] = 0;
		  testt = current->tstamp;
		  if (testt > colred)
		    attron (COLOR_PAIR (1));
		  else if (testt > colyel)
		    attron (COLOR_PAIR (2));
		  else if (testt > colgreen)
		    attron (COLOR_PAIR (3));
		  else
		    attron (COLOR_PAIR (4));
		  switch (dispopt)
		    {
		    case DISP_TX_RC:
		      sprintf (ttt, "%-24s %6d %6d", thost,
			       current->pktcntsend, current->pktcntrec);
		      break;
		    case DISP_IPPROTO:
		      thost[16] = 0;
		      sprintf (ttt, "%-16s %8s %10s    ", thost,
			       current->ip_pr, current->servicename);
		      break;
		    case DISP_DEST1:
		      pc = (unsigned char *) current->othaddr;
		      thost[21] = 0;
		      sprintf (ttt, "%-21s %u.%u.%u.%u        ", thost,
			       pc[0], pc[1], pc[2], pc[3]);
		      break;
		    case DISP_DEST2:
		      pc = (unsigned char *) current->othaddr;
		      makeaddr (current->addr, thost);
		      thost[21] = 0;
		      sprintf (ttt, "%-21s %u.%u.%u.%u        ", thost,
			       pc[0], pc[1], pc[2], pc[3]);
		      break;
		    case DISP_ACCT:
		      thost[16] = 0;
		      sprintf (ttt, "%-16s %10lu %10lu", thost,
			current->sendbytes,current->recbytes);
		      break;
		    }
		  ttt[38] = 0;
		  mvprintw (yact, xright, ttt);
		  attron (COLOR_PAIR (4));
		  if (current->update)
		    {
		      if (current->update == 1)
			updaterest = 1;
		      current->update = 0;
		    }
		}
	    }
	  else
	    {
	      if (yact < 0)
		      mvaddch (3,79, ACS_UARROW);
	      else
		      mvaddch (3,79, ACS_DARROW);
	    }
	  current = current->flink;
	}
      remoteupdate = 0;
    }
  poschange = 0;
  refresh ();
}
