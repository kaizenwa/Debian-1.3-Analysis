/*****************************************************************************
 * uutraf.c -- UUCP Traffic analyzer and cost estimator                      *
 *                                                                           *
 * Reads an Taylor-UUCP Log-/Statsfile, and generates a report out of it     *
 *                                                                           *
 * uutraf.c is (c) David Frey, 1995    					     *
 *								             *
 * Modifications by Yves Arrouye, 1996                                       *
 *								             *
 * This program is free software; you can redistribute it and/or modify it   *
 * under the terms of the GNU General Public License as published by the     *
 * Free Software Foundation; either version 2 of the License, or (at your    *
 * option) any later version.                                                *
 *									     *
 * This program is distributed in the hope that it will be useful, but       *
 * WITHOUT ANY WARRANTY; without even the implied warranty of                *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                      *
 * See the GNU General Public License for more details.                      *
 *									     *
 * You should have received a copy of the GNU General Public License along   *
 * with this program; if not, write to the Free Software Foundation, Inc.,   *
 * 675 Mass Ave, Cambridge, MA 02139, USA.                                   *
 *****************************************************************************/

/* $Log: uutraf.c,v $ * Revision 1.0  1996/01/01 16:51:45  david * Initial
 * revision * */

static char RCSId[] = "$Id: uutraf.c,v 1.0 1996/01/01 16:51:45 david Rel $";

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>
#include <malloc.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <math.h>

#include "uutraf.h"

/* Options */
static struct option const long_options[] =
{
  {"help", no_argument, 0, 'h'},
  {"help", no_argument, 0, '?'},
  {"version", no_argument, 0, 'v'},
  {"news", no_argument, 0, 'n'},
  {"nonews", no_argument, 0, 'N'},
  {0, 0, 0, 0}
};

/* Globals */

char *progname;					 /* name of the program */
hostentry *hosttab;				 /* linked list of hosts */

time_t startlogtime, stoplogtime;

void AnalyzeLogfile(char *logfilename, int newsstats)
/* 
 * Analyze the Logfile. We got the file statistice already out of the
 * Stats-file.
 */
{
  FILE *logfile;
  char logline[LINE_MAX + 1];
  char hostname[HOSTNAMELENGTH + 1];
  hostentry *host;
  char *text;
  char text0[LINE_MAX + 1];

  int sec100;
  struct tm time;
  off_t size;
  float cost;
  statsentry *statsline;

  int outgoing;					 /* is it an outgoing call ? */

  host = NULL;
  outgoing = 0;
  logfile = fopen(logfilename, "r");
  if (logfile == NULL)
  {
    fprintf(stderr, "%s: can't open %s: %s\n",
	    progname, logfilename, strerror(errno));
    exit(1);
  }

  /* Read, parse and analyse the Logfile */
  startlogtime = 0;
  while (fgets(logline, LINE_MAX, logfile) != NULL)
  {
    sscanf(logline, "%*s %8s %*s (%d-%d-%d %d:%d:%d.%d %*d) %[ -~]",
	   hostname, &time.tm_year, &time.tm_mon, &time.tm_mday,
	   &time.tm_hour, &time.tm_min, &time.tm_sec,
	   &sec100, text0);

    if (hostname[0] != '-')
      host = gethostentry(hostname);
    text = &text0[0];

    if (startlogtime == 0)
      startlogtime = unixtime(time);
    stoplogtime = unixtime(time);

    if ((strncmp(text0, CALLINGSYSTEM, sizeof(CALLINGSYSTEM) - 1) == 0) ||
	(strncmp(text0, INCOMINGCALL, sizeof(INCOMINGCALL) - 1) == 0))
    {
      outgoing = (strncmp(text, CALLINGSYSTEM, sizeof(CALLINGSYSTEM) - 1) == 0);

      if (outgoing)
	host->calls++;
    }

    if ((strncmp(text0, LOGINSUCCESS, sizeof(LOGINSUCCESS) - 1) == 0))
    {
      if (outgoing)
	host->pttcost += host->pttcostpercall;
    }

    if ((strncmp(text0, RECEIVING, sizeof(RECEIVING) - 1) == 0))
    {
      int ismail = 0, isnews = 0;
      char *oparen, *cparen;

      text += (sizeof(RECEIVING) - 1);


      ismail = ((strncmp(text, "D.", 2) == 0) ||
		(strncmp(text, "rmail ", 6) == 0));
      isnews = (strncmp(text, "rnews ", 6) == 0);
      size = 0;

      if ((oparen = strchr(text, '(')))
      {
	size = strtol(oparen + 1, &cparen, 10);
      }
      else
	cparen = NULL;

      if (cparen == oparen)
      {
	statsline = GetStatsEntry(host->statslist, 1, size,
				  statstime(time, sec100));

	if (statsline)
	{
	  size = statsline->size;
	}
      }

      if (ismail)
      {
	host->mailrecbytes += size;
	host->mailrecnum++;

	cost = host->provfeepermail;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeepermailkb;
	}
	host->provmailcost += cost;
      }
      else if (isnews)
      {
	host->newsrecbytes += size;
	host->newsrecnum++;

	cost = host->provfeepernews;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeepernewskb;
	}
	host->provnewscost += cost;
      }
      else
      {
	host->filerecbytes += size;
	host->filerecnum++;

	cost = host->provfeeperfile;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeeperfilekb;
	}
	host->provfilecost += cost;
      }

      host->datarecbytes -= size;
      host->datarecnum--;
      host->provcost += cost;
    }

    if ((strncmp(text0, SENDING, sizeof(SENDING) - 1) == 0))
    {
      int ismail = 0, isnews = 0;
      char *oparen, *cparen;

      text += (sizeof(SENDING) - 1);

      ismail = ((strncmp(text, "rmail ", 6) == 0) ||
		(strncmp(text, "rsmtp ", 6) == 0));
      isnews = (strncmp(text, "rnews ", 6) == 0);

      size = 0;

      if ((oparen = strchr(text, '(')))
      {
	size = strtol(oparen + 1, &cparen, 10);
      }
      else
	cparen = NULL;

      if (cparen == oparen)
      {
	statsline = GetStatsEntry(host->statslist, 1, size,
				  statstime(time, sec100));

	if (statsline)
	{
	  size = statsline->size;
	}
      }
      if (ismail)
      {
	host->mailsentbytes += size;
	host->mailsentnum++;

	cost = host->provfeepermail;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeepermailkb;
	}
	host->provmailcost += cost;
      }
      else if (isnews)
      {
	host->newssentbytes += size;
	host->newssentnum++;

	cost = host->provfeepernews;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeepernewskb;
	}
	host->provmailcost += cost;
      }
      else
      {
	host->filesentbytes += size;
	host->filesentnum++;

	cost = host->provfeeperfile;
	if (host->provfeeperkbmult)
	{
	  cost += ceil(size / KBYTE) * host->provfeeperfilekb;
	}
	host->provfilecost += cost;
      }

      host->datasentbytes -= size;
      host->datasentnum--;
      host->provcost += cost;
    }

    if (strncmp(text, CALLCOMPLETE, sizeof(CALLCOMPLETE) - 1) == 0)
    {
      time_t connected;

      text += (sizeof(CALLCOMPLETE) - 1);
      connected = atol(text);

      while (isdigit(*text))
	text++;
      while (!isdigit(*text))
	text++;
      size = atof(text);

      if (outgoing)
      {
	host->connected += connected;

	cost = ceil(connected / 60.0) * host->provlinecostpermin;
	host->provcost += cost;
	host->provfilecost += cost;
	host->pttcost += (ceil(connected / 60.0) * host->pttlinecostpermin);

	host->bruttobytes += size;
      }
    }
  }
  fclose(logfile);

  /* update cost entries */

  for (host = hosttab; host; host = host->next)
  {
    if (!host->provfeeperkbmult)
    {
      cost = ceil((host->filerecbytes + host->filesentbytes) / KBYTE)
	* host->provfeeperfilekb;
      host->provfilecost += cost;
      host->provcost += cost;

      cost = ceil((host->mailrecbytes + host->mailsentbytes) / KBYTE)
	* host->provfeepermailkb;
      host->provmailcost += cost;
      host->provcost += cost;

      cost = ceil((host->newsrecbytes + host->newssentbytes) / KBYTE)
	* host->provfeepernewskb;
      host->provnewscost += cost;
      host->provcost += cost;
    }

    if (host->filerecnum != host->datarecnum
	|| host->filesentnum != host->datasentnum
	|| host->filerecbytes != host->datarecbytes
	|| host->filesentbytes != host->datasentbytes)
    {

      fprintf(stderr, "%s: warning: incoherent file statistics for %s, costs may be wrong\n", progname, host->hostname);
      cost = (host->datarecnum - host->filerecnum
	      + host->datasentnum - host->filesentnum) * host->provfeeperfile
	+ ceil((host->datarecbytes - host->filerecbytes
		+ host->datasentbytes - host->filesentbytes) / KBYTE)
	* host->provfeeperfilekb;

      host->provfilecost += cost;
      host->provcost += cost;
    }
  }
}

void PrintResults(hostentry * hosttab, int newsstats)
/* 
 * Output out results 
 */
{
  hostentry *h;

  off_t tfilerecbytes, tmailrecbytes, tnewsrecbytes, trecbytes;
  off_t tfilesentbytes, tmailsentbytes, tnewssentbytes, tsentbytes;
  uint tfilerecnum, tmailrecnum, tnewsrecnum, trecnum;
  uint tfilesentnum, tmailsentnum, tnewssentnum, tsentnum;
  float tsenttime, trectime;
  time_t tconnected;
  short tcalls;
  float tpttcost, tprovcost;
  float tprovfilecost, tprovmailcost, tprovnewscost;

  off_t recbytes, sentbytes;
  uint recnum, sentnum;
  int reccps, sentcps;

  char hostname[HOSTNAMELENGTH + 1];
  char starttimestr[20], stoptimestr[20];

  gethostname(&hostname[0], HOSTNAMELENGTH);
  strftime(&starttimestr[0], 20, "%a %b %d %Y", gmtime(&startlogtime));
  strftime(&stoptimestr[0], 20, "%a %b %d %Y", gmtime(&stoplogtime));

  printf("  UUCP traffic on host %s from %s to %s\n\n",
	 hostname, starttimestr, stoptimestr);

  printf("                                File Traffic\n");
  printf("--Site--  ------Files------  -----Size [kB]----                -Cost--\n");
  printf("           Recd  Sent Total   Recd  Sent  Total                Provid.\n");
  printf("----------------------------------------------------------------------\n");

  tfilerecbytes = tfilesentbytes = tfilerecnum = tfilesentnum = 0;
  tprovfilecost = 0.0;

  h = hosttab;
  while (h != NULL)
  {
    if (h->connected > 0)
    {
      printf("%-8s   %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n",
	     h->hostname,
	     h->filerecnum, h->filesentnum, h->filerecnum + h->filesentnum,
	     ceil(h->filerecbytes / KBYTE), ceil(h->filesentbytes / KBYTE),
	     ceil((h->filerecbytes + h->filesentbytes) / KBYTE),
	     h->provfilecost);

      tfilerecbytes += h->filerecbytes;
      tfilerecnum += h->filerecnum;
      tfilesentbytes += h->filesentbytes;
      tfilesentnum += h->filesentnum;
      tprovfilecost += h->provfilecost;
    }
    h = h->next;
  }
  printf("----------------------------------------------------------------------\n");
  printf("Totals     %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n\n",
	 tfilerecnum, tfilesentnum, tfilerecnum + tfilesentnum,
	 ceil(tfilerecbytes / KBYTE), ceil(tfilesentbytes / KBYTE),
	 ceil((tfilerecbytes + tfilesentbytes) / KBYTE), tprovfilecost);

  tnewsrecbytes = tnewssentbytes = tnewsrecnum = tnewssentnum = 0;
  tprovnewscost = 0.0;

  if (newsstats)
  {
    printf("                                News Traffic\n");
    printf("--Site--  ------Files------  -----Size [kB]----                -Cost--\n");
    printf("           Recd  Sent Total   Recd  Sent  Total                Provid.\n");
    printf("----------------------------------------------------------------------\n");

    h = hosttab;
    while (h != NULL)
    {
      if (h->connected > 0)
      {
	printf("%-8s   %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n",
	       h->hostname,
	       h->newsrecnum, h->newssentnum, h->newsrecnum + h->newssentnum,
	       ceil(h->newsrecbytes / KBYTE), ceil(h->newssentbytes / KBYTE),
	       ceil((h->newsrecbytes + h->newssentbytes) / KBYTE),
	       h->provnewscost);

	tnewsrecbytes += h->newsrecbytes;
	tnewsrecnum += h->newsrecnum;
	tnewssentbytes += h->newssentbytes;
	tnewssentnum += h->newssentnum;
	tprovnewscost += h->provnewscost;
      }
      h = h->next;
    }
    printf("----------------------------------------------------------------------\n");
    printf("Totals     %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n\n",
	   tnewsrecnum, tnewssentnum, (tnewsrecnum + tnewssentnum),
	   ceil(tnewsrecbytes / KBYTE), ceil(tnewssentbytes / KBYTE),
	   ceil((tnewsrecbytes + tnewssentbytes) / KBYTE), tprovnewscost);

  }

  printf("                                Mail Traffic\n");
  printf("--Site--  ------Files------  -----Size [kB]----                -Cost--\n");
  printf("           Recd  Sent Total   Recd  Sent  Total                Provid.\n");
  printf("----------------------------------------------------------------------\n");

  tmailrecbytes = tmailsentbytes = tmailrecnum = tmailsentnum = 0;
  tprovmailcost = 0.0;

  h = hosttab;
  while (h != NULL)
  {
    if (h->connected > 0)
    {
      printf("%-8s   %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n",
	     h->hostname,
	     h->mailrecnum, h->mailsentnum, h->mailrecnum + h->mailsentnum,
	     ceil(h->mailrecbytes / KBYTE), ceil(h->mailsentbytes / KBYTE),
	     ceil((h->mailrecbytes + h->mailsentbytes) / KBYTE),
	     h->provmailcost);

      tmailrecbytes += h->mailrecbytes;
      tmailrecnum += h->mailrecnum;
      tmailsentbytes += h->mailsentbytes;
      tmailsentnum += h->mailsentnum;
      tprovmailcost += h->provmailcost;
    }
    h = h->next;
  }
  printf("----------------------------------------------------------------------\n");
  printf("Totals     %4d  %4d %5d  %5.0f %5.0f %6.0f                %7.2f\n\n",
	 tmailrecnum, tmailsentnum, (tmailrecnum + tmailsentnum),
	 ceil(tmailrecbytes / KBYTE), ceil(tmailsentbytes / KBYTE),
	 ceil((tmailrecbytes + tmailsentbytes) / KBYTE), tprovmailcost);

  printf("                              Combined Traffic\n");
  printf("--Site--  ------Files------  ----Size [kB]-----  -Time [mm:ss]  -----Cost-----\n");
  printf("           Recd  Sent Total   Recd  Sent  Total    Recd   Sent  Provid.    PTT\n");
  printf("------------------------------------------------------------------------------\n");

  tsentnum = trecnum = tsentbytes = trecbytes = 0;
  trectime = tsenttime = 0.0;
  tprovcost = tpttcost = 0.0;

  h = hosttab;
  while (h != NULL)
  {
    if (h->connected > 0)
    {
      sentbytes = h->filesentbytes + h->mailsentbytes + h->newssentbytes;
      recbytes = h->filerecbytes + h->mailrecbytes + h->newsrecbytes;
      sentnum = h->filesentnum + h->mailsentnum + h->newssentnum;
      recnum = h->filerecnum + h->mailrecnum + h->newsrecnum;

      printf("%-8s   %4d  %4d %5d  %5.0f %5.0f %6.0f  %3d:%02d %3d:%02d %7.2f %7.2f\n",
	     h->hostname, recnum, sentnum, recnum + sentnum,
	     ceil(recbytes / KBYTE), ceil(sentbytes / KBYTE),
	     ceil((sentbytes + recbytes) / KBYTE),
	     (int)(h->rectime / 60.0), (int)(h->rectime) % 60,
	     (int)(h->senttime / 60.0), (int)(h->senttime) % 60,
	     h->provcost, h->pttcost);

      tsentnum += sentnum;
      trecnum += recnum;
      tsentbytes += sentbytes;
      trecbytes += recbytes;
      tsenttime += h->senttime;
      trectime += h->rectime;
      tprovcost += h->provcost;
      tpttcost += h->pttcost;
    }
    h = h->next;
  }
  printf("------------------------------------------------------------------------------\n");
  printf("Totals     %4d  %4d %5d  %5.0f %5.0f %6.0f  %3d:%02d %3d:%02d %7.2f %7.2f\n",
	 trecnum, tsentnum, trecnum + tsentnum,
	 ceil(trecbytes / KBYTE), ceil(tsentbytes / KBYTE),
	 ceil((trecbytes + tsentbytes) / KBYTE),
	 (int)(trectime / 60.0), (int)(trectime) % 60,
	 (int)(tsenttime / 60.0), (int)(tsenttime) % 60,
	 tprovcost, tpttcost);
  printf("==============================================================================\n\n");

  printf("                                    Statistics\n");
  printf("--Site-- ------Time [mm:ss]-----  -Avg CPS- Calls  U/D-R -Eff-\n");
  printf("            Recd    Sent    Conn  Recd Sent                   \n");
  printf("--------------------------------------------------------------\n");

  tconnected = 0.0;
  tcalls = 0;

  h = hosttab;
  while (h != NULL)
  {
    if (h->connected > 0)
    {
      float udr, eff;

      sentbytes = h->filesentbytes + h->mailsentbytes + h->newssentbytes;
      recbytes = h->filerecbytes + h->mailrecbytes + h->newssentbytes;

      reccps = (h->rectime == 0) ? 0 : (int)ceil(recbytes / h->rectime);
      sentcps = (h->senttime == 0) ? 0 : (int)ceil(sentbytes / h->senttime);

      eff = (float)(h->rectime + h->senttime) / (float)h->connected * 100.0;
      udr = (recbytes == 0) ? 0 : (float)sentbytes / (float)recbytes *100.0;

      printf("%-8s  %3d:%02d  %3d:%02d  %3d:%02d  %4d %4d  %4d %5.1f%% %4.0f%%\n",
	     h->hostname,
	     (int)(h->rectime / 60.0), (int)(h->rectime) % 60,
	     (int)(h->senttime / 60.0), (int)(h->senttime) % 60,
	     (int)(h->connected / 60.0), (int)(h->connected) % 60,
	     reccps, sentcps, h->calls, udr, eff);

      tcalls += h->calls;
      tconnected += h->connected;
    }
    h = h->next;
  }
  printf("\n");
}

static void usage(int status)
{
  if (status != 0)
    fprintf(stderr, "Try `%s --help' for more information.\n", progname);
  else
  {
    printf("Usage: %s [ -?, -h, --help ] [ -v, --version ] [ -n, --news ] [ -N, --nonews ] [ logfile statsfile ]\n\n",
	   progname);
    printf("    --version         output version information and exit\n");
    printf("    --news            display separate stats for news articles\n");
    printf("    --nonews          do not display stats for news articles\n");
  }
  exit(status);
}

int main(int argc, char *argv[])
{
#ifdef DEFAULT_NEWSSTATS
  int newsstats = 1;

#else
  int newsstats = 0;

#endif

  char c;

  char logfilename[FILENAME_MAX], statsfilename[FILENAME_MAX];

  progname = strrchr(argv[0], '/');	/* The program's name */
  if (progname)
  {
    ++progname;
  }
  else
  {
    progname = argv[0];
  }

  while ((c = getopt_long(argc, argv, "h?vnN",
			  long_options, (int *)0)) != EOF)
  {
    switch (c)
    {
    case 0:
      break;
    case 'h':
    case '?':
      usage(0);
      break;
    case 'v':
      fprintf(stderr, "%s version $Revision: 1.0 $\n", progname);
      return 1;
      break;
    case 'n':
      newsstats = 1;
      break;
    case 'N':
      newsstats = 0;
      break;
    default:
      usage(1);
      break;
    }
  }

  if (((argc - optind) != 2) && (argc != optind))
    usage(1);

  if (argc == optind)
  {
    strcpy(logfilename, LOGFILEPATH);
    strcpy(statsfilename, STATSFILEPATH);
  }
  else
  {
    strcpy(logfilename, argv[optind]);
    optind++;
    strcpy(statsfilename, argv[optind]);
  }

  hosttab = BuildHostTable(CFGFILE);
  ReadStatsfile(statsfilename);

  AnalyzeLogfile(logfilename, newsstats);
  PrintResults(hosttab, newsstats);

  return (0);
}
