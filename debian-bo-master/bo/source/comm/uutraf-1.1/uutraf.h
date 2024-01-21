/*****************************************************************************
 * uutraf.h -- Part of uutraf.c, UUCP Traffic analyzer and cost estimator    *
 *                                                                           *
 * Reads an Taylor-UUCP Log-/Statsfile, and generates a report out of it     *
 *                                                                           *
 * uutraf.h is (c) David Frey, 1995    					     *
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

/* $Log: uutraf.h,v $
 * Revision 1.2  1996/08/03 18:23:55  david
 * GetStatsEntry doesn't need the received flag.
 * The internal time is now measured in etime_t (=unsigned long int)
 *
 * Revision 1.1  1996/04/13 21:55:25  david
 * Changed Log and Stats directory location to the official one,
 * namely /var/log/uucp/{Log,Stats}.
 *
 * Revision 1.0  1996/01/01 16:56:06  david
 * Initial revision
 * */

#define KBYTE 1000.0
#define HOSTNAMELENGTH    8

#define LOGFILEPATH   "/var/log/uucp/Log"
#define STATSFILEPATH "/var/log/uucp/Stats"

/* "Magic" constants */

/* Text in the Log file */

#define CALLINGSYSTEM "Calling system "
#define LOGINSUCCESS  "Login successful"
#define INCOMINGCALL  "Incoming call "
#define CALLCOMPLETE  "Call complete ("

#define RECEIVING     "Receiving "
#define SENDING       "Sending "
#define EXECUTING     "Executing "
#define ERROR	      "ERROR: "

/* and in the Stats file */
#define FAILED        "failed after "
#define RECEIVED      "received "
#define SENT          "sent "

typedef unsigned long int etime_t;

struct Statsentry
{
  etime_t when;
  int received;
  off_t   size;
  struct  Statsentry *next;
};

struct Hostentry
{
  char   hostname[HOSTNAMELENGTH+1];
  off_t  datarecbytes, datasentbytes, filerecbytes, filesentbytes; 
  off_t  mailrecbytes, mailsentbytes, newsrecbytes, newssentbytes;
  off_t  bruttobytes;
  uint   datarecnum, datasentnum,     filerecnum, filesentnum;
  uint   mailrecnum, mailsentnum,     newsrecnum, newssentnum;
  float  rectime, senttime, connected;
  uint   calls;
  float  pttcostpercall, pttlinecostpermin, pttcost;
  float  provlinecostpermin, provfeepermail, provfeepermailkb;
  float  provfeeperfile, provfeeperfilekb;
  float  provfeeperkbmult, provfeepernews, provfeepernewskb;
  float  provmailcost, provnewscost, provfilecost, provcost;
  int    nocost;
  struct Statsentry *statslist;
  struct Hostentry *next;
};

typedef struct Hostentry hostentry;
typedef struct Statsentry statsentry;

/* Globals */

extern char       *progname;		/* name of the program */
extern hostentry  *hosttab;		/* linked list of hosts */
extern statsentry *statslist;		/* linked list of statsentries */

extern char       *sec2str(time_t secs);
extern time_t      unixtime(struct tm time);
extern etime_t     statstime(struct tm time, time_t sec100);

extern hostentry  *gethostentry(char *hostname);
extern hostentry  *BuildHostTable(char *cfgfilename);
extern void        ReadStatsfile(char *statsfilename);
extern void        ProcessStatsfile(hostentry *host);
extern statsentry *GetStatsEntry(statsentry *statslist, int received,
				 off_t size, etime_t when);
