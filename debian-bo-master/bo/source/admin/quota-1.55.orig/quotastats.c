/*
 * QUOTA    An implementation of the diskquota system for the LINUX operating
 *          system. QUOTA is implemented using the BSD systemcall interface
 *          as the means of communication with the user level. Should work for
 *          all filesystems because of integration into the VFS layer of the
 *          operating system. This is based on the Melbourne quota system wich
 *          uses both user and group quota files.
 * 
 *          Program to query for the internal statistics.
 * 
 * Author:  Marco van Wieringen <mvw@planets.ow.nl> <mvw@tnix.net>
 *
 * Version: $Id: quotastats.c,v 1.1 1995/11/02 13:56:44 mvw Exp mvw $
 *
 *          This program is free software; you can redistribute it and/or
 *          modify it under the terms of the GNU General Public License as
 *          published by the Free Software Foundation; either version 2 of
 *          the License, or (at your option) any later version.
 */

#include <sys/types.h>
#include <linux/quota.h>
#include <stdio.h>
#include <unistd.h>

static inline int get_stats(struct dqstats *dqstats)
{
   return quotactl(QCMD(Q_GETSTATS, 0), (char *)NULL, 0, (caddr_t)dqstats);
}

static inline int print_stats(struct dqstats *dqstats)
{
   fprintf(stdout, "Number of dquot lookups: %ld\n", dqstats->lookups);
   fprintf(stdout, "Number of dquot drops: %ld\n", dqstats->drops);
   fprintf(stdout, "Number of still active inodes with quota : %ld\n",
           dqstats->lookups - dqstats->drops);
   fprintf(stdout, "Number of dquot reads: %ld\n", dqstats->reads);
   fprintf(stdout, "Number of dquot writes: %ld\n", dqstats->writes);
   fprintf(stdout, "Number of quotafile syncs: %ld\n", dqstats->syncs);
   fprintf(stdout, "Number of dquot cache hits: %ld\n", dqstats->cache_hits);
   fprintf(stdout, "Number of pages allocated: %ld\n", dqstats->pages_allocated);
   fprintf(stdout, "Number of allocated dquots: %ld\n", dqstats->allocated_dquots);
   fprintf(stdout, "Number of free dquots: %ld\n", dqstats->free_dquots);
   fprintf(stdout, "Number of in use dquot entries (user/group): %ld\n",
           dqstats->allocated_dquots - dqstats->free_dquots);
   return(0); 
}

main(int argc, char **argv)
{
   struct dqstats dqstats;

   if (!get_stats(&dqstats))
      print_stats(&dqstats);
}
