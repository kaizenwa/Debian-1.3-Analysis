/*
 * rstatd for linux using /proc/stat
 * Copyright (C) 1993 Rudolf Koenig 
 *
 * Adapted for Linux kernel version 0.99.15y and newer by Karl Keyte
 * Date: 94.03.09          e-Mail: KKEYTE@ESOC.BITNET
 *
 * Adapted for Linux kernel version 1.00 and newer by Malcolm Reeves
 * Date: 94.05.24          e-Mail: reeves@pangea.usask.ca
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <rpc/rpc.h>
#include <stdio.h>
#include "rstat.h"

struct statstime  s_t;
struct statsswtch s_s;
struct stats      s_o; /* orig */
int havedisk = 0;

static void fill_s_t();

struct statstime *
rstatproc_stats_3(v, cl)
  void *v;
  struct svc_req *cl;
{
  fill_s_t();
  return &s_t;
}

struct statsswtch *
rstatproc_stats_2(v, cl)
  void *v;
  struct svc_req *cl;
{
  fill_s_t();
  bcopy((char *)&s_t, (char *)&s_s, sizeof(s_s) - sizeof(int));
  s_s.if_opackets = s_t.if_opackets;
  return &s_s;
}

struct stats *
rstatproc_stats_1(v, cl)
  void *v;
  struct svc_req *cl;
{
  fill_s_t();
  bcopy((char *)&s_t, (char *)&s_o, sizeof(s_o) - sizeof(int));
  s_o.if_opackets = s_t.if_opackets;
  return &s_o;
}

unsigned int *
rstatproc_havedisk_3(v, cl)
  void *v;
  struct svc_req *cl;
{
  fill_s_t();
  return &havedisk;
}

unsigned int *
rstatproc_havedisk_2(v, cl)
  void *v;
  struct svc_req *cl;
{
  return rstatproc_havedisk_3(v, cl);
}

unsigned int *
rstatproc_havedisk_1(v, cl)
  void *v;
  struct svc_req *cl;
{
  return rstatproc_havedisk_3(v, cl);
}

/* FIXME: unsigned/signed overflow (But: It should seldom occur :-) */
static void
fill_s_t()
{
  FILE *fp;
  double ld1, ld2, ld3;
/**** Added 94:05:24 *****/   
  struct timeval tv;
  struct timezone tz;
/*************************/

  if(!(fp = fopen("/proc/stat", "r")))
    return;
  fscanf(fp, "cpu  %u %u %u %u\n", 
	  &s_t.cp_time[0], &s_t.cp_time[1], &s_t.cp_time[2], &s_t.cp_time[3]);
  fscanf(fp, "disk %u %u %u %u\n", 
	  &s_t.dk_xfer[0], &s_t.dk_xfer[1], &s_t.dk_xfer[2], &s_t.dk_xfer[3]);
  fscanf(fp, "page %u %u\n", &s_t.v_pgpgin, &s_t.v_pgpgout);
  fscanf(fp, "swap %u %u\n", &s_t.v_pswpin, &s_t.v_pswpout);
  fscanf(fp, "intr %u\n", &s_t.v_intr);
  fscanf(fp, "ctxt %u\n", &s_t.v_swtch);
  fclose(fp);

  if(!(fp = fopen("/proc/loadavg", "r")))
    return;
  fscanf(fp, "%lf %lf %lf\n", &ld1, &ld2, &ld3);
  s_t.avenrun[0] = ld1 * FSCALE;
  s_t.avenrun[1] = ld2 * FSCALE;
  s_t.avenrun[2] = ld3 * FSCALE;
  fclose(fp);

/**** Added 94:05:24 *****/   
/* get uptime & idletime */
  if(!(fp = fopen("/proc/uptime", "r")))
    return;
  fscanf(fp, "%lf %lf\n", &ld1, &ld2);
  fclose(fp);

/* had trouble reading btime from /proc/stat */
/* so did it this way using gettimeofday()   */
/* had to change rstat_timeval struct to     */
/* accommodate u_long values for boottime    */ 
  gettimeofday(&tv, &tz);
  s_t.curtime.tv_sec = tv.tv_sec;
  s_t.boottime.tv_sec = s_t.curtime.tv_sec - (u_long)ld1;

/* don't bother with the microseconds    */
  s_t.curtime.tv_usec = 0;
  s_t.boottime.tv_usec = 0;
/**** End of added code ****/

  havedisk = 0;
}
