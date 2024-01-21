/***************************************
  $Header: /home/amb/procmeter/RCS/procmeter.h 2.6 1996/08/28 18:55:40 amb Exp $

  ProcMeter - A simple performance monitor using /proc - Version 2.1.
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1994,95,96 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#ifndef PROCMETER_H
#define PROCMETER_H    /*+ To stop multiple inclusions. +*/

typedef enum { CPU,
               CPU_USER,
               CPU_NICE,
               CPU_SYS,
               CPU_IDLE,
               LOAD,
               PROC,
               CONTEXT,
               SWAP,
               SWAP_IN,
               SWAP_OUT,
               PAGE,
               PAGE_IN,
               PAGE_OUT,
               DISK,
               INTR,
               MEM_FREE,
               MEM_USED,
               MEM_BUFF,
               MEM_CACHE,
               MEM_SWAP,
               LPKT,
               FPKT,
               FPKT_RX,
               FPKT_TX,
               SPKT,
               SPKT_RX,
               SPKT_TX,
               N_STATS
              } Statistics;

typedef struct _Stats
{
 char name[10];                 /*+ The name of the statistic. +*/
 char units[6];                 /*+ The name of the units. +*/
 char avail;                    /*+ A true value if the statistic is available. +*/
 long scale;                    /*+ The scaling factor to apply. +*/
}
Stats;

extern Stats stats[N_STATS];

/* In statistics.c */

void VerifyStatistics(void);
void GetStatistics(void);
double CurrentValue(Statistics type);

#endif /* PROCMETER_H */
