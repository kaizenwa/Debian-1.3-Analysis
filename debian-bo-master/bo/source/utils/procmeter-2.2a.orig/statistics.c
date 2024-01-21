/***************************************
  $Header: /home/amb/procmeter/RCS/statistics.c 2.9 1997/03/15 19:50:22 amb Exp $

  ProcMeter - A simple performance monitor using /proc - Version 2.2a.
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1994,95,96 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#include <stdio.h>
#include <stdlib.h>

#include <time.h>
#include <sys/time.h>

#include "procmeter.h"


Stats stats[N_STATS]=
{
 /* CPU       */  {"cpu"      , "%"   , 0 ,   20  },
 /* CPU_USER  */  {"cpu-user" , "%"   , 0 ,   20  },
 /* CPU_NICE  */  {"cpu-nice" , "%"   , 0 ,   20  },
 /* CPU_SYS   */  {"cpu-sys"  , "%"   , 0 ,   20  },
 /* CPU_IDLE  */  {"cpu-idle" , "%"   , 0 ,   20  },
 /* LOAD      */  {"load"     , "1"   , 0 , 1000  }, /* Scale is only used to convert the value to a long */
 /* PROC      */  {"proc"     , "10"  , 0 ,   10  },
 /* CONTEXT   */  {"context"  , "100" , 0 ,  100  },
 /* SWAP      */  {"swap"     , "50"  , 0 ,   50  },
 /* SWAP_IN   */  {"swap-in"  , "50"  , 0 ,   50  },
 /* SWAP_OUT  */  {"swap-out" , "50"  , 0 ,   50  },
 /* PAGE      */  {"page"     , "50"  , 0 ,  100  },
 /* PAGE_IN   */  {"page-in"  , "50"  , 0 ,  100  },
 /* PAGE_OUT  */  {"page-out" , "50"  , 0 ,  100  },
 /* DISK      */  {"disk"     , "25"  , 0 ,   25  },
 /* INTR      */  {"intr"     , "100" , 0 ,  100  },
 /* MEM_FREE  */  {"mem-free" , "1 MB", 0 , 1<<20 }, /* This may get reset later */
 /* MEM_USED  */  {"mem-used" , "1 MB", 0 , 1<<20 }, /* This may get reset later */
 /* MEM_BUFF  */  {"mem-buff" , "1 MB", 0 , 1<<20 }, /* This may get reset later */
 /* MEM_CACHE */  {"mem-cache", "1 MB", 0 , 1<<20 }, /* This may get reset later */
 /* MEM_SWAP  */  {"mem-swap" , "1 MB", 0 , 1<<20 }, /* This may get reset later */
 /* LPKT      */  {"lpkt"     , "100" , 0 ,  100  },
 /* FPKT      */  {"fpkt"     , "50"  , 0 ,   50  },
 /* FPKT_RX   */  {"fpkt-rx"  , "50"  , 0 ,   50  },
 /* FPKT_TX   */  {"fpkt-tx"  , "50"  , 0 ,   50  },
 /* SPKT      */  {"spkt"     , "5"   , 0 ,    5  },
 /* SPKT_RX   */  {"spkt-rx"  , "5"   , 0 ,    5  },
 /* SPKT_TX   */  {"spkt-tx"  , "5"   , 0 ,    5  }
};

/*+ The interval between updates. +*/
static long interval;

/*+ The values for the statistics, +*/
static long values[2][N_STATS], /*+ an array containing the previous and current values. +*/
            *current=values[0], /*+ a pointer to the array of current values. +*/
            *previous=values[1];/*+ a pointer to the array of previous values. +*/

/*+ A flag to indicate the availability of the file +*/
static int proc_stat=0,         /*+ /proc/stat. +*/
           proc_loadavg=0,      /*+ /proc/loadavg. +*/
           proc_meminfo=0,      /*+ /proc/meminfo. +*/
           proc_net_dev=0;      /*+ /proc/net/dev. +*/

/*+ The format of the statistics in /proc/net/dev. (changed in V ~2.1.28). +*/
static char *proc_net_dev_format=NULL;


/*++++++++++++++++++++++++++++++++++++++
  Confirm the format of the files in /proc.
  ++++++++++++++++++++++++++++++++++++++*/

void VerifyStatistics(void)
{
 int error=0;
 FILE* f;
 char line[256];
 int i;

 /* Verify the statistics from /proc/stat */

 f=fopen("/proc/stat","r");
 if(!f)
    error=fprintf(stderr,"ProcMeter: Failed to open the '/proc/stat' file.\n"),
          fprintf(stderr,"           No cpu, cpu-user, cpu-nice, cpu-sys & cpu-idle\n"),
          fprintf(stderr,"           No disk\n"),
          fprintf(stderr,"           No page, page-in & page-out\n"),
          fprintf(stderr,"           No swap, swap-in & swap-out\n"),
          fprintf(stderr,"           No intr\n"),
          fprintf(stderr,"           No context\n");
 else
    if(!fgets(line,256,f))
       error=fprintf(stderr,"ProcMeter: Failed to read from the '/proc/stat' file.\n"),
             fprintf(stderr,"           No cpu, cpu-user, cpu-nice, cpu-sys & cpu-idle\n"),
             fprintf(stderr,"           No disk\n"),
             fprintf(stderr,"           No page, page-in & page-out\n"),
             fprintf(stderr,"           No swap, swap-in & swap-out\n"),
             fprintf(stderr,"           No intr\n"),
             fprintf(stderr,"           No context\n");
    else
      {
       unsigned long d0,d1,d2,d3;

       if(sscanf(line,"cpu %lu %lu %lu %lu",&current[CPU_USER],&current[CPU_NICE],&current[CPU_SYS],&current[CPU_IDLE])==4)
          stats[CPU].avail=stats[CPU_USER].avail=stats[CPU_NICE].avail=stats[CPU_SYS].avail=stats[CPU_IDLE].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'cpu' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No cpu, cpu-user, cpu-nice, cpu-sys & cpu-idle\n");

       fgets(line,256,f);
       if(sscanf(line,"disk %lu %lu %lu %lu",&d0,&d1,&d2,&d3)==4)
          stats[DISK].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'disk' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No disk\n");
       while(line[0]=='d')      /* kernel version > ~1.3.0 */
          fgets(line,256,f);

       if(sscanf(line,"page %lu %lu",&current[PAGE_IN],&current[PAGE_OUT])==2)
          stats[PAGE].avail=stats[PAGE_IN].avail=stats[PAGE_OUT].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'page' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No page, page-in & page-out\n");

       fgets(line,256,f);
       if(sscanf(line,"swap %lu %lu",&current[SWAP_IN],&current[SWAP_OUT])==2)
          stats[SWAP].avail=stats[SWAP_IN].avail=stats[SWAP_OUT].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'swap' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No swap, swap-in & swap-out\n");

       fgets(line,256,f);
       if(sscanf(line,"intr %lu",&current[INTR])==1)
          stats[INTR].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'intr' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No intr\n");

       fgets(line,256,f);
       if(sscanf(line,"ctxt %lu",&current[CONTEXT])==1)
          stats[CONTEXT].avail=1;
       else
          error=fprintf(stderr,"ProcMeter: Unexpected 'ctxt' line in the '/proc/stat' file.\n"),
                fprintf(stderr,"           No context\n");

       proc_stat=stats[CPU].avail|stats[DISK].avail|stats[PAGE].avail|stats[SWAP].avail|stats[INTR].avail|stats[CONTEXT].avail;
      }

 if(f)
    fclose(f);

 /* Get the statistics from /proc/loadavg */

 f=fopen("/proc/loadavg","r");
 if(!f)
    error=fprintf(stderr,"ProcMeter: Failed to open the '/proc/loadavg' file.\n"),
          fprintf(stderr,"           No load\n"),
          fprintf(stderr,"           No proc\n");
 else
    if(!fgets(line,256,f))
       error=fprintf(stderr,"ProcMeter: Failed to read from the '/proc/loadavg' file.\n"),
             fprintf(stderr,"           No load\n"),
             fprintf(stderr,"           No proc\n");
    else
      {
       double d;

       fgets(line,256,f);
       if(sscanf(line,"%lf %*f %*f %*d/%ld",&d,&current[PROC])==2)
          stats[LOAD].avail=stats[PROC].avail=1;
       else
          if(sscanf(line,"%lf",&d)==1)
             stats[LOAD].avail=1;
          else
             error=fprintf(stderr,"ProcMeter: Unexpected 'load' line in the '/proc/loadavg' file.\n"),
                   fprintf(stderr,"           No load\n"),
                   fprintf(stderr,"           No proc\n");

       proc_loadavg=stats[LOAD].avail;
      }

 if(f)
    fclose(f);

 /* Get the statistics from /proc/meminfo */

 f=fopen("/proc/meminfo","r");
 if(!f)
    error=fprintf(stderr,"ProcMeter: Failed to open the '/proc/meminfo' file.\n"),
          fprintf(stderr,"           No mem-free, mem-used, mem-buff & mem-cache\n"),
          fprintf(stderr,"           No mem-swap\n");
 else
    if(!fgets(line,256,f))
       error=fprintf(stderr,"ProcMeter: Failed to read from the '/proc/meminfo' file.\n"),
             fprintf(stderr,"           No mem-free, mem-used, mem-buff & mem-cache\n"),
             fprintf(stderr,"           No mem-swap\n");
    else
       if(strcmp(line,"        total:   used:    free:   shared:  buffers:\n") && /* kernel version < ~2.0.0 */
          strcmp(line,"        total:    used:    free:  shared: buffers:  cached:\n")) /* kernel version > ~2.0.0 */
          error=fprintf(stderr,"ProcMeter: Unexpected header line in '/proc/meminfo'.\n"),
                fprintf(stderr,"           No mem-free, mem-used, mem-buff & mem-cache\n"),
                fprintf(stderr,"           No mem-swap\n");
       else
         {
          long mem_tot=0;

          fgets(line,256,f);
          if(sscanf(line,"Mem: %lu %lu %lu %*u %lu %lu",&mem_tot,&current[MEM_USED],&current[MEM_FREE],&current[MEM_BUFF],&current[MEM_CACHE])==5)
             stats[MEM_FREE].avail=stats[MEM_USED].avail=stats[MEM_BUFF].avail=stats[MEM_CACHE].avail=1;
          else
             if(sscanf(line,"Mem: %lu %lu %lu %*u %lu",&mem_tot,&current[MEM_USED],&current[MEM_FREE],&current[MEM_BUFF])==4)
                stats[MEM_FREE].avail=stats[MEM_USED].avail=stats[MEM_BUFF].avail=1;
             else
                error=fprintf(stderr,"ProcMeter: Unexpected 'Mem' line in the '/proc/meminfo' file.\n"),
                      fprintf(stderr,"           No mem-free, mem-used, mem-buff & mem-cache\n");

          fgets(line,256,f);
          if(sscanf(line,"Swap: %*u %lu",&current[MEM_SWAP])==1)
             stats[MEM_SWAP].avail=1;
          else
             error=fprintf(stderr,"ProcMeter: Unexpected 'Swap' line in the '/proc/meminfo' file.\n"),
                   fprintf(stderr,"           No mem-swap\n");

          proc_meminfo=stats[MEM_FREE].avail|stats[MEM_SWAP].avail;

          if(proc_meminfo && mem_tot>(1<<24))
            {
             long scale=1;
             char str[6];

             mem_tot>>=24;
             while(mem_tot)
               {mem_tot>>=1; scale<<=1;}

             sprintf(str,"%ld MB",scale); scale<<=20;
             strcpy(stats[MEM_FREE ].units,str); stats[MEM_FREE ].scale=scale;
             strcpy(stats[MEM_USED ].units,str); stats[MEM_USED ].scale=scale;
             strcpy(stats[MEM_BUFF ].units,str); stats[MEM_BUFF ].scale=scale;
             strcpy(stats[MEM_CACHE].units,str); stats[MEM_CACHE].scale=scale;
             strcpy(stats[MEM_SWAP ].units,str); stats[MEM_SWAP ].scale=scale;
            }
         }

 if(f)
    fclose(f);

 /* Get the statistics from /proc/net/dev */

 f=fopen("/proc/net/dev","r");
 if(!f)
    error=fprintf(stderr,"ProcMeter: Failed to open the '/proc/net/dev' file.\n"),
          fprintf(stderr,"           No lpkt\n"),
          fprintf(stderr,"           No fpkt, fpkt-rx & fpkt-tx\n"),
          fprintf(stderr,"           No spkt, spkt-rx & spkt-tx\n");
 else
    if(!fgets(line,256,f))
       error=fprintf(stderr,"ProcMeter: Failed to read from the '/proc/net/dev' file.\n"),
             fprintf(stderr,"           No lpkt\n"),
             fprintf(stderr,"           No fpkt, fpkt-rx & fpkt-tx\n"),
             fprintf(stderr,"           No spkt, spkt-rx & spkt-tx\n");
    else
       if(strcmp(line,"Inter-|   Receive                  |  Transmit\n") && /* all existing kernel versions */
          strcmp(line,"Inter-|   Receive                           |  Transmit\n")) /* possible future change */
          error=fprintf(stderr,"ProcMeter: Unexpected header line 1 in the '/proc/net/dev' file.\n"),
                fprintf(stderr,"           No lpkt\n"),
                fprintf(stderr,"           No fpkt, fpkt-rx & fpkt-tx\n"),
                fprintf(stderr,"           No spkt, spkt-rx & spkt-tx\n");
       else
         {
          fgets(line,256,f);
          if(strcmp(line," face |packets errs drop fifo frame|packets errs drop fifo colls carrier\n") && /* kernel version < ~2.1.28 */
             strcmp(line," face |bytes    packets errs drop fifo frame|bytes    packets errs drop fifo colls carrier\n")) /* kernel version > ~2.1.28 */
             error=fprintf(stderr,"ProcMeter: Unexpected header line 2 in the '/proc/net/dev' file.\n"),
                   fprintf(stderr,"           No lpkt\n"),
                   fprintf(stderr,"           No fpkt, fpkt-rx & fpkt-tx\n"),
                   fprintf(stderr,"           No spkt, spkt-rx & spkt-tx\n");
          else
            {
             if(!strcmp(line," face |packets errs drop fifo frame|packets errs drop fifo colls carrier\n"))
                proc_net_dev_format="%lu %*u %*u %*u %*u %lu";
             else
                proc_net_dev_format="%*u %lu %*u %*u %*u %*u %*u %lu";

             while(fgets(line,256,f))
               {
                int i;
                char intf[2];
                long rx=0,tx=0;

                for(i=strlen(line);i>6 && line[i]!=':';i--); ++i;
                if(sscanf(line,"%1s",intf)==1 && sscanf(&line[i],proc_net_dev_format,&rx,&tx)==2)
                  {
                   if(*intf=='l' || *intf=='d') /* 'lo' or 'dummy' devices. */
                      stats[LPKT].avail=1;
                   else
                      if(*intf=='s' || *intf=='p' || *intf=='f' || *intf=='i') /* 'sl' or 'ppp'/'plip' or 'flip' or 'isdn'/'ippp' devices. */
                         stats[SPKT].avail=stats[SPKT_RX].avail=stats[SPKT_TX].avail=1;
                      else /* other devices */
                         stats[FPKT].avail=stats[FPKT_RX].avail=stats[FPKT_TX].avail=1;
                  }
                else
                   if(strcmp(&line[i]," No statistics available.\n"))
                      error=fprintf(stderr,"ProcMeter: Unexpected device line in the '/proc/net/dev' file.\n"),
                            fprintf(stderr,"           No ?pkt, ?pkt-rx & ?pkt-tx (maybe)\n");
               }

             proc_net_dev=stats[LPKT].avail|stats[FPKT].avail|stats[SPKT].avail;
            }
         }

 if(f)
    fclose(f);

 /* Print an error message if needed. */

 if(error)
   {
    fprintf(stderr,"\n"
                   "ProcMeter: Warning, one or more incompatibilities in /proc were encountered in\n"
                   "           the initialisation of the meters.  This will lead to one or more of\n"
                   "           the meters being unavailable.\n");
    fprintf(stderr,"           Please report this to ProcMeter's author amb@gedanken.demon.co.uk,\n"
                   "           stating the version of ProcMeter, the error message, the contents\n"
                   "           of the file in error in /proc and the version of the Linux kernel.\n"
                   "\n");

    if(!(proc_stat|proc_loadavg|proc_meminfo|proc_net_dev))
      {
       fprintf(stderr,"ProcMeter: There are no statistics available\n"
                      "           Are you sure that /proc is mounted?\n");
       exit(1);
      }
   }

 /* Get ready to run. */

 for(i=0;i<N_STATS;i++)
    current[i]=previous[i]=0;

 GetStatistics();
}


/*++++++++++++++++++++++++++++++++++++++
  Find out the statistics by looking in /proc.
  ++++++++++++++++++++++++++++++++++++++*/

void GetStatistics(void)
{
 FILE* f;
 char line[256];

 static int which=0;
 static struct timeval t[2],*cur_time,*prev_time;

 /* Select which of the two sets of values to use. */

 previous=values[which];
 prev_time=&t[which];

 which=which^1;

 current=values[which];
 cur_time=&t[which];

 /* Get the time */

 gettimeofday(cur_time,NULL);

 /* Get the statistics from /proc/stat */

 if(proc_stat)
   {
    unsigned long d0,d1,d2,d3;

    f=fopen("/proc/stat","r");
    if(!f)
      {fprintf(stderr,"ProcMeter: Failed to open the '/proc/stat' file.\n");exit(1);}

    fgets(line,256,f);
    if(stats[CPU].avail)
       sscanf(line,"cpu %lu %lu %lu %lu",&current[CPU_USER],&current[CPU_NICE],&current[CPU_SYS],&current[CPU_IDLE]);
    fgets(line,256,f);
    if(stats[DISK].avail)
       sscanf(line,"disk %lu %lu %lu %lu",&d0,&d1,&d2,&d3);current[DISK]=d0+d1+d2+d3;
    while(line[0]=='d')      /* kernel version > ~1.3.x */
       fgets(line,256,f);
    if(stats[PAGE].avail)
       sscanf(line,"page %lu %lu",&current[PAGE_IN],&current[PAGE_OUT]);
    fgets(line,256,f);
    if(stats[SWAP].avail)
       sscanf(line,"swap %lu %lu",&current[SWAP_IN],&current[SWAP_OUT]);
    fgets(line,256,f);
    if(stats[INTR].avail)
       sscanf(line,"intr %lu",&current[INTR]);
    fgets(line,256,f);
    if(stats[CONTEXT].avail)
       sscanf(line,"ctxt %lu",&current[CONTEXT]);

    fclose(f);
   }

 /* Get the statistics from /proc/loadavg */

 if(proc_loadavg)
   {
    double d;

    f=fopen("/proc/loadavg","r");
    if(!f)
      {fprintf(stderr,"ProcMeter: Failed to open the '/proc/loadavg' file.\n");exit(1);}

    fgets(line,256,f);
    sscanf(line,"%lf %*f %*f %*d/%ld",&d,&current[PROC]);
    current[LOAD]=(long)(d*stats[LOAD].scale);

    fclose(f);
   }

 /* Get the statistics from /proc/meminfo */

 if(proc_meminfo)
   {
    f=fopen("/proc/meminfo","r");
    if(!f)
      {fprintf(stderr,"ProcMeter: Failed to open the '/proc/meminfo' file.\n");exit(1);}

    fgets(line,256,f);
    fgets(line,256,f);
    if(stats[MEM_FREE].avail)
       sscanf(line,"Mem: %*u %lu %lu %*u %lu %lu",&current[MEM_USED],&current[MEM_FREE],&current[MEM_BUFF],&current[MEM_CACHE]);
    fgets(line,256,f);
    if(stats[MEM_SWAP].avail)
       sscanf(line,"Swap: %*u %lu",&current[MEM_SWAP]);

    fclose(f);
   }

 /* Get the statistics from /proc/net/dev */

 if(proc_net_dev)
   {
    f=fopen("/proc/net/dev","r");
    if(!f)
      {fprintf(stderr,"ProcMeter: Failed to open the '/proc/net/dev' file.\n");exit(1);}

    current[LPKT]=0;
    current[FPKT_RX]=current[FPKT_TX]=0;
    current[SPKT_RX]=current[SPKT_TX]=0;
    fgets(line,256,f);
    fgets(line,256,f);
    while(fgets(line,256,f))
      {
       int i;
       char intf[2];
       long rx=0,tx=0;

       sscanf(line,"%1s",intf);
       for(i=strlen(line);i>6 && line[i]!=':';i--); ++i;
       sscanf(&line[i],proc_net_dev_format,&rx,&tx);
       if(*intf=='l' || *intf=='d') /* 'lo' or 'dummy' devices. */
          current[LPKT]+=tx;
       else
          if(*intf=='s' || *intf=='p' || *intf=='f' || *intf=='i') /* 'sl' or 'ppp'/'plip' or 'flip' or 'isdn'/'ippp' devices. */
            {current[SPKT_RX]+=rx;current[SPKT_TX]+=tx;}
          else /* other devices */
            {current[FPKT_RX]+=rx;current[FPKT_TX]+=tx;}
      }

    fclose(f);
   }

 /* The derived values */

 if(stats[CPU].avail)
    current[CPU]=current[CPU_USER]+current[CPU_NICE]+current[CPU_SYS];

 if(stats[PAGE].avail)
    current[PAGE]=current[PAGE_IN]+current[PAGE_OUT];

 if(stats[SWAP].avail)
    current[SWAP]=current[SWAP_IN]+current[SWAP_OUT];

 if(stats[MEM_FREE].avail)
    current[MEM_USED]-=current[MEM_BUFF];
 if(stats[MEM_CACHE].avail)
    current[MEM_USED]-=current[MEM_CACHE];

 if(stats[FPKT].avail)
    current[FPKT]=current[FPKT_RX]+current[FPKT_TX];
 if(stats[SPKT].avail)
    current[SPKT]=current[SPKT_RX]+current[SPKT_TX];

 interval=(cur_time->tv_sec-prev_time->tv_sec)*1000+(cur_time->tv_usec-prev_time->tv_usec)/1000;
}


/*++++++++++++++++++++++++++++++++++++++
  Return the value that is to be used for the meter.

  double CurrentValue Returns the current value for the specified statistic.

  Statistics type The type of the Widget.
  ++++++++++++++++++++++++++++++++++++++*/

double CurrentValue(Statistics type)
{
 double value;

 switch(type)
   {
   case CPU:
   case CPU_USER:
   case CPU_NICE:
   case CPU_SYS:
   case CPU_IDLE:
    value=100.0*(double)(current[type]-previous[type])/(double)(current[CPU]+current[CPU_IDLE]-previous[CPU]-previous[CPU_IDLE]);
    if(value>100.0)
       value=100.0;
    break;

   case LOAD:

   case PROC:

   case MEM_USED:
   case MEM_FREE:
   case MEM_BUFF:
   case MEM_CACHE:
   case MEM_SWAP:
    value=current[type];
    break;

   default:
    value=1000.0*(double)(current[type]-previous[type])/interval;
   }

 if(value<0)
    value=0;

 value/=(double)stats[type].scale;

 return(value);
}
