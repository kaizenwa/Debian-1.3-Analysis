/* speed.c */
/* Copyright (C) 1993 Eric Young - see README for more details */
/* 11-Sep-92 Andrew Daviel   Support for Silicon Graphics IRIX added */
/* 06-Apr-92 Luke Brennan    Support for VMS and add extra signal calls */


typedef unsigned char des_cblock[8];
typedef struct des_ks_struct
	{
	union	{
		des_cblock _;
		/* make sure things are correct size on machines with
		 * 8 byte longs */
		unsigned long pad[2];
		} ks;
/* #define _	ks._ */
	} des_key_schedule[16];

#define TIMES

#include <stdio.h>
#include <signal.h>
#ifndef VMS
#ifndef _IRIX
#include <time.h>
#endif
#ifdef TIMES
#include <sys/types.h>
#include <sys/times.h>
#endif /* TIMES */
#else /* VMS */
#include <types.h>
struct tms {
	time_t tms_utime;
	time_t tms_stime;
	time_t tms_uchild;	/* I dunno...  */
	time_t tms_uchildsys;	/* so these names are a guess :-) */
	}
#endif
#ifndef TIMES
#include <sys/timeb.h>
#endif

#include "des_crypt.h"

/* The following if from times(3) man page.  It may need to be changed */
#ifndef CLK_TCK
#ifndef VMS
#define HZ	60.0
#else /* VMS */
#define HZ	100.0
#endif
#else /* CLK_TCK */
#define HZ ((double)CLK_TCK)
#endif

#define BUFSIZE	((long)1024*8)
long run=0;

#ifdef SIGALRM
#ifdef __STDC__
#define SIGRETTYPE void
#else
#define SIGRETTYPE int
#endif 

SIGRETTYPE sig_done(sig)
int sig;
	{
	signal(SIGALRM,sig_done);
	run=0;
	}
#endif

#define START	0
#define STOP	1

double Time_F(s)
int s;
	{
	double ret;
#ifdef TIMES
	static struct tms tstart,tend;

	if (s == START)
		{
		times(&tstart);
		return(0);
		}
	else
		{
		times(&tend);
		ret=((double)(tend.tms_utime-tstart.tms_utime))/HZ;
		return((ret == 0.0)?1e-6:ret);
		}
#else /* !times() */
	static struct timeb tstart,tend;
	long i;

	if (s == START)
		{
		ftime(&tstart);
		return(0);
		}
	else
		{
		ftime(&tend);
		i=(long)tend.millitm-(long)tstart.millitm;
		ret=((double)(tend.time-tstart.time))+((double)i)/1000.0;
		return((ret == 0.0)?1e-6:ret);
		}
#endif
	}

#define COND(c)	(run)
#define COUNT(d) (count)

main(argc,argv)
int argc;
char *argv[];
	{
	long count;
	static unsigned char buf[BUFSIZE];
	static des_cblock key={0x12,0x34,0x56,0x78,0x9a,0xbc,0xde,0xf0};
	des_key_schedule sch;
	double d,a,b,c;
	long ca,cb,cc,cd;

	signal(SIGALRM,sig_done);

	printf("Doing ecb_crypt on %ld byte blocks for 10 seconds\n",
		BUFSIZE);
	alarm(10);

	Time_F(START);
	for (count=0,run=1; COND(cb); count++)
		ecb_crypt(key, buf, BUFSIZE, DES_ENCRYPT);
	d=Time_F(STOP);
	printf("%ld ecb_crypt's in %.2f second\n",count,d);
	b=((double)COUNT(cb)*BUFSIZE)/d;

	printf("Doing cbc_crypt on %ld byte blocks for 10 seconds\n",
		BUFSIZE);
	alarm(10);

	Time_F(START);
	for (count=0,run=1; COND(cc); count++)
		cbc_crypt(key,buf,BUFSIZE,DES_ENCRYPT,buf);
	d=Time_F(STOP);
	printf("%ld cbc_crypt's of %ld byte blocks in %.2f second\n",
		count,BUFSIZE,d);
	c=((double)COUNT(cc)*BUFSIZE)/d;

	printf("DES ecb bytes per sec = %12.2f (%5.1fuS)\n",b,8.0e6/b);
	printf("DES cbc bytes per sec = %12.2f (%5.1fuS)\n",c,8.0e6/c);
}
