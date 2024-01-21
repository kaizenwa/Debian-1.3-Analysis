/***
 *** xfree_compat.h: misc. stuff to get the XFREE code to compile. This is junk :-(
 *** Written by Koen Gadeyne
 ***
 ***/
 
#ifndef _COMPAT_H
#define _COMPAT_H

#define NeedFunctionPrototypes 1

#include <stdio.h>
#include <unistd.h>

typedef int Bool;

#define ErrorF printf

typedef void *ScrnInfoRec;  /* dummy, not used by SVGATextMode */



extern void GlennsIODelay();

extern int CirrusSetClock(int freq);

Bool xf86DisableInterrupts();

void xf86EnableInterrupts();

void s3OutTiIndReg(unsigned char reg, unsigned char mask, unsigned char data);
unsigned char s3InTiIndReg(unsigned char reg);

void s3OutTi3026IndReg(unsigned char reg, unsigned char mask, unsigned char data);
unsigned char s3InTi3026IndReg(unsigned char reg);


void s3ProgramTi3025Clock();

extern int ARKgendacSetClock(
#if NeedFunctionPrototypes
        long,
        int
#endif
);

void set_ti_SOG(Bool SOG);

#endif

