/* misc.c - Miscellaneous functions for xmp
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: misc.c,v 1.5 1997/03/13 13:17:25 claudio Exp $
 *
 * $Log: misc.c,v $
 * Revision 1.5  1997/03/13 13:17:25  claudio
 * Type names changed & other minor adjusts. Added check_alignment().
 *
 * Revision 1.4  1997/01/19 01:38:19  claudio
 * Cosmetic changes.
 *
 * Revision 1.3  1997/01/02 00:06:37  claudio
 * Added Amiga octave range and Hipolito's C2SPD conversion function.
 *
 * Revision 1.2  1996/12/10 19:21:27  claudio
 * Minor changes.
 *
 * Revision 1.1  1996/12/07 20:41:22  claudio
 * Initial revision
 *
 */

#include "config.h"

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include "xmp.h"

static uint16 period_amiga[] = {
  /*  0       1       2       3       4       5       6       7   */
   0x1c56, 0x1c22, 0x1bee, 0x1bbb, 0x1b87, 0x1b55, 0x1b22, 0x1af0,  /* B  */
   0x1abf, 0x1a8e, 0x1a5d, 0x1a2c, 0x19fc, 0x19cc, 0x199c, 0x196d,  /* C  */
   0x193e, 0x1910, 0x18e2, 0x18b4, 0x1886, 0x1859, 0x182c, 0x1800,  /* C# */
   0x17d4, 0x17a8, 0x177c, 0x1751, 0x1726, 0x16fb, 0x16d1, 0x16a7,  /* D  */
   0x167d, 0x1654, 0x162b, 0x1602, 0x15d9, 0x15b1, 0x1589, 0x1562,  /* D# */
   0x153a, 0x1513, 0x14ec, 0x14c6, 0x149f, 0x1479, 0x1454, 0x142e,  /* E  */
   0x1409, 0x13e4, 0x13c0, 0x139b, 0x1377, 0x1353, 0x1330, 0x130c,  /* F  */
   0x12e9, 0x12c6, 0x12a4, 0x1282, 0x125f, 0x123e, 0x121c, 0x11fb,  /* F# */
   0x11da, 0x11b9, 0x1198, 0x1178, 0x1157, 0x1137, 0x1118, 0x10f8,  /* G  */
   0x10d9, 0x10ba, 0x109b, 0x107d, 0x105e, 0x1040, 0x1022, 0x1004,  /* G# */
   0x0fe7, 0x0fca, 0x0fad, 0x0f90, 0x0f73, 0x0f57, 0x0f3a, 0x0f1e,  /* A  */
   0x0f02, 0x0ee7, 0x0ecb, 0x0eb0, 0x0e95, 0x0e7a, 0x0e5f, 0x0e45,  /* A# */
   0x0e2b, 0x0e11, 0x0df7, 0x0ddd, 0x0dc3, 0x0daa, 0x0d91, 0x0d78,  /* B  */
};


/* Get period from note and finetune */
int note_to_period(int note,int finetune,int type)
{
   int aux,aux2;

   if(type) return (((120-note)<<7)-finetune)>>3;	/* Linear */

   if(finetune<0) finetune+=128; else ++note;
   aux=((note%12)<<3)+(finetune>>4);
   aux2=((period_amiga[aux]-period_amiga[aux+1])*(finetune&0x0f))>>4;
   return ((period_amiga[aux]+aux2)>>(note/12));	/* Amiga */
}


/* Get note from period using Amiga frequency table */
/* Note: this function is used only by the MOD loader! */
int period_to_note(int p)
{
    int aux,note,finetune;

    if(!p) return p;
    for(note=NOTE_Bb0;p<=(MAX_PERIOD/2);note+=12,p<<=1);
    for(aux=MAX_NOTE;p>period_amiga[aux];aux-=8,note--);
    for(finetune=7;finetune&&(period_amiga[aux]>p);aux++,finetune--);

    return note-(finetune>>2);
}


/* Get pitchbend from base note and period */ 
int period_to_bend(int p,int n,int a,int g,int t)
{
    int aux,note,s;

    if(a) {					/* Force Amiga limits */
        if(p>AMIGA_LIMIT_LOWER) p=AMIGA_LIMIT_LOWER;
        if(p<AMIGA_LIMIT_UPPER) p=AMIGA_LIMIT_UPPER;
    }

    if(t) return (100*(((120-n)<<4)-p))>>4;	/* Linear */
  
    if(p<MIN_PERIOD_A) p=MIN_PERIOD_A;
    for(note=NOTE_B0-1,s=0;p<=(MAX_PERIOD/2);note+=12,p<<=1,s++);
    for(aux=MAX_NOTE;p>period_amiga[aux];aux-=8,note--);
    note-=n; p>>=s;
    n=period_amiga[aux]>>s;
    aux=period_amiga[aux+8]>>s;
    aux=100*note+100*(n-p)/(n-aux);		/* Amiga */
    return g?aux/100*100:aux;			/* Glissando switch */
}


/* Convert finetunes=1200*log2(C2SPD/8363)) using the Amiga frequency table */
void c2spd_to_note(int c2spd,char *n,char *f)
{
    int aux,note,finetune;

    c2spd=(140*c2spd)>>8;
    if(!c2spd) { *n=*f=0; return; }
    for(note=8;c2spd<=(MAX_PERIOD/2);note-=12,c2spd<<=1);
    for(;c2spd>MAX_PERIOD;note+=12,c2spd>>=1);
    for(aux=0;period_amiga[aux]>c2spd;aux+=8,note--);
    for(finetune=-1;period_amiga[aux]<c2spd;aux--,finetune++);
    *n=(int8)note; *f=(int8)(finetune<<4);
}


void die(char *fmt,...)		/* I love Perl :) */
{
    va_list a;
    
    va_start(a,fmt);
    fprintf(stderr,"Fatal: ");
    vfprintf(stderr,fmt,a);
    fprintf(stderr,"\n");
    va_end(a);

    exit(-1);
}


int report(char *fmt,...)
{
    va_list a;
    int n;
    
    va_start(a,fmt);
    n=vfprintf(stderr,fmt,a);
    va_end(a);

    return n;
}


/* Convert differential to absolute sample data */
void diff2abs(int l,int r,char *p)
{
    uint16 new,old=0;
    uint16 *w=(uint16 *)p;

    if(r) {
        for(l>>=2;l--;) {
            new=*w+old;
            *w++=new;
            old=new;
        } 
    } else {
        for(;l--;) {
            new=*p+old;
            *p++=(uint8)new;
            old=(uint8)new;
        } 
    }
}



/* Convert signed to unsigned sample data */
void sig2uns(int l,int r,char* p)
{
    uint16 *w=(uint16 *)p;

    if(r) {
        for(l>>=2;l--;) {
            *w+=0x8000;
            w++;
        } 
    } else {
        for(;l--;) {
            p+=0x80;
            p++;
        } 
    }
}


char *str_adj(char *s)
{
    int i;

    for(i=0;i<strlen(s);i++)
        if(!isprint(s[i])||((uint8)s[i]>127)) s[i]=' ';
    while(*s&&(s[strlen(s)-1]==' ')) s[strlen(s)-1]=0;
    return s;
}


void check_alignment()
{
    struct { int8 lawful; int32 neutral; } PACKED good;
    int /*chaotic=42,*/evil=0;
    char *true_neutral="(You'll need to recompile)";

    for(
#ifdef XMP_LITTLE_ENDIAN
    evil=1
#endif
    ;sizeof(good)-5;)
	die("Structures are not byte-aligned. %s",true_neutral);
#if 0
    if((*(uint8*)&chaotic==42)^evil)
	die("Endianism is incorrect. %s",true_neutral);
#endif
}

