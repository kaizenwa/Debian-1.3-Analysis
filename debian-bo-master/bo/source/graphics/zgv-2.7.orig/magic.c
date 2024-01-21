/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * magic.c - Determines type of image file.
 */

#include <stdio.h>
#include <string.h>
#include "magic.h"


int magic_ident(filename)
char *filename;
{
FILE *in;
unsigned char buf[3];

if((in=fopen(filename,"rb"))==NULL)
  return(_IS_BAD);

buf[0]=buf[1]=buf[2]=0;
fread(buf,1,3,in);
fclose(in);

/* We use the following rules:
 * P?M files must have 'P', then a digit; '1'<=digit<='6'.
 * GIF files must have "GIF"
 * JPEG files must have first byte=0xff, second byte=0xd8 (M_SOI)
 * BMP files must start with "BM"
 * TGA files suck rocks ;-) (heuristics in this case)
 * 
 */

/* PBM/PGM/PPM */
if(buf[0]=='P' && buf[1]>='1' && buf[1]<='6')
  return(_IS_PNM);

/* GIF */
if(strncmp(buf,"GIF",3)==0)
  return(_IS_GIF);

/* JPEG */
if(buf[0]==0xff && buf[1]==0xd8)
  return(_IS_JPEG);

/* BMP */
if(buf[0]=='B' && buf[1]=='M')
  return(_IS_BMP);

#ifdef PNG_SUPPORT
/* PNG */
if(buf[0]==0x89 && buf[1]=='P' && buf[2]=='N')
  return(_IS_PNG);		/* XXX should test the rest I s'pose */
#endif

/* TGA */
/* this is hairy, since TGA files don't have a magic number.
 * we make a guess based on some of the image info.
 * (whether it has a colourmap or not, and the type)
 */
if((buf[1]==1 && (buf[2]==1 || buf[2]==9)) ||
   (buf[1]<=1 && (buf[2]==2 || buf[2]==10)))
  return(_IS_TGA);

/* if no valid header */
return(_IS_BAD);
}
