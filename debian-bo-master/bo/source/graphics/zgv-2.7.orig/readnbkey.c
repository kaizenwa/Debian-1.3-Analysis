/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * readnbkey.c - I'd call this inkey$.c if it wasn't so awkward; reads a
 *               non-blocking key. Header file provides #defines for weirdo
 *               keys and such. This is to replace the mess I had before!
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "readnbkey.h"



/* returns a 'normal' ASCII value, or one of the values in readnbkey.h,
 * or zero if no key was pressed.
 */
int readnbkey(ttyfd)
int ttyfd;
{
int realkey,f;
char keybuf[1024];

/* this gets all the characters sent by the key into an ASCIIZ string */
f=0;
while((keybuf[f++]=getnbkey(ttyfd)));

realkey=0;

if((keybuf[0]==27)&&(strlen(keybuf)>1))
  {
  if(!strcmp(keybuf+1,"[A"))   realkey=RK_CURSOR_UP;
  if(!strcmp(keybuf+1,"[B"))   realkey=RK_CURSOR_DOWN;
  if(!strcmp(keybuf+1,"[D"))   realkey=RK_CURSOR_LEFT;
  if(!strcmp(keybuf+1,"[C"))   realkey=RK_CURSOR_RIGHT;
  
  if(!strcmp(keybuf+1,"[[A"))  realkey=RK_F1;
  if(!strcmp(keybuf+1,"[[B"))  realkey=RK_F2;
  if(!strcmp(keybuf+1,"[[C"))  realkey=RK_F3;
  if(!strcmp(keybuf+1,"[[D"))  realkey=RK_F4;
  if(!strcmp(keybuf+1,"[[E"))  realkey=RK_F5;
  if(!strcmp(keybuf+1,"[17~")) realkey=RK_F6;
  if(!strcmp(keybuf+1,"[18~")) realkey=RK_F7;
  if(!strcmp(keybuf+1,"[19~")) realkey=RK_F8;
  if(!strcmp(keybuf+1,"[20~")) realkey=RK_F9;
  if(!strcmp(keybuf+1,"[21~")) realkey=RK_F10;
  if(!strcmp(keybuf+1,"[23~")) realkey=RK_F11; /* can be shift F1 or F11 too */
  if(!strcmp(keybuf+1,"[24~")) realkey=RK_F12; /* can be shift F2 of F12 too */
  
  if(!strcmp(keybuf+1,"[25~")) realkey=RK_SHIFT_F3;
  if(!strcmp(keybuf+1,"[26~")) realkey=RK_SHIFT_F4;
  if(!strcmp(keybuf+1,"[28~")) realkey=RK_SHIFT_F5;
  if(!strcmp(keybuf+1,"[29~")) realkey=RK_SHIFT_F6;
  if(!strcmp(keybuf+1,"[31~")) realkey=RK_SHIFT_F7;
  if(!strcmp(keybuf+1,"[32~")) realkey=RK_SHIFT_F8;
  if(!strcmp(keybuf+1,"[33~")) realkey=RK_SHIFT_F9;
  if(!strcmp(keybuf+1,"[34~")) realkey=RK_SHIFT_F10;
  
  /* ctrl + Fn is the same as normal Fn, and you can't use alt + Fn. */
  
  if(!strcmp(keybuf+1,"[2~"))  realkey=RK_INSERT;
  if(!strcmp(keybuf+1,"[3~"))  realkey=RK_DELETE;
  if(!strcmp(keybuf+1,"[1~"))  realkey=RK_HOME;
  if(!strcmp(keybuf+1,"[4~"))  realkey=RK_END;
  if(!strcmp(keybuf+1,"[5~"))  realkey=RK_PAGE_UP;
  if(!strcmp(keybuf+1,"[6~"))  realkey=RK_PAGE_DOWN;
  }
else  
  realkey=keybuf[0];
    
return(realkey);
}



int getnbkey(ttyfd)
int ttyfd;
{
unsigned char c;

c=0;
read(ttyfd,&c,1);
return((int)c);
}
