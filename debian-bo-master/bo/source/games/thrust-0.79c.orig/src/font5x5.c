
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <ctype.h>

#include "keyboard.h"
#include "thrust_types.h"
#include "thrust.h"
#include "font5x5.h"
#include "fast_gr.h"
#include "gr_drv.h"

byte chcolor=TEXTCOLOR;
byte chpaper=0;
byte chflag=0;

void
printgc(int x, int y, byte ch)
{
  int dx;
  int dy;
  char width = font[ch][0];
  byte bits=1<<(width-1);
  byte bit=bits;

  for(dy=1; dy<6; dy++, bit=bits) {
    for(dx=0; dx<width; dx++, bit>>=1) {
      if(font[ch][dy] & bit)
	putpixel(x+dx, y+dy-1, chcolor);
      else if(chflag)
	putpixel(x+dx, y+dy-1, chpaper);
    }
    if(chflag)
      putpixel(x+dx, y+dy-1, chpaper);
  }
}

int
printgs(int x, int y, char *string)
{
  byte i;
  byte len=strlen(string);
  byte ch;
  int ox=x;

  for(i=0; i<len; i++) {
    ch=*(string+i);
    if(ch < 96)
      switch(ch)
	{
	case 10:
	  y+=6;
	case 13:
	  x=ox;
	  break;
	default:
	  printgc(x, y, ch);
	  x += font[ch][0]+1;
	}
  }

  return(x);
}

int
readgs(int x, int y, char string[], int maxc, int maxp, char flag)
{
  int i, j;
  int key;
  byte ch;
  byte temp=chcolor;
  int leftx=x;
  byte cursor=0;
  int xc;

  maxp+=x;
  i=strlen(string);
  for(j=0; j<i; j++)
    string[j] = toupper(string[j]);
  x=printgs(x, y, string);

  do {
    do {
      key=0;
      cursor=1-cursor;
      for(j=0; j<12 && !key; j++) {
	key=getkey();
	if(!key)
	  usleep(25000L);
      }
      /*      vga_lockvc();*/
      for(j=0; j<5; j++)
	putpixel(x+j, y+5, cursor*temp);
      displayscreen();
      /*      vga_unlockvc();*/
    } while(!key);
    /*    vga_lockvc();*/
    for(j=0; j<5; j++)
      putpixel(x+j, y+5, chpaper);
    displayscreen();
    /*    vga_unlockvc();*/

    ch=toupper(key);
    switch(ch) {
    case 10:
      string[i]=0;
      ch=27;
      break;
    case 27:
      x=-1;
      break;
    case 127:
      if(i) {
	xc = 1+font[(int)string[--i]][0];
	x -= xc;
	chcolor=chpaper;
	/*	vga_lockvc();*/
	printgc(x, y, string[i]);
	displayscreen();
	/*	vga_unlockvc();*/
	chcolor=temp;
      }
      break;
    default:
      if(!font[ch][0]) {
	printf("\007");
	fflush(stdout);
      }
      else if(i!=maxc && x+(xc=1+font[ch][0])<=maxp) {
	string[i++]=ch;
	/*	vga_lockvc();*/
	printgc(x, y, ch);
	displayscreen();
	/*	vga_unlockvc();*/
	x+=xc;
      }
    }
  }
  while(ch!=27);

  if(flag) {
    /*    vga_lockvc();*/
    for(i=y; i<y+5; i++)
      for(xc=leftx; xc<maxp; xc++)
	putpixel(xc, i, chpaper);
    /*    vga_unlockvc();*/
    displayscreen();
  }
  return(x);
}

int
gstrlen(char *string)
{
  int len=strlen(string);
  int width=0;
  int count=0;
  int i;
  byte ch;

  for(i=0; i<len; i++)
    if((ch=*(string+i)) < 96)
      switch(ch) {
      case 13:
      case 10:
	if(count>width)
	  width=count;
	count=0;
	break;
      default:
	count += font[ch][0]+1;
      }

  return((count>width) ? count : width);
}
