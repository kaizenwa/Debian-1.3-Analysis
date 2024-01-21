
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "thrust_types.h"
#include "level.h"
#include "things.h"
#include "fast_gr.h"
#include "graphics.h"
#include "thrust.h"

int
matchsliders(void)
{
  int i,j;
  int match,dist;

  for(i=0; i<nrsliders; i++) {
    if(!sliders[i].match && sliders[i].dir) {
      match=nrsliders;
      dist=lenx;
      for(j=0; j<nrsliders; j++) {
	if(!sliders[j].match && !sliders[j].dir
	   && ((sliders[i].type-1)/3==(sliders[j].type-1)/3)) {
	  switch(sliders[i].dir) {
	  case 1:
	    if(sliders[j].type!=12) { /* Horizontal sliders */
	      if((sliders[i].y1 == sliders[j].y1)
		 && (sliders[j].x1>sliders[i].x1)
		 && (sliders[j].x1-sliders[i].x1 < dist)) {
		dist=sliders[j].x1-sliders[i].x1;
		match=j;
	      }
	    }
	    else /* Vertical sliders */
	      if((sliders[i].x1 == sliders[j].x1)
		 && (sliders[j].y1>sliders[i].y1)
		 && (sliders[j].y1-sliders[i].y1 < dist)) {
		dist=sliders[j].y1-sliders[i].y1;
		match=j;
	      }	      
	    break;
	  case 2:
	    if(sliders[j].type!=12) { /* Horizontal sliders */
	      if((sliders[i].y1 == sliders[j].y1)
		 && (sliders[i].x1>sliders[j].x1)
		 && (sliders[i].x1-sliders[j].x1 < dist)) {
		dist=sliders[i].x1-sliders[j].x1;
		match=j;
	      }
	    }
	    else /* Vertical sliders */
	      if((sliders[i].x1 == sliders[j].x1)
		 && (sliders[i].y1>sliders[j].y1)
		 && (sliders[i].y1-sliders[j].y1 < dist)) {
		dist=sliders[i].y1-sliders[j].y1;
		match=j;
	      }
	    break;
	  }
	}
      }
      if(match==nrsliders) {
	printf("Found slider with no blocker.\n");
	return(0);
      }
      sliders[i].match=1;
      sliders[match].match=1;
      sliders[i].x2=sliders[match].x1;
      sliders[i].y2=sliders[match].y1;
      j=closestbutton((sliders[i].x1+sliders[i].x2)>>1,
		      (sliders[i].y1+sliders[i].y2)>>1);
      if(j<0) {
	printf("Found no botton to connect the slider with.\n");
	return(0);
      }
      j=majorbutton(j);
      sliders[i].next=((buttondata *)things[j].data)->sliders;
      ((buttondata *)things[j].data)->sliders=&sliders[i];
    }
  }

  return(1);
}

int
ismajorbutton(int tag)
{
  int i;

  for(i=0; i<nrthings; i++)
    if((things[i].type==7 || things[i].type==8)
      && (((buttondata *)things[i].data)->tag==tag))
	return(i);
  return(-1);
}

void
releasebana(void)
{
  int i;

  for(i=0; i<nrthings; i++)
    if(things[i].data)
      free(things[i].data);
  nrthings=0;
  nrsliders=0;
  nrbarriers=0;
  nrrestartpoints=0;
}

int
readbana(char **ptr)
     /* Läser in banan i variablen 'bana'.
     // Filen startar med åtta värden, t.ex
     // 123
     // 69
     // 8
     // 10
     // 16
     // R
     // G
     // B
     // Dessa fem tal innebär följande:
     // Banan är 123 tecken bred (lenx) och 69 tecken
     // hög (leny). Den börjar med 8 rader med stjärnor och
     // därefter följer 10 tomrader.
     // Banan slutar med 16 rader med ifyllda block.
     // Efter dessa tal kommer banans definition som
     // placeras mellan tomraderna och de ifyllda blocken. */
{
  int stat=1;
  word t1,t2,x,y,z;
  word sx,sy,sz;
  char *temp;
  buttondata *bdata;

  releasebana();

  spacestation=0;
  lenx=atoi(ptr[0]);
  if(lenx%BBILDX!=0)
    stat=0;
  lenx3=lenx<<3;
  leny=atoi(ptr[1]);
  sx=atoi(ptr[2]);
  for(sy=0; sy<3; sy++)
    if(sx<2*BBILDY) {
      sx+=BBILDY-(leny%BBILDY);
      leny+=BBILDY-(leny%BBILDY);
    }
  leny3=leny<<3;
  if(lenx*(long)leny>(long)maxlenx*maxleny)
    stat=0;
  sy=atoi(ptr[3]);
  sz=atoi(ptr[4]);
  x=y=lenx*(sx-2*BBILDY);
  colorr=atoi(ptr[5]);
  colorg=atoi(ptr[6]);
  colorb=atoi(ptr[7]);
  if(stat) {
    memset(bana,32,lenx*(sx+sy));
    x>>=6;
    for(z=0; z<BBILDY; z++) {
      t1=random()%(BBILDY*lenx);
      t2=random()%16;
      *(bana+t1)=t2;
      *(bana+t1+BBILDY*lenx)=t2;
    }
    for(; x; x--)
      *(bana+2*BBILDY*lenx+(random()%y))=random()%16;
    for(y=sx+sy; y<leny-sz && stat; y++) {
      temp=ptr[y-sx-sy+8];
      for(x=0; x<lenx && stat; x++) {
	switch(temp[x]) {
	case '#':
	  if(nrbarriers<maxbarriers) {
	    barriers[nrbarriers].x=x;
	    barriers[nrbarriers++].y=y;
	  }
	  else
	    stat=0;
	  break;
	case '*':
	  if(nrrestartpoints<maxrestartpoints) {
	    restartpoints[nrrestartpoints].x=x;
	    restartpoints[nrrestartpoints++].y=y;
	  }
	  else
	    stat=0;
	  break;
	case '@':       /* Slide \block to the right */
	case 'A':       /* Slide \block to the left */
	case 'B':       /* Horizontal \blocker */
	case 'C':       /* Slide /block to the right */
	case 'D':       /* Slide /block to the left */
	case 'E':       /* Horizontal /blocker */
	case 'F':       /* Slide |block to the right */
	case 'G':       /* Slide |block to the left */
	case 'H':       /* Horizontal |blocker */
	case 'I':       /* Slide -block downwards */
	case 'J':       /* Slide -block upwards */
	case 'K':       /* Vertical -blocker */
	  if(nrsliders<maxsliders)
	    newslider(x,y,temp[x]-'@'+1);
	  else
	    stat=0;
	  break;
	case 'L':       /* Button on left wall */
	  if(nrthings<maxthings) {
	    bdata=malloc(sizeof(buttondata));
	    if(!bdata) {
	      printf("Out of memory.\n");
	      stat=0;
	    }
	    else {
	      bdata->sliders=NULL;
	      bdata->tag=temp[x-1];
	      bdata->major=ismajorbutton(bdata->tag);
	      newthing((x<<3)+9,(y<<3)+8,x,y,7,bdata);
	    }
	  }
	  else
	    stat=0;
	  break;
	case 'N':       /* Button on right wall */
	  if(nrthings<maxthings) {
	    bdata=malloc(sizeof(buttondata));
	    if(!bdata) {
	      printf("Out of memory.\n");
	      stat=0;
	    }
	    else {
	      bdata->sliders=NULL;
	      bdata->tag=temp[x-1];
	      bdata->major=ismajorbutton(bdata->tag);
	      newthing((x<<3)-1,(y<<3)+8,x,y,8,bdata);
	    }
	  }
	  else
	    stat=0;
	  break;
	case 'P':       /*  |\x Bunker */
	  if(nrthings<maxthings)
	    newthing((x<<3)+14,(y<<3)+8,x,y,3,NULL);
	  else
	    stat=0;
	  break;
	case 'U':       /*  x/| Bunker */
	  if(nrthings<maxthings)
	    newthing((x<<3)+10,(y<<3)+8,x,y,4,NULL);
	  else
	    stat=0;
	  break;
	case '[':       /*  |/x Bunker */
	  if(nrthings<maxthings)
	    newthing((x<<3)-2,(y<<3)+8,x-2,y,5,NULL);
	  else
	    stat=0;
	  break;
	case '\\':      /*  x\| Bunker */
	  if(nrthings<maxthings)
	    newthing((x<<3)+10,(y<<3)+7,x,y,6,NULL);
	  else
	    stat=0;
	  break;
	case '`':       /* Fuel */
	  if(nrthings<maxthings)
	    newthing((x<<3)+8,(y<<3)+8,x,y,1,NULL);
	  else
	    stat=0;
	  break;
	case 'd':       /* Spacestation */
	  if(nrthings<maxthings && !spacestation) {
	    spacestation=1;
	    ssblip=1;
	    ssx=x+2;
	    ssy=y;
	    newthing((x<<3)+12,(y<<3)+9,x,y,2,NULL);
	  }
	  else
	    stat=0;
	  break;
	case 'm':
	  loadbx=x;
	  loadby=y;
	  break;
	}
	if(!stat)
	  printf("Unable to create a thing.\n");
      }
      memcpy(bana+(long)y*lenx,temp,lenx);
    }
    memset(bana+(long)y*lenx,112,lenx*sz);
  }

  if(stat) {
    for(x=0; x<nrthings; x++) {
      if(things[x].type==7)
	*(bana+(long)things[x].py*lenx+things[x].px-1)=112;
      if(things[x].type==8)
	*(bana+(long)things[x].py*lenx+things[x].px-1)=32;
    }
    for(x=0; x<nrsliders; x++)
      *(bana+(long)sliders[x].y1*lenx+sliders[x].x1)=112;
    if(!matchsliders()) {
      printf("Unable to match all sliders.\n");
      stat=0;
    }
    if(nrrestartpoints && !nrbarriers) {
      printf("No restartpoint for the barriers.\n");
      stat=0;
    }
    for(x=0; x<nrbarriers; x++) {
      barriers[x].restart=&restartpoints[0];
      for(y=1; y<nrrestartpoints; y++)
	if((abs(barriers[x].x-restartpoints[y].x)
	    + abs(barriers[x].y-restartpoints[y].y))
	   < (abs(barriers[x].x-barriers[x].restart->x)
	      + abs(barriers[x].y-barriers[x].restart->y)))
	  barriers[x].restart=&restartpoints[y];
    }
  }

  return(stat);
}
