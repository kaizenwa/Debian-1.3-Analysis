/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/28/1993
*/
#include <stdio.h>
#include <string.h>

struct fstree {
     struct fstree *next;
     struct fstree *child;
     char *name;
     int mountopt;
#define MTOPT_RW 0x01
#define MTOPT_RO 0x02
     int refcount;
     int rocount, rwcount;
};

struct hosts {
     struct hosts *next;
     struct fstree *fs;
     char *name;
};

static struct hosts *head = (struct hosts *)0;

void
addfsent(char *host, char *path, int mopt)
{
     struct hosts *hr;
     struct fstree *fr, *frs, **fhead;
     char *s, *e, *a, *b;
     int i;

     if(!host){
	  return;
     }
     if(!path){
	  return;
     }

     for(hr=head;hr;hr=hr->next)
	  if(!strcmp(hr->name, host))
	       break;

     if(!hr){
	  hr = (struct hosts *)malloc(sizeof(struct hosts));
	  hr->next = head;
	  head = hr;
	  hr->name = strdup(host);
	  hr->fs = (struct fstree *)0;
     }

     s = path;
     fr=hr->fs;
     fhead = &hr->fs;
     frs = 0;
     while(*s){
	  struct fstree *tail;
	  while(*s && *s == '/')
	       s++;
	  if(*s)
	       for(e=s+1;*e && *e != '/';e++)
		    ;
	  else
	       e = s;
	  tail = fr;
	  while(fr){
	       for(a=s,b=fr->name;*a && *b && a < e && *a == *b;a++,b++)
		    ;
	       if(!*b && a == e)
		    break;
	       tail = fr;
	       fr=fr->next;
	  }

	  if(!fr){
	       fr = (struct fstree *)malloc(sizeof(struct fstree));
	       fr->next = 0;
	       if(tail)
		    tail->next = fr;
	       if(!*(fhead))
		    *(fhead) = fr;
	       if(frs){
		    if(mopt & MTOPT_RO){
			 fr->mountopt = MTOPT_RO;
			 frs->rocount++;
		    }
		    else {
			 fr->mountopt = MTOPT_RW;
			 frs->rwcount++;
		    }
		    frs->refcount++;
	       }
	       fr->child = 0;
	       fr->name = (char *)malloc(e-s);
	       strncpy(fr->name, s, e-s);
	       fr->name[e-s] = 0;
	       fr->refcount = fr->rocount = fr->rwcount = 0;
	  }
	  else {
	       if(mopt & MTOPT_RO)
		    if(!(fr->mountopt & MTOPT_RO)){
			 fr->mountopt |= MTOPT_RO;
			 if(frs)
			      frs->rocount++;
		    }
	       else if(!(fr->mountopt & MTOPT_RW)){
		    fr->mountopt |= MTOPT_RW;
		    if(frs)
			 frs->rwcount++;
	       }
	  }

	  fhead = &fr->child;
	  if(!*e || *e == '/'){
	       s = e;
	       if(*s == '/')
		    s++;
	  }
	  if(!*s){
	       fr->mountopt = mopt;
	       fr->refcount = 1;
	       if(mopt & MTOPT_RO)
		    fr->rocount++;
	       else
		    fr->rwcount++;
	  }
	  frs = fr;
	  fr = fr->child;
     }
}

void
dispfs(char *p, int rw, int ro)
{
     static int col = 0;
     char buffer[256];
     int len;

     sprintf(buffer, "%4d %4d %s", rw, ro, p);
     fputs(buffer, stdout);
     
     if(!col){
	  len = 40-strlen(buffer);
	  for(;len > 0;--len)
	       putchar(' ');
	  col = 1;
     }
     else {
	  putchar('\n');
	  col = 0;
     }
}
     

void
fsshow(struct fstree *fr, char *pref, int lvl, int cumref)
{
     struct fstree *child, *fr2;
     char path[1025];
     
     if(fr){
	  for(fr2=fr;fr2;fr2=fr2->next){
	       sprintf(path, "%s/%s", pref, fr2->name);
	       if(fr2->refcount > 1 ||
		  (cumref == lvl && !fr2->child))
		    dispfs(path, fr2->rwcount, fr2->rocount);
	       fsshow(fr2->child, path, lvl+1, cumref+fr2->refcount);
	  }
     }
}
     

void
fsdump(void)
{
     struct hosts *hr = head;
     struct fstree *fr1, *fr2, *fr3;
     char path[1025];

     printf(" R/W  R/O Directory                      R/W  R/O Directory\n");

     for(;hr;hr=hr->next){
	       sprintf(path, "%s:", hr->name);
	       fsshow(hr->fs, path, 0, 0);
     }
}
