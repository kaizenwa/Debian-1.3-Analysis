/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "defs.h"
#include "globals.h"
#include "subs.h"
#include "bookmks.h"

/* #define DEBUGBMK */


int iiiii;
#define LISTFILES pfprot("\nlistf\n"); for(iiiii=0;iiiii<nfilemks;iiiii++) pfprot("dfn %s dfnb %s\n",filemks[iiiii]->dvifilename,filemks[iiiii]->lastpos.dvifilename)
#define LISTBOOKS pfprot("\nlistb\n"); for(iiiii=0;iiiii<curbmks->n;iiiii++) pfprot("n %d p %d x %d y %d\n",curbmks->d[iiiii]->name,curbmks->d[iiiii]->pagenum,curbmks->d[iiiii]->dvixpos,curbmks->d[iiiii]->dviypos)



void bookmksinit(bookmarklist* bookmks) {
  if(bookmks==NULL) return;
  bookmks->n=0;
  bookmks->d=NULL;
}

void bookmkskill(bookmarklist* bookmks) {
  int i;
  if(bookmks==NULL) return;
  for(i=0;i<bookmks->n;i++) {
    freemem(&(bookmks->d[i]->dvifilename));
    freemem(bookmks->d+i);
  }
  freemem(&(bookmks->d));
}

void rolldownbookmk(bookmarklist* bookmks) {
  int i;
  bookmark* bbu;
  if(bookmks==NULL) return;
  if(bookmks->n <=0) return;
  bbu=bookmks->d[0];
  for(i=0;i<bookmks->n-1;i++) bookmks->d[i]=bookmks->d[i+1];
  bookmks->d[i]=bbu;
}

int findrolldownbookmk(bookmarklist* bookmks,int name) {
  int i;
  bookmark *bbu;
  if(bookmks==NULL) return(0);
  for(i=0;i<bookmks->n;i++) if(bookmks->d[i]->name == name) break;
  if(i>=bookmks->n) return(0);
  for(;i>0;i--) rolldownbookmk(bookmks);
  return(1);
}

 
int checkbookmk(bookmarklist* bookmks,int name) {
  int i;
  if(bookmks==NULL) return(0);
#ifdef DEBUGBMK
  pfprot("(checkbkm..)");
#endif
  for(i=0;i<bookmks->n;i++) 
    if(bookmks->d[i]->name ==name) break;
  if(i>=bookmks->n) return(0); 
  return(1);
} 

void freebookmknumber(bookmarklist* bookmks,int i) {
  if(bookmks==NULL) return;
  if(bookmks->n<=i) return;
  freemem(&(bookmks->d[i]->dvifilename));
  freemem(bookmks->d+i);
  for(;i<bookmks->n-1;i++) bookmks->d[i]=bookmks->d[i+1];
  bookmks->n--;
  reallocmem(&(bookmks->d),sizeof(bookmark*)*bookmks->n);  
} 

void freebookmk(bookmarklist* bookmks) {
  freebookmknumber(bookmks,0);
}


void allocnewbookmk(bookmarklist* bookmks) {
  int i;
  if(bookmks==NULL) return;
#ifdef DEBUGBMK
  pfprot("(allocbookmk ..");
#endif
  reallocmem(&(bookmks->d),sizeof(bookmark*)*(bookmks->n+1));
  for(i=bookmks->n;i>0;i--) bookmks->d[i]=bookmks->d[i-1];
  allocmem(bookmks->d,sizeof(bookmark));
  bookmks->d[0]->dvifilename=NULL;
  bookmks->d[0]->name=-1;
  bookmks->n++;
#ifdef DEBUGBMK
  pfprot(" allocbookmk done)");
#endif
} 


int amiatbookmkqm(bookmarklist* bookmks,int i) {
  bookmark *thebmk;
  if(bookmks==NULL) return(0);
  if(bookmks->n<=i) return(0);
#ifdef DEBUGBMK
  pfprot("(amiatbookmk ..");
#endif
  thebmk=bookmks->d[i];
  if(thebmk->dvixpos!=dvixpos) return(0); 
  if(thebmk->dviypos!=dviypos) return(0);
  if(thebmk->marksxpxl!=marksxpxl) return(0); 
  if(thebmk->marksypxl!=marksypxl) return(0);
  if(thebmk->pagenum!=cpage->num) return(0);
  if(fabs(thebmk->fshrink-fshrink)>0.005) return(0);
  if(thebmk->dvifilename==NULL) return(1);
  if(strcmp(thebmk->dvifilename,visfmk->dvifilename)!=0) return(0);
#ifdef DEBUGBMK
  pfprot(" yes)");
#endif
  return(1);
}


int existsbookmkqm(bookmarklist* bookmks) {
  bookmark *thebmk;
  int i;
  if(bookmks==NULL) return(-1);
#ifdef DEBUGBMK
  pfprot("(exbookmk ..");
#endif
  for(i=0;i<bookmks->n;i++) {
    thebmk=bookmks->d[i];
    if(thebmk->dvixpos!=dvixpos) continue; 
    if(thebmk->dviypos!=dviypos) continue;
    if(thebmk->pagenum!=cpage->num) continue;
    if(fabs(thebmk->fshrink-fshrink)>0.005) continue;
    if(thebmk->dvifilename==NULL) break;
    if(strcmp(thebmk->dvifilename,visfmk->dvifilename)!=0) continue;
    break;
  }
#ifdef DEBUGBMK
  pfprot(" done xbookmk %d)",i);
#endif
  if(i<bookmks->n) return(i);
  return(-1);
}

void setbookmk(bookmark* thebmk, int doname) {
  if(thebmk==NULL) return;
#ifdef DEBUGBMK
  pfprot("(setbookmk ... ");
#endif
  if(doname)
    stralloccpy(&(thebmk->dvifilename),visfmk->dvifilename);
  thebmk->pagenum=cpage->num;
  thebmk->dvixpos=dvixpos; 
  thebmk->dviypos=dviypos;
  thebmk->fshrink=fshrink;
  thebmk->marksxpxl=marksxpxl;
  thebmk->marksypxl=marksypxl;
#ifdef DEBUGBMK
  pfprot(" done)");
#endif
} 

void addbookmk(bookmarklist* bookmks, int doname) {
  bookmark *thebmk;
  if(bookmks==NULL) return;
#ifdef DEBUGBMK
  pfprot("(addbookmk ..");
#endif
  allocnewbookmk(bookmks);
  thebmk=bookmks->d[0]; 
  setbookmk(thebmk,doname);
}

void filemksinit(void) {
  nfilemks=0;
  filemks=NULL;
  curfmk=NULL;
  curbmks=NULL;
  visfmk=NULL;
  visbmks=NULL;
}

void filemkskill(void) {
  int i;
  for(i=0;i<nfilemks;i++) {
    freemem(&(filemks[i]->dvifilename));
    bookmkskill(&(filemks[i]->bookmks));
  }
  freemem(&filemks);
  curfmk=NULL;
  curbmks=NULL;
  visbmks=NULL;
  visfmk=NULL;
}

void rolldownfilemk(void) {
  int i;
  filemark* bbu;
#ifdef DEBUGBMK
  pfprot("(rolldownfile... )");
#endif
  if(nfilemks<=0) return;
  bbu=filemks[0];
  for(i=0;i<nfilemks-1;i++) filemks[i]=filemks[i+1];
  filemks[i]=bbu;
  curfmk=filemks[0];
  curbmks=&(curfmk->bookmks);
}

void rollupfilemk(void) {
  int i;
  filemark* bbu;
#ifdef DEBUGBMK
  pfprot("(rollupfile... )");
#endif
  if(nfilemks<=0) return;
  bbu=filemks[nfilemks-1];
  for(i=nfilemks-1;i>0;i--) filemks[i]=filemks[i-1];
  filemks[0]=bbu;
  curfmk=filemks[0];
  curbmks=&(curfmk->bookmks);
}

int findrolldownfilemk(char* fname) {
  int i;
  filemark *bbu;
#ifdef DEBUGBMK
  pfprot("(findfile. %s in %d.. )",fname,nfilemks);
#endif
  for(i=0;i<nfilemks;i++)
    if(strcmp(filemks[i]->dvifilename,fname)==0) break;
  if(i>=nfilemks) return(0);
#ifdef DEBUGBMK
  pfprot("..found-)");
#endif
  for(;i>0;i--) rolldownfilemk();
  curfmk=filemks[0];
  curbmks=&(curfmk->bookmks);
  return(1);
}

void freefilemknumber(int i) {
#ifdef DEBUGBMK
  pfprot("(freefilei... )");
#endif
  if(nfilemks<=i) return;
  if(visfmk==filemks[i]) {visfmk=NULL; visbmks=NULL;}
  freemem(&(filemks[i]->dvifilename));
  bookmkskill(&(filemks[i]->bookmks));
  freemem(filemks+i);
  for(;i<nfilemks-1;i++) filemks[i]=filemks[i+1];
  reallocmem(&filemks,sizeof(filemark*)*(nfilemks-1));
  nfilemks--;
  if(nfilemks>0) {
    curfmk=filemks[0];
    curbmks=&(curfmk->bookmks);
  } else {
    curfmk=NULL;
    curbmks=NULL;
    visbmks=NULL;
    visfmk=NULL;
  }
} 

void freefilemk(void) {
  freefilemknumber(0);
}

void freefilemkname(char* fname) {
  int i,j;
  filemark *bbu;
#ifdef DEBUGBMK
  pfprot("(freefilename... )");
#endif
  for(i=0;i<nfilemks;i++) {
    if(strcmp(filemks[i]->dvifilename,fname)!=0) continue;
    freefilemknumber(i);
    i--;
  }
}

void allocnewfilemk(char* name) {
  int i;
#ifdef DEBUGBMK
  pfprot("(allocfile... ");
#endif
  if(name==NULL) return; /* shoild not happen ! */
  reallocmem(&filemks,sizeof(filemark*)*(nfilemks+1));
  for(i=nfilemks;i>0;i--) filemks[i]=filemks[i-1];
  nfilemks++;
  allocmem(filemks,sizeof(filemark));
  curfmk=filemks[0];
  memcpy(curfmk,&fmkstar,sizeof(filemark)); /* all ptrs NULL, : init lastpos*/
  stralloccpy(&(curfmk->dvifilename),name); 
  curfmk->lastpos.dvixpos=
           (MMTOPXL(curfmk->papxmm)-vgaxdim)/2-MMTOPXL(curfmk->hoffmm);
  curfmk->lastpos.dviypos= -INSIDEPXL - MMTOPXL(curfmk->voffmm);
  curfmk->lastpos.fshrink= fshrink;

  curbmks=&(curfmk->bookmks);
  bookmksinit(curbmks);
#ifdef DEBUGBMK
  LISTFILES;
  pfprot("done)");
#endif
} 

void updatefilemk(void) {
  int i;
  if(visfmk==NULL) return;
#ifdef DEBUGBMK
  pfprot("(updatefile. ");
#endif
  setbookmk(&(visfmk->lastpos),1);
#ifdef DEBUGBMK
  LISTFILES;
  pfprot("done");
#endif
}











