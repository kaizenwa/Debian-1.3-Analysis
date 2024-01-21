/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


#ifdef __unix__
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#ifdef DOSFILES
#include <sys/nls.h>
#endif


#include "defs.h"
#include "globals.h"
#include "subs.h"

/* #define DEBUGMEM     */
/* #define DEBUGKILLLRU */
/* #define DEBUGBITMAPS */
/* #define DEBUGFILE */


void pfprot(char* fmt, ... ) {
	va_list args;

	va_start(args, fmt);
	vfprintf(stdout,fmt,args);
        fflush(stdout);
	va_end(args);
}

void pfverb(char* fmt, ... ) {
        va_list args;

	if(!verbose) return;
	va_start(args, fmt);
	vfprintf(stdout,fmt,args);
	fflush(stdout);
	va_end(args);
}

long scaled(long w, long z) {  long s; ulong t=w, u=z;
  /* should return  w * z / 2**20  */
  s = ( ( (((t & 65535L) * (u & 65535L)) >> 16)
	+   (t & 65535L) * (u >>    16)
	+   (t >>    16) * (u & 65535L)         ) >> 4)
    + ( (   (t >>    16) * (u >>    16)         ) << 12);
  if (w < 0)  s -= z << 12;  return s;
}



/*  one day here will be a kind of memorymenagement !*/


#define MEM(ap)   (*((char**)(ap)))

#define REF(ap)   ((char***)(ap))
#define NEXT(ap)   (((char**)(ap))+1)
#define PREV(ap)   (((char**)(ap))+2)
#define SIZE(ap)   (long*)(((char**)(ap))+3)
#define USER(ap)  (char*)(((char**)(ap))+4)
#define BASE(ap)  (char*)(((char**)(ap))-4)


static char* firstlru=NULL;
static char* lastlru=NULL;
static long totallrumem=0;

void printlrumem(void) {
  char* iptr;
  char* before;
  int count;
  iptr=firstlru;
  count=0;
  pfprot("\nLRU MEM LIST:\n");
  while(iptr!=NULL) {
    if(iptr==firstlru && *PREV(iptr) != NULL)
      pfprot("*** LINK ERROR ***\n");
    if(iptr==lastlru && *NEXT(iptr) != NULL)
      pfprot("*** LINK ERROR ***\n");
    if(**REF(iptr)!=iptr+16)
      pfprot("*** REF ERROR ***\n");
    count++;
    pfprot("mem at 0x%x *rev 0x%x size %d prev 0x%x next 0x%x \n",
           iptr,**REF(iptr),*SIZE(iptr),*PREV(iptr),*NEXT(iptr));
    before=iptr;
    iptr=*NEXT(iptr);
    if(iptr!=NULL)
      if(*PREV(iptr)!=before)
        pfprot("*** LINK ERROR ***\n");
  }
  pfprot("\nTHATS IT, total bytes %d in %d blocks\n\n",totallrumem,count);
}

void Tptr(char* ap) {  
  if(ap==NULL) {pfprot(" - not enough memory :-( "); exit(1);}
}

int killlru(void){
  char* bup;
  if(firstlru==NULL) return(0);
#ifdef DEBUGKILLLRU
  pfprot("(mem: killlru)");
  printlrumem();  
#endif
#ifdef DEBUGMEM
  pfprot("(mem: killlru 0x%x)",firstlru+16);
#endif
  bup=*NEXT(firstlru);
  totallrumem-=*SIZE(firstlru);
  **REF(firstlru)=NULL;
  free(firstlru);
  firstlru=bup;
  *PREV(firstlru)=NULL;
#ifdef DEBUGKILLLRU
  printlrumem();
#endif  
  return(1);
}

void addlru(char** ap, long size){
  if(firstlru!= NULL) {
    *NEXT(lastlru)=MEM(ap);
    *PREV(MEM(ap))=lastlru;
    *NEXT(MEM(ap))=NULL;
  } else {
    *PREV(MEM(ap))=NULL;
    *NEXT(MEM(ap))=NULL;
    firstlru=MEM(ap);
  }
  lastlru=MEM(ap);
  *SIZE(MEM(ap))=size;
  totallrumem+=size;
  *(REF(MEM(ap)))=ap;
  MEM(ap)=USER(MEM(ap));
}
  
  
void alloclrumem(void* ap,long size) { 
  char* res;
#ifdef DEBUGMEM
  pfprot("(mem: alloc 0x%x %d at ...",MEM(ap),size);
#endif
  size=MAX(1,size);
  while(totallrumem+size>MAXLRUPOOL) 
    if(!killlru()) Tptr(NULL);
  do
    if((res = malloc(size+4*sizeof(char*))) != NULL) break;  
  while(killlru());
  Tptr(res);
  MEM(ap)=res;
#ifdef DEBUGMEM
  pfprot("... 0x%x)",USER(MEM(ap)));
#endif
  addlru(ap,size);
}

void realloclrumem(void* ap, long size) { 
  char *res;
  char** ptme1;
  char** ptme2;
  char* me;
  long oldsize;

#ifdef DEBUGMEM
  pfprot("(mem: realloclru 0x%x %d at ...",MEM(ap),size);
#endif
  size=MAX(1,size);
  if(MEM(ap)==NULL) alloclrumem(ap,size);
  me=BASE(MEM(ap));
  ptme1=PREV(me);
  ptme2=NEXT(me);
  oldsize=*SIZE(me);
  while(totallrumem+size-oldsize>MAXLRUPOOL) 
    if(!killlru()) Tptr(NULL);
  do
    if((res = realloc(me,size+4*sizeof(char*))) != NULL) break;  
  while(killlru());
  Tptr(res);
  if(*ptme1!=NULL) *NEXT(*ptme1)=res; else firstlru=res;
  if(*ptme2!=NULL) *PREV(*ptme2)=res; else lastlru=res;
  *SIZE(res)=size;
  totallrumem+=size-oldsize;
  MEM(ap)=USER(res);  
#ifdef DEBUGMEM
  pfprot("... 0x%x)",MEM(ap));
#endif
}

void freelrumem(void* ap) {
  char** ptme1;
  char** ptme2;
  char* me;

#ifdef DEBUGMEM
  pfprot("(mem: freelru 0x%x)",MEM(ap));
#endif
  if(MEM(ap)==NULL) return;
  me=BASE(MEM(ap));
  ptme1=PREV(me);
  ptme2=NEXT(me);
  totallrumem-=*SIZE(me);
  if(firstlru==lastlru) {
    firstlru=NULL;
    lastlru=NULL;
  } else {
    if(*ptme1!=NULL) *NEXT(*ptme1)=*ptme2; else firstlru=*ptme2; 
    if(*ptme2!=NULL) *PREV(*ptme2)=*ptme1; else lastlru=*ptme1;
  }
  free(me);
  MEM(ap)=NULL;
}
 
void touchlrumem(void* ap) {
  char** ptme1;
  char** ptme2;
  char* me;
#ifdef DEBUGMEM
  pfprot("(mem: touchlru 0x%x)",MEM(ap));
#endif
  if(firstlru==lastlru) return;
  me=BASE(MEM(ap));
  if(me==lastlru) return;
#ifdef DEBUGMEM
  pfprot("(mem: dotouch ...");
#endif
  ptme1=PREV(me);
  ptme2=NEXT(me);
  if(*ptme1!=NULL) *NEXT(*ptme1)=*ptme2; else firstlru=*ptme2; 
  *PREV(*ptme2)=*ptme1; 
  *NEXT(lastlru)=me;
  *PREV(me)=lastlru;
  *NEXT(me)=NULL;
  lastlru=me;
#ifdef DEBUGMEM
  pfprot("...)");
#endif
}  

void allocmem(void* ap,long anz) { 
  char* res;
#ifdef DEBUGMEM
  pfprot("(mem: alloc 0x%x %d at ...",MEM(ap),anz);
#endif
  anz=MAX(1,anz);
  do {
    if((res = malloc(anz)) != NULL) break;  
  } while(killlru());
#ifdef DEBUGMEM
  pfprot("... 0x%x)",res);
#endif
  Tptr(res);
  MEM(ap)=res; 
}

reallocmem(void *ap, long anz) { 
  char* res;
#ifdef DEBUGMEM
  pfprot("(mem: realloc 0x%x %d)",MEM(ap),anz);
#endif
  anz=MAX(1,anz);
  if(MEM(ap)==NULL) allocmem(ap,anz);
  do
    if((res = realloc(MEM(ap),anz)) != NULL) break;  
  while(killlru());
  Tptr(res);
  MEM(ap)=res;  
}


void freemem(void* ap) {
#ifdef DEBUGMEM
  pfprot("(mem: free 0x%x)",MEM(ap));
#endif
  if(MEM(ap)==NULL) return;
  free(MEM(ap));
  MEM(ap)=NULL;
}


char *stralloccpy(char** dest, char* src)
{
  freemem(dest);
  if (! src) *dest = NULL;
  else {
    allocmem(dest,strlen(src) + 1);
    strcpy(*dest, src);
  }
  return *dest;
}



/* recursive directory searching ... 

   ... and theyre telling everyone that i, thomas moor, dont know too much
   about exaiming directories. And they are the reason of the very slow
   starting up. Workaround: name the directories to be scanned explicietly,
   first, separeted by colons, followed by those, which are eventually to be
   scanned recursivly. 
   
   To speed things up a little, now the directories in which some file 
   was found, are kept in a history. And a trick was stolen from kpathsea.
   Karl Berry wrote in AUTHORS about this trick:

   The implementation of the link trick in pathsrch.c is taken from GNU
   find, implemented by David MacKenzie from Matthew Farwell's suggestion.
  
*/ 

#ifndef DOSFILES

int checkapath(char* prepath,char* path) {
  char nprepath[MAXPATHSTR], npath[MAXPATHSTR];
  int i,sep,pn,ppn;

  /* fprintf(prot,"\n chkapath %s =?= %s \n",prepath,path); */
  pn=strlen(path);
  ppn=strlen(prepath);
 
  if(pn>=1) if(path[pn-1]=='/') 
     path[pn--]=0;

  sep=-1;
  for(i=0;sep== -1 && i<ppn ;i++) 
    if(prepath[i]=='/' ) if (prepath[i+1]=='/') {
      sep=i;
    }
  
  if(sep==-1) {
    if(ppn>=1) if(prepath[pn-1]=='/') 
      prepath[pn--]=0;
    if(strcmp(prepath,path)==0) return(1);
    else return(0);
  }
  
  if(sep > 0) {
    if(strncmp(prepath,path,sep)!=0) return(0);
    return(checkapath(prepath+sep,path+sep));
  }

  if(sep==0) {
      if(ppn==2) return(1); 
      if(path[0]=='/') {
        path++;
        pn--;
      }
      if(checkapath(prepath+2,path))  return(1);
      for(i=0;path[i]!='/' && i< pn;i++);
      if(i==pn) return(0);
      return(checkapath(prepath,path+i));
    return(0);
  }
}



#define RSHIS 50
char* rshis[RSHIS]={NULL}; /* all NULL pointers now. */

void addrshis(char* thenew) {
  int i,l;
  if(thenew[0]==0) return;
  freemem(rshis+RSHIS-1);
  for(i=RSHIS-1;i>0;i--) rshis[i]=rshis[i-1];
  l=strlen(thenew);
  allocmem(rshis,l+2);
  strcpy(rshis[0],thenew);
  if(l>0 && (rshis[0][l-1]) != '/') {
    (rshis[0])[l]='/';
    (rshis[0])[l+1]=0;
  }
  /*fprintf(prot,"\naddrshis: ");
  for(i=0;i<RSHIS;i++)
    if(rshis[i]!=NULL)
      fprintf(prot,"%s:",rshis[i]);
  fprintf(prot,"\n");*/
}

int chkrshis(char* result,char* prepath, char* name) {
  int i,j;
  FILE* testfile;
  char* bu;
  for(i=0; i< RSHIS; i++) {
    result[0]=0;
    if(rshis[i]!=NULL){
      if(checkapath(prepath,rshis[i])){;
        strcpy(result,rshis[i]);
        strcat(result,name);
        testfile = fopen(result,"rb");
        if(testfile != NULL) {
          fclose(testfile);
          bu=rshis[i];
          for(j=i;j>0;j--) rshis[j]=rshis[j-1];
          rshis[0]=bu;
          return(1);
        }
        fclose(testfile);
      }
    }
  }
  return(0);
}

int rsearch(char* done,char* path,int dep) {
  char ndone[MAXPATHSTR], npath[MAXPATHSTR];
  FILE* testfile;
  DIR *thisdir, *testdir;
  struct dirent *thisentry;
  int i,dn,sep,ndep,links;
  struct stat stats;
  
  /* set sep to the position of next "//" in path or -1 if no such */
  sep=-1;
  for(i=0;sep== -1 && path[i] != 0 ;i++) 
    if(path[i]=='/' ) if (path[i+1]=='/') {
      sep=i;
    }

  dn=strlen(done);
  
  /* no more recursive ? fine, just look for the file */
  if(sep==-1) {
    strcat(done,path);
    testfile = fopen(done,"rb");
    if(testfile != NULL) {
      fclose(testfile);
      done[dn]=0;
      addrshis(done);
      strcat(done,path);
      return(1);
    }
    return(0);
  }
 
  /* more recursive, but with some non-recursive dirs befor */
  if(sep > 0) {
    strncat(done,path,sep);
    return(rsearch(done,path+sep,0));
  }
 
  /* recursive just now, part 1: cant do it, no more subdirs */
  if(sep==0) {
    if(stat(done, &stats) == 0)
    if(S_ISDIR(stats.st_mode) && stats.st_nlink<=2) 
      return(rsearch(done,path+1,0));
  }

  /* recursive just now, part 2: try it, should find more subdirs */ 
  if(sep==0 && dep < MAXPATHDEP) { 
      if(rsearch(done,path+1,dep))  return(1);
      done[dn]=0;
      if ((thisdir=opendir(done))==NULL) return(0);
      while(thisentry=readdir(thisdir))
        if(    (strcmp(thisentry->d_name,".") != 0)
           && (strcmp(thisentry->d_name,"..") != 0)
           && (strlen(thisentry->d_name)+dn+2 < MAXPATHSTR) ){   
          done[dn]='/';
          strcpy(done+dn+1,thisentry->d_name);     
	  if((testdir=opendir(done))!= NULL) {
            closedir(testdir);
            if(rsearch(done,path,dep+1)) {
              closedir(thisdir);
              return(1);
            }
          }
          done[dn]=0;
        }
    closedir(thisdir);
    return(0);
  }
  return(0);
}
          

int msearch(char* mpath,char* filename,char* result) {
  int mpnpo, pnpo,hopo, found;
  char pn[MAXPATHSTR];
  char* homedir;
  char ch;
 
  homedir = getenv("HOME");
  if(homedir == NULL) homedir="/home_not_def";
  if(strlen(homedir) + strlen(mpath) + 1 >MAXPATHSTR) return(0);
  mpnpo=0;
  while (mpath[mpnpo]== ':') mpnpo++;
  found=0;
  while(mpath[mpnpo] != 0) {
    pnpo=0;
    if(mpath[mpnpo] == '~' && 
       (mpath[mpnpo+1]==':' || mpath[mpnpo+1]=='/' || mpath[mpnpo+1]==0)) { 
      for(hopo=0;homedir[hopo] != 0;hopo++)
          pn[pnpo++]=homedir[hopo];
      mpnpo++;
      if(pn[pnpo-1]=='/' && mpath[mpnpo]=='/') mpnpo++; 
    }
    while((mpath[mpnpo] != ':') && (mpath[mpnpo] != 0)) 
      pn[pnpo++]=mpath[mpnpo++];
    while (mpath[mpnpo]== ':') mpnpo++;
    if(pnpo>0){
      if(pn[pnpo-1] != '/') pn[pnpo++]='/';
      pn[pnpo]=0;
      if(chkrshis(result,pn,filename)) {
         /* fprintf(prot,"\n rshis hit \n"); */
         return(1);
      }
      /* fprintf(prot,"\n rshis no hit \n"); */
      strcat(pn,filename);
      result[0]=0;
      if(rsearch(result,pn,0)) return(1); 
    }
  }
  return(0);
}


int completefile(char* total, int which) {
  char npath[MAXPATHSTR], name[MAXPATHSTR];
  FILE* testfile;
  DIR *thisdir, *testdir;
  struct dirent *thisentry;
  int plen,nlen,tlen;
  char* homedir;
  int found,i,j; 

#ifdef DEBUGFILE
  pfprot("(completefile %s ",total);
#endif

  tlen=strlen(total);
  if(tlen>MAXPATHSTR) return(0);
  strcpy(npath,total);
  if(npath[0] == '~') { 
    homedir = getenv("HOME");
    if(homedir != NULL) {
      if(tlen+strlen(homedir) +1 < MAXPATHSTR) {
        strcpy(npath,homedir);
        strcat(npath,total+1);
      }
    }
  }
  j=-1;
  for(i=0;npath[i]!=0;i++) 
    if(npath[i]=='/') j=i;
  if(j > -1) {
    strcpy(name,npath+j+1);
    npath[j+1]=0;
  } else {
    strcpy(name,npath);
    strcpy(npath,"./");
  } 
  nlen=strlen(name);
  plen=strlen(npath);
 

#ifdef DEBUGFILE
  pfprot("path %s name %s ",npath,name);
#endif
  
  if((thisdir=opendir(npath))==NULL) return(0);
  found=0;
  while(thisentry=readdir(thisdir))
    if(    (strncmp(thisentry->d_name,name,nlen) ==0)
        && (strlen(thisentry->d_name)+plen+2 < MAXPATHSTR) ){ 
      found++;   
      if(found==1) 
        strcpy(name,thisentry->d_name);
      for(i=0;thisentry->d_name[i]==name[i] && name[i]!=0;i++);
      name[i]=0;
      if(found==which) 
        strcat(npath,thisentry->d_name);     
  } 
  closedir(thisdir);
  if(which==0 || which > found) 
    strcat(npath,name);
  nlen=strlen(npath);
  if((testdir=opendir(npath))!= NULL) {
    closedir(testdir);
    if(npath[nlen-1] != '/' && found == 1)
      strcat(npath,"/");
  }     
  strcpy(total,npath);
#ifdef DEBUGFILE
  pfprot("found %d)",found);
#endif
  return(found);
}

void setworkdir(void) {
 workdir=getcwd(NULL, 0);
  if(workdir==NULL) {pfprot("\nfatal error: memory .. getcwd ??");exit(1);}
}

int dvistandard(char** dst,char* src) {  /* may be src==filename */
  uchar o;
  char rbu[4];
  FILE* file;
  char s[MAXPATHSTR+1];
  char ss[MAXPATHSTR+1];
  int slen,i,lastsep;
#ifdef DEBUGFILE
  pfprot("(dvistandard 1.<%s> ",src);
#endif
  if(src[0]!=DIRSEPCHAR) {
    if(strlen(workdir)+strlen(src)>MAXPATHSTR) 
       {pfprot("\nfatal error: memory .. stddvi ??");exit(1);}
    strcpy(ss,workdir);
    strcat(ss,DIRSEPSTR);
    strcat(ss,src);
  } else
    strcpy(ss,src);
#ifdef DEBUGFILE
  pfprot(" 2.<%s> ",ss);
#endif
  slen=strlen(ss);
  for(i=0;i<slen-1;i++) {
    if(ss[i]!=DIRSEPCHAR) continue;
    if(ss[i+1]!=DIRSEPCHAR) continue;
    memmove(ss+i,ss+i+1,slen-i);
    slen-=1;
    i--;
  }
#ifdef DEBUGFILE
  pfprot(" 3.<%s> ",ss);
#endif
  for(i=0;i<slen-2;i++) {
    if(ss[i]!=DIRSEPCHAR) continue;
    if(ss[i+1]!='.')        continue;
    if(ss[i+2]!=DIRSEPCHAR) continue;
    memmove(ss+i,ss+i+2,slen-i-1);
    slen-=2;
    i--;
  }
#ifdef DEBUGFILE
  pfprot(" 4.<%s> ",ss);
#endif
  for(i=0;i<slen-3;){
    lastsep=0;
    for(i=0;i<slen-3;i++) {
      if(ss[i]==DIRSEPCHAR && ss[i+1]!='.') lastsep=i;  
      if(ss[i]!=DIRSEPCHAR) continue;
      if(ss[i+1]!='.')        continue;
      if(ss[i+2]!='.')        continue;
      if(ss[i+3]!=DIRSEPCHAR) continue;
      memmove(ss+lastsep+1,ss+i+4,slen-i-3);
      slen=strlen(ss);
      break;
    }
  }
#ifdef DEBUGFILE
  pfprot(" 5.<%s> ",ss);
#endif
  slen=strlen(ss);
  strcpy(s, ss); 
  if(s[MAX(0,slen-1)] == '.') strcat(s,"dvi");
  else { 
    if(STRCASECMP(s+MAX(0,slen-4),".dvi")!=0) 
        strcat(s,".dvi");
  }
#ifdef DEBUGFILE
  pfprot(" 6.<%s> ",s);
#endif
  file = fopen(s, "rb");
  if(file == NULL) {
    strcpy(s,ss);  
#ifdef DEBUGFILE
    pfprot(" 7.<%s> ",s);
#endif
    file= fopen(s, "rb");
    if(file == NULL) {
      stralloccpy(dst,s);
      return(0);
    }
  }
  stralloccpy(dst,s);
#ifdef DEBUGFILE
  pfprot(" found file...");
#endif
  if(fseek(file, 0L, 0) !=0)    {fclose(file); return(0);} 
  if(fread(rbu,1,1,file) == 0)  {fclose(file); return(0);}
  if(rbu[0] != (char) 247)      {fclose(file); return(0);}
  fclose(file);
#ifdef DEBUGFILE
  pfprot("...standarddvi)");
#endif 
  return(1);
}



#else   /*********************************************ifdef DOSFILES */

int msearch(char* mpath,char* filename,char* result) {
  int mpnpo, pnpo,hopo;
  char ch;
  FILE* test;

#ifdef DEBUGFILE
  pfprot("(msearch path %s name %s ...",mpath,filename);
#endif 
  
  if(strlen(mpath) + 1 >MAXPATHSTR) return(0);
  mpnpo=0;
  while (mpath[mpnpo]== ';') mpnpo++;
  while(mpath[mpnpo] != 0) {
    pnpo=0;
    while((mpath[mpnpo] != ';') && (mpath[mpnpo] != 0)) 
      result[pnpo++]=mpath[mpnpo++];
    while (mpath[mpnpo]== ';') mpnpo++;
    if(pnpo>0){
      if(result[pnpo-1] != '\\') result[pnpo++]='\\';
      result[pnpo]=0;
      strcat(result,filename);
      /* pfprot("(msea: %s)",result); */
      if((test=fopen(result,"rb")) != NULL) {
        fclose(test);
#ifdef DEBUGFILE
        pfprot("found %s)",result);
#endif 
        return(1);
      } 
    }
  }
  strcpy(result,"c:\\gibsnich");
#ifdef DEBUGFILE
  pfprot("not found %s)",result);
#endif 
  return(0);
}


int completefile(char* total, int which) {

#ifdef DEBUGFILE
  pfprot("(completefile %s disabled on dosfilesystems. sorry)",total);
#endif
  return(0);
}

void setworkdir(void){
 int i;

 _nls_init();  /* hack: do this here ... */
 workdir=_getcwd2(NULL,0);
 if(workdir==NULL) {pfprot("\nfatal error: memory .. getcwd ??");exit(1);}
 for(i=strlen(workdir)-1;i>=0;i--) 
   if(workdir[i]=='/') workdir[i]= DIRSEPCHAR; /* translate back */
#ifdef DEBUGFILE
  pfprot("(setworkdir <%s> )",workdir);
#endif
}



int dvistandard(char** dst,char* src) {  /* may be src==filename */
  uchar o;
  int adw;
  char rbu[4];
  FILE* file;
  char s[MAXPATHSTR+1];
  char ss[MAXPATHSTR+1];
  int slen,i,lastsep;
#ifdef DEBUGFILE
  pfprot("(dvistandard 1.<%s> ",src);
#endif
  adw=0;
  slen=strlen(src);
  if(slen<2) adw=1;
  else if(src[1]!=':') adw=1;
  if(adw) {
    if(strlen(workdir)+slen>MAXPATHSTR) 
       {pfprot("\nfatal error: memory .. stddvi ??");exit(1);}
    strcpy(ss,workdir);
    strcat(ss,DIRSEPSTR);
    strcat(ss,src);
  } else
    strcpy(ss,src);
#ifdef DEBUGFILE
  pfprot(" 2.<%s> ",ss);
#endif
  slen=strlen(ss);
  for(i=0;i<slen-1;i++) {
    if(ss[i]!=DIRSEPCHAR) continue;
    if(ss[i+1]!=DIRSEPCHAR) continue;
    memmove(ss+i,ss+i+1,slen-i);
    slen-=1;
    i--;
  }
#ifdef DEBUGFILE
  pfprot(" 3.<%s> ",ss);
#endif
  for(i=0;i<slen-2;i++) {
    if(ss[i]!=DIRSEPCHAR) continue;
    if(ss[i+1]!='.')        continue;
    if(ss[i+2]!=DIRSEPCHAR) continue;
    memmove(ss+i,ss+i+2,slen-i-1);
    slen-=2;
    i--;
  }
#ifdef DEBUGFILE
  pfprot(" 4.<%s> ",ss);
#endif
  for(i=0;i<slen-3;){
    lastsep=0;
    for(i=0;i<slen-3;i++) {
      if(ss[i]==DIRSEPCHAR && ss[i+1]!='.') lastsep=i;  
      if(ss[i]!=DIRSEPCHAR) continue;
      if(ss[i+1]!='.')        continue;
      if(ss[i+2]!='.')        continue;
      if(ss[i+3]!=DIRSEPCHAR) continue;
      memmove(ss+lastsep+1,ss+i+4,slen-i-3);
      slen=strlen(ss);
      break;
    }
  }
#ifdef DEBUGFILE
  pfprot(" 5.<%s> ",ss);
#endif
  _nls_strlwr(ss);
#ifdef DEBUGFILE
  pfprot(" 5.5.<%s> ",ss);
#endif
  slen=strlen(ss);
  strcpy(s, ss); 
  if(s[MAX(0,slen-1)] == '.') strcat(s,"dvi");
  else { 
    if(strcmp(s+MAX(0,slen-4),".dvi")!=0) 
        strcat(s,".dvi");
  }
#ifdef DEBUGFILE
  pfprot(" 6.<%s> ",s);
#endif
  file = fopen(s, "rb");
  if(file == NULL) {
    strcpy(s,ss);  
#ifdef DEBUGFILE
    pfprot(" 7.<%s> ",s);
#endif
    file= fopen(s, "rb");
    if(file == NULL) {
      stralloccpy(dst,s);
      return(0);
    }
  }
  stralloccpy(dst,s);
#ifdef DEBUGFILE
  pfprot(" found file...");
#endif
  if(fseek(file, 0L, 0) !=0)    {fclose(file); return(0);} 
  if(fread(rbu,1,1,file) == 0)  {fclose(file); return(0);}
  if(rbu[0] != (char) 247)      {fclose(file); return(0);}
  fclose(file);
#ifdef DEBUGFILE
  pfprot("...standarddvi)");
#endif 
  return(1);
}





#endif  /* end ********************+************************* dosfiles */




void alloc_bitmapbw(bitmap* bmp) {
  unsigned long	size;
  bmp->bmu_wide = ROUNDUP(bmp->w, BITS_PER_BMUNIT);
  size = ((long)bmp->bmu_wide)* BYTES_PER_BMUNIT * bmp->h;
  alloclrumem(&(bmp->bits),MAX(size,1)); 
  memset(bmp->bits,0,size);
}

void alloc_bitmapgs(bitmap* bmp) {
  unsigned long	size;
  bmp->bmu_wide = ROUNDUP(bmp->w * bmp->type, BITS_PER_BMUNIT);
  size = ((long) bmp->bmu_wide)*BYTES_PER_BMUNIT * bmp->h;
  alloclrumem(&(bmp->bits),MAX(size,1)); 
  memset(bmp->bits,0,size);
}

void clear_bitmap(bitmap* bmp) {
  memset((char*) bmp->bits,0,(long)(bmp->bmu_wide)*BYTES_PER_BMUNIT * bmp->h);
}


/* for debugging only */
#ifdef DEBUGBITMAPS 
void print_bitmap(bitmap *bmp) {
	BMUNIT *pt = (BMUNIT *) bmp->bits;
	int x, y, i;

	if (pt == NULL) return;
	pfprot("(printbitmap at 0x%x: \n",pt);
	for (y = 0; y < bmp->h; ++y) {
            pfprot("[",y);
	    for (x = bmp->bmu_wide; x > 0; x--) {
		for (i = BITS_PER_BMUNIT - bmp->type; i >= 0;i-=bmp->type)
		    pfprot("%c",' '+((*pt >> i) & ((1<<bmp->type)-1)));
		++pt;
	    }
	    pfprot("]\n");
	}
        pfprot("sizes: type = %d, w = %d, h = %d, bmu wide = %d)\n",
	    bmp->type,bmp->w, bmp->h, bmp->bmu_wide);

}


#endif
  








