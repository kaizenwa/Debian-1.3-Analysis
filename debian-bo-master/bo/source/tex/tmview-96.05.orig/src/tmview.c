/*
This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor


Once this was ...

(C)opyright 1994 Wolfgang R. Mueller, Computing Centre,
Heinrich-Heine-University Duesseldorf, Germany,
wolfgang@uni-duesseldorf.de, +49-211-311-3905.

... but this is some months ago, and only a view lines of the 
original code are left.   


This program may be used without any warranty. It may be modified and 
distributed without any restictions.
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

#include "defs.h"

#ifdef KPATHSEA
#include <kpathsea/proginit.h>
#include <kpathsea/progname.h>
#endif

#include "help.h"
#include "globals.h"
#include "subs.h"
#include "writedis.h"
#include "readdvi.h"
#include "halfhyp.h"
#include "bookmks.h"
#include "rwconf.h"

/* #define DEBUGCOM */




void comdoresize(void) {
  vgasetstatuslines(1); /* sets vgaxdim ydim !! */
  drawstatusline();
  drawupdatepage();    
}



/****************************************************************************/
/* edit numberarg ***********************************************************/
/****************************************************************************/

/* globals used for numberarg editing */
#define NASTART 0
#define NAINT 1
#define NAINTE 2
#define NAFRAC 3
#define NAFRACE 4
#define NAONLYE 5
#define NAMAXARG 20

/* now extern static char  numberargstr[MAXARGSTR+1]; */
static double numberarg[NAMAXARG];
static int   numberargcount;
static int   numberargmode;
/* now extern static int   numberargstrlen;*/

void numberarginit(void) {
  /* fprintf(prot,"(numberarginit)"); */
  numberargstrlen=0;
  numberargcount=0;
  numberargmode=NASTART;
  memset(numberargstr,0,MAXARGSTR+1);
}

int numberargsub(int); /* forward */

int numberargadd(char ch) {
  if(ch==127) return(numberargsub(1));
  if(numberargstrlen==MAXARGSTR) return(1);
  switch (numberargmode) {
  case NASTART:
    if(ch=='.' || ch=='+' || ch=='-' || ch=='*' || ch=='#' || 
       (ch>='0' && ch<='9')) { 
       numberargstr[numberargstrlen++]=ch;
       if(ch=='.') 
         numberargmode=NAFRAC;
       if(ch=='*' || ch=='#')
         numberargmode=NAONLYE;
       if(ch >=0 && ch <= '9') 
         numberargmode=NAINTE;
       if(ch=='+' || ch=='-')
         numberargmode=NAINT;
       return(0);
    } 
    return(1);
  case NAINT:
    if(ch=='.' || (ch>='0' && ch<='9')) { 
       numberargstr[numberargstrlen++]=ch;
       if(ch=='.') numberargmode=NAFRAC;
       numberargmode=NAINTE;
       return(0);
    } 
    return(1);
  case NAINTE:
    if(ch=='.' || ch==';' || ch==',' || (ch>='0' && ch<='9')) { 
       numberargstr[numberargstrlen++]=ch;
       if(ch=='.') numberargmode=NAFRACE;
       if(ch==';'|| ch==',') numberargmode=NASTART;
       return(0);
    } 
    return(1);
  case NAFRAC:
    if(ch>='0' && ch<='9') { 
       numberargstr[numberargstrlen++]=ch;
       numberargmode=NAFRACE;
       return(0);
    } 
    return(1);
  case NAFRACE:
    if(ch==';' || ch==','|| (ch>='0' && ch<='9')) { 
       numberargstr[numberargstrlen++]=ch;
       if(ch==';' || ch==',') numberargmode=NASTART;
       return(0);
    } 
    return(1);
  case NAONLYE:
    if(ch==';' || ch==',') {
       numberargstr[numberargstrlen++]=ch;
       numberargmode=NASTART;
       return(0);
     }
    return(1);
  default:
  }
}

int numberargsub(int count) {
  char argsave[MAXARGSTR+1];
  int newlen;
  if(numberargstrlen-count < 0 || count <0) return(1);
  newlen=numberargstrlen-count;
  strncpy(argsave,numberargstr,newlen);
  numberarginit();
  for(count=0;count<newlen;count++) 
     numberargadd(argsave[count]);
}

int numberargread(void) {
  int pos=0;
  int len;
  char mas[MAXARGSTR+1];
  /* fprintf(prot,"(numberargread argstr <%s> -- len %d ...\n",
     numberargstr,numberargstrlen); */
  numberargcount=0;
  if(numberargstrlen==0) return(1);
  strncpy(mas,numberargstr,numberargstrlen+1);
  while(pos<numberargstrlen) {
    for(len=0;mas[pos+len] != ';' && mas[pos+len] != ',' && 
              mas[pos+len] != 0;len++);
    if(len==0) break; /* last could be ; */
    mas[pos+len]=0;
    if(++numberargcount> NAMAXARG) {numberargcount=0; return(1);}
    /* fprintf(prot,"<%s> -atof- %f ###",mas+pos,atof(mas+pos)); */
    if(mas[pos]!='*' && mas[pos]!='#')
      numberarg[numberargcount-1]=atof(mas+pos);
    else
      numberarg[numberargcount-1]=PGMAGIC;
    pos+=len+1;
  }
  /* fprintf(prot,"\n count %d o.k.)\n",numberargcount); */
  return(1);
}

int numberargedit(int line,int pos, const char* defa) {
  uchar o;
  int count;
  int allesfrisch=1;
  numberarginit();
  for(count=0;count<strlen(defa);count++) 
     numberargadd(defa[count]);
  numberargstr[numberargstrlen]=0;
  drawsr(numberargstr,line,pos,vgastatuslen-pos);
  vgaupdatestatus();
  do{
    o=vgagetchar(1);
    switch(o) {
    case 127: case ';': case ',': case '.': case '+': case '-': case '*':
    case '0': case '1': case '2': case '3': case '4': case '5': case '#':
    case '6': case '7': case '8': case '9':
      if(allesfrisch){
        allesfrisch=0;
        numberarginit();
      }
      numberargadd(o);  
      numberargstr[numberargstrlen]=0;
      drawsr(numberargstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
      break;
    case KEYESC: 
      numberarginit();
      return(1);
    case KEYRESIZE: 
      numberarginit();
      comdoresize();
      return(1);
    case KEYRET:
      numberargread();
      return(0);
    default:
    }
  } while(1);
}

/****************************************************************************/
/* edit stringarg ***********************************************************/
/****************************************************************************/


#define STRARGHIS 50

char stringarg[MAXARGSTR+1];
char* stringarghis[STRARGHIS]={NULL};
char filearg[MAXARGSTR+1];

/*#define PRINTHIS pfprot("\n-- hispos %d --\n",hispos); for(i=0;i<STRARGHIS;i++) if(stringarghis[i]!=NULL) pfprot("%d at 0x%x %s\n",i,stringarghis[i],stringarghis[i]) */
#define PRINTHIS


void stringarginit(void) {
  int i;
  for(i=0;i<STRARGHIS;i++) stringarghis[i]=NULL;
  stringarg[0]=0;
  filearg[0]=0;
}


int stringargedit(int line,int pos,char* defa) {
  uchar o;
  int len,i;
  int allesfrisch=1;
  int hispos;
  char editstr[MAXARGSTR+1];
  char comstr[MAXPATHSTR+1];
 
  freemem(stringarghis+STRARGHIS-1);
  for(i=STRARGHIS-1;i>0;i--) stringarghis[i]=stringarghis[i-1];
  stringarghis[0]=NULL;
  if(defa!=NULL) 
    stralloccpy(stringarghis,defa);
  else
    stralloccpy(stringarghis,"");
  hispos=-1;
  PRINTHIS;
  o=KEYUP;
  do{
    if(' '<=o && o<127){
      if(allesfrisch){
        allesfrisch=0;
        len=0;
        editstr[0]=0;
      }
      if(len<MAXARGSTR) {
        editstr[len++]=o;  
        editstr[len]=0;  
      }
      drawsr(editstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
  
    } 
    if(len>0 && o==127) {
      allesfrisch=0;
      editstr[--len]=0; 
      drawsr(editstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
    }
    if(o==KEYUP || o==KEYDOWN) {
      if(0<= hispos && hispos < STRARGHIS) { /* ask for (not) startup */
        stralloccpy(stringarghis+hispos,editstr);
        allesfrisch=0;
      }
      if(o==KEYUP) hispos++; else hispos--;
      hispos=MAX(0,MIN(STRARGHIS-1,hispos));
      while(hispos>0 && stringarghis[hispos]==NULL) hispos--;
      PRINTHIS;
      strcpy(editstr,stringarghis[hispos]);
      editstr[MAXARGSTR]=0;
      len=strlen(editstr);
      drawsr(editstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
    }
    if(o==KEYESC) 
      return(1);
    if(o==KEYRESIZE) {
      comdoresize();
      return(1);
    }
    if(o==KEYRET){
      strcpy(stringarg,editstr);
      stralloccpy(stringarghis,editstr);
      if(stringarghis[1]!=NULL) 
      if(strcmp(stringarghis[0],stringarghis[1])==0) {
        for(i=0;i<STRARGHIS-1;i++) stringarghis[i]=stringarghis[i+1];
        stringarghis[i]=NULL;
      }
      PRINTHIS;
      return(0);
    }
    o=vgagetchar(1);
  } while(1);
}


int fileargedit(int line,int pos) {
  uchar o;
  int len,i;
  int allesfrisch=1;
  char editstr[MAXPATHSTR+1];
  char comstr[MAXPATHSTR+1];
 
  if(visfmk!=NULL)
    strcpy(editstr,visfmk->dvifilename);
  else
    strcpy(editstr,"");
  editstr[MAXARGSTR]=0;
  drawsr(editstr,line,pos,vgastatuslen-pos);
  vgaupdatestatus();
  while(1) {
    o=vgagetchar(1);
    if(' '<=o && o<127){
      if(allesfrisch){
        allesfrisch=0;
        len=0;
        editstr[0]=0;
      }
      if(len<MAXARGSTR) {
        editstr[len++]=o;  
        editstr[len]=0;  
      }
      drawsr(editstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
    } 
    if(len>0 && o==127) {
      allesfrisch=0;
      editstr[--len]=0; 
      drawsr(editstr,line,pos,vgastatuslen-pos);
      vgaupdatestatus();
    }
    if(o==KEYUP || o==KEYDOWN) {
      if(o==KEYDOWN) rolldownfilemk();
      if(o==KEYUP)   rollupfilemk();
      if(curfmk!=NULL) {
        allesfrisch=1;
        strcpy(editstr,curfmk->dvifilename);
        editstr[MAXARGSTR]=0;
        len=strlen(editstr);
        drawsr(editstr,line,pos,vgastatuslen-pos);
        vgaupdatestatus();
      }
    }
    if(o==KEYTAB) {
      strcpy(comstr,editstr);
      if(completefile(comstr,0)>=1) if(strlen(comstr)<MAXARGSTR) {
        strcpy(editstr,comstr);
        len=strlen(comstr);
        drawsr(editstr,line,pos,vgastatuslen-pos);
        vgaupdatestatus();
      }
    }
    if(o==KEYESC) 
      return(1);
    if(o==KEYRESIZE) {
      comdoresize();
      return(1);
    }
    if(o==KEYRET){
      strcpy(filearg,editstr);
      return(0);
    }
  }
}




/****************************************************************************/
/* (help) messages **********************************************************/
/****************************************************************************/
 
int shorthelp(const char* desrc, const char* argdescr, const char* argdef) {
  /* uchar o; */
  int result;
  vgasetstatuslines(3);
  drawsl(desrc,0,0,vgastatuslen);
  drawsl("ARGUMENTS: ",1,0,vgastatuslen);
  drawsl(argdescr,1,11,vgastatuslen);
  drawsl(": ",1,11+strlen(argdescr),2);
  return(numberargedit(2,0,argdef));
}
  

int notethis(const char* message) {
  uchar o;
  drawsl(message,0,0,vgastatuslen);
  drawsr("PRESS <RET>",0,vgastatuslen-11,11);
  vgaupdatestatus();
  do {
    o=vgagetchar(1);
    if(o==KEYRET) return(0);
    if(o==KEYESC) return(1);
    if(o==KEYRESIZE) {
      comdoresize();
      return(1);
    }
  } while(1); 
}

int canclethis(char* message) {
  uchar o;
  drawsl(message,0,0,vgastatuslen);
  drawsr("PRESS <RET>/<ESC>",0,vgastatuslen-11,11);
  vgaupdatestatus();
  do {
    o=vgagetchar(1);
    if(o==KEYRET) return(0);
    if(o==KEYESC) return(1);
    if(o==KEYRESIZE) {
      comdoresize();
      return(1);
    }
  } while(1); 
}

  
void ignorethis(const char* message) {
  if(vgastatuslines!=1) drawstatusline();
  drawsl(message,0,0,vgastatuslen);
  drawsr(">>",0,vgastatuslen-2,2);
  vgaupdatestatus();
  statusforce=1; 
}

int proceedthis(char* message) {
  uchar o;
  drawsr(message,0,vgastatuslen-strlen(message),vgastatuslen);
  vgaupdatestatus();
  o=vgagetchar(0);
  if(o==KEYESC) return(1);
  return(0); 
}


int helpline=0;
int helppos=0;
  
void help(void) {
  uchar o;
  int h,w,i,j,k,l,dline,dpos;
  vgasetstatuslines(vgamaxstatuslines);
  h=vgastatuslines-3;
  w=vgastatuslen;
  dline=helpline;
  dpos=helppos;
  drawsl("",h,0,w);
  drawsl("",h+1,0,w);
  drawsl("",h+2,0,w);
  drawsr("PRESS <U>/<N>/<H>/<J> TO MOVE OR <ESC> TO EXIT",h+2,w-46,46);
  do {
    if(w>HELPWIDTH) 
      dpos=-(w-HELPWIDTH)/2;
    helppos=dpos;
    for(i=0,j=0; helpstr[i]!=0;i++) {
      if(j==dline) break;
      if(helpstr[i]==10) j++;
    }
    if(j==dline) helpline=dline;
    for(i=0,j=0; helpstr[i]!=0;i++) {
      if(j==helpline) break;
      if(helpstr[i]==10) j++;
    }
    for(;helpstr[i]!=0 && j<helpline+h;j++) {
      for(l=0;helpstr[i+l]!=10 && helpstr[i+l]!=0;l++);
      k=i+l+1;
      drawsl("",j-helpline,0,MAX(0,-helppos));
      l=MIN(l,l-helppos);
      drawsl(helpstr+i+MAX(0,helppos),j-helpline,MAX(0,-helppos),l); 
      drawsl("",j-helpline,l+MAX(0,-helppos),w);
      i=k;
    }   
    for(;j<helpline+h;j++)
      drawsl("",j-helpline,0,w);
    dline=helpline;
    dpos=helppos;
    vgaupdatestatus();
    do {
      o=vgagetchar(1);
      if('a'<= o && o <= 'z') o=o-'a'+'A';
      switch(o) {
      case KEYUP: case 'U':
        dline=MAX(0,dline-h/3); break;
      case KEYDOWN: case 'N':
        dline+=h/3; break;
      case KEYRIGHT: case 'J':
        dpos=MAX(0,MIN(dpos+w/3,HELPWIDTH-2.0/3*w)); break;
      case KEYLEFT: case 'H':
        dpos=MAX(-HELPWIDTH+2.0/3*w,dpos-w/3);
      case KEYESC: break;
      case KEYRESIZE: 
        comdoresize();
        break;
      default: o=KEYNOP; break;
      }
    } while(o==KEYNOP);
  } while(o!=KEYESC && o!=KEYRESIZE);
  drawstatusline();
}


/****************************************************************************/
/* commandprocessing ********************************************************/
/****************************************************************************/

/********* globals for commandprocessing  ... */

static int papxpxl, papypxl;
static int pagemovedef;
static int moveoffdef;
static int centhis;
static float zoomfinedef;
static float zoomdef;
static double gotodef[10];
static char gotodefstr[MAXARGSTR+1];
static char movedefstr[MAXARGSTR+1];
static char zoomfinedefstr[MAXARGSTR+1];
static bookmarklist backbmks={0,0};
static int backbmksdate=0;
static char* searchstring=NULL;
static int searchpage=-1;
static int searchanc=-1;
static long searchpos=-1;


static int theunit;
static char* unitnames[]=
  {"MM", "CM", "IN", "BP", "PT", "PC", "CC", "DD"  };
static int unitcommas[]=
  { 1,    2,    2,    1,    1,    2,    2,    1    };
static float tommfactors[]=
  { 1.0,  10.0, 25.4, .353, .351, 4.22, 4.51, .376 };

#define NUMBEROFUNITS 8


void setunit(int unitnumber) {
  theunit=MAX(0,MIN(NUMBEROFUNITS-1,unitnumber));
  unitstar=theunit;
  unitomm=tommfactors[theunit];
  mmtounit=1/unitomm;
  unitcomma=unitcommas[theunit]; 
  unitname= unitnames[theunit];
}



/************* often needed makros ? ... */

#define CATCHX(dpx) (MAX(-vgaxdim/2,MIN(papxpxl-vgaxdim/2,dpx)))
#define CATCHY(dpx) (MAX(-vgaydim/2,MIN(papypxl-vgaydim/2,dpx)))
#define CATCHXDVI(dpx) (CATCHX(dpx+MMTOPXL(visfmk->hoffmm))-MMTOPXL(visfmk->hoffmm))
#define CATCHYDVI(dpx) (CATCHY(dpx+MMTOPXL(visfmk->voffmm))-MMTOPXL(visfmk->voffmm))
#define SETMOVEOFFDEFPZ(pz) moveoffdef=MAX(1,vgaxdim*MIN(pz/100,0.9))
#define SETMOVEOFFDEFPX(px) moveoffdef=MAX(1,MIN(px,0.9*vgaxdim))
#define SETMOVEDEFSTR sprintf(movedefstr,"%.1f", 100.0*moveoffdef/vgaxdim)

/********* reset search and other cheap stuff ... */

void resetsearch(void) {
  searchpage=-1;
  searchpos=-1;
  searchanc=-1;
  freemem(&searchstring);
  if(texton==1) { 
     drawhideallmarks();
     texton=0;
     drawshowallmarks();
  }
  updatelist();
}


/*********** commandprocessing part 1: dont need anything setup,
                                       i.e. cpage, visfmk */


void comdomarks(int x, int y); /*forward*/


void comdoposit(int dpn, int dxp, int dyp, float dfs, int force) {
  char bustr[MAXARGSTR+MAXPATHSTR];
  int dord=force;

#ifdef DEBUGCOM
   pfprot("(comdoposit pn %d dxp %d dyp %d fsh %1.2f ...",
       dpn,dxp,dyp,dfs);
#endif
  if(cpage == NULL) dord=1; /* should not happen !!! */ 
  else if(dpn!=cpage->num) dord=1;
  if(dfs!=fshrink) dord=1;
  if(dord) {
    readpage(dpn,dfs,colors,1);
    papxpxl=MMTOPXL(visfmk->papxmm);
    papypxl=MMTOPXL(visfmk->papymm);
  }
  dvixpos=CATCHXDVI(dxp); 
  dviypos=CATCHYDVI(dyp);
  drawupdatepage();
  drawstatusline();
  updatefilemk();
#ifdef DEBUGCOM
  pfprot(" done)");
#endif
}

int comdofile(char* fname) {
  char *bustr=NULL;
  char bustr2[MAXPATHSTR+80];
  if(fname==NULL) return(0);
#ifdef DEBUGCOM
  pfprot("(comdofile %s ...?) ",fname);
#endif
  stralloccpy(&bustr,fname);
  dvistandard(&bustr,fname);
  if(visfmk!=NULL && !dvierrorflag) 
    if(strcmp(bustr,visfmk->dvifilename)==0) {
    freemem(&bustr);
    return(0);
  } 
  sprintf(bustr2,"LOADING FILE <%s> ..." ,bustr);
  drawsl(bustr2,0,0,vgastatuslen);
  vgaupdatestatus();
  updatefilemk(); 
  confwrite();
  killdvi(); 
  if(!findrolldownfilemk(bustr)) 
    allocnewfilemk(bustr);
  visfmk=curfmk;
  visbmks=curbmks;
  initdvi();  
  freemem(&bustr);
#ifdef DEBUGCOM
  pfprot(" ... comdofile done)");
#endif
  return(1);
}


void comfile(int mode) {
  char* bustr=NULL;
  uchar o;
  switch(mode) {
  case COMDO:
    drawsl("SELECT: LOAD FILE OR KILL FILE OR ESCAPE",0,0,vgastatuslen);
    drawsr("<L>/<K>/<ESC>",0,vgastatuslen-13,13);
    vgaupdatestatus();
    do {
      o=vgagetchar(1);
      if(o<='Z' && 'A' <= o) o=o+('a'-'A');
    } while(o!='l' && o!='k' && o!= KEYESC && o != KEYRESIZE);
    switch(o) {
       case KEYESC: break;
       case KEYRESIZE: break;
       case 'l':
         drawsl("LOAD FILE:",0,0,vgastatuslen);
         if(fileargedit(0,11)) break; 
         comdofile(filearg);
         comdoposit(visfmk->lastpos.pagenum, visfmk->lastpos.dvixpos,
           visfmk->lastpos.dviypos, visfmk->lastpos.fshrink,1);
         break;
       case 'k':
         if(visfmk==NULL) break;
         drawsl("KILL FILE:",0,0,vgastatuslen);
         if(fileargedit(0,11)) break;
         dvistandard(&bustr,filearg);
         if(strcmp(bustr,visfmk->dvifilename)==0) {
           rolldownfilemk();
           if(curfmk!=visfmk) {
             comdofile(curfmk->dvifilename);           
             comdoposit(visfmk->lastpos.pagenum, visfmk->lastpos.dvixpos,
               visfmk->lastpos.dviypos, visfmk->lastpos.fshrink,1);
           } else killdvi();
         }
         freefilemkname(bustr);
         freemem(&bustr);
         if(visfmk==NULL) dvierrorflag=1;
         break;
       }
    resetsearch(); 
    drawstatusline();
    break;    
  case COMDEF:
    if(!canclethis("<D> LOAD/KILL FILE"))
      comfile(COMDO);
    break;
  default:
  }
}

void comredraw(int mode){
  int pn;
  switch(mode) {
  case COMDO:
    if(visfmk==NULL) return;
    drawsl("RE-READING CURRENT DVI-FILE ...",0,0,vgastatuslen);
    updatefilemk();
    vgaupdatestatus();
    pn=cpage->num;
    killdvi(); 
    initdvi();  
    comdoposit(pn,dvixpos,dviypos,fshrink,1);
    resetsearch();
    numberarginit();
    break;
  case COMDEF:
    if(!canclethis("<R> RE-READ THE DVI-FILE")) comredraw(COMDO);
    else drawstatusline();
  } 
}


void comhelp(int mode) {
  switch(mode){
  case COMDO:
    drawsr("PRESS ANY KEY FOR A SHORT DESCRIPTION",
           0,0,vgastatuslen);
    vgaupdatestatus();
    break;
  case COMDEF:
    help(); break; 
    statusforce=1;
    break;
  }
}       

/*********** commandprocessing part 2: do need anything setup,
                                       i.e. cpage, visfmk */
#define MAXBACKBMKS 50



void comdokilloldbackbmk(void) {
   int i;
   for(i=backbmks.n-1; i>0;i--)  
     if(backbmks.d[i]->name>backbmks.d[0]->name) 
       freebookmknumber(&backbmks,i);
   if(backbmks.n>0)
     backbmksdate=backbmks.d[0]->name;  
}

void comdosetbackbmk(void) {
   if(amiatbookmkqm(&backbmks,0)) return;
   comdokilloldbackbmk();
   if(backbmks.n>=MAXBACKBMKS) freebookmknumber(&backbmks,backbmks.n-1);
   addbookmk(&backbmks,1);
   backbmks.d[0]->name= ++backbmksdate;
}



void comsetbookmk(int mode){
  int name,i;
  char bustr[80];
  switch(mode) {
  case COMDO:
#ifdef DEBUGCOM
    pfprot("(comsetbookmk..");
#endif
    if((i=existsbookmkqm(visbmks))>=0) {
      freebookmknumber(visbmks,i);
      while((i=existsbookmkqm(visbmks))>=0) 
        freebookmknumber(visbmks,i);
      ignorethis("UNDEFINED MANUAL-BOOKMARK");
    } else {  
      name=1;
      if(numberargcount==1 && numberarg[0]!=PGMAGIC && numberarg[0]>0
        && numberarg[0]==LROUND(numberarg[0]))  
        name=MIN(MAXMKS-1,LROUND(numberarg[0]));
      for(;name<MAXMKS;name++)
        if(checkbookmk(visbmks,name)==0) break;
      if(name>MAXMKS) {
        for(name=1;name<MAXMKS;name++)
          if(checkbookmk(visbmks,name)==0) break;
      }
      addbookmk(visbmks,0);
      visbmks->d[0]->name=name;
      sprintf(bustr,"DEFINED MANUAL-BOOKMARK (%d)",name);
      ignorethis(bustr); 
    }   
    break;
  case COMDEF:
    if(!shorthelp(
       "<B> (UN)DEFINE MANUAL-BOOKMARK",
       "OPTIONAL ARGUMENT NAME","") ) comsetbookmk(COMDO);
    else drawstatusline();
  } 
}


void comgobackbookmk(int mode) {
  bookmark *thebmk;
  int i,max,maxi, found,ddf,total;
  char bustr[80], bustr2[MAXPATHSTR+80];
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]!=PGMAGIC && numberarg[0]>=0) { 
      total=LROUND(fabs(numberarg[0]));
      if(backbmks.n>total) {
        max=-1;maxi=-1;
        for(i=0;i<backbmks.n;i++) 
          if(backbmks.d[i]->name>max) {
            max=backbmks.d[i]->name;
            maxi=i;
          }
        for(i=0;i<maxi;i++) 
          rolldownbookmk(&backbmks);
        for(i=backbmks.n-total;i>0;i--) 
          freebookmknumber(&backbmks,backbmks.n-1);
        for(i=0;i<backbmks.n;i++)
          backbmks.d[i]->name=backbmks.n-i;
      }
      backbmksdate=backbmks.n;
      sprintf(bustr,"BACK-BMK: TOTAL %d",backbmks.n);
      ignorethis(bustr);
      break;
    }
    updatefilemk();
    found=0;
    if(backbmks.n>0) {
      thebmk=backbmks.d[0];
      rolldownbookmk(&backbmks);
      found=1;
    }
    if(found) {
      resetsearch();
      ddf=comdofile(thebmk->dvifilename);
      comdoposit(thebmk->pagenum, thebmk->dvixpos,
        thebmk->dviypos, thebmk->fshrink,ddf);
      comdomarks(thebmk->marksxpxl,thebmk->marksypxl);
      vgasetmouse(marksxpxl,marksypxl);
      sprintf(bustr,"BACK-BMK (%d/%d)",thebmk->name,backbmks.n);
      sprintf(bustr2,"%s IN %s",bustr,
         visfmk->dvifilename);
      ignorethis(bustr2);
    }
    break;    
  case COMDEF:
    if(!shorthelp(
       "<^> MOVE TO BACK-BOOKMARK",
       "OPTIONAL ARGUMENT: TOTAL","") ) comgobackbookmk(COMDO);
    else drawstatusline();
    break;
  default:
  }
}




void comgoprevbookmk(int mode) {
  bookmark *thebmk, *startbmk;
  int name, found,ddf;
  char bustr[80], bustr2[MAXPATHSTR+80];
  switch(mode) {
  case COMDO:
    /* toggle mode ? ****************************************/
    if(numberargcount==1 && numberarg[0]==PGMAGIC) { 
      if(bookmkmode==FILETYP) { 
        bookmkmode=MANTYP; 
        ignorethis("SET TO MANUAL-BOOKMARKS");
      } else {      
      if(bookmkmode==MANTYP) { 
        bookmkmode=FILETYP; 
        ignorethis("SET TO FILE-BOOKMARKS");
      }}
      return;
    }
    updatefilemk();
    found=0;
    /* find by name ?*******************************************/
    if(numberargcount==1 && numberarg[0]!=PGMAGIC && numberarg[0]>0
        && numberarg[0]==LROUND(numberarg[0])) {  
      name=MIN(MAXMKS-1,LROUND(numberarg[0]));
      if(!(findrolldownbookmk(visbmks,name))) {
        sprintf(bustr,"NO BOOKMARK (%d) DEFINED",name);
        ignorethis(bustr);
      } else {
        thebmk=visbmks->d[0];
        found=MANTYP;
      }
    } else { /* find by bookmkmode ?****************************/ 
      if(bookmkmode==FILETYP) {
        rolldownfilemk();
        thebmk=&(curfmk->lastpos);
        found=FILETYP;  
      } 
      if(bookmkmode==MANTYP) {
        rolldownbookmk(visbmks);
        if(visbmks->n>0) {
          thebmk=visbmks->d[0];
          found=MANTYP;
	}
      }    
    }
    if(found != 0) {
      resetsearch();
      comdosetbackbmk(); 
      ddf=comdofile(thebmk->dvifilename);
      comdoposit(thebmk->pagenum, thebmk->dvixpos,
        thebmk->dviypos, thebmk->fshrink,ddf);
      if(found == FILETYP) 
        strcpy(bustr,"FILE-BMK");
      if(found == MANTYP) 
        sprintf(bustr,"MANUAL-BMK (%d)",thebmk->name);
      sprintf(bustr2,"%s IN %s",bustr,
         visfmk->dvifilename);
      ignorethis(bustr2);
    }
    break;    
  case COMDEF:
    if(!shorthelp(
       "<W> MOVE TO BOOKMARK",
       "OPTIONAL ARGUMENT: NAME","") ) comgoprevbookmk(COMDO);
    else drawstatusline();
    break;
  default:
  }
}

 
void comstatus(int mode){
  switch(mode) {
  case COMDO:
    ++statustype;
    if(statustype==STATUSTYPES) statustype=0;
    numberarginit();
    drawstatusline();
    break;
  case COMDEF:
    if(!canclethis("<X> SELECT STATUSLINE INFORMATION")) comstatus(COMDO);
    else drawstatusline();
    break;
  } 
}

void comdounit(int line){
  char bustr[4*NUMBEROFUNITS+10];
  int newunit,i;
  uchar o;
  bustr[0]=' ';
  bustr[1]=0;
  for(i=0;i<NUMBEROFUNITS;i++) {
    strcat(bustr,unitnames[i]);
    strcat(bustr,"  ");
  } 
  if(vgastatuslen>4*NUMBEROFUNITS+ 32) 
    drawsr("PRESS <H>/<J>/<RET> OR <ESC>",line,0,vgastatuslen);
  newunit=theunit;
  do {
    drawsn(bustr,line,0);
    drawsn("[",line,newunit*4);
    drawsn("]",line,newunit*4+3);
    vgaupdatestatus();
    do {
      o=vgagetchar(1);
      switch(o){
      case 'H': case 'h': case KEYLEFT: 
        --newunit; break;
      case 'J': case 'j': case KEYRIGHT: 
        ++newunit; break;
      case KEYRESIZE: 
        comdoresize();
        break;
      case KEYRET: case KEYESC:
        break;
      default:
        o=KEYNOP;
      }
    } while(o==KEYNOP);
    if(newunit < 0) newunit=NUMBEROFUNITS-1;
    if(newunit >= NUMBEROFUNITS) newunit=0;
  } while(o!=KEYESC && o!=KEYRET);
  if(o==KEYRET) setunit(newunit);
}

void comunit(int mode){
  switch(mode) {
  case COMDO:
    comdounit(0);
    drawstatusline();
    break;
  case COMDEF:
    vgasetstatuslines(2);
    drawsl("<T> SELECT THE UNIT OF MEASURE",0,0,vgastatuslen);
    comdounit(1);
    drawstatusline();
    break;
  } 
}

int comdoprintpage(int dpg, int gotop) {
  int dodr;
  dodr = readpage(dpg,fshrink,colors,0); 
  if(gotop==1) {
    if(dviypos != MMTOPXL(visfmk->centerymm-visfmk->voffmm)-vgaydim/2) dodr=1;
    if(dvixpos != MMTOPXL(visfmk->centerxmm-visfmk->hoffmm)-vgaxdim/2) dodr=1;
    dviypos= MMTOPXL(visfmk->centerymm-visfmk->voffmm)-vgaydim/2;
    dvixpos= MMTOPXL(visfmk->centerxmm-visfmk->hoffmm)-vgaxdim/2;
  }
  if(!dodr) return(0); 
  resetsearch();
  drawupdatepage();    
  drawstatuspage();
  drawstatusmarks();
  return(1);
} 
 

void comprintpage(int mode, int lm){
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]==PGMAGIC) { 
       pagemovetop=1-pagemovetop;
       if(pagemovetop==1)
         ignorethis("MOVE-TOP-OF-PAGE TURNED ON");
       if(pagemovetop==0)
         ignorethis("MOVE-TOP-OF-PAGE TURNED OFF");
    } else {
      if(numberargcount==1 && numberarg[0]>0 && numberarg[0]<100) 
         pagemovedef= numberarg[0]; 
      comdoprintpage(cpage->num+lm*pagemovedef,pagemovetop);
    }
    break;
  case COMDEF:
    if(!shorthelp(
       "<I>/<M> SELECT A PAGE, W.R.T. THE CURRENT PAGE",
       "AMOUNT OF MOVEMENT (PAGES)","1")  ) comprintpage(COMDO,lm);
    drawstatusline();
    break;
  default:
  }
}

void comgoto(int mode) {
  short i;
  char bustr[MAXARGSTR];
  switch(mode) {
  case COMDO:
    if(numberargcount!=0) {
      for(i=0;i<MIN(numberargcount,10); i++) 
        gotodef[i]=numberarg[i];
      strncpy(gotodefstr,numberargstr,numberargstrlen);
      gotodefstr[numberargstrlen]=0;
      for(;i<10;i++) gotodef[i]=PGMAGIC;
    }
    comdoprintpage(gotocccpage(gotodef,cpage->num),pagemovetop);
    break;
  case COMDEF:
    if(!
     shorthelp("<G> SELECT A PAGE, W.R.T. TeX PAGE-COUNTERS",
               "COUNT0; ...; COUNT9",gotodefstr)  ) comgoto(COMDO);
     drawstatusline(); 
    break;
  default:
  }
}


void comdomoveright(int off){
  int n,d;
  n=CATCHXDVI(dvixpos+off);
  d=n-dvixpos;
  if(d<=0) return;
  drawhidefixedmarksonly();               /*might reset vgascreen      */ 
  dvixpos=n;
#if CURSORSNOTONSCREEN || SCROLLONSCREEN  /* so scrolling is on screen */
  updatelist();                          
  vgascreen(1);
#endif
  vgaxscroll(-d);
#if SCROLLONSCREEN                    
  vgascreen(0);
#endif
  drawshowfixedmarksonly();
  drawpage(vgaxdim-d,0,vgaxdim-1,vgaydim-1);
  updateall();        
  drawstatusmarks();
}

void comdomoveleft(int off) {
  int n,d;
  n=CATCHXDVI(dvixpos-off);
  d=dvixpos-n;
  if(d<=0) return;
  drawhidefixedmarksonly(); 
  dvixpos=n; 
#if CURSORSNOTONSCREEN || SCROLLONSCREEN   /* so scrolling is on screen */
  updatelist();                          
  vgascreen(1);
#endif
  vgaxscroll(d);
#if SCROLLONSCREEN                    
  vgascreen(0);
#endif
  drawshowfixedmarksonly();
  drawpage(0,0,d-1,vgaydim-1);
  updateall();
  drawstatusmarks();
}

void comdomovedown(int off,int over) { 
  int n,d;
  n=CATCHYDVI(dviypos+off);
  d=n-dviypos;
  if(d<=0) {
    if(over==0) return;
    n=dviypos;
    dviypos=CATCHYDVI(-vgaydim-MMTOPXL(visfmk->voffmm));
    if(!comdoprintpage(cpage->num+1,0)) 
      dviypos=n;
    return;
  }
  drawhidefixedmarksonly(); 
  dviypos=n;
#if CURSORSNOTONSCREEN || SCROLLONSCREEN   /* so scrolling is on screen */
  updatelist();                          
  vgascreen(1);
#endif
  vgayscroll(-d);
#if SCROLLONSCREEN                    
  vgascreen(0);
#endif
  drawshowfixedmarksonly();
  drawpage(0,vgaydim-d,vgaxdim-1,vgaydim-1); 
  updateall();
  drawstatusmarks();
}

void comdomoveup(int off, int over) { 
  int n,d;
  n=CATCHYDVI(dviypos-off);
  d=dviypos-n;
  if(d<=0) {
    if(over==0) return;
    n=dviypos;
    dviypos=CATCHYDVI(papypxl-MMTOPXL(visfmk->voffmm));
    if(!comdoprintpage(cpage->num-1,0)) 
     dviypos=n;
    return;
  }
  drawhidefixedmarksonly(); 
  dviypos=n;
#if CURSORSNOTONSCREEN || SCROLLONSCREEN   /* so scrolling is on screen */
  updatelist();                          
  vgascreen(1);
#endif
  vgayscroll(d);
#if SCROLLONSCREEN                    
  vgascreen(0);
#endif
  drawshowfixedmarksonly();
  drawpage(0,0,vgaxdim-1,d-1); 
  updateall();
  drawstatusmarks();
}



void comdomarks(int x, int y) {
   int xx,yy;
   if(markon==0 && hypon==0) return;
   xx=x;
   yy=y;
   if(xx<0) { 
     comdomoveleft(-x);
     xx=0;
   }
   if(xx>vgaxdim-1) { 
     comdomoveright(x-vgaxdim+1);
     xx=vgaxdim-1;
   }
   if(yy<0){ 
     comdomoveup(-y,moveoverpages);
     yy=0;
   }
   if(yy>vgaydim-1) { 
     comdomovedown(y-vgaydim+1,moveoverpages);
     yy=vgaydim-1;
   }
   if(xx!=marksxpxl || yy!=marksypxl) {
     drawhidefixedmarksonly();
     marksxpxl=xx;
     marksypxl=yy;
     /* was: setmouse */
     drawshowfixedmarksonly();
     drawstatusmarks();
   } else
     if(x!=xx || y!=yy) 
       drawstatusmarks();
   updatelist();
}


void commoveright(int mode) {
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]!= PGMAGIC) 
      SETMOVEOFFDEFPZ(numberarg[0]);
#ifndef HASMOUSE
    if(markon || hypon) 
      comdomarks(marksxpxl+moveoffdef,marksypxl);
    else  
#endif
      comdomoveright(moveoffdef);
    break;
  case COMDEF:
    SETMOVEDEFSTR;
    if(!shorthelp(
    "<U>/<N>/<H>/<J> SCROLL THE VISABLE AREA",
    "AMOUNT OF MOVEMENT (%)" ,movedefstr)  ) commoveright(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void commoveleft(int mode) {
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]!= PGMAGIC) 
      SETMOVEOFFDEFPZ(numberarg[0]);
#ifndef HASMOUSE
    if(markon || hypon) 
      comdomarks(marksxpxl-moveoffdef,marksypxl);
    else
#endif  
      comdomoveleft(moveoffdef);
    break;
  case COMDEF:
    SETMOVEDEFSTR;
    if(!shorthelp(
    "<U>/<N>/<H>/<J> SCROLL THE VISABLE AREA",
    "AMOUNT OF MOVEMENT (%)" ,movedefstr)  ) commoveleft(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void commovedown(int mode) {
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]==PGMAGIC) {
      moveoverpages=1-moveoverpages;
      if(moveoverpages==1)
        ignorethis("MOVE-OVER-PAGES TURNED ON");
      else
        ignorethis("MOVE-OVER-PAGES TURNED OFF");
    } else { 
      if(numberargcount==1 && numberarg[0]!=PGMAGIC) 
         SETMOVEOFFDEFPZ(numberarg[0]);
#ifndef HASMOUSE
      if(markon || hypon) 
        comdomarks(marksxpxl,marksypxl+moveoffdef);
      else  
#endif
        comdomovedown(moveoffdef,moveoverpages);
    }
    break;
  case COMDEF:
    SETMOVEDEFSTR;
    if(!shorthelp(
    "<U>/<N>/<H>/<J> SCROLL THE VISABLE AREA",
    "AMOUNT OF MOVEMENT (%)" ,movedefstr)  ) commovedown(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void commoveup(int mode) {
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]==PGMAGIC) {
      moveoverpages=1-moveoverpages;
      if(moveoverpages==1)
        ignorethis("MOVE-OVER-PAGES TURNED ON");
      else
        ignorethis("MOVE-OVER-PAGES TURNED OFF");
    } else { 
     if(numberargcount==1 && numberarg[0]!=PGMAGIC) 
         SETMOVEOFFDEFPZ(numberarg[0]);
#ifndef HASMOUSE
      if(markon || hypon) 
        comdomarks(marksxpxl,marksypxl-moveoffdef);
      else  
#endif
        comdomoveup(moveoffdef,moveoverpages);
    }
    break;
  case COMDEF:
    SETMOVEDEFSTR;
    if(!shorthelp(
    "<U>/<N>/<H>/<J> SCROLL THE VISABLE AREA",
    "AMOUNT OF MOVEMENT (%)" ,movedefstr)  ) commoveup(COMDO);
    drawstatusline();
    break;
  default:
  }
}


void comfine(int mode, int lm) {
  char bustr[80];
  switch(mode) {
  case COMDO:
    if(lm==-1) SETMOVEOFFDEFPX(MIN(moveoffdef/FINE,moveoffdef-1)); 
    if(lm==+1) SETMOVEOFFDEFPX(MAX(moveoffdef*FINE,moveoffdef+1)); 
    sprintf(bustr,"CURRENT MOVEOFFSET: %.1f%%",100.0*moveoffdef/vgaxdim);
    ignorethis(bustr);
    break;
  case COMDEF:
    if(!canclethis(
      "<F>/<C> MAKE SCROLLING THE VISABLE AREA MORE FINE/COARSE")) 
      comfine(COMDO,lm);
    drawstatusline();
    break;
  default:
  }
}

void comdozoom(float newfshrink) {
    float oldshrink;
    char bustr[80];
    oldshrink=fshrink;
    if(!readpage(cpage->num,newfshrink,colors,0)) return;
    resetsearch();
    papxpxl=MMTOPXL(visfmk->papxmm);
    papypxl=MMTOPXL(visfmk->papymm);
    if(markon) {
      dvixpos=ROUND(oldshrink/ fshrink*(dvixpos+marksxpxl)-marksxpxl);
      dviypos=ROUND(oldshrink/ fshrink*(dviypos+marksypxl)-marksypxl);
    } else {
      dvixpos=ROUND(oldshrink/ fshrink*(dvixpos+vgaxdim/2)-vgaxdim/2);
      dviypos=ROUND(oldshrink/ fshrink*(dviypos+vgaydim/2)-vgaydim/2);
    }
    /* if(texton) {
      textx1pxl=LFLOOR(oldshrink/ fshrink*textx1pxl);
      texty1pxl=LFLOOR(oldshrink/ fshrink*texty1pxl);
      textx2pxl=LCEIL(oldshrink/ fshrink*textx2pxl);
      texty2pxl=LCEIL(oldshrink/ fshrink*texty2pxl);
      textx3pxl=LFLOOR(oldshrink/ fshrink*textx3pxl);
      texty3pxl=LFLOOR(oldshrink/ fshrink*texty3pxl);
      textx4pxl=LCEIL(oldshrink/ fshrink*textx4pxl);
      texty4pxl=LCEIL(oldshrink/ fshrink*texty4pxl);
    } */
    dvixpos=CATCHXDVI(dvixpos);
    dviypos=CATCHYDVI(dviypos);
    drawupdatepage();    
}

void comzoomfine(int mode, int lm) {
  char bustr[80];
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]!= PGMAGIC) {
       ishrinkonly=0;
       zoomfinedef=1.0+MAX(0.01,MIN(2.0,numberarg[0]/100.0));
       sprintf(zoomfinedefstr,"%.1f", MAX(10,MIN(200,numberarg[0])));
    }
    if(ishrinkonly) comdozoom(fshrink-lm);
    else{
      if(lm== -1) comdozoom(fshrink*zoomfinedef);
      if(lm== +1) comdozoom(fshrink/zoomfinedef);
    }
    sprintf(bustr,"CURRENT ZOOMFACTOR: %.3f",1.0/fshrink);
    ignorethis(bustr);
    break;
  case COMDEF:
    if(!shorthelp(
    "<+>/<-> ZOOMING IN/OUT",
    "DECREMENT RESP. INCREMENT (%)" ,zoomfinedefstr)  ) comzoomfine(COMDO,lm);
    else drawstatusline();
    break;
  default:
  }
}

void comzoomset(int mode) {
  char bustr1[MAXARGSTR],bustr2[MAXARGSTR];
  switch(mode) {
  case COMDO:
    if(numberargcount==1) {
       if(numberarg[0]==PGMAGIC){
         ishrinkonly=1;
         zoomdef=1/MAX(1,ROUND(fshrink));
       }
       else {
         zoomdef=numberarg[0];
         ishrinkonly=0;
       }
    } 
    comdozoom(1/zoomdef);
    zoomdef=1/fshrink;
    if(ishrinkonly==0) 
      strcpy(bustr2,"ARBYTRARY-MODE");
    else
      strcpy(bustr2,"INTEGER-MODE");
    sprintf(bustr1,"CURRENT ZOOMFACTOR (%s): %.3f",bustr2,1.0/fshrink);
    ignorethis(bustr1);
    break;
  case COMDEF:
    sprintf(bustr1,"%.3f", zoomdef);
    sprintf(bustr2,"(CURRENTLY %.3f)", 1/fshrink);
    if(!shorthelp(
    "<V> SET THE ZOOMFACTOR",
    bustr2 ,bustr1)  ) comzoomset(COMDO);
    else drawstatusline();
    break;
  default:
  }
}

void comcenter(int mode) {
  char bustr1[40], bustr2[MAXARGSTR];
  int d, cxpxl,cypxl;
  /*fprintf(prot,"(comcenter %d...)",centhis);*/
  switch(mode) {
  case COMDO:
    if(numberargcount==1 && numberarg[0]==PGMAGIC ) {
      if(markon) {
        visfmk->centerxmm=PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm;
        visfmk->centerymm=PXLTOMM(dviypos+marksypxl)+visfmk->voffmm;
      } else {
        visfmk->centerymm=PXLTOMM(dviypos+vgaydim/2)+visfmk->hoffmm;
        visfmk->centerxmm=PXLTOMM(dvixpos+vgaxdim/2)+visfmk->voffmm;
      } 
    } 
    if(numberargcount==2 && numberarg[0]!=PGMAGIC && numberarg[1]!=PGMAGIC) {
      visfmk->centerxmm=numberarg[0];
      visfmk->centerymm=numberarg[1];
    }
    visfmk->centerxmm=MAX(0,MIN(visfmk->papxmm,visfmk->centerxmm));  
    visfmk->centerymm=MAX(0,MIN(visfmk->papymm,visfmk->centerymm));  
    cxpxl=MMTOPXL(visfmk->centerxmm-visfmk->hoffmm);
    cypxl=MMTOPXL(visfmk->centerymm-visfmk->voffmm);

    switch(centhis) {
    case 0:
      d=-dvixpos+cxpxl-vgaxdim/2;
      if(d>0) comdomoveright(d); else comdomoveleft(-d);
      break;
    case 1: 
      d= -dviypos+cypxl-vgaydim/2;
      if(d>0) comdomovedown(d,0); else comdomoveup(-d,0);
      break;
    default:
    }
    centhis++;
    if(centhis==2) centhis=1;
    break;
  case COMDEF:
    sprintf(bustr1,"%%.%df;%%.%df",
            unitcomma,unitcomma);
    sprintf(bustr2,bustr1,
     mmtounit*visfmk->centerxmm,mmtounit*visfmk->centerymm);
    if(!shorthelp(
      "<Z> CENTER THE VISABLE AREA",
      "CENTERPOINT", bustr2)) comcenter(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void comframe(int mode) {
  char bustr1[40], bustr2[MAXARGSTR];
  switch(mode) {
  case COMDO:
    if(numberargcount==4 && numberarg[0]!= PGMAGIC 
                         && numberarg[1]!= PGMAGIC
                         && numberarg[2]!= PGMAGIC
                         && numberarg[3]!= PGMAGIC) {
       visfmk->lrandmm=unitomm*numberarg[0]; 
       visfmk->rrandmm=unitomm*numberarg[1];
       visfmk->orandmm=unitomm*numberarg[2];
       visfmk->urandmm=unitomm*numberarg[3];
       frameon=0;                         /* go on soon */
    }
    if(numberargcount==1 && numberarg[0]==PGMAGIC) {
      if(markon) {
        visfmk->lrandmm=
          MIN(markdxmm,PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm); 
        visfmk->rrandmm= visfmk->papxmm-
          MAX(markdxmm,PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm);
        visfmk->orandmm=
          MIN(markdymm,PXLTOMM(dviypos+marksypxl)+visfmk->voffmm); 
        visfmk->urandmm= visfmk->papymm-
          MAX(markdymm,PXLTOMM(dviypos+marksypxl)+visfmk->voffmm);
      } else {
        visfmk->urandmm=fmkstar.urandmm;
        visfmk->lrandmm=fmkstar.lrandmm;
        visfmk->rrandmm=fmkstar.rrandmm;
        visfmk->orandmm=fmkstar.orandmm;
      }  
      frameon=0;                         /* go on soon */
    }
    frameon=1-frameon;
    drawupdatepage();    
    break;
  case COMDEF:
    sprintf(bustr1,"%%.%df;%%.%df;%%.%df;%%.%df",
            unitcomma,unitcomma,unitcomma,unitcomma);
    sprintf(bustr2,bustr1,
     mmtounit*visfmk->lrandmm,mmtounit*visfmk->rrandmm,
     mmtounit*visfmk->orandmm,mmtounit*visfmk->urandmm);
    if(!shorthelp(
      "<P> SHOW/HIDE THE PRINTABLE AREA",
      "MARGINS: LEFT; RIGHT; TOP; BOTTOM", bustr2)) comframe(COMDO);
    drawstatusline();
    break;
  default:
  }
}



void compaper(int mode) {
  char bustr1[40], bustr2[MAXARGSTR];
  float nho,nvo,npx,npy,dx,dy;
  switch(mode) {
  case COMDO:
    if(numberargcount==4 && numberarg[0]!= PGMAGIC 
                         && numberarg[1]!= PGMAGIC
                         && numberarg[2]!= PGMAGIC
                         && numberarg[3]!= PGMAGIC) {
       nho=unitomm*numberarg[0]; 
       nvo=unitomm*numberarg[1];
       npx=unitomm*numberarg[2];
       npy=unitomm*numberarg[3];
    } else {
    if(numberargcount==1 && numberarg[0]==PGMAGIC) {
      if(markon) {
        nho= -MIN(markdxmm-visfmk->hoffmm,PXLTOMM(dvixpos+marksxpxl));  
        nvo= -MIN(markdymm-visfmk->voffmm,PXLTOMM(dviypos+marksypxl));
        npx= MAX(markdxmm-visfmk->hoffmm,PXLTOMM(dvixpos+marksxpxl)) +nho;
        npy= MAX(markdymm-visfmk->voffmm,PXLTOMM(dviypos+marksypxl)) +nvo;
      } else {
        nho=fmkstar.hoffmm;
        nvo=fmkstar.voffmm;
        npx=fmkstar.papxmm;
        npy=fmkstar.papymm;
      }
    } else break;}
    dx=nho-visfmk->hoffmm;
    dy=nvo-visfmk->voffmm;
    markdxmm+=dx;
    markdymm+=dy;
    rectxmm+=dx;
    rectymm+=dy;
    visfmk->centerxmm+=dx;
    visfmk->centerymm+=dx;
    visfmk->hoffmm=nho;
    visfmk->voffmm=nvo;
    visfmk->papxmm=npx;
    visfmk->papymm=npy;
    papxpxl=MMTOPXL(npx);
    papypxl=MMTOPXL(npy);
    CATCHXDVI(dvixpos);
    CATCHYDVI(dviypos);
    resetsearch();
    drawupdatepage();    
    break;
  case COMDEF:
    sprintf(bustr1,"%%.%df;%%.%df;%%.%df;%%.%df",
            unitcomma,unitcomma,unitcomma,unitcomma);
    sprintf(bustr2,bustr1,
     mmtounit*visfmk->hoffmm,mmtounit*visfmk->voffmm,
     mmtounit*visfmk->papxmm,mmtounit*visfmk->papymm);
    if(!shorthelp(
      "<E> PAPERSIZE AND OFFSET",
      "MARGINS: HOFF; VOFF; WIDTH; HEIGHT", bustr2)) compaper(COMDO);
    drawstatusline();
    break;
  default:
  }
}



void comrect(int mode) {
  float rxmm,rymm,rwmm,rhmm;
  char bustr1[40], bustr2[MAXARGSTR];
  switch(mode) {
  case COMDO:
    if(markon && recton) /* dont flicker */
    if(
  !MMTOPXL(rectxmm-MIN(markdxmm,PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm)) &&
  !MMTOPXL(rectwmm-fabs(markdxmm-PXLTOMM(dvixpos+marksxpxl)-visfmk->hoffmm)) &&
  !MMTOPXL(rectymm-MIN(markdymm,PXLTOMM(dviypos+marksypxl)+visfmk->voffmm)) &&
  !MMTOPXL(recthmm-fabs(markdymm-PXLTOMM(dviypos+marksypxl)-visfmk->voffmm))
      ) break;
    if(markon) {
       drawhiderect();
       rectxmm=MIN(markdxmm,PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm); 
       rectwmm=fabs(markdxmm-PXLTOMM(dvixpos+marksxpxl)-visfmk->hoffmm);
       rectymm=MIN(markdymm,PXLTOMM(dviypos+marksypxl)+visfmk->voffmm);
       recthmm=fabs(markdymm-PXLTOMM(dviypos+marksypxl)-visfmk->voffmm);
       recton=0;                         /* go on soon */
    }
    if(numberargcount==4 && numberarg[0]!= PGMAGIC 
                         && numberarg[1]!= PGMAGIC
                         && numberarg[2]!= PGMAGIC
                         && numberarg[3]!= PGMAGIC) {
       drawhiderect();
       rectxmm=unitomm*numberarg[0]; 
       rectymm=unitomm*numberarg[1];
       rectwmm=unitomm*numberarg[2];
       recthmm=unitomm*numberarg[3];
       recton=0;                         /* go on soon */
    }
    if(recton==1) {
      drawhiderect();
      recton=0;
    } else { 
      recton=1;
      drawshowrect();
    }
    updatelist(); 
    break;
  case COMDEF:
    if(markon) {
       rxmm=MIN(markdxmm,PXLTOMM(dvixpos+marksxpxl)+visfmk->hoffmm); 
       rwmm=fabs(markdxmm-PXLTOMM(dvixpos+marksxpxl)-visfmk->hoffmm);
       rymm=MIN(markdymm,PXLTOMM(dviypos+marksypxl)+visfmk->voffmm);
       rhmm=fabs(markdymm-PXLTOMM(dviypos+marksypxl)-visfmk->voffmm);
    }
    sprintf(bustr1,"%%.%df;%%.%df;%%.%df;%%.%df",
            unitcomma,unitcomma,unitcomma,unitcomma);
    sprintf(bustr2,bustr1,
     mmtounit*rxmm,mmtounit*rymm,mmtounit*rwmm,mmtounit*rhmm);
    if(!shorthelp(
      "<A> SHOW/HIDE MARKED RECTANGLE",
      "MARGINS+SIZE: LEFT; TOP; WIDTH; HIGHT", bustr2)) comrect(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void commark(int mode) {
  char bustr1[40], bustr2[MAXARGSTR];
  switch(mode) {
  case COMDO:
    if(hypon) {
       hypon=0;
       drawupdatepage();
       markon=0; /* go on soon (is 0 anyway) */
    }
    if(numberargcount==2 && numberarg[0]!= PGMAGIC 
                         && numberarg[1]!= PGMAGIC) {
      if(markon!=0) {
        drawhideallmarks();
        markon=0;  /* go on soon */
      }    
      markdxmm=unitomm*numberarg[0]; 
      markdymm=unitomm*numberarg[1];
    }
    if(markon==1) {
      drawhideallmarks();
      markon=0;
      drawshowallmarks();
      if(recton==1) {
        recton=0;
        drawupdatepage();
      }
    } else {
      markon=1;
      drawshowallmarks();
#ifndef HASWINDOWS
      vgasetmouse(marksxpxl,marksypxl);
#endif
    }
    updatelist();
    resetsearch();
    break;
  case COMDEF:
    sprintf(bustr1,"%%.%df;%%.%df",
            unitcomma,unitcomma);
    sprintf(bustr2,bustr1,
     mmtounit*markdxmm,mmtounit*markdymm);
    if(!shorthelp(
      "<L> SHOW/HIDE PAGEMARK AND SCREENMARK",
      "PAGEMARK-POSITION: PM_X; PM_Y", bustr2)) commark(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void comdomarkfollow(void) {
  int x,y;
  if(markon==0) return;
  drawhideallmarks();
  markdxmm =PXLTOMM(marksxpxl+dvixpos)+visfmk->hoffmm;
  markdymm =PXLTOMM(marksypxl+dviypos)+visfmk->voffmm;
  drawshowallmarks();
  drawstatusmarks();
  updatelist();
}

void commarkfollow(int mode) {
  switch(mode) {
  case COMDO:
    comdomarkfollow();
    break;
  case COMDEF:
    if(!canclethis("<Y> SET PAGEMARK AT SCREENMARK")) commarkfollow(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void comhyp(int mode) {
  switch(mode) {
  case COMDO:
    if(markon) {
       drawhideallmarks();
       hypon=0;
       markon=0; /* go on soon */
       if(recton==1) 
         recton=0;
    }
    if(hypon==1) hypon=0; 
    else {
      hypon=1;
#ifndef HASWINDOWS
      vgasetmouse(marksxpxl,marksypxl);
#endif
    }
    drawupdatepage(); 
    resetsearch(); 
    break;
  case COMDEF:
    if(!canclethis("<K> HYPERDVI ON/OFF")) comhyp(COMDO);
    drawstatusline();
    break;
  default:
  }
}

void comdohypfollow(void) {
  char* href=NULL;
  int xpxl, ypxl, pn;
  int dodr, nxp,nyp;
  
  if(hypon) 
    htex_clickmouse(cpage,marksxpxl+dvixpos, marksypxl+dviypos, &href);
  if(!hypon && !markon && texton) {
    htex_clickmouse(cpage,LROUND(0.5*(textx2pxl+textx1pxl)),
                          LROUND(0.5*(texty2pxl+texty1pxl)),&href);
  }
  if(href==NULL) return;
  resetsearch();
  pn=cpage->num;
  if(!searchhref(href,&pn, &xpxl, &ypxl)) {drawstatusline(); return;}
  comdosetbackbmk();
  dodr=0;
  nxp=MAX(xpxl-vgaxdim,MIN(xpxl,dvixpos));
  nyp=MAX(ypxl-0.8*vgaydim,MIN(ypxl-0.2*vgaydim,dviypos));
  if(nxp!=dvixpos || nyp !=dviypos) {
    dvixpos=nxp;
    dviypos=nyp;
    dodr=1;
  }
  if(readpage(pn,fshrink,colors,0)) dodr=1;
  if(dodr) drawupdatepage();
  comdomarks(xpxl-dvixpos,ypxl-dviypos);
  drawstatusline();
  vgasetmouse(marksxpxl,marksypxl);
  comdosetbackbmk();
}

void comdohypnext(void) {
  char* href;
  int xpxl, ypxl, pn;
  int dodr, nxp,nyp;
  int found;
  
  found=0;
  drawhideallmarks();
  texton=0;
  if(hypon) {
    xpxl=marksxpxl+dvixpos;
    ypxl=marksypxl+dviypos;
    found=htex_findnearanchor(cpage,&xpxl,&ypxl);
    nxp=MAX(xpxl-vgaxdim,MIN(xpxl,dvixpos));
    nyp=MAX(ypxl-0.8*vgaydim,MIN(ypxl-0.2*vgaydim,dviypos));
  }
  if(!markon && !hypon) { 
    found=htex_findnumberanchor(cpage,&searchanc);
    nxp=MAX(textx2pxl-vgaxdim,MIN(textx1pxl,dvixpos));
    nyp=MAX(texty2pxl-0.8*vgaydim,MIN(texty1pxl-0.2*vgaydim,dviypos));
  }
  if(found) {
    dodr=0;
    if(nxp!=dvixpos || nyp !=dviypos) {
      dvixpos=nxp;
      dviypos=nyp;
      dodr=1;
    }
    if(readpage(cpage->num,fshrink,colors,0)) dodr=1;
    if(dodr) drawupdatepage();
  
    if(hypon) {
      comdomarks(xpxl-dvixpos,ypxl-dviypos);
      vgasetmouse(marksxpxl,marksypxl);
    }
    else {
      texton=1;
      drawshowallmarks();
    }
  } else {                                     /* not found */
    resetsearch();
    drawshowallmarks();
  }
  updatelist();
}

void comhypnext(int mode) {
  switch(mode) {
  case COMDO: 
    comdohypnext();
    break;
  case COMDEF:
    if(!canclethis("<TAB> GOTO NEXT HREF")) comhypnext(COMDO);
    drawstatusline();
    break;
  default:
  }
}



void comgrey(int mode) {
  switch(mode) {
  case COMDO:
    if(colors==BLACKNWHITE) 
      readpage(cpage->num,fshrink,GREYSCALE,0);
    else 
      readpage(cpage->num,fshrink,BLACKNWHITE,0);
    drawupdatepage();    
    if(colors==BLACKNWHITE)
      ignorethis("DISPLAY: BLACKNWHITE");
    else
      ignorethis("DISPLAY: GREYSCALES");
    break;    
  case COMDEF:
    if(!canclethis("<O> TOGGLE GREYSCALING")) comgrey(COMDO);
    drawstatusline();
    break;
  default:
  }
}


void comsearch(int mode) {
  int dodr;
  const char* restr;
  int xpxl,ypxl,nxp,nyp;
  char bustr1[80], bustr2[MAXARGSTR+80];
  switch(mode) {
  case COMDO:
    drawsl("SEARCH:",0,0,vgastatuslen);
    if(stringargedit(0,8,NULL)) { drawstatusline(); return;}
    if(searchstring!=NULL) 
    if(strcmp(searchstring,stringarg)!=0) 
       resetsearch();
    stralloccpy(&searchstring,stringarg);
    drawhideallmarks();     
    texton=0; 
    if(searchpage==-1) searchpage=cpage->num;
    searchpos++;
    sprintf(bustr2,"SEARCHING <%s> ...",stringarg);
    drawsl(bustr2,0,0,vgastatuslen);
    vgaupdatestatus();
    restr=searchdvi(stringarg,&searchpage,&searchpos);
    if(restr==NULL) {  /*found*/
      comdosetbackbmk();
      dodr=0;
      nxp=MAX(textx2pxl-vgaxdim,MIN(textx1pxl,dvixpos));
      nyp=MAX(texty2pxl-0.8*vgaydim,MIN(texty1pxl-0.2*vgaydim,dviypos));
      if(nxp!=dvixpos || nyp !=dviypos) {
        dvixpos=nxp;
        dviypos=nyp;
        dodr=1;
      }
      if(readpage(searchpage,fshrink,colors,0)) dodr=1;
      if(dodr) drawupdatepage();
      if(markon || hypon) {   
        comdomarks(textx1pxl-dvixpos,texty2pxl-dviypos);
        vgasetmouse(marksxpxl,marksypxl);
      } else {
        texton=1;
        drawshowallmarks();
      }
      sprintf(bustr1,"FOUND <%%s> ON PAGE [%%d] AT (%%.%df,%%.%df) [%s]",
          unitcomma,unitcomma,unitname);
      sprintf(bustr2,bustr1,stringarg,searchpage,
              PXLTOMM(textx1pxl),PXLTOMM(texty2pxl));
      ignorethis(bustr2);
      comdosetbackbmk();
    } else  {                                  /* not found */
      if(searchpage==0) {
        resetsearch(); 
        drawshowallmarks();
      } else {
        texton=0;
        drawshowallmarks();
        searchpage=0;
        searchpos=0;
      }   
      ignorethis(restr);
    }
    updatelist();
    break;    
  case COMDEF:
    /* ignorethis("<S> SEARCH TEXT"); */
    comsearch(COMDO);
    break;
  default:
  }
}


main(int argc, char **argv) { 
  uchar o, wasnum, inpanic=0; 
  int mousex, mousey, mouseleft, mouseright;
  int comode;
  char *parm;
  char *filename=NULL;
  int i,j,k,ox,oy,ol,or;
  long f;


  pfprot( "This is some kind of a DVIewer, V 96.05\n");
  pfprot( "(C)opyright 1995 Thomas Moor\n\n");

  vgaopen();            /* open a window part 1 */

  /* set constants */
  allocmem(&bit_masks,BYTES_PER_BMUNIT * (BITS_PER_BMUNIT+1));
  bit_masks[0]=0;
  for(j=1;j<=BITS_PER_BMUNIT;j++) 
      bit_masks[j] = (bit_masks[j-1] << 1) | 1;
  allocmem(&bit_count,256);
  for(j=0;j<256;) {
    k=0;
    for(i=0;i!=7;i++) k+= (j >> i) & 1; 
    bit_count[j++]=k;
  }
 
  setworkdir();  

  /* set compiletime defaults */
  newmag = 0;
  savestartup=SAVESTARTUP;   
  stralloccpy(&fontprefix,PKDIR);
  stralloccpy(&fontformat,PKNAME);
  stralloccpy(&tfmprefix,TFMDIR);
  stralloccpy(&startupfilename,STARTUPFILENAME);
  /*env=getenv("DVIDRVFONTS");
  if(env!=NULL) stralloccpy(fontprefix,env);
  env=getenv("TEXTFM");
  if(env!=NULL) strcpy(tfmprefix,env); */ 
  vgaxdim=VGAXDIM;
  vgaydim=VGAYDIM;
  memset(&fmkstar,0,sizeof(filemark)); /* ptrs all NULL */
  fmkstar.hoffmm = HOFFMM;  
  fmkstar.voffmm= HOFFMM;
  fmkstar.papxmm=PAPXMM;
  fmkstar.papymm=PAPYMM;
  fmkstar.lrandmm=LRANDMM;
  fmkstar.rrandmm=RRANDMM;
  fmkstar.orandmm=ORANDMM;
  fmkstar.urandmm=URANDMM;
  fmkstar.centerxmm=fmkstar.papxmm/2;
  fmkstar.centerymm=fmkstar.papymm/2;

  xres=XRES;
  yres=YRES;
  
  verbose = VERBOSE; 
 
  filemksinit();     
  initfontdatabase();
  ishrinkonly=ISHRINKONLY;
  fshrink=SHRINK;
  colors=PCOLORS;
  unitstar=UNIT;

  markon=MARKON;
  hypon=HYPON;
  texton=0;
  frameon=0;
  recton=0;
  rectxmm=0;
  rectymm=0;
  recthmm=0;
  rectwmm=0;

  gotodef[0]=1.0; 
  for(i=1;i<10;i++) gotodef[i]=PGMAGIC; 
  strcpy(gotodefstr,"1;*;*;"); 
  pagemovedef=1;
  pagemovetop=PAGEMOVETOP;
  moveoverpages=MOVEOVERPAGES;
  bookmkmode=FILETYP;
  marksxpxl=0; 
  marksypxl=0; 
  markdxmm=0;
  markdymm=0;
  allocmem(&numberargstr,MAXARGSTR+1);
  numberarginit();
  stringarginit();

  
  /* read commandline-option 's' for startupfile */
  for(j = 1; j < argc; j++) {  
    parm = argv[j];
    if(parm[0] != '-') continue;
    if(parm[1] != 's' && parm[1] != 'S') continue;
    stralloccpy(&startupfilename, parm+2);  
    break;
  }
  
  confread(); 

  /* read other commandline-options */
  for (j = 1; j < argc; j++) {  parm = argv[j];
    if (parm[0] != '-') {  stralloccpy(&filename, parm);
    } else {  switch (parm[1]) {
      case 'f':  case 'F':  
        stralloccpy(&fontprefix, parm+2);  break;
      case 'n':  case 'N':  
        stralloccpy(&fontformat, parm+2);  break;
      case 't':  case 'T':  
        stralloccpy(&tfmprefix, parm+2);  break;
      case 'm':  case 'M':  
        sscanf(parm+2, "%ld", &newmag);  break;
      case 'h':  case 'H':  
        sscanf(parm+2, "%f", &fmkstar.hoffmm);  break;
      case 'v':  case 'V':  
        sscanf(parm+2, "%f", &fmkstar.voffmm);  break;
      case 'd':  case 'D':  
        sscanf(parm+2, "%dx%d", &vgaxdim,&vgaydim); break;
      case 'p':  case 'P':  
        sscanf(parm+2, "%fx%f", &fmkstar.papxmm,&fmkstar.papymm); break;
      case 'r':  case 'R':  
        sscanf(parm+2, "%dx%d", &xres,&yres); break;
      case 'k':  case 'K':  
        sscanf(parm+2, "%f,%f,%f,%f", 
          &fmkstar.lrandmm,&fmkstar.rrandmm,
          &fmkstar.orandmm,&fmkstar.urandmm); break;
      case '?':  
        pfprot( "use with dvifilename and optional parameters\n");
        pfprot( " -h<horizontal offset>   (default: %5.1f mm)\n", 
          fmkstar.hoffmm);
        pfprot( " -v<vertical offset>     (default: %5.1f mm)\n", 
          fmkstar.voffmm);
        pfprot( " -p<paperwidth>x<height> (default: %5.1fx%5.1f mm**2)\n",
          fmkstar.papxmm,fmkstar.papymm);
        pfprot( " -r<xres>x<yres>         (default: %dx%d dpi**2)\n",
                                                      xres,yres);
        pfprot( " -f<font directory>      (default: %s)\n", fontprefix);
        pfprot( " -n<font name format>    (default: %s)\n", fontformat); 
        pfprot( " -t<TFMfile directory>   (default: %s)\n", tfmprefix);
        pfprot( " -d<diplaywidth>x<height>(default: %dx%d pxl**2)\n",
                                                      vgaxdim,vgaydim);
        pfprot( " -m<new magnification>   (default: from DVIfile)\n");
        pfprot( 
           " -k<lr>,<rr>,<or>,<ur>   (default: %2.1f,%2.1f,%2.1f,%2.1f mm)\n",
           fmkstar.lrandmm,fmkstar.rrandmm,fmkstar.orandmm,fmkstar.urandmm);
        pfprot( " -s<startupfile>         (default: %s)\n", startupfilename);
        exit(0);

  }}}

#ifdef KPATHSEA
    kpse_set_progname (argv[0]);
    kpse_init_prog ("TMVIEW", xres,KPATHMODE,KPATHMAKE,KPATHDEFF);
#endif


  vgagraph();           /* open a window part 2*/
  vgasetstatuslines(1); /* sets vgaxdim ydim !! */
#if NOTHINGONSCREEN || SCROLLONSCREEN
  vgascreen(0);      
#else
  vgascreen(1);
#endif

  setunit(unitstar);

  moveoffdef=vgaxdim/5;
  strcpy(movedefstr,"20.0");
  zoomfinedef=1.1892;
  zoomdef=1/fshrink; 
  strcpy(zoomfinedefstr,"18.9");


  
  dvierrorflag=1;
  if(filename!=NULL)
    comdofile(filename); 
  else 
    if(curfmk!=NULL) comdofile(curfmk->dvifilename);
  if(!dvierrorflag) {
    comdoposit(visfmk->lastpos.pagenum,visfmk->lastpos.dvixpos,
      visfmk->lastpos.dviypos,visfmk->lastpos.fshrink,1);
  }
  
 
 
  drawsl("TMVIEW V 96.05",0,0,vgastatuslen);
  drawsr("PRESS <?> TWICE FOR HELP",0,vgastatuslen-25,25);  
  vgaupdatestatus();


#ifdef HASMOUSE
  if(markon || hypon) 
    vgasetmouse(marksxpxl,marksypxl);
  else
    vgasetmouse(vgaxdim/2,vgaydim/2);
  vgagetmouse(&ox,&oy,&ol,&or);
#endif

  centhis=0;    
  comode=COMDO; 
  statustype= 0;
  statusforce= 1;
  do{
    if(dvierrorflag && !inpanic) {
      drawsl("DVI PANIC",0,0,vgastatuslen-31);
      drawsr("<D> FILE  <R> RE-READ  <Q> QUIT",0,vgastatuslen-31,31);
      vgaupdatestatus();
      statusforce=0;
      inpanic=1;
    }

    o=vgagetchar(1);    
    wasnum=0;
    if('A' <= o && o<= 'Z') {
      o+='a'-'A';
      comode=COMDEF;
    }

    if(inpanic) {
      if(dvierrorflag && o!='r' && o!='d' && o!='q'&& o!=KEYRESIZE) {
        o=KEYNOP;
        statusforce=0;
      }
      if(!dvierrorflag || o!=KEYNOP) {
        inpanic=0;
        statusforce=1;
      }
    }

    switch(o) {
    case 'b':  comsetbookmk(comode);     break;
    case 'w':  comgoprevbookmk(comode);  break;
    case '^':  comgobackbookmk(comode);  break;
    case 'r':  comredraw(comode);        break;
    case 'd':  comfile(comode);    break;
    case 'e':  compaper(comode);    break;
    case 'x':  comstatus(comode);  break;
    case 't':  comunit(comode);    break;
    case 'f':  comfine(comode,-1); break;
    case 'c':  comfine(comode,+1); break;
    case 'g':  comgoto(comode);    break;
    case 'p':  comframe(comode);   break;
    case 'a':  comrect(comode);    break;
    case 'l':  commark(comode);    break; 
    case 'k':  comhyp(comode);     break; 
    case 'y':  commarkfollow(comode);break; 
    case 'z':  comcenter(comode);  break; 
    case 'o':  comgrey(comode);    break;
    case 'v':  comzoomset(comode); break; 
    case '+':  comzoomfine(comode,+1); break; 
    case '-':  comzoomfine(comode,-1); break;
    case 's':  comsearch(comode);  break; 
 
 
    case 'm': case KEYNEXT:  comprintpage(comode,+1);            break;
    case 'i': case KEYPREV:  comprintpage(comode,-1);            break;
    case 'u': case KEYUP:    commoveup(comode);                  break;
    case 'n': case KEYDOWN:  commovedown(comode);                break;
    case 'h': case KEYLEFT:  commoveleft(comode);                break;
    case 'j': case KEYRIGHT: commoveright(comode);               break;
    case           KEYHOME:  comdoprintpage(0,pagemovetop);      break;
    case           KEYEND:   comdoprintpage(9999, pagemovetop);  break;
    case           KEYRESIZE: comdoresize();                     break;

    case '?': 
      comhelp(comode); 
      if(comode==COMDO) 
        comode=COMDEF; 
      else 
        comode=COMDO;
      break;

    case 127: case ';': case ',': case '.':  case '*': case '#': 
    case '0': case '1': case '2': case '3': case '4':  /* +/- not used here */
    case '5': case '6': case '7': case '8': case '9':
      if(comode==COMDEF) 
        ignorethis("<O>-<9>/<;>/<,>/<.>/<*>/<DEL> ENTER ARGUMENTS ");
      else {
        if(numberargadd(o)) numberarginit();
        numberargread();
        wasnum=1;
      }
      break;
    case ' ': 
      if(comode==COMDEF)
        ignorethis("HIT THE SPACE-BAR AGAIN TO KICK OFF THIS MESSAGE");
      else
        if(statusforce==1) drawstatusline();
      break;
    case KEYESC: 
      if(comode==COMDEF)
        ignorethis("<ESC> ESCAPE FROM SOME DIALOGS ");
      else
        if(statusforce==1)drawstatusline();
      break;
    case KEYRET:
      if(comode==COMDEF)
        ignorethis("<TAB> MOVE TO THE NEXT HREF");
      else {
        if(statusforce==1) drawstatusline();
        if(comode==COMDO) comdohypfollow();
      }
      break;
    case KEYTAB:
      comhypnext(comode);
      break;
    case 'q': 
      if(comode==COMDEF)
        ignorethis("<Q> QUIT TMVIEW ");
      else
        o='Q';
      break;
    default:
      if(comode==COMDEF && o!= KEYNOP)
        ignorethis("THE PREVIOUS PRESSED KEY PERFORMS ALMOST NO OPERATION");
      o=KEYNOP;
    }                    /* end switch o */
#ifdef HASMOUSE          /* now mouse actions ... */
    if(!dvierrorflag){   /* if not in error */
    vgagetmouse(&mousex,&mousey,&mouseleft,&mouseright);
#ifndef HASWINDOWS
    if(markon || hypon)
      vgasetmouse(MAX(0,MIN(vgaxdim-1,mousex)),MAX(0,MIN(vgaydim-1,mousey)));
    else 
      vgasetmouse(vgaxdim/2,vgaydim/2);
#else
    if(!(markon || hypon) && (mouseleft || mouseright)) {
      vgasetmouse(vgaxdim/2,vgaydim/2);
      ox=vgaxdim/2;
      oy=vgaydim/2;
    }
    if(!(markon || hypon) && ((mouseleft && !ol) || (mouseright && !or))) {
      mousex=vgaxdim/2;
      mousey=vgaydim/2;
    }
#endif 
    if(markon) {                                          /* markon */
      if(!mouseright) {
        mousex=MAX(0,MIN(vgaxdim-1,mousex));
        mousey=MAX(0,MIN(vgaydim-1,mousey));
      }
      comdomarks(mousex,mousey);   
      /* ifnowin setmouse ? */
      if(mouseright) 
        comrect(COMDO); 
      if(mouseleft)              
        comdomarkfollow();
    } else { if(hypon) {                                  /* hypon */
      if(!mouseright) {
        mousex=MAX(0,MIN(vgaxdim-1,mousex));
        mousey=MAX(0,MIN(vgaydim-1,mousey));
      }
      comdomarks(mousex,mousey); 
      if(mouseleft && !ol) 
        comdohypfollow();
    } else {                                              /* alloff */
      if(mouseleft) {
        if(mousex>ox) 
          comdomoveright(mousex-ox);
        if(mousex<ox) 
          comdomoveleft(ox-mousex);
      }
      if(mouseright) {
        if(mousey>oy) 
          comdomovedown(mousey-oy,moveoverpages);
        if(mousey<oy) 
          comdomoveup(oy-mousey,moveoverpages); 
      }
    }}                                         /* end mouse */
#ifndef HASWINDOWS 
    if(markon || hypon) {
      ox=MAX(0,MIN(vgaxdim-1,mousex));
      oy=MAX(0,MIN(vgaydim-1,mousey));
    } else {
      ox=vgaxdim/2;
      oy=vgaydim/2;
    }
#else
    if(!(markon || hypon)) {
      ox=vgaxdim/2;
      oy=vgaydim/2;
    }
#endif 
    ol=mouseleft;
    or=mouseright;
    }                                        /* end if !dvieerorflag */
#endif                                       /* end if HASMOUSE */
    if(o!='z'    && o!=KEYNOP) centhis=0;
    if(o!='?'    && o!=KEYNOP) comode=COMDO;
    if(wasnum==0 && o!=KEYNOP) numberarginit();
    if(wasnum==1 || (o!=KEYNOP && statusforce==0)) drawstatusarg();  
  } while(o != 'Q');
   
  pfprot("\n"); 
  vgaclose(); 
  updatefilemk();
  killdvi();
  killfontdatabase();
 
  confwrite();
  filemkskill();
  bookmkskill(&backbmks);
  freemem(&tfmprefix);
  freemem(&fontprefix);
  freemem(&filename);
  freemem(&fontformat);
  freemem(&numberargstr);
  freemem(&bit_masks);
  freemem(&bit_count);
  freemem(&filename);
  freemem(&workdir);
  exit(0);
}

 











