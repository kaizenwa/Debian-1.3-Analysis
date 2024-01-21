/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */



#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>


#include "defs.h"
#include "globals.h"
#include "subs.h"
#include "bookmks.h"
#include "rwconf.h"


/* #define DEBUGCON */

void conferror(char* mess) {
  pfprot("\nfatal error: startupfile %s: %s\n",startupfilename,mess);
  exit(1);
}

FILE *conffile;

int confopen(int mode) {    /* 0=read 1=write*/
  char* bustr=NULL;
  char* homedir;
  
  homedir = getenv("HOME");
  if(startupfilename[0]=='~' && homedir!=NULL){
    stralloccpy(&bustr,startupfilename);
    freemem(&startupfilename);
    allocmem(&startupfilename,strlen(bustr)+strlen(homedir)+1);
    strcpy(startupfilename,homedir);
    strcat(startupfilename,bustr+1);
    freemem(&bustr);
  }
  conffile=NULL;
  if(mode==1)
    conffile=fopen(startupfilename,"w");
  if(mode==0)
    conffile=fopen(startupfilename,"r");
}

void confclose(void) {
  fclose(conffile);
}


void writebmk(bookmark* thebmk){
  fprintf(conffile,"%d ",thebmk->name);
  fprintf(conffile,"%d ",thebmk->pagenum);
  fprintf(conffile,"%d ",thebmk->dvixpos);
  fprintf(conffile,"%d ",thebmk->dviypos);
  fprintf(conffile,"%6.3f ",thebmk->fshrink);
}

void confwrite(void) { 
  int i,j;
  if(!savestartup) {  
    pfprot("\nwarning: writing startupfile disabled\n");
    return;
  }
  confopen(1);
  if(conffile==NULL) {
    pfprot("\nwarning: cant write startupfile %s\n",startupfilename);
    return;
  }
  pfprot("\nwriting startupfile %s ...",startupfilename);
  fprintf(conffile,"# **** this is a startupfile of tmview *****\n\n");
  fprintf(conffile,"\n#save the startupfile ?\n");
  fprintf(conffile,"sufp \"%s\"\n",startupfilename);
  fprintf(conffile,"#be verbose ?\n");
  fprintf(conffile,"verb %d\n",verbose);
  fprintf(conffile,"\n\n#commandline options\n");
  fprintf(conffile,"hoff %6.2f\nvoff %6.2f\n",fmkstar.hoffmm,fmkstar.voffmm);
  fprintf(conffile,"papx %6.2f\npapy %6.2f\n",fmkstar.papxmm,fmkstar.papymm);
  fprintf(conffile,"xres %d\nyres %d\n",xres,yres);
  fprintf(conffile,"tfmp \"%s\"\n",tfmprefix);
  fprintf(conffile,"ffor \"%s\"\n",fontformat);
  fprintf(conffile,"fntp \"%s\"\n",fontprefix);
  fprintf(conffile,"disx %d\ndisy %d\n",truevgaxdim,truevgaydim);
  fprintf(conffile,"nmag %d\n",newmag);
  fprintf(conffile,"lrnd %6.2f\nrrnd %6.2f\nornd %6.2f\nurnd %6.2f\n",
          fmkstar.lrandmm,fmkstar.rrandmm,fmkstar.orandmm,fmkstar.urandmm);
  fprintf(conffile,"\n#other defaults\n");
  fprintf(conffile,"fshk %5.3f\n",fshrink);
  fprintf(conffile,"ison %d\n",ishrinkonly);
  fprintf(conffile,"colo %d\n",colors);
  fprintf(conffile,"mkon %d\n",markon);
  fprintf(conffile,"hyon %d\n",hypon);
  fprintf(conffile,"unit %d\n",unitstar);
  fprintf(conffile,"mvpt %d\n",pagemovetop);
  fprintf(conffile,"mvop %d\n",moveoverpages);
 
  fprintf(conffile,"\n#filemarks and bookmarks\n\n");
  for(i=0;i<nfilemks;i++) {
     rollupfilemk();
     fprintf(conffile,"fmk ");
     fprintf(conffile,"\"%s\" ",curfmk->dvifilename);
     pfverb("\n writing fmk %s ",curfmk->dvifilename);
     fprintf(conffile,"%6.2f ",curfmk->papxmm);
     fprintf(conffile,"%6.2f ",curfmk->papymm);
     fprintf(conffile,"%6.2f ",curfmk->hoffmm);
     fprintf(conffile,"%6.2f ",curfmk->voffmm);
     fprintf(conffile,"%6.2f ",curfmk->centerxmm);
     fprintf(conffile,"%6.2f ",curfmk->centerymm);
     fprintf(conffile,"%6.2f ",curfmk->lrandmm);
     fprintf(conffile,"%6.2f ",curfmk->rrandmm);
     fprintf(conffile,"%6.2f ",curfmk->orandmm);
     fprintf(conffile,"%6.2f ",curfmk->urandmm);
     writebmk(&(curfmk->lastpos));
     fprintf(conffile,"\n");
     for(j=curbmks->n-1;j>=0;j--) {
       fprintf(conffile,"bmk ");
       writebmk(curbmks->d[j]);
       fprintf(conffile,"\n");
     }
     pfverb("and %d related manual-bmks",curbmks->n);
     fprintf(conffile,"\n\n");
  }
  fprintf(conffile,"\n# end of startup-file\n");
  confclose();
  pfverb("\n");
  pfprot("... done\n");
}


#define LEXNUM 256
#define LEXSTR 257
#define LEXNAM 258
#define LEXEOF 259

char*  lexstr;
int   nlexstr;
double lexnum;
int lexline, lexpos;
pagelistelement bupale;


void initlex(void) {
  nlexstr=1;
  allocmem(&lexstr,nlexstr+1);
  lexline=1;
  lexpos=0;
  cpage=&bupale;
}

void killlex(void) {
  freemem(&lexstr);
}

int mygetch(void) {
  int c;
  c=fgetc(conffile);
  if(c=='\n') {lexline++; lexpos=0;}
  lexpos++;
  return(c);
}

void myungetch(int ch){
  ungetc(ch,conffile);
} 
  

void lexerror(char* mes) {
  pfprot("\nfatal error in configfile (line %d pos %d): %s\n",
       lexline,lexpos,mes);
  exit(1);
}


int lex(void) {
  int c,i;

  c=mygetch();
  
  while(c=='#' || c==' ' || c=='\t' || c=='\n'){
    if(c==' ' || c=='\t' || c=='\n')   
      c=mygetch();
    if(c=='#') {
      do c=mygetch();
      while(c!=EOF && c != '\n');
    }
  }
  if (c == EOF) return LEXEOF;
  
  /* Char starts a number => parse the number.         */
  if(c == '.' || isdigit (c) || c=='-' || c=='+'){
    myungetch(c);
    fscanf(conffile,"%lf",&lexnum);
#ifdef DEBUGCON
    pfprot("*%f*",lexnum);
#endif    
    return LEXNUM;
  }

  /* Char starts an identifier => read the name.       */
  if(isalpha (c)) {
     i = 0;
     do{
       if(i >= nlexstr){
         nlexstr*= 2;
         reallocmem(&lexstr, nlexstr+ 1);
       }
       lexstr[i++] = c;
       c = mygetch();
     } while (c != EOF && isalnum (c));
     myungetch(c);
     lexstr[i] = '\0';
     return LEXNAM;
   }

   /* Char starts an string => read the string.       */
   if(c == '"') {
     i = 0;
     do{
       c=mygetch();
       if(c==EOF) break;
       if(c=='"'){
          c=mygetch();
          if(c!='"') break;
       }
       if(i >= nlexstr){
         nlexstr*= 2;
         reallocmem (&lexstr, nlexstr+ 1);
       }
       lexstr[i++] = c;
     } while(1);
     myungetch(c);
     lexstr[i] = '\0';
#ifdef DEBUGCON
     pfprot("*%s*",lexstr);
#endif    
     return LEXSTR;
   }
     
   /* Any other character is a token by itself.        */
   return c;
}

double readdouble(void){
  if(lex()!=LEXNUM) lexerror("float expected"); 
  return lexnum;
}  

int readint(void){
  if(lex()!=LEXNUM)          lexerror("no number while integer expected"); 
  if(lexnum!=LROUND(lexnum)) lexerror("float while integer expected");
  return LROUND(lexnum);
}  

void readstring(char** dst){
  if(lex()!=LEXSTR) lexerror("string expected");
  stralloccpy(dst,lexstr);
} 

void readbmk(bookmark* thebmk){
  thebmk->name=readint();
  thebmk->pagenum=readint();
  thebmk->dvixpos=readint();
  thebmk->dviypos=readint();
  thebmk->fshrink=readdouble();
} 

void confread(void) {
  int lexres;
  char* bustr=NULL;
  confopen(0);
  if(conffile==NULL) {
    pfprot("\nwarning: cant read startupfile %s\n",startupfilename);
    pfprot(  "         will write startupfile\n");
    savestartup=1;
    return;
  }
  pfprot("\nreading startupfile %s ... ",startupfilename);
  initlex();
  while((lexres=lex())!=LEXEOF) {
    if(lexres!=LEXNAM) lexerror("symbol expected");
    if(strcmp(lexstr,"fmk")==0) {
      readstring(&bustr);
      allocnewfilemk(bustr);
      pfverb("\n reading fmk %s ",curfmk->dvifilename);
      curfmk->papxmm=readdouble();
      curfmk->papymm=readdouble();
      curfmk->hoffmm=readdouble();
      curfmk->voffmm=readdouble();
      curfmk->centerxmm=readdouble();
      curfmk->centerymm=readdouble();
      curfmk->lrandmm=readdouble();
      curfmk->rrandmm=readdouble();
      curfmk->orandmm=readdouble();
      curfmk->urandmm=readdouble();
      readbmk(&(curfmk->lastpos));
      stralloccpy(&(curfmk->lastpos.dvifilename),curfmk->dvifilename);
      continue;
    }
    if(strcmp(lexstr,"bmk")==0) {
      if(curbmks==NULL) lexerror("no fmk set");
      pfverb(" bmk");
      allocnewbookmk(curbmks);
      readbmk(curbmks->d[0]);
      continue;
    }
    if(strcmp(lexstr,"sufp")==0) {
      readstring(&startupfilename);
      pfprot("\nwarning: found ""sufp"" %s\n",startupfilename);
      pfprot(  "         will write startupfile\n");
      savestartup=1;
      continue;
    }
    pfverb(" symbol %s\n",lexstr);
    if(strcmp(lexstr,"hoff")==0) {
      fmkstar.hoffmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"voff")==0) {
      fmkstar.voffmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"voff")==0) {
      fmkstar.voffmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"papx")==0) {
      fmkstar.papxmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"papy")==0) {
      fmkstar.papymm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"xres")==0) {
      xres=readint();
      continue;
    }
    if(strcmp(lexstr,"yres")==0) {
      yres=readint();
      continue;
    }
    if(strcmp(lexstr,"tfmp")==0) {
      readstring(&tfmprefix);
      continue;
    }
    if(strcmp(lexstr,"ffor")==0) {
      readstring(&fontformat);
      continue;
    }
    if(strcmp(lexstr,"fntp")==0) {
      readstring(&fontprefix);
      continue;
    }
    if(strcmp(lexstr,"tfmp")==0) {
      readstring(&tfmprefix);
      continue;
    }
    if(strcmp(lexstr,"disx")==0) {
      vgaxdim=readint();
      continue;
    }
    if(strcmp(lexstr,"disy")==0) {
      vgaydim=readint();
      continue;
    }
    if(strcmp(lexstr,"nmag")==0) {
      newmag=readint();
      continue;
    }
    if(strcmp(lexstr,"lrnd")==0) {
      fmkstar.lrandmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"rrnd")==0) {
      fmkstar.rrandmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"ornd")==0) {
      fmkstar.orandmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"urnd")==0) {
      fmkstar.urandmm=readdouble();
      continue;
    }
    if(strcmp(lexstr,"ison")==0) {
      ishrinkonly=readint();
      continue;
    }
    if(strcmp(lexstr,"fshk")==0) {
      fshrink=readdouble();
      continue;
    }
    if(strcmp(lexstr,"verb")==0) {
      verbose=readint();
      continue;
    }
   if(strcmp(lexstr,"colo")==0) {
      colors=readint();
      continue;
    }
   if(strcmp(lexstr,"mkon")==0) {
      markon=readint();
      continue;
    }
   if(strcmp(lexstr,"hyon")==0) {
      hypon=readint();
      continue;
    }
   if(strcmp(lexstr,"unit")==0) {
      unitstar=readint();
      continue;
    }
    if(strcmp(lexstr,"mvpt")==0) {
      pagemovetop=readint();
      continue;
    }
    if(strcmp(lexstr,"mvop")==0) {
      moveoverpages=readint();
      continue;
    }
    lexerror("undefined symbol");
  }
  killlex();
  freemem(&bustr);
  confclose();
  pfverb("\n");
  pfprot(" ... done\n");
}


