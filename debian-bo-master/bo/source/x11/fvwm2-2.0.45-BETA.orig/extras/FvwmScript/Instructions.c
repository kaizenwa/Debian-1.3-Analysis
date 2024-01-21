#include "FvwmScript.h"

extern int fd[2];
extern Window ref;

void (*TabCom[30]) (int NbArg,long *TabArg);
char *(*TabFunc[10]) (int *NbArg, long *TabArg);
int (*TabComp[15]) (char *arg1,char *arg2);

extern X11base *x11base;
extern int grab_serve;
extern struct XObj *tabxobj[30];
extern void LoadIcon(struct XObj *xobj);

extern int nbobj;
extern char **TabVVar;
extern int TabIdObj[31];
extern char *ScriptName;

char *BufCom;
char Command[50]="None";
time_t TimeCom=0;

/*************************************************************/
/* Ensemble de fonction de comparaison de deux entiers       */
/*************************************************************/
int Inf(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (an1<an2);
}

int InfEq(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (an1<=an2);
}

int Equal(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (strcmp(arg1,arg2)==0);
}

int SupEq(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (an1>=an2);
}

int Sup(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (an1>an2);
}

int Diff(char *arg1,char *arg2)
{
 int an1,an2;
 an1=atoi(arg1);
 an2=atoi(arg2);
 return (strcmp(arg1,arg2)!=0);
}

/*****************************************************/
/* Fonction qui retourne la valeur d'un argument     */
/*****************************************************/
char *CalcArg (long *TabArg,int *Ix)
{
 char *TmpStr;

 if (TabArg[*Ix]>100000)	/* Cas du codage d'un nombre */
 {
  TmpStr=(char*)calloc(1,sizeof(char)*10);
  sprintf(TmpStr,"%d",TabArg[*Ix]-200000);
 }
 else if (TabArg[*Ix]<-200000)/* Cas d'un id de fonction de comparaison */
 {
  TmpStr=(char*)calloc(1,sizeof(char)*10);
  sprintf(TmpStr,"%d",TabArg[*Ix]+250000);
 }
 else if (TabArg[*Ix]< -100000)	/* Cas d'un id de fonction */
 {
  TmpStr=TabFunc[TabArg[*Ix]+150000](Ix,TabArg);
 }
 else				/* Cas d'une variable */
 {
  TmpStr=strdup(TabVVar[TabArg[*Ix]]);
 }
 return (TmpStr);
}

/*************************************************************/
/* Ensemble des fonctions pour recuperer les prop d'un objet */
/*************************************************************/
char *FuncGetValue(int *NbArg, long *TabArg)
{
 char *tmp;
 long Id;

 (*NbArg)++;			/* La fonction GetValue n'a qu'un seul argument */
 tmp=CalcArg(TabArg,NbArg);
 Id=atoi(tmp);
 free(tmp);
 tmp=(char*)calloc(1,sizeof(char)*10);
 sprintf(tmp,"%d",tabxobj[TabIdObj[Id]]->value);
 return tmp;
}

/* Fonction qui retourne le titre d'un objet */
char *FuncGetTitle(int *NbArg, long *TabArg)
{
 char *tmp;
 long Id;

 (*NbArg)++;
 tmp=CalcArg(TabArg,NbArg);
 Id=atoi(tmp);
 free(tmp);
 tmp=strdup(tabxobj[TabIdObj[Id]]->title);
 return tmp;
}

/* Fonction qui retourne la sortie d'une commande */
char *FuncGetOutput(int *NbArg, long *TabArg)
{
 char *cmndbuf;
 char *str;
 int line,index,i=2,j=0,k,NewWord;
 FILE *f;
 int maxsize=32000;
 int size;

 (*NbArg)++;		
 cmndbuf=CalcArg(TabArg,NbArg);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 line=atoi(str);
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 index=atoi(str);
 free(str);
 
 if ((strcmp(Command,cmndbuf))||((time(NULL)-TimeCom)>1)||(TimeCom==0))
 {
  if ((f = popen(cmndbuf,"r")) == NULL)
  {
   fprintf(stderr,"%s: can't run %s\n",ScriptName,cmndbuf);
   str=(char*)calloc(1,10);
   free(cmndbuf);
   return str;
  }
  else
  {
   if (strcmp(Command,"None"))
    free(BufCom);
   BufCom=(char*)calloc(1,maxsize);
   size=fread(BufCom,1,maxsize,f);
   pclose(f);
   strcpy(Command,cmndbuf);
   TimeCom=time(NULL);
  }
 }
 
 /* Recherche de la ligne */
 while ((i<=line)&&(BufCom[j]!='\0'))
 {
  j++;
  if (BufCom[j]=='\n') i++;
 }
 
 /* Recherche du mot */
 str=(char*)calloc(1,256);
 if (index!=-1)
 {
  if (i!=2) j++;
  i=1;
  NewWord=0;
  while ((i<index)&&(BufCom[j]!='\n')&&(BufCom[j]!='\0'))
  {
   j++;
   if (BufCom[j]==' ')
   {
    if (NewWord)
    {
     i++;
     NewWord=0;
    }
   }
   else
    if (!NewWord) NewWord=1;
  }
  sscanf(&BufCom[j],"%s",str);
 }
 else		/* Lecture de la ligne complete */
 {
  j++;
  k=j;
  while ((BufCom[k]!='\n')&&(BufCom[k]!='\0'))
   k++;
  memmove(str,&BufCom[j],k-j);
 }
  
 free(cmndbuf);
 return str;
}

/* Convertion decimal vers hexadecimal */
char *FuncNumToHex(int *NbArg, long *TabArg)
{
 char *str;
 int value,nbchar;
 int i,j;
 
 (*NbArg)++;		
 str=CalcArg(TabArg,NbArg);
 value=atoi(str);
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 nbchar=atoi(str);
 free(str);
 
 str=(char*)calloc(1,nbchar+10);
 sprintf(str,"%X",value);
 j=strlen(str);
 if (j<nbchar)
 {
  memmove(&str[nbchar-j],str,j);
  for (i=0;i<(nbchar-j);i++)
   str[i]='0';
 }

 return str;
}

/* Convertion hexadecimal vers decimal */
char *FuncHexToNum(int *NbArg, long *TabArg)
{
 char *str,*str2;
 long int k;
 
 (*NbArg)++;		
 str=CalcArg(TabArg,NbArg);
 if (str[0]=='#')
  memmove(str,&str[1],strlen(str));
 k=strtol(str,NULL,16);
 free(str);
 
 str2=(char*)calloc(1,20);
 sprintf(str2,"%d",k);
 return str2;
}

char *FuncAdd(int *NbArg, long *TabArg)
{
 char *str;
 int val1,val2;
 int i,j;
 
 (*NbArg)++;		
 str=CalcArg(TabArg,NbArg);
 val1=atoi(str);
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 val2=atoi(str);
 free(str);
 str=(char*)calloc(1,20);
 sprintf(str,"%d",val1+val2);
 return str;
}

char *FuncMult(int *NbArg, long *TabArg)
{
 char *str;
 int val1,val2;
 int i,j;
 
 (*NbArg)++;		
 str=CalcArg(TabArg,NbArg);
 val1=atoi(str);
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 val2=atoi(str);
 free(str);
 str=(char*)calloc(1,20);
 sprintf(str,"%d",val1*val2);
 return str;
}

char *FuncDiv(int *NbArg, long *TabArg)
{
 char *str;
 int val1,val2;
 int i,j;
 
 (*NbArg)++;		
 str=CalcArg(TabArg,NbArg);
 val1=atoi(str);
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 val2=atoi(str);
 free(str);
 str=(char*)calloc(1,20);
 sprintf(str,"%d",val1/val2);
 return str;
}

char *FuncStrCopy(int *NbArg, long *TabArg)
{
 char *str,*strsrc;
 int i1,i2;
 
 (*NbArg)++;		
 strsrc=CalcArg(TabArg,NbArg);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 i1=atoi(str);
 if (i1<1) i1=1;
 free(str);
 (*NbArg)++;
 str=CalcArg(TabArg,NbArg);
 i2=atoi(str);
 if (i2<1) i2=1;
 free(str);
 
 if ((i1<=i2)&&(i1<=strlen(strsrc)))
 {
  if (i2>strlen(strsrc)) i2=strlen(strsrc);
  str=(char*)calloc(1,i2-i1+2);
  memmove(str,&strsrc[i1-1],i2-i1+1);
 }
 else
 {
  str=(char*)calloc(1,1);
  printf("Erreur\n");
 }
 free(strsrc);
 return str;
}

/***********************************************/
/* Ensemble des commandes possible pour un obj */
/***********************************************/

void ComExec (int NbArg,long *TabArg)
{
 int leng;
 char *execstr;
 char *tempstr;
 char *temp2str;
 int i;
 int num;

 execstr=(char*)calloc(1,256);
 for (i=0;i<NbArg;i++)
 {
  tempstr=CalcArg(TabArg,&i);
  execstr=strcat(execstr,tempstr);
  free(tempstr);
 }
 
 write(fd[0], &ref, sizeof(Window));
 leng = strlen(execstr);
 write(fd[0], &leng, sizeof(int));
 write(fd[0], execstr, leng);
 leng = 1;
 write(fd[0], &leng, sizeof(int));
 
 
 free(execstr);
}

void ComHideObj (int NbArg,long *TabArg)
{
 char *arg[1];
 int IdItem; 
 int i=0;

 arg[0]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->state=Hiden;
 /* On cache la fentre pour la faire disparaitre */
 XUnmapWindow(x11base->display,tabxobj[IdItem]->ObjWin);
 free(arg[0]);
}

void ComShowObj (int NbArg,long *TabArg)
{
 char *arg[1];
 int IdItem; 
 int i=0;

 arg[0]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->state=Actif;
 /* On cache la fentre pour la faire disparaitre */
 XMapWindow(x11base->display,tabxobj[IdItem]->ObjWin);
 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
}

void ComChangeValue (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 
 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 
 tabxobj[TabIdObj[atoi(arg[0])]]->value=atoi(arg[1]);
 /* On redessine l'objet pour le mettre a jour */
 tabxobj[TabIdObj[atoi(arg[0])]]->DrawObj(tabxobj[TabIdObj[atoi(arg[0])]]);
 free(arg[0]);
 free(arg[1]);
}

void ComChangeValueMax (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int j;
 
 arg[0]=CalcArg(TabArg,&i);
 j=atoi(arg[0]);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 
 tabxobj[TabIdObj[j]]->value3=atoi(arg[1]);
 /* On redessine l'objet pour le mettre a jour */
 if (tabxobj[TabIdObj[j]]->value>tabxobj[TabIdObj[j]]->value3)
 {
  tabxobj[TabIdObj[j]]->value=atoi(arg[1]);
  tabxobj[TabIdObj[j]]->DrawObj(tabxobj[TabIdObj[j]]);
 }
 free(arg[0]);
 free(arg[1]);
}

void ComChangeValueMin (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int j;
 
 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 j=atoi(arg[0]);
 
 tabxobj[TabIdObj[j]]->value2=atoi(arg[1]);
 /* On redessine l'objet pour le mettre a jour */
 if (tabxobj[TabIdObj[j]]->value<tabxobj[TabIdObj[j]]->value2)
 {
  tabxobj[TabIdObj[j]]->value=atoi(arg[1]);
  tabxobj[TabIdObj[j]]->DrawObj(tabxobj[TabIdObj[j]]);
 }
 free(arg[0]);
 free(arg[1]);
}

void ComChangePos (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[3];
 int an[3]; 
 int IdItem;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 i++;
 arg[2]=CalcArg(TabArg,&i);

 IdItem= TabIdObj[atoi(arg[0])];
 for (i=1;i<3;i++)
  an[i]=atoi(arg[i]);
 tabxobj[IdItem]->x=an[1];
 tabxobj[IdItem]->y=an[2];
 XMoveWindow(x11base->display,tabxobj[IdItem]->ObjWin,an[1],an[2]);

 free(arg[0]);
 free(arg[1]);
 free(arg[2]);

}

void ComChangeFont (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;
 XColor TempColor;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->font=strdup(arg[1]);
 if ((tabxobj[IdItem]->xfont=XLoadQueryFont(tabxobj[IdItem]->display,tabxobj[IdItem]->font))==NULL)
  {
   fprintf(stderr,"Can't load font %s\n",tabxobj[IdItem]->font);
  }
 else
 {
  XSetFont(tabxobj[IdItem]->display,tabxobj[IdItem]->gc,tabxobj[IdItem]->xfont->fid);
  XFreeFont(tabxobj[IdItem]->display,tabxobj[IdItem]->xfont);
 }
 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
}

void ComChangeSize (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[3];
 int an[3]; 
 int IdItem;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 i++;
 arg[2]=CalcArg(TabArg,&i);

 IdItem= TabIdObj[atoi(arg[0])];
 for (i=1;i<3;i++)
  an[i]=atoi(arg[i]);
 tabxobj[IdItem]->width=an[1];
 tabxobj[IdItem]->height=an[2];
 XResizeWindow(x11base->display,tabxobj[IdItem]->ObjWin,an[1],an[2]);
 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
 free(arg[2]);
}

void ComChangeTitle (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->title=strdup(arg[1]);
 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
}

void ComChangeIcon (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];
/* if (tabxobj[IdItem]->icon!=NULL)
 {
  free(tabxobj[IdItem]->icon);
  if (tabxobj[IdItem]->iconPixmap!=None)
   XFreePixmap(tabxobj[IdItem]->display,tabxobj[IdItem]->iconPixmap);
  if (tabxobj[IdItem]->icon_maskPixmap!=None)
   XFreePixmap(tabxobj[IdItem]->display,tabxobj[IdItem]->icon_maskPixmap);
 }*/
 tabxobj[IdItem]->icon=strdup(arg[1]);
 LoadIcon(tabxobj[IdItem]);
 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
}

void ComChangeForeColor (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;
 XColor TempColor;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->forecolor=strdup(arg[1]);
 if (!XAllocNamedColor(tabxobj[IdItem]->display,*(tabxobj[IdItem])->colormap,
	tabxobj[IdItem]->forecolor,&(tabxobj[IdItem])->TabColor[fore],&TempColor)) 
   fprintf(stderr,"Can't alloc named color.\n");

 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
}

void ComChangeBackColor (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;
 XColor TempColor;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];

 tabxobj[IdItem]->backcolor=strdup(arg[1]);
 if (!XAllocNamedColor(tabxobj[IdItem]->display,*(tabxobj[IdItem])->colormap,
	tabxobj[IdItem]->backcolor,&(tabxobj[IdItem])->TabColor[back],&TempColor)) 
   fprintf(stderr,"Can't alloc named color.\n");

 tabxobj[IdItem]->DrawObj(tabxobj[IdItem]);
 free(arg[0]);
 free(arg[1]);
}

void ComSetVar (int NbArg,long *TabArg)
{
 int i;
 char *str,*tempstr;
 
 str=(char*)calloc(1,256);
 for (i=1;i<NbArg;i++)
 {
  tempstr=CalcArg(TabArg,&i);
  str=strcat(str,tempstr);
  free(tempstr);
 }

 free(TabVVar[TabArg[0]]);
 TabVVar[TabArg[0]]=str;
}

void ComSendSign (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2];
 int IdItem;
 int TypeMsg;

 arg[0]=CalcArg(TabArg,&i);
 i++;
 arg[1]=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg[0])];
 TypeMsg=atoi(arg[1]);
 SendMsg(tabxobj[IdItem],TypeMsg);
 free(arg[0]);
 free(arg[1]);
}

void WarpPointer(int NbArg,long *TabArg)
{
 int i=0;
 char *arg;
 int IdItem;
 int x,y;

 arg=CalcArg(TabArg,&i);
 IdItem= TabIdObj[atoi(arg)];
 /* Deplacement du pointeur sur l'objet */
 XWarpPointer(x11base->display,None,tabxobj[IdItem]->ObjWin,0,0,0,0,
	tabxobj[IdItem]->width/2,tabxobj[IdItem]->height+10);
 free(arg);
}

void ComSleep(int NbArg,long *TabArg)
{
 char *arg;
 struct timeval *tv;
 long tus,ts,d,s;
 int i=0;

 arg=CalcArg(TabArg,&i);
 s= atoi(arg);
 
 tv=(struct timeval*)calloc(1,sizeof(struct timeval));
 gettimeofday(tv,NULL);
 tus=tv->tv_usec;
 ts=tv->tv_sec;
 s=(long)(16667*s);
 do
 {
  gettimeofday(tv,NULL);
  d=(long)((long)(tv->tv_usec-tus)+(long)((long)(tv->tv_sec-ts)*(long)1000000));
  if (d<0)
  {
   free(tv);
   free(arg);
   return;
  }
 }
 while (d<s);
 free(tv);
 free(arg);
}

void ComQuit (int NbArg,long *TabArg)
{
 int i;

 /* On quitte proprement le serveur X */
 for (i=0;i<nbobj;i++)
  tabxobj[i]->DestroyObj(tabxobj[i]);
 XFreeFont(x11base->display,x11base->xfont);
 XFreeGC(x11base->display,x11base->gc);
 XFreeColormap(x11base->display,x11base->colormap);
 XDestroyWindow(x11base->display,x11base->win);
 XCloseDisplay(x11base->display);
 exit(0);
}

void ComIfThen (int NbArg,long *TabArg)
{
 Bloc *bloc1;
 Bloc *bloc2;
 char *arg[2];
 int i;
 int CurrArg=0;
 int IdFuncComp;

 /* Verification de la condition */
 for (i=0;i<NbArg-2;i++)
 {
  if (TabArg[i]>100000)	/* Cas du codage d'un nombre */
  {
   arg[CurrArg]=(char*)calloc(1,sizeof(char)*10);
   sprintf(arg[CurrArg],"%d",TabArg[i]-200000);
   CurrArg++;
  }
  else if (TabArg[i]<-200000)/* Cas d'un id de fonction de comparaison */
  {
   IdFuncComp=TabArg[i]+250000;
  }
  else if (TabArg[i]<-100000)	/* Cas d'un id de fonction */
  {
   arg[CurrArg]=TabFunc[TabArg[i]+150000](&i,TabArg);
   CurrArg++;
  }
  else				/* Cas d'une variable */
  {
    arg[CurrArg]=strdup(TabVVar[TabArg[i]]);
    CurrArg++;
  }
 }

 /* Comparaison des arguments */
 if (TabComp[IdFuncComp](arg[0],arg[1]))
  ExecBloc((Bloc*)TabArg[NbArg-2]);
 else if (TabArg[NbArg-1]!=0)
  ExecBloc((Bloc*)TabArg[NbArg-1]);

 free(arg[0]);
 free(arg[1]);
}

/* Instruction boucle */
void ComLoop (int NbArg,long *TabArg)
{
 Bloc *bloc;
 int IdVar;
 char *arg[2];
 int limit[2];
 int i;
 int CurrArg=0;

 /* le premier argument est une variable */
 IdVar=TabArg[0];
 /*On ajuste la taille de la var pour contenir un nombre */
 TabVVar[TabArg[0]]=(char*)realloc(TabVVar[TabArg[0]],sizeof(char)*10);
 /* Calcul des 2 autres arguments */
 for (i=1;i<NbArg-1;i++)
 {
  if (TabArg[i]>100000)	/* Cas du codage d'un nombre */
  {
   arg[CurrArg]=(char*)calloc(1,sizeof(char)*10);
   sprintf(arg[CurrArg],"%d",TabArg[i]-200000);
  }
  else if (TabArg[i]<-100000)	/* Cas d'un id de fonction */
  {
   arg[CurrArg]=TabFunc[TabArg[i]+150000](&i,TabArg);
  }
  else				/* Cas d'une variable */
    arg[CurrArg]=strdup(TabVVar[TabArg[i]]);
  CurrArg++;
 }
 limit[0]=atoi(arg[0]);
 limit[1]=atoi(arg[1]);
 if (limit[0]<limit[1])
  for (i=limit[0];i<=limit[1];i++)
  {
   /* On met a jour la variable */
   sprintf(TabVVar[TabArg[0]],"%d",i);
   ExecBloc((Bloc*)TabArg[NbArg-1]);
  }
 else
  for (i=limit[0];i<=limit[1];i++)
  {
   sprintf(TabVVar[TabArg[0]],"%d",i);
   ExecBloc((Bloc*)TabArg[NbArg-1]);
  }

 free(arg[0]);
 free(arg[1]);
}

/* Instruction While */
void ComWhile (int NbArg,long *TabArg)
{
 Bloc *bloc;
 char *arg[3],*str;
 int i;
 int Loop=1;
 int IdFuncComp;

 while (Loop)
 {
  i=0;
  arg[0]=CalcArg(TabArg,&i);
  i++;
  str=CalcArg(TabArg,&i);
  IdFuncComp=atoi(str);
  free(str);
  i++;
  arg[1]=CalcArg(TabArg,&i);

  Loop=TabComp[IdFuncComp](arg[0],arg[1]);
  if (Loop) ExecBloc((Bloc*)TabArg[NbArg-1]);
  free(arg[0]);
  free(arg[1]);
 }
}

void WriteToFile (int NbArg,long *TabArg)
{
 int i=0;
 char *arg[2],str[50],*tempstr;
 FILE *f;
 char StrBegin[50];
 char StrEnd[5];
 size_t  size;
 char *buf;
 int maxsize=32000;
 int CurrPos=0,CurrPos2;
 int OldPID;

 arg[0]=CalcArg(TabArg,&i);
 arg[1]=(char*)calloc(1,256);
 for (i=1;i<NbArg;i++)
 {
  tempstr=CalcArg(TabArg,&i);
  arg[1]=strcat(arg[1],tempstr);
  free(tempstr);
 }
 if (arg[1][strlen(arg[1])-1]!='\n')
 {
  i=strlen(arg[1]);
  arg[1]=(char*)realloc(arg[1],strlen(arg[1])+1);
  arg[1][i]='\n';
  arg[1][i+1]='\0';
 }
  
 sprintf(StrEnd,"#end\n");
 sprintf(StrBegin,"#%s,",ScriptName);
 
 buf=(char*)calloc(1,maxsize);
 f=fopen(arg[0],"a+");
 fseek(f,0,SEEK_SET);
 size=fread(buf,1,maxsize,f);
 while(((strncmp(StrBegin,&buf[CurrPos],strlen(StrBegin)))!=0)&&(CurrPos<size))
  CurrPos++;
 if (CurrPos==size)
 {
  sprintf(buf,"%s\n%s%d\n",buf,StrBegin,getpid());
  sprintf(buf,"%s%s",buf,arg[1]);
  sprintf(buf,"%s%s\n",buf,StrEnd);
 }
 else
 {
  sscanf(&buf[CurrPos+strlen(StrBegin)],"%d",&OldPID);
  if (OldPID==getpid())
  {
   sprintf(str,"%d\n",OldPID);
   while(((strncmp(StrEnd,&buf[CurrPos],strlen(StrEnd)))!=0)&&(CurrPos<size))
    CurrPos++;
   memmove(&buf[CurrPos+strlen(arg[1])],&buf[CurrPos],strlen(buf)-CurrPos);
   memmove(&buf[CurrPos],arg[1],strlen(arg[1]));
  }
  else				/* Remplacement des anciennes commandes */
  {
   CurrPos=CurrPos+strlen(StrBegin);
   CurrPos2=CurrPos;
   while(((strncmp(StrEnd,&buf[CurrPos2],strlen(StrEnd)))!=0)&&(CurrPos2<size))
    CurrPos2++;
   sprintf(str,"%d\n%s",getpid(),arg[1]);
   memmove(&buf[CurrPos+strlen(str)],&buf[CurrPos2],strlen(buf)-CurrPos2);
   buf[strlen(buf)-((CurrPos2-CurrPos)-strlen(str))]='\0';
   memmove(&buf[CurrPos],str,strlen(str));
  }
 }
 
 fclose(f);
 f=fopen(arg[0],"w");
 fwrite(buf,1,strlen(buf),f);
 fclose(f);
 
 free(arg[0]);
 free(arg[1]);
}

/****************************************************/
/* Fonction d'initialisation de TabCom et TabFunc   */
/****************************************************/
void InitCom()
{
 /* commande */
 TabCom[1]=ComExec;
 TabCom[2]=ComHideObj;
 TabCom[3]=ComShowObj;
 TabCom[4]=ComChangeValue;
 TabCom[5]=ComChangePos;
 TabCom[6]=ComChangeSize;
 TabCom[7]=ComChangeIcon;
 TabCom[8]=ComChangeTitle;
 TabCom[9]=ComChangeFont;
 TabCom[10]=ComChangeForeColor;
 TabCom[11]=ComSetVar;
 TabCom[12]=ComSendSign;
 TabCom[13]=ComQuit;
 TabCom[14]=ComIfThen;
 TabCom[15]=ComLoop;
 TabCom[16]=ComWhile;
 TabCom[17]=WarpPointer;
 TabCom[18]=WriteToFile;
 TabCom[19]=ComChangeBackColor;
 TabCom[20]=ComSleep;
 TabCom[21]=ComChangeValueMax;
 TabCom[22]=ComChangeValueMin;

 /* Fonction */
 TabFunc[1]=FuncGetValue;
 TabFunc[2]=FuncGetTitle;
 TabFunc[3]=FuncGetOutput;
 TabFunc[4]=FuncNumToHex;
 TabFunc[5]=FuncHexToNum;
 TabFunc[6]=FuncAdd;
 TabFunc[7]=FuncMult;
 TabFunc[8]=FuncDiv;
 TabFunc[9]=FuncStrCopy;
 
 /* Fonction de comparaison */
 TabComp[1]=Inf;
 TabComp[2]=InfEq;
 TabComp[3]=Equal;
 TabComp[4]=SupEq;
 TabComp[5]=Sup;
 TabComp[6]=Diff;

}











































