
/*  A Bison parser, made from source/bisonin with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	STR	258
#define	GSTR	259
#define	VAR	260
#define	NUMBER	261
#define	WINDOWTITLE	262
#define	WINDOWSIZE	263
#define	WINDOWPOSITION	264
#define	FONT	265
#define	FORECOLOR	266
#define	BACKCOLOR	267
#define	SHADCOLOR	268
#define	LICOLOR	269
#define	OBJECT	270
#define	INIT	271
#define	MAIN	272
#define	END	273
#define	PROP	274
#define	TYPE	275
#define	SIZE	276
#define	POSITION	277
#define	VALUE	278
#define	VALUEMIN	279
#define	VALUEMAX	280
#define	TITLE	281
#define	SWALLOWEXEC	282
#define	ICON	283
#define	STATE	284
#define	WARP	285
#define	SLEEP	286
#define	WRITETOFILE	287
#define	HIDEN	288
#define	INACTIF	289
#define	ACTIF	290
#define	CASE	291
#define	CLIC	292
#define	BEG	293
#define	POINT	294
#define	EXEC	295
#define	HIDE	296
#define	SHOW	297
#define	CHFORECOLOR	298
#define	CHBACKCOLOR	299
#define	GETVALUE	300
#define	CHVALUE	301
#define	CHVALUEMAX	302
#define	CHVALUEMIN	303
#define	ADD	304
#define	DIV	305
#define	MULT	306
#define	GETTITLE	307
#define	GETOUTPUT	308
#define	STRCOPY	309
#define	NUMTOHEX	310
#define	HEXTONUM	311
#define	QUIT	312
#define	GET	313
#define	SET	314
#define	SENDSIGN	315
#define	IF	316
#define	THEN	317
#define	ELSE	318
#define	FOR	319
#define	TO	320
#define	DO	321
#define	WHILE	322
#define	BEGF	323
#define	ENDF	324
#define	EQUAL	325
#define	INFEQ	326
#define	SUPEQ	327
#define	INF	328
#define	SUP	329
#define	DIFF	330

#line 1 "source/bisonin"

#include "global.h"
#include <stdio.h>

extern int numligne;
ScriptProp *scriptprop;
int nbobj=-1;			/* Nombre d'objets */
int HasPosition,HasSize,HasType=0;
TabObj *tabobj;		/* Tableau d'objets, limite=30 */
int TabIdObj[31]; 	/* Tableau d'indice des objets */
Bloc **TabIObj;		/* TabIObj[Obj][Case] -> bloc attache au case */
Bloc *PileBloc[10];	/* Au maximum 10 imbrications de boucle conditionnelle */
int TopPileB=0;		/* Sommet de la pile des blocs */
CaseObj *TabCObj;	/* Struct pour enregistrer les valeurs des cases et leur nb */
int CurrCase;
int i;
char **TabNVar;		/* Tableau des noms de variables */
char **TabVVar;		/* Tableau des valeurs de variables */
int NbVar;
long BuffArg[6][20];	/* Les arguments s'ajoute par couche pour chaque fonction imbriquee */
int NbArg[6];		/* Tableau: nb d'args pour chaque couche */
int SPileArg;		/* Taille de la pile d'arguments */
long l;

/* Initialisation globale */
void InitVarGlob()
{
 scriptprop=(ScriptProp*) calloc(1,sizeof(ScriptProp));
 scriptprop->x=-1;
 scriptprop->y=-1;
 scriptprop->initbloc=NULL;

 tabobj=(TabObj*) calloc(1,sizeof(TabObj));
 for (i=0;i<30;i++) 
  (*tabobj)[i].state=Actif;
 for (i=0;i<31;i++)
  TabIdObj[i]=-1;
 TabNVar=NULL;
 TabVVar=NULL;
 NbVar=-1;

 SPileArg=-1;
}

/* Initialisation pour un objet */
void InitObjTabCase(int HasMainLoop)
{
 if (nbobj==0)
 {
  TabIObj=(Bloc**)calloc(1,sizeof(long));
  TabCObj=(CaseObj*)calloc(1,sizeof(CaseObj));
 }
 else
 {
  TabIObj=(Bloc**)realloc(TabIObj,sizeof(long)*(nbobj+1));
  TabCObj=(CaseObj*)realloc(TabCObj,sizeof(CaseObj)*(nbobj+1));
 }

 if (!HasMainLoop)
  TabIObj[nbobj]=NULL;
 CurrCase=-1;
 TabCObj[nbobj].NbCase=-1;
}

/* Ajout d'un case dans la table TabCase */
/* Initialisation d'un case of: agrandissement de la table */
void InitCase(int cond)
{
 CurrCase++;

 /* On enregistre la condition du case */
 TabCObj[nbobj].NbCase++;
 if (TabCObj[nbobj].NbCase==0)
  TabCObj[nbobj].LstCase=(int*)malloc(1,sizeof(int));
 else
  TabCObj[nbobj].LstCase=(int*)realloc(TabCObj[nbobj].LstCase,sizeof(int)*CurrCase+1);
 TabCObj[nbobj].LstCase[CurrCase]=cond;

 if (CurrCase==0)
  TabIObj[nbobj]=(Bloc*)calloc(1,sizeof(Bloc));
 else
  TabIObj[nbobj]=(Bloc*)realloc(TabIObj[nbobj],sizeof(Bloc)*(CurrCase+1));

 TabIObj[nbobj][CurrCase].NbInstr=-1;
 TabIObj[nbobj][CurrCase].TabInstr=NULL;

 /* Ce case correspond au bloc courant d'instruction: on l'empile */
 PileBloc[0]=&TabIObj[nbobj][CurrCase];
 TopPileB=0; 
}

/* Enleve un niveau d'args dans la pile BuffArg */
void RmLevelBufArg()
{
  SPileArg--;
}

/* Fonction de concatenation des n derniers etage de la pile */
/* Retourne les elts trie et depile et la taille */
long *Depile(int NbLevelArg, int *s)
{
 long *Temp;
 int j;
 int i;
 int size;

 if (NbLevelArg>0)
 {
  Temp=(long*)calloc(1,sizeof(long));
  size=0;
  for (i=SPileArg-NbLevelArg+1;i<=SPileArg;i++)
  {
   size=NbArg[i]+size+1;
   Temp=(long*)realloc (Temp,sizeof(long)*size);
   for (j=0;j<=NbArg[i];j++)
   {
    Temp[j+size-NbArg[i]-1]=BuffArg[i][j];
   }
  }
  *s=size;
  for (i=0;i<NbLevelArg;i++)	/* On depile les couches d'arguments */
   RmLevelBufArg();
  return Temp;
 }
 else
 {
  return NULL;
  *s=0;
 }
}

/* Ajout d'une commande */
void AddCom(int Type, int NbLevelArg)
{
 int CurrInstr;
 int i;
 int size;
 int j;


 PileBloc[TopPileB]->NbInstr++;
 CurrInstr=PileBloc[TopPileB]->NbInstr;

 if (CurrInstr==0)
  PileBloc[TopPileB]->TabInstr=(Instr*)calloc(1,sizeof(Instr)*(CurrInstr+1));
 else
  PileBloc[TopPileB]->TabInstr=(Instr*)realloc(PileBloc[TopPileB]->TabInstr,
				sizeof(Instr)*(CurrInstr+1));
 /* Rangement des instructions dans le bloc */
 PileBloc[TopPileB]->TabInstr[CurrInstr].Type=Type;
 /* On enleve la derniere couche d'argument et on la range dans la commande */

 PileBloc[TopPileB]->TabInstr[CurrInstr].TabArg=Depile(NbLevelArg,
		&PileBloc[TopPileB]->TabInstr[CurrInstr].NbArg);
}

/* Initialisation du buffer contenant les arguments de la commande courante */
/* Ajout d'une couche d'argument dans la pile*/
void AddLevelBufArg()
{
 /* Agrandissment de la pile */
 SPileArg++;
 NbArg[SPileArg]=-1;
}

/* Ajout d'un arg dans la couche arg qui est au sommet de la pile TabArg */ 
void AddBufArg(long *TabLong,int NbLong)
{
 int i;

 for (i=0;i<NbLong;i++)
 {
  BuffArg[SPileArg][i+NbArg[SPileArg]+1]=TabLong[i];
 }
 NbArg[SPileArg]=NbArg[SPileArg]+NbLong;
}

/* Recheche d'un nom de var dans TabVar, s'il n'existe pas il le cree */
/* Retourne un Id */
void AddVar(char *Name)		/* ajout de variable a la fin de la derniere commande pointee */
{
 int i;

 /* Comparaison avec les variables deja existante */
 for (i=0;i<=NbVar;i++)
  if (strcmp(TabNVar[i],Name)==0)
  {
   l=(long)i;
   AddBufArg(&l,1);
   return ;
  }

 if (NbVar>58) 	
 {
  fprintf(stderr,"Line %d: too many variables (>60)\n",numligne);
  exit(1);
 }

 /* La variable n'a pas ete trouvee: creation */
 NbVar++;

 if (NbVar==0)
 {
  TabNVar=(char**)calloc(1,sizeof(long));
  TabVVar=(char**)calloc(1,sizeof(long));
 }
 else
 {
  TabNVar=(char**)realloc(TabNVar,sizeof(long)*(NbVar+1));
  TabVVar=(char**)realloc(TabVVar,sizeof(long)*(NbVar+1));
 }

 TabNVar[NbVar]=(char*)strdup(Name);
 TabVVar[NbVar]=(char*)calloc(1,sizeof(char));
 TabVVar[NbVar][0]='\0';


 /* Ajout de la variable dans le buffer Arg */
 l=(long)NbVar;
 AddBufArg(&l,1);
 return ;
}

/* Ajout d'une constante str comme argument */
void AddConstStr(char *Name)	
{
 /* On cree une nouvelle variable et on range la constante dedans */
 NbVar++;
 if (NbVar==0)
 {
  TabVVar=(char**)calloc(1,sizeof(long));
  TabNVar=(char**)calloc(1,sizeof(long));
 }
 else
 {
  TabVVar=(char**)realloc(TabVVar,sizeof(long)*(NbVar+1));
  TabNVar=(char**)realloc(TabNVar,sizeof(long)*(NbVar+1));
 }

 TabNVar[NbVar]=(char*)calloc(1,sizeof(char));
 TabNVar[NbVar][0]='\0';
 TabVVar[NbVar]=(char*)strdup(Name);

 /* Ajout de l'id de la constante dans la liste courante des arguments */
 l=(long)NbVar;
 AddBufArg(&l,1);
}

/* Ajout d'une constante numerique comme argument */
void AddConstNum(long num)	
{

 /* On ne cree pas de nouvelle variable */
 /* On code la valeur numerique afin de le ranger sous forme d'id */
 l=num+200000;
 /* Ajout de la constante dans la liste courante des arguments */
 AddBufArg(&l,1);
}

/* Ajout d'une fonction comme argument */
/* Enleve les args de func de la pile, */
/* le concate, et les range dans la pile */
void AddFunct(int code,int NbLevelArg)	
{
 int size;
 long *l;
 int i;

 /* Methode: depiler BuffArg et completer le niveau inferieur de BuffArg */
 l=Depile(NbLevelArg, &size);

 size++;
 if (size==1)
  l=(long*)calloc(1,sizeof(long));
 else
 {
  l=(long*)realloc(l,sizeof(long)*(size));
  for (i=size-2;i>-1;i--)	/* Deplacement des args */
  {
   l[i+1]=l[i];
  }
 }
 l[0]=(long)code-150000;

 AddBufArg(l,size);
}

/* Ajout d'une instruction de test pour executer un ou plusieurs blocs */
/* enregistre l'instruction et le champs de ces blocs = NULL */
void AddComBloc(int TypeCond, int NbLevelArg, int NbBloc)
{
 int size;	/* Taille des args */
 long *l;
 int i;
 int OldNA;
 int CurrInstr;

 /* Ajout de l'instruction de teste comme d'une commande */
 AddCom(TypeCond, NbLevelArg);

 /* On initialise ensuite les deux champs reserve à bloc1 et bloc2 */
 CurrInstr=PileBloc[TopPileB]->NbInstr;
 /* Attention NbArg peur changer si on utilise en arg une fonction */
 OldNA=PileBloc[TopPileB]->TabInstr[CurrInstr].NbArg;

 PileBloc[TopPileB]->TabInstr[CurrInstr].TabArg=(long*)realloc( 
		PileBloc[TopPileB]->TabInstr[CurrInstr].TabArg,sizeof(long)*(OldNA+NbBloc));
 for (i=0;i<NbBloc;i++)
 {
  PileBloc[TopPileB]->TabInstr[CurrInstr].TabArg[OldNA+i]=0;
 }
 PileBloc[TopPileB]->TabInstr[CurrInstr].NbArg=OldNA+NbBloc;
}

/* Creer un nouveau bloc, et l'empile: il devient le bloc courant */
void EmpilerBloc()
{
 Bloc *TmpBloc;

 TmpBloc=(Bloc*)calloc(1,sizeof(Bloc));
 TmpBloc->NbInstr=-1;
 TmpBloc->TabInstr=NULL;
 TopPileB++; 
 PileBloc[TopPileB]=TmpBloc;

}

/* Depile le bloc d'initialisation du script et le range a sa place speciale */
void DepilerBloc(int IdBloc)
{
 Bloc *Bloc1;
 Instr *IfInstr;

 Bloc1=PileBloc[TopPileB];
 TopPileB--; 
 IfInstr=&PileBloc[TopPileB]->TabInstr[PileBloc[TopPileB]->NbInstr];
 IfInstr->TabArg[IfInstr->NbArg-IdBloc]=(long)Bloc1;
}


#line 345 "source/bisonin"
typedef union {  char *str;
          int number;
       } YYSTYPE;

#ifndef YYLTYPE
typedef
  struct yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  yyltype;

#define YYLTYPE yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		374
#define	YYFLAG		-32768
#define	YYNTBASE	76

#define YYTRANSLATE(x) ((unsigned)(x) <= 330 ? yytranslate[x] : 135)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
    16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
    66,    67,    68,    69,    70,    71,    72,    73,    74,    75
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     5,     6,     7,    11,    15,    20,    25,    29,    33,
    37,    41,    45,    46,    52,    53,    61,    63,    64,    68,
    73,    78,    82,    86,    90,    94,    98,   102,   106,   110,
   114,   118,   122,   126,   128,   130,   132,   133,   135,   141,
   142,   143,   148,   153,   155,   157,   161,   162,   166,   170,
   174,   178,   182,   186,   190,   194,   198,   202,   206,   210,
   214,   218,   222,   226,   230,   234,   238,   242,   246,   250,
   253,   256,   259,   264,   269,   274,   281,   288,   293,   298,
   303,   308,   313,   319,   324,   325,   328,   331,   336,   341,
   345,   349,   357,   358,   362,   363,   367,   371,   381,   389,
   391,   393,   395,   397,   399,   400,   403,   406,   411,   415,
   418,   422,   426,   430,   435,   436,   439,   442,   445,   448,
   451,   457,   459,   461,   463,   465,   467,   472,   474,   476,
   478,   483,   485,   487,   492,   494,   496,   501,   503,   505,
   507,   509,   511,   513
};

static const short yyrhs[] = {    77,
    78,    79,    80,     0,     0,     0,     7,     4,    78,     0,
    28,     3,    78,     0,     9,     6,     6,    78,     0,     8,
     6,     6,    78,     0,    12,     4,    78,     0,    11,     4,
    78,     0,    13,     4,    78,     0,    14,     4,    78,     0,
    10,     3,    78,     0,     0,    16,   116,    38,    91,    18,
     0,     0,    15,    81,    19,    82,    84,    85,    80,     0,
     6,     0,     0,    20,     3,    82,     0,    21,     6,     6,
    82,     0,    22,     6,     6,    82,     0,    23,     6,    82,
     0,    24,     6,    82,     0,    25,     6,    82,     0,    26,
     4,    82,     0,    27,     4,    82,     0,    28,     3,    82,
     0,    12,     4,    82,     0,    11,     4,    82,     0,    13,
     4,    82,     0,    14,     4,    82,     0,    10,     3,    82,
     0,    29,    83,    82,     0,    33,     0,    34,     0,    35,
     0,     0,    18,     0,    17,    86,    36,    87,    18,     0,
     0,     0,    88,    39,    90,    87,     0,    89,    39,    90,
    87,     0,    37,     0,     6,     0,    38,    91,    18,     0,
     0,    40,    92,    91,     0,    30,   108,    91,     0,    31,
   109,    91,     0,    32,   110,    91,     0,    41,    93,    91,
     0,    42,    94,    91,     0,    46,    95,    91,     0,    47,
    96,    91,     0,    48,    97,    91,     0,    22,    98,    91,
     0,    21,    99,    91,     0,    26,   101,    91,     0,    28,
   100,    91,     0,    10,   102,    91,     0,    43,   103,    91,
     0,    44,   104,    91,     0,    59,   105,    91,     0,    60,
   106,    91,     0,    57,   107,    91,     0,    61,   111,    91,
     0,    64,   112,    91,     0,    67,   113,    91,     0,   126,
   128,     0,   126,   130,     0,   126,   130,     0,   126,   130,
   126,   130,     0,   126,   130,   126,   130,     0,   126,   130,
   126,   130,     0,   126,   130,   126,   130,   126,   130,     0,
   126,   130,   126,   130,   126,   130,     0,   126,   130,   126,
   131,     0,   126,   130,   126,   132,     0,   126,   130,   126,
   131,     0,   126,   130,   126,   132,     0,   126,   130,   126,
   132,     0,   126,   133,    58,   126,   128,     0,   126,   130,
   126,   130,     0,     0,   126,   130,     0,   126,   130,     0,
   126,   131,   126,   128,     0,   114,   116,   117,   115,     0,
   119,   116,   118,     0,   120,   116,   118,     0,   126,   129,
   126,   134,   126,   129,    62,     0,     0,    63,   116,   118,
     0,     0,    38,    91,    18,     0,    38,    91,    18,     0,
   126,   133,    58,   126,   129,    65,   126,   129,    66,     0,
   126,   129,   126,   134,   126,   129,    66,     0,     5,     0,
     3,     0,     4,     0,     6,     0,    37,     0,     0,    45,
   130,     0,    52,   130,     0,    53,   132,   130,   130,     0,
    55,   130,   130,     0,    56,   132,     0,    49,   130,   130,
     0,    51,   130,   130,     0,    50,   130,   130,     0,    54,
   132,   130,   130,     0,     0,   125,   128,     0,   121,   128,
     0,   123,   128,     0,   122,   128,     0,   124,   128,     0,
    68,   126,   127,    69,   128,     0,   121,     0,   125,     0,
   123,     0,   122,     0,   124,     0,    68,   126,   127,    69,
     0,   125,     0,   124,     0,   121,     0,    68,   126,   127,
    69,     0,   121,     0,   122,     0,    68,   126,   127,    69,
     0,   121,     0,   123,     0,    68,   126,   127,    69,     0,
   121,     0,    73,     0,    71,     0,    70,     0,    72,     0,
    74,     0,    75,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   367,   370,   374,   375,   378,   381,   386,   391,   394,   397,
   400,   403,   409,   410,   416,   417,   420,   435,   436,   440,
   445,   450,   453,   456,   459,   462,   465,   468,   471,   474,
   477,   480,   483,   485,   488,   491,   497,   512,   513,   516,
   518,   519,   520,   523,   526,   529,   534,   536,   537,   538,
   539,   540,   541,   542,   543,   544,   545,   546,   547,   548,
   549,   550,   551,   552,   553,   554,   555,   556,   557,   560,
   562,   564,   566,   568,   570,   572,   574,   576,   578,   580,
   582,   584,   586,   588,   590,   592,   594,   596,   598,   600,
   602,   606,   608,   609,   611,   613,   615,   619,   623,   628,
   630,   632,   634,   636,   638,   640,   641,   642,   643,   644,
   645,   646,   647,   648,   653,   654,   655,   656,   657,   658,
   659,   664,   665,   666,   667,   668,   669,   673,   674,   675,
   676,   680,   681,   682,   686,   687,   688,   692,   696,   697,
   698,   699,   700,   701
};

static const char * const yytname[] = {   "$","error","$illegal.","STR","GSTR",
"VAR","NUMBER","WINDOWTITLE","WINDOWSIZE","WINDOWPOSITION","FONT","FORECOLOR",
"BACKCOLOR","SHADCOLOR","LICOLOR","OBJECT","INIT","MAIN","END","PROP","TYPE",
"SIZE","POSITION","VALUE","VALUEMIN","VALUEMAX","TITLE","SWALLOWEXEC","ICON",
"STATE","WARP","SLEEP","WRITETOFILE","HIDEN","INACTIF","ACTIF","CASE","CLIC",
"BEG","POINT","EXEC","HIDE","SHOW","CHFORECOLOR","CHBACKCOLOR","GETVALUE","CHVALUE",
"CHVALUEMAX","CHVALUEMIN","ADD","DIV","MULT","GETTITLE","GETOUTPUT","STRCOPY",
"NUMTOHEX","HEXTONUM","QUIT","GET","SET","SENDSIGN","IF","THEN","ELSE","FOR",
"TO","DO","WHILE","BEGF","ENDF","EQUAL","INFEQ","SUPEQ","INF","SUP","DIFF","script",
"initvar","head","initbloc","object","id","init","state","verify","mainloop",
"addtabcase","case","modif","number","bloc","instr","exec","hide","show","chvalue",
"chvaluemax","chvaluemin","position","size","icon","title","font","chforecolor",
"chbackcolor","set","sendsign","quit","warp","sleep","writetofile","ifthenelse",
"loop","while","headif","else","creerbloc","bloc1","bloc2","headloop","headwhile",
"var","str","gstr","num","clic","addlbuff","function","args","arg","numarg",
"strarg","gstrarg","vararg","compare",""
};
#endif

static const short yyr1[] = {     0,
    76,    77,    78,    78,    78,    78,    78,    78,    78,    78,
    78,    78,    79,    79,    80,    80,    81,    82,    82,    82,
    82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
    82,    82,    82,    83,    83,    83,    84,    85,    85,    86,
    87,    87,    87,    88,    89,    90,    91,    91,    91,    91,
    91,    91,    91,    91,    91,    91,    91,    91,    91,    91,
    91,    91,    91,    91,    91,    91,    91,    91,    91,    92,
    93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
   103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
   113,   114,   115,   115,   116,   117,   118,   119,   120,   121,
   122,   123,   124,   125,   126,   127,   127,   127,   127,   127,
   127,   127,   127,   127,   128,   128,   128,   128,   128,   128,
   128,   129,   129,   129,   129,   129,   129,   130,   130,   130,
   130,   131,   131,   131,   132,   132,   132,   133,   134,   134,
   134,   134,   134,   134
};

static const short yyr2[] = {     0,
     4,     0,     0,     3,     3,     4,     4,     3,     3,     3,
     3,     3,     0,     5,     0,     7,     1,     0,     3,     4,
     4,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     1,     1,     1,     0,     1,     5,     0,
     0,     4,     4,     1,     1,     3,     0,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
     2,     2,     4,     4,     4,     6,     6,     4,     4,     4,
     4,     4,     5,     4,     0,     2,     2,     4,     4,     3,
     3,     7,     0,     3,     0,     3,     3,     9,     7,     1,
     1,     1,     1,     1,     0,     2,     2,     4,     3,     2,
     3,     3,     3,     4,     0,     2,     2,     2,     2,     2,
     5,     1,     1,     1,     1,     1,     4,     1,     1,     1,
     4,     1,     1,     4,     1,     1,     4,     1,     1,     1,
     1,     1,     1,     1
};

static const short yydefact[] = {     2,
     3,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    13,     3,     0,     0,     3,     3,     3,     3,     3,     3,
    95,    15,     4,     3,     3,    12,     9,     8,    10,    11,
     5,     0,     0,     1,     7,     6,    47,    17,     0,   105,
   105,   105,   105,   105,   105,   105,   105,   105,   105,   105,
   105,   105,   105,   105,   105,    85,   105,   105,   105,   105,
   105,     0,    18,    47,     0,    47,     0,    47,     0,    47,
     0,    47,     0,    47,     0,    47,     0,    47,     0,    47,
   115,    47,     0,    47,     0,    47,     0,    47,     0,    47,
     0,    47,     0,    47,     0,    47,    47,     0,    47,     0,
    47,    95,     0,    47,    95,     0,    47,    95,     0,    14,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    37,    61,   100,   103,   104,
   105,   130,   129,   128,   105,    58,   105,    57,   105,    59,
   105,    60,   105,    49,    86,    50,    87,    51,   101,   105,
   132,   133,   105,    48,   102,   105,   115,   115,   115,   115,
   115,    70,    52,    71,    53,    72,    62,   105,    63,   105,
    54,   105,    55,   105,    56,   105,    66,    64,   138,     0,
    65,   105,    67,     0,   105,   122,   125,   124,   126,   123,
   105,    68,     0,     0,    69,     0,   105,    18,    18,    18,
    18,    18,    18,     0,     0,    18,    18,    18,    18,    18,
    18,    34,    35,    36,    18,     0,     0,     0,     0,     0,
     0,     0,     0,   115,     0,   117,   119,   118,   120,   116,
     0,     0,     0,     0,     0,   105,     0,    47,    93,     0,
     0,    47,    90,   105,    91,     0,    32,    29,    28,    30,
    31,    19,    18,    18,    22,    23,    24,    25,    26,    27,
    33,    40,    38,    15,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    80,   105,   105,   105,   135,   136,
    79,    78,     0,    88,     0,    81,    82,    73,    74,    75,
   115,    84,     0,    95,    89,     0,   141,   140,   142,   139,
   143,   144,   105,     0,     0,   105,    20,    21,     0,    16,
   106,     0,     0,     0,   107,     0,     0,     0,   110,   131,
     0,     0,     0,   134,   115,    83,    96,     0,   127,     0,
    97,     0,     0,    41,   111,   113,   112,     0,     0,   109,
    77,    76,     0,   121,    94,     0,   105,     0,    45,    44,
     0,     0,     0,   108,   114,   137,    92,     0,    99,    39,
     0,     0,     0,    47,    41,    41,    98,     0,    42,    43,
    46,     0,     0,     0
};

static const short yydefgoto[] = {   372,
     1,    11,    22,    34,    39,   126,   215,   216,   264,   309,
   351,   352,   353,   365,    62,    80,    82,    84,    90,    92,
    94,    68,    66,    72,    70,    64,    86,    88,    97,    99,
    96,    74,    76,    78,   101,   104,   107,   102,   295,    32,
   239,   243,   105,   108,   132,   158,   159,   133,   134,    65,
   274,   162,   191,   135,   153,   281,   180,   303
};

static const short yypact[] = {-32768,
   165,    29,    31,    37,    46,    56,    59,    83,    85,    90,
    79,   165,    93,    95,   165,   165,   165,   165,   165,   165,
-32768,    88,-32768,   165,   165,-32768,-32768,-32768,-32768,-32768,
-32768,    70,   108,-32768,-32768,-32768,   367,-32768,    98,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   100,   262,   367,   155,   367,   155,   367,   155,   367,
   155,   367,   155,   367,   155,   367,   155,   367,    24,   367,
    74,   367,   155,   367,   155,   367,   155,   367,   155,   367,
   155,   367,   155,   367,   155,   367,   367,   114,   367,   155,
   367,-32768,   183,   367,-32768,   114,   367,-32768,   183,-32768,
   119,   120,   121,   124,   130,   133,   131,   132,   134,   137,
   146,   149,   153,   159,    50,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    74,    74,    74,    74,
    74,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   105,
-32768,-32768,-32768,   147,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   152,   113,-32768,   152,-32768,   262,   262,   262,
   262,   262,   262,   185,   188,   262,   262,   262,   262,   262,
   262,-32768,-32768,-32768,   262,    53,   203,    24,   155,   155,
    62,    24,   203,    74,   203,-32768,-32768,-32768,-32768,-32768,
    62,    62,   155,   155,   155,-32768,   155,   367,   135,   203,
   330,   367,-32768,-32768,-32768,   330,-32768,-32768,-32768,-32768,
-32768,-32768,   262,   262,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,    88,   155,   155,   155,   155,   155,    62,
    62,   155,    62,   128,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   136,-32768,   138,-32768,-32768,-32768,-32768,-32768,
    74,-32768,   181,-32768,-32768,   141,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,   184,   183,-32768,-32768,-32768,   168,-32768,
-32768,   155,   155,   155,-32768,   155,   155,   155,-32768,-32768,
   155,   155,   203,-32768,    74,-32768,-32768,   152,-32768,   183,
-32768,   148,   183,    19,-32768,-32768,-32768,   155,   155,-32768,
-32768,-32768,   145,-32768,-32768,   139,-32768,   150,-32768,-32768,
   197,   179,   180,-32768,-32768,-32768,-32768,   183,-32768,-32768,
   186,   186,   156,   367,    19,    19,-32768,   208,-32768,-32768,
-32768,   221,   227,-32768
};

static const short yypgoto[] = {-32768,
-32768,   285,-32768,   -34,-32768,   237,-32768,-32768,-32768,-32768,
  -259,-32768,-32768,  -130,   -42,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   -44,
-32768,  -173,-32768,-32768,     7,   -64,   -62,   -12,    23,   -41,
  -172,   -85,   -88,   291,  -187,  -150,   127,    -2
};


#define	YYLAST		630


static const short yytable[] = {    67,
    69,    71,    73,    75,    77,    79,    81,    83,    85,    87,
    89,    91,    93,    95,   152,    98,   100,   103,   106,   109,
   197,   127,   245,   136,   349,   138,   149,   140,   128,   142,
   275,   144,    12,   146,   282,   148,    13,   154,   187,   163,
   188,   165,    14,   167,   187,   169,   188,   171,    15,   173,
   283,   175,   285,   177,   178,   350,   181,   184,   183,    16,
   193,   192,    17,   196,   195,   155,   128,   296,   160,   262,
   263,   226,   227,   228,   229,   230,   149,   155,   128,   129,
   286,   287,   212,   213,   214,   151,    18,   157,    19,   217,
   189,   150,    20,   218,    21,   219,   189,   220,    24,   221,
    25,   222,    33,   161,   179,   369,   370,    37,   223,   186,
   130,   224,   179,    38,   225,   186,    63,   110,   128,   316,
   317,   198,   319,   199,   200,   190,   231,   201,   232,   278,
   233,   190,   234,   202,   235,   203,   204,   205,   284,   206,
   237,   156,   207,   240,   160,   160,   160,   160,   160,   241,
   343,   208,   209,   152,   345,   246,   210,   152,   280,   128,
   129,   211,   236,   157,   157,   157,   157,   157,   280,   280,
   244,     2,     3,     4,     5,     6,     7,     8,     9,   161,
   161,   161,   161,   161,   238,   149,   155,   128,   129,   242,
   253,   130,    10,   254,   291,   293,   320,   294,   327,   304,
   357,   331,   305,   334,   324,   326,   325,   280,   280,   329,
   280,   160,   347,   356,   360,   359,   332,   361,   362,   130,
   373,   367,   131,   364,   151,   371,   374,   279,   151,   310,
   157,   366,   194,     0,   321,   322,   323,   279,   279,   344,
   187,   346,   188,   306,   348,     0,   161,   265,     0,   328,
   185,   266,   267,   268,   269,   270,   271,   272,   273,     0,
     0,   330,     0,     0,   333,   187,     0,   188,   187,   363,
   188,   111,   112,   113,   114,   115,   279,   279,   160,   279,
     0,   116,   117,   118,   119,   120,   121,   122,   123,   124,
   125,     0,   189,   187,     0,   188,    23,   157,     0,    26,
    27,    28,    29,    30,    31,   358,     0,     0,    35,    36,
     0,   186,   160,   161,     0,     0,     0,   189,     0,     0,
   189,   368,     0,     0,     0,     0,     0,   190,     0,     0,
     0,   157,     0,     0,     0,     0,   186,     0,     0,   186,
     0,     0,     0,     0,     0,   189,     0,   161,     0,     0,
     0,     0,   190,     0,     0,   190,     0,   137,     0,   139,
     0,   141,     0,   143,   186,   145,     0,   147,     0,     0,
     0,     0,     0,   164,     0,   166,    40,   168,     0,   170,
   190,   172,     0,   174,     0,   176,     0,    41,    42,     0,
   182,     0,    43,     0,    44,     0,    45,    46,    47,   297,
   298,   299,   300,   301,   302,     0,    48,    49,    50,    51,
    52,     0,    53,    54,    55,     0,     0,     0,     0,     0,
     0,     0,     0,    56,     0,    57,    58,    59,     0,     0,
    60,     0,     0,    61,   247,   248,   249,   250,   251,   252,
     0,     0,   255,   256,   257,   258,   259,   260,     0,     0,
     0,   261,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   307,
   308,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   276,
   277,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   288,   289,   290,     0,   292,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   311,   312,   313,   314,   315,
     0,     0,   318,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   335,   336,   337,     0,   338,   339,   340,     0,
     0,   341,   342,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   354,   355
};

static const short yycheck[] = {    41,
    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    52,    53,    54,    55,    79,    57,    58,    59,    60,    61,
   109,    64,   196,    66,     6,    68,     3,    70,     5,    72,
   218,    74,     4,    76,   222,    78,     6,    80,   103,    82,
   103,    84,     6,    86,   109,    88,   109,    90,     3,    92,
   223,    94,   225,    96,    97,    37,    99,   102,   101,     4,
   105,   104,     4,   108,   107,     4,     5,   240,    81,    17,
    18,   157,   158,   159,   160,   161,     3,     4,     5,     6,
   231,   232,    33,    34,    35,    79,     4,    81,     4,   131,
   103,    68,     3,   135,    16,   137,   109,   139,     6,   141,
     6,   143,    15,    81,    98,   365,   366,    38,   150,   103,
    37,   153,   106,     6,   156,   109,    19,    18,     5,   270,
   271,     3,   273,     4,     4,   103,   168,     4,   170,    68,
   172,   109,   174,     4,   176,     3,     6,     6,   224,     6,
   182,    68,     6,   185,   157,   158,   159,   160,   161,   191,
   323,     6,     4,   218,   328,   197,     4,   222,   221,     5,
     6,     3,    58,   157,   158,   159,   160,   161,   231,   232,
    58,     7,     8,     9,    10,    11,    12,    13,    14,   157,
   158,   159,   160,   161,    38,     3,     4,     5,     6,    38,
     6,    37,    28,     6,   236,   238,    69,    63,    18,   242,
    62,    18,   244,    36,    69,   291,    69,   270,   271,    69,
   273,   224,    65,    69,    18,    66,   305,    39,    39,    37,
     0,    66,    68,    38,   218,    18,     0,   221,   222,   264,
   224,   362,   106,    -1,   276,   277,   278,   231,   232,   325,
   305,   330,   305,   246,   333,    -1,   224,    45,    -1,   294,
    68,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
    -1,   303,    -1,    -1,   306,   330,    -1,   330,   333,   358,
   333,    10,    11,    12,    13,    14,   270,   271,   291,   273,
    -1,    20,    21,    22,    23,    24,    25,    26,    27,    28,
    29,    -1,   305,   358,    -1,   358,    12,   291,    -1,    15,
    16,    17,    18,    19,    20,   347,    -1,    -1,    24,    25,
    -1,   305,   325,   291,    -1,    -1,    -1,   330,    -1,    -1,
   333,   364,    -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,
    -1,   325,    -1,    -1,    -1,    -1,   330,    -1,    -1,   333,
    -1,    -1,    -1,    -1,    -1,   358,    -1,   325,    -1,    -1,
    -1,    -1,   330,    -1,    -1,   333,    -1,    67,    -1,    69,
    -1,    71,    -1,    73,   358,    75,    -1,    77,    -1,    -1,
    -1,    -1,    -1,    83,    -1,    85,    10,    87,    -1,    89,
   358,    91,    -1,    93,    -1,    95,    -1,    21,    22,    -1,
   100,    -1,    26,    -1,    28,    -1,    30,    31,    32,    70,
    71,    72,    73,    74,    75,    -1,    40,    41,    42,    43,
    44,    -1,    46,    47,    48,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    57,    -1,    59,    60,    61,    -1,    -1,
    64,    -1,    -1,    67,   198,   199,   200,   201,   202,   203,
    -1,    -1,   206,   207,   208,   209,   210,   211,    -1,    -1,
    -1,   215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   253,
   254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   219,
   220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,   233,   234,   235,    -1,   237,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,   265,   266,   267,   268,   269,
    -1,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   312,   313,   314,    -1,   316,   317,   318,    -1,
    -1,   321,   322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   338,   339
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


void *alloca (unsigned int a)
{
}


/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/lib/bison.simple"
int
yyparse()
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
      yyss = (short *) alloca (yystacksize * sizeof (*yyssp));
      __yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
      yyvs = (YYSTYPE *) alloca (yystacksize * sizeof (*yyvsp));
      __yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) alloca (yystacksize * sizeof (*yylsp));
      __yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 370 "source/bisonin"
{ InitVarGlob(); ;
    break;}
case 4:
#line 375 "source/bisonin"
{		/* Titre de la fenetre */
				 scriptprop->titlewin=yyvsp[-1].str;
				;
    break;}
case 5:
#line 378 "source/bisonin"
{
				 scriptprop->icon=yyvsp[-1].str;
				;
    break;}
case 6:
#line 382 "source/bisonin"
{		/* Position et taille de la fenetre */
				 scriptprop->x=yyvsp[-2].number;
				 scriptprop->y=yyvsp[-1].number;
				;
    break;}
case 7:
#line 387 "source/bisonin"
{		/* Position et taille de la fenetre */
				 scriptprop->width=yyvsp[-2].number;
				 scriptprop->height=yyvsp[-1].number;
				;
    break;}
case 8:
#line 391 "source/bisonin"
{ 		/* Couleur de fond */
				 scriptprop->backcolor=yyvsp[-1].str;
				;
    break;}
case 9:
#line 394 "source/bisonin"
{ 		/* Couleur des lignes */
				 scriptprop->forecolor=yyvsp[-1].str;
				;
    break;}
case 10:
#line 397 "source/bisonin"
{ 		/* Couleur des lignes */
				 scriptprop->shadcolor=yyvsp[-1].str;
				;
    break;}
case 11:
#line 400 "source/bisonin"
{ 		/* Couleur des lignes */
				 scriptprop->licolor=yyvsp[-1].str;
				;
    break;}
case 12:
#line 403 "source/bisonin"
{
				 scriptprop->font=yyvsp[-1].str;
				;
    break;}
case 14:
#line 410 "source/bisonin"
{
				 scriptprop->initbloc=PileBloc[TopPileB];
				 TopPileB--; 
				;
    break;}
case 17:
#line 420 "source/bisonin"
{ nbobj++;
				  if (nbobj>30)
				  { yyerror("Too many items\n");
				    exit(1);}
				  if ((yyvsp[0].number<1)||(yyvsp[0].number>30))
				  { yyerror("Choose item id between 1 and 30\n");
				    exit(1);} 
				  if (TabIdObj[yyvsp[0].number]!=-1) 
				  { i=yyvsp[0].number; fprintf(stderr,"Line %d: item id %d already used:\n",numligne,yyvsp[0].number);
				    exit(1);}
			          TabIdObj[yyvsp[0].number]=nbobj;
				  (*tabobj)[nbobj].id=yyvsp[0].number;
				;
    break;}
case 19:
#line 436 "source/bisonin"
{
				 (*tabobj)[nbobj].type=yyvsp[-1].str;
				 HasType=1;
				;
    break;}
case 20:
#line 440 "source/bisonin"
{
				 (*tabobj)[nbobj].width=yyvsp[-2].number;
				 (*tabobj)[nbobj].height=yyvsp[-1].number;
				 HasSize=1;
				;
    break;}
case 21:
#line 445 "source/bisonin"
{
				 (*tabobj)[nbobj].x=yyvsp[-2].number;
				 (*tabobj)[nbobj].y=yyvsp[-1].number;
				 HasPosition=1;
				;
    break;}
case 22:
#line 450 "source/bisonin"
{
				 (*tabobj)[nbobj].value=yyvsp[-1].number;
				;
    break;}
case 23:
#line 453 "source/bisonin"
{
				 (*tabobj)[nbobj].value2=yyvsp[-1].number;
				;
    break;}
case 24:
#line 456 "source/bisonin"
{
				 (*tabobj)[nbobj].value3=yyvsp[-1].number;
				;
    break;}
case 25:
#line 459 "source/bisonin"
{
				 (*tabobj)[nbobj].title=yyvsp[-1].str;
				;
    break;}
case 26:
#line 462 "source/bisonin"
{
				 (*tabobj)[nbobj].swallow=yyvsp[-1].str;
				;
    break;}
case 27:
#line 465 "source/bisonin"
{
				 (*tabobj)[nbobj].icon=yyvsp[-1].str;
				;
    break;}
case 28:
#line 468 "source/bisonin"
{
				 (*tabobj)[nbobj].backcolor=yyvsp[-1].str;
				;
    break;}
case 29:
#line 471 "source/bisonin"
{
				 (*tabobj)[nbobj].forecolor=yyvsp[-1].str;
				;
    break;}
case 30:
#line 474 "source/bisonin"
{
				 (*tabobj)[nbobj].shadcolor=yyvsp[-1].str;
				;
    break;}
case 31:
#line 477 "source/bisonin"
{
				 (*tabobj)[nbobj].licolor=yyvsp[-1].str;
				;
    break;}
case 32:
#line 480 "source/bisonin"
{
				 (*tabobj)[nbobj].font=yyvsp[-1].str;
				;
    break;}
case 34:
#line 485 "source/bisonin"
{
				 (*tabobj)[nbobj].state=Hiden;
				;
    break;}
case 35:
#line 488 "source/bisonin"
{
				 (*tabobj)[nbobj].state=Inactif;
				;
    break;}
case 36:
#line 491 "source/bisonin"
{
				 (*tabobj)[nbobj].state=Actif;
				;
    break;}
case 37:
#line 497 "source/bisonin"
{ 
				  if (!HasPosition)
				   { yyerror("No position for object");
				     exit(1);}
				  if (!HasSize)
				   { yyerror("No size for object");
				     exit(1);}
				  if (!HasType)
				   { yyerror("No type for object");
				     exit(1);}
				  HasSize=0;
				  HasPosition=0;
				  HasType=0;
				 ;
    break;}
case 38:
#line 512 "source/bisonin"
{ InitObjTabCase(0); ;
    break;}
case 40:
#line 516 "source/bisonin"
{ InitObjTabCase(1); ;
    break;}
case 44:
#line 523 "source/bisonin"
{ InitCase(0); ;
    break;}
case 45:
#line 526 "source/bisonin"
{ InitCase(yyvsp[0].number); ;
    break;}
case 47:
#line 534 "source/bisonin"
{/* vide donc fin d'une serie d'instruction d'un case */
				;
    break;}
case 70:
#line 560 "source/bisonin"
{ AddCom(1,1); ;
    break;}
case 71:
#line 562 "source/bisonin"
{ AddCom(2,1);;
    break;}
case 72:
#line 564 "source/bisonin"
{ AddCom(3,1);;
    break;}
case 73:
#line 566 "source/bisonin"
{ AddCom(4,2);;
    break;}
case 74:
#line 568 "source/bisonin"
{ AddCom(21,2);;
    break;}
case 75:
#line 570 "source/bisonin"
{ AddCom(22,2);;
    break;}
case 76:
#line 572 "source/bisonin"
{ AddCom(5,3);;
    break;}
case 77:
#line 574 "source/bisonin"
{ AddCom(6,3);;
    break;}
case 78:
#line 576 "source/bisonin"
{ AddCom(7,2);;
    break;}
case 79:
#line 578 "source/bisonin"
{ AddCom(8,2);;
    break;}
case 80:
#line 580 "source/bisonin"
{ AddCom(9,2);;
    break;}
case 81:
#line 582 "source/bisonin"
{ AddCom(10,2);;
    break;}
case 82:
#line 584 "source/bisonin"
{ AddCom(19,2);;
    break;}
case 83:
#line 586 "source/bisonin"
{ AddCom(11,2);;
    break;}
case 84:
#line 588 "source/bisonin"
{ AddCom(12,2);;
    break;}
case 85:
#line 590 "source/bisonin"
{ AddCom(13,0);;
    break;}
case 86:
#line 592 "source/bisonin"
{ AddCom(17,1);;
    break;}
case 87:
#line 594 "source/bisonin"
{ AddCom(20,1);;
    break;}
case 88:
#line 596 "source/bisonin"
{ AddCom(18,2);;
    break;}
case 92:
#line 606 "source/bisonin"
{ AddComBloc(14,3,2); ;
    break;}
case 95:
#line 611 "source/bisonin"
{ EmpilerBloc(); ;
    break;}
case 96:
#line 613 "source/bisonin"
{ DepilerBloc(2); ;
    break;}
case 97:
#line 615 "source/bisonin"
{ DepilerBloc(1); ;
    break;}
case 98:
#line 619 "source/bisonin"
{ AddComBloc(15,3,1); ;
    break;}
case 99:
#line 623 "source/bisonin"
{ AddComBloc(16,3,1); ;
    break;}
case 100:
#line 628 "source/bisonin"
{ AddVar(yyvsp[0].str); ;
    break;}
case 101:
#line 630 "source/bisonin"
{ AddConstStr(yyvsp[0].str); ;
    break;}
case 102:
#line 632 "source/bisonin"
{ AddConstStr(yyvsp[0].str); ;
    break;}
case 103:
#line 634 "source/bisonin"
{ AddConstNum(yyvsp[0].number); ;
    break;}
case 104:
#line 636 "source/bisonin"
{ AddConstNum(0); ;
    break;}
case 105:
#line 638 "source/bisonin"
{ AddLevelBufArg(); ;
    break;}
case 106:
#line 640 "source/bisonin"
{ AddFunct(1,1); ;
    break;}
case 107:
#line 641 "source/bisonin"
{ AddFunct(2,1); ;
    break;}
case 108:
#line 642 "source/bisonin"
{ AddFunct(3,1); ;
    break;}
case 109:
#line 643 "source/bisonin"
{ AddFunct(4,1) ;
    break;}
case 110:
#line 644 "source/bisonin"
{ AddFunct(5,1) ;
    break;}
case 111:
#line 645 "source/bisonin"
{ AddFunct(6,1) ;
    break;}
case 112:
#line 646 "source/bisonin"
{ AddFunct(7,1) ;
    break;}
case 113:
#line 647 "source/bisonin"
{ AddFunct(8,1) ;
    break;}
case 114:
#line 648 "source/bisonin"
{ AddFunct(9,1); ;
    break;}
case 115:
#line 653 "source/bisonin"
{ ;
    break;}
case 139:
#line 696 "source/bisonin"
{ l=1-250000; AddBufArg(&l,1); ;
    break;}
case 140:
#line 697 "source/bisonin"
{ l=2-250000; AddBufArg(&l,1); ;
    break;}
case 141:
#line 698 "source/bisonin"
{ l=3-250000; AddBufArg(&l,1); ;
    break;}
case 142:
#line 699 "source/bisonin"
{ l=4-250000; AddBufArg(&l,1); ;
    break;}
case 143:
#line 700 "source/bisonin"
{ l=5-250000; AddBufArg(&l,1); ;
    break;}
case 144:
#line 701 "source/bisonin"
{ l=6-250000; AddBufArg(&l,1); ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/lib/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;
}
#line 704 "source/bisonin"















