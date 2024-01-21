
/*****************************************************************************
 *
 * ObjectPak Test Program 
 * 
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <objpak/objpak.h>

#define CLTN_MODE (1)
#define SET_MODE  (2)
#define DIC_MODE  (3)
#define SORT_MODE (4)

static int setMode(int argc,STR *argv)
{
    int i;
    
    for(i=1;i<argc;i++) {
	id arg = [ObjStr str:argv[i]];
	if ([arg isEqualSTR:"-cltn"]) return CLTN_MODE;
	if ([arg isEqualSTR:"-set"])  return SET_MODE;
	if ([arg isEqualSTR:"-dic"])  return DIC_MODE;
	if ([arg isEqualSTR:"-sort"]) return SORT_MODE;
	fprintf(stderr,"usage: words [-cltn][-set][-dic][-sort]\n");exit(1);
    }
    
    return 0;
}

static void printCltn()
{
    extern id words;
    extern int yylex();
    words = [ObjCltn new];yylex();[words printToFile:stdout];
}

static void printSet()
{
    extern id words;
    extern int yylex();
    words = [ObjSet new];yylex();[words printToFile:stdout];
}

static void printSort()
{
    extern id words;
    extern int yylex();
    words = [ObjSort newDictCompare];yylex();[words printToFile:stdout];
}

static id createDic(id words)
{
    id aStr,aSeq;
    id aDic = [ObjDic new];
    
    aSeq = [words eachElement];
    while ((aStr = [aSeq next])) {
	id value;
	if ((value = [aDic atKey:aStr])) {
	    [[aDic atKey:aStr put:[ObjStr sprintf:"%i",[value asInt]+1]] free];
	} else {
	    [aDic atKey:aStr put:[ObjStr sprintf:"%i",1]];
	}
    }
    aSeq = [aSeq free];
    
    return aDic;
}

static void printDic()
{
    extern id words;
    extern int yylex();
    words = [ObjCltn new];yylex();[createDic(words) printToFile:stdout];
}

void main(int argc,STR *argv)
{
    int mode = setMode(argc,argv);
    
    switch(mode) {
	case CLTN_MODE : { printCltn();break; } 
	case SET_MODE  : { printSet(); break; } 
	case DIC_MODE  : { printDic(); break; } 
	case SORT_MODE : { printSort();break; } 
	default        : { break; }
    }
}
