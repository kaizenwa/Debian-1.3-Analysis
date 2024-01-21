/*$Id: e_exp.cc,v 11.24 96/02/25 14:10:08 al Exp $ -*- C++ -*-
 * Behavioral modeling parse and print functions.  This is a mess.
 * I plan to redo all of the behavioral modeling stuff.
 */
#include "ap.h"
#include "e_elemnt.h"
#include "error.h"
#include "e_exp.h"
#include "io.h"
#include "u_opt.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//	void	ELEMENT::parseexpr(CS&);
//	void	ELEMENT::printexpr(int)const;
static	void	getkey(int*,int*,CS&);
/*--------------------------------------------------------------------------*/
static const char e_om[] = "out of memory: %s\n";
/*--------------------------------------------------------------------------*/
void ELEMENT::parseexpr(CS& cmd)
{
  double numbers[EXPRSIZE];	/* there are two dynamic arrays		    */
  int keys[EXPRSIZE];		/* one for the keys			    */
  int numcount;			/* other for arguments (doubles)	    */
  int keycount;			/* keys are first.			    */
  
  val = cmd.ctof();
  cmd.pmatch("METHOD");
  /**/ ::set(cmd, "GEAR",	&method_u, mGEAR)
    || ::set(cmd, "TRAPezoidal",&method_u, mTRAPEZOID)
    || ::set(cmd, "STIFF",	&method_u, mSTIFF);

  /* Initially, they are unknown size, so	    */
  numcount = keycount = 0;	/* use local fixed allocation.  Then copy   */
  for (;;){			/* in appropriate size to malloc space,	    */
    int key;			/* with links to parent branch struct.	    */
    int args;
    int paren = 0;
    getkey(&key, &args, cmd);
    if (!key)
      break;
    if ((keycount + 1 >= EXPRSIZE) || (numcount + args > EXPRSIZE))
      error(bERROR, "expression too long\n");
    keys[keycount++] = key;
    if (args == aVARIABLE){		/* variable # of args	    */
      double *argcount;			/* 1st arg is count	    */
      paren += cmd.skiplparen();
      argcount = &(numbers[numcount++]);
      *argcount = 1.;
      while (cmd.is_float()){
	numbers[numcount++] = cmd.ctof();
	(*argcount)++;
      }
      paren -= cmd.skiprparen();
    }else if (args == aASSIGN){
      cmd.skipequal();
      numbers[numcount++] = cmd.ctof();
    }else if (args > 0){
      paren += cmd.skiplparen();
      for (int arg1 = 1;  arg1 <= args;  arg1++)
	numbers[numcount++] = cmd.ctof();
      paren -= cmd.skiprparen();
    }else{
      /* no args, do nothing */
    }
    if (paren != 0)
      cmd.warn(bWARNING);
  }
  
  if (keycount != 0){		/* allocate mem to save this stuff,	    */
    struct expr *xx;		/* then copy.				    */
    size_t ssize;

    trfun = trfix;
    acfun = acfix;

    x = (generic_t*)calloc(1,sizeof(struct expr));
    if (!x)
      error(bERROR, e_om, "partslist (expression)");
    xx = (struct expr*)x;
    xx->ssize = sizeof(struct expr);

    keys[keycount++] = eEND;

    ssize = sizeof(generic_t) + keycount*sizeof(int);
    xx->x = (generic_t*)calloc(1, ssize);
    if (!xx->x)
      error(bERROR, e_om, "partslist (expression)");
    xx->x->ssize = ssize;
    xx->keys = (struct ints*)xx->x;

    ssize = sizeof(generic_t) + numcount*sizeof(double);
    xx->x->x = (generic_t*)calloc(1, ssize);
    if (!xx->x->x)
      error(bERROR, e_om, "partslist (expression)");
    xx->x->x->ssize = ssize;
    xx->args = (struct dbls*)xx->x->x;

    xx->x->x->x = (generic_t*)NULL;

    memcpy((void*)xx->keys->args,(void*)keys,   keycount*sizeof(int));
    memcpy((void*)xx->args->args,(void*)numbers,numcount*sizeof(double));
  }else{
    x = (generic_t*)NULL;
  }
}
/*--------------------------------------------------------------------------*/
/* getkey: scan for a keyword. Return its key and desired arg count.
 * returns key == eNO if no match.
 */
static void getkey(int *key, int *args, CS& cmd)
{
      if (cmd.pmatch("AC"	  )) {*key = eAC; 	 *args = aAC;}
 else if (cmd.pmatch("DC"	  )) {*key = eDC; 	 *args = aDC;}
 else if (cmd.pmatch("DCTran"	  )) {*key = eDCTRAN;	 *args = aDCTRAN;}
 else if (cmd.pmatch("Frequency"  )) {*key = eFREQUENCY; *args = aFREQUENCY;}
 else if (cmd.pmatch("PEriod"	  )) {*key = ePERIOD;	 *args = aPERIOD;}
 else if (cmd.pmatch("Ramp"	  )) {*key = eRAMP;	 *args = aRAMP;}
 else if (cmd.pmatch("TIme"	  )) {*key = eTIME;	 *args = aTIME;}
 else if (cmd.pmatch("TRansient"  )) {*key = eTRAN;	 *args = aTRAN;}

 else if (cmd.pmatch("BAndwidth"  )) {*key = eBANDWIDTH; *args = aBANDWIDTH;}
 else if (cmd.pmatch("COMplex"	  )) {*key = eCOMPLEX;   *args = aCOMPLEX;}
 else if (cmd.pmatch("CORNERDown" )) {*key = eCORNERDOWN;*args = aCORNERDOWN;}
 else if (cmd.pmatch("CORNERUp"   )) {*key = eCORNERUP;  *args = aCORNERUP;}
 else if (cmd.pmatch("DElay"	  )) {*key = eDELAY;	 *args = aDELAY;}
 else if (cmd.pmatch("EXP"	  )) {*key = eEXP;	 *args = aEXP;}
 else if (cmd.pmatch("EXPTerm"	  )) {*key = eEXPTERM;   *args = aEXPTERM;}
 else if (cmd.pmatch("Generator"  )) {*key = eGENERATOR; *args = aGENERATOR;}
 else if (cmd.pmatch("Max"	  )) {*key = eMAX;	 *args = aMAX;}
 else if (cmd.pmatch("NEtfunction")) {*key = eNETFUNC;   *args = aNETFUNC;}
 else if (cmd.pmatch("NOtch"	  )) {*key = eNOTCH;	 *args = aNOTCH;}
 else if (cmd.is_float(		  )) {*key = eNUMERIC;   *args = aNUMERIC;}
 else if (cmd.pmatch("Offset"	  )) {*key = eOFFSET;	 *args = aOFFSET;}
 else if (cmd.pmatch("POLAr"	  )) {*key = ePOLAR;	 *args = aPOLAR;}
 else if (cmd.pmatch("POLYTerm"   )) {*key = ePOLYTERM;  *args = aPOLYTERM;}
 else if (cmd.pmatch("PUlse"	  )) {*key = ePULSE;	 *args = aPULSE;}
 else if (cmd.pmatch("PWl"	  )) {*key = ePWL;	 *args = aPWL;}
 else if (cmd.pmatch("SFfm"	  )) {*key = eSFFM;	 *args = aSFFM;}
 else if (cmd.pmatch("SIn"	  )) {*key = eSIN;	 *args = aSIN;}
 else if (cmd.pmatch("Tanh"	  )) {*key = eTANH;	 *args = aTANH;}

 else if (cmd.pmatch("IC"	  )) {*key = eIC; 	 *args = aIC;}
 else if (cmd.pmatch("II"	  )) {*key = eII; 	 *args = aII;}
 else if (cmd.pmatch("IV"	  )) {*key = eIV; 	 *args = aIV;}
 else if (cmd.pmatch("TEmpco"	  )) {*key = eTEMPCO;	 *args = aTEMPCO;}
 else 	 {cmd.check(bWARNING);        *key = eNO;	 *args = 0;}
}
/*--------------------------------------------------------------------------*/
void ELEMENT::printexpr(int where)const
{
  if (!x  ||  val != 0.){
    mprintf(where, "%s", ftos(val, "", 7, 0));
  }
  switch (method_u){
    case mTRAPEZOID: mprintf(where, " method=trapezoid "); break;
    case mGEAR:      mprintf(where, " method=gear ");	   break;
    case mSTIFF:     mprintf(where, " stiff ");		   break;
    case mUNKNOWN:   /* nothing */			   break;
  }

  if (x){
    struct expr *xx;
    double *arg1;
    int *key;
    const char *fname;
    int terms;

    xx = (struct expr*)x;
    arg1 = xx->args->args;
    for (key = xx->keys->args;  *key;  key++){
      switch (*key){
	 case eAC:	  fname = "ac";	   	terms = aAC;		break;
	 case eDC:	  fname = "dc";	   	terms = aDC;		break;
	 case eDCTRAN:    fname = "dctran";	terms = aDCTRAN;	break;
	 case eFREQUENCY: fname = "freq";	terms = aFREQUENCY;	break;
	 case ePERIOD:    fname = "period";	terms = aPERIOD;	break;
	 case eRAMP:	  fname = "ramp";	terms = aRAMP;		break;
	 case eTIME:	  fname = "time";	terms = aTIME;		break;
	 case eTRAN:	  fname = "tran";	terms = aTRAN;		break;

	 case eBANDWIDTH: fname = "bandwidth";  terms = aBANDWIDTH;	break;
	 case eCOMPLEX:   fname = "complex";	terms = aCOMPLEX;	break;
	 case eCORNERDOWN:fname = "cornerdown"; terms = aCORNERDOWN;	break;
	 case eCORNERUP:  fname = "cornerup";	terms = aCORNERUP;	break;
	 case eDELAY:     fname = "delay";	terms = aDELAY;		break;
	 case eEXP:	  fname = "exp";	terms = aEXP;		break;
	 case eEXPTERM:   fname = "expterm";	terms = aEXPTERM;	break;
	 case eGENERATOR: fname = "generator";  terms = aGENERATOR;	break;
	 case eMAX:	  fname = "max";	terms = aMAX;		break;
	 case eNOTCH:     fname = "notch";	terms = aNOTCH;		break;
	 case eNETFUNC:   fname = "netfunction";terms = aNETFUNC;	break;
	 case eNUMERIC:   fname = "";	   	terms = aNUMERIC;	break;
	 case eOFFSET:    fname = "offset";	terms = aOFFSET;	break;
	 case ePOLAR:     fname = "polar";	terms = aPOLAR;		break;
	 case ePOLYTERM:  fname = "polyterm";   terms = aPOLYTERM;	break;
	 case ePULSE:     fname = "pulse";	terms = aPULSE;		break;
	 case ePWL:	  fname = "pwl";	terms = aPWL;		break;
	 case eSFFM:	  fname = "sffm";	terms = aSFFM;		break;
	 case eSIN:	  fname = "sin";	terms = aSIN;		break;
	 case eTANH:	  fname = "tanh";	terms = aTANH;		break;

	 case eIC:	  fname = "ic";	   	terms = aIC;		break;
	 case eII:	  fname = "ii";	   	terms = aII;		break;
	 case eIV:	  fname = "iv";	   	terms = aIV;		break;
	 case eTEMPCO:    fname = "tempco";	terms = aTEMPCO;	break;
	 default:	  fname = "\n+ ERROR";  terms = 0;		break;
      }
      mprintf(where, " %s", fname);
      if (*key == eNUMERIC){
	mprintf(where,"%s ",ftos(*(arg1++), "", 7, 0));
      }else if (terms == aVARIABLE){
	terms = (int)(*(arg1++)) - 1;
	mprintf(where, "(", fname);
	for (int i = 1;   i <= terms;   i++){
	  mprintf(where,"%s ",ftos(*(arg1++), "", 7, 0));
	}
	mprintf(where, ") ");
      }else if (terms == aASSIGN){
	mprintf(where, "=", fname);
	mprintf(where,"%s ",ftos(*(arg1++), "", 7, 0));
      }else if (terms > 0){
	mprintf(where, "(", fname);
	for (int i = 1;   i <= terms;   i++){
	  mprintf(where,"%s ",ftos(*(arg1++), "", 7, 0));
	}
	mprintf(where, ") ");
      }
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
