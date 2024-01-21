/*$Id: e_exp.h,v 11.22 96/02/18 11:46:13 al Exp $ -*- C++ -*-
 * structures, etc for in-line expressions
 */
#include "e_card.h"
#ifndef E_EXP_H
#define E_EXP_H

#define EXPRSIZE 100

#define aVARIABLE	(-2)
#define aASSIGN		(-1)
#define eEND		0
#define eNO		0

#define eAC		1
#define eDC		2
#define eDCTRAN		3
#define eFREQUENCY	4
#define ePERIOD		5
#define eRAMP		6
#define eTIME		7
#define eTRAN		8

#define eBANDWIDTH	11
#define eCOMPLEX	12
#define eCORNERDOWN	13
#define eCORNERUP	14
#define eDELAY		15
#define eEXP		16
#define eEXPTERM	17
#define eGENERATOR	18
#define eMAX		19
#define eNETFUNC	31
#define eNOTCH		20
#define eNUMERIC	21
#define eOFFSET		22
#define ePOLAR		23
#define ePOLYTERM	24
#define ePULSE		25
#define ePWL		26
#define eSFFM		28
#define eSIN		29
#define eTANH		30

#define eIC		41
#define eII	 	42
#define eIV		43
#define eTEMPCO		44

#define aAC		0
#define aDC		0
#define aDCTRAN		0
#define aFREQUENCY	1
#define aPERIOD		1
#define aRAMP		1
#define aTIME		1
#define aTRAN		0

#define aBANDWIDTH	2
#define aCOMPLEX	2
#define aCORNERDOWN	2
#define aCORNERUP	2
#define aDELAY		2
#define aEXP		6
#define aEXPTERM	2
#define aGENERATOR	1
#define aMAX		2
#define aNETFUNC	aVARIABLE
#define aNOTCH		2
#define aNUMERIC	1
#define aOFFSET		2
#define aPOLAR		2
#define aPOLYTERM	2
#define aPULSE		7
#define aPWL		aVARIABLE
#define aSFFM		5
#define aSIN		5
#define aTANH		2
#define aIC		aASSIGN
#define aII	 	aASSIGN
#define aIV		aASSIGN
#define aTEMPCO		aASSIGN

struct ints {
   generic_t *x;
   size_t  ssize;
   int	   args[1];	/* a fudge.  sizeof() >= 1		*/
};

struct dbls {
   generic_t *x;
   size_t  ssize;
   double  args[1];	/* a fudge.  sizeof() >= 1		*/
};

struct expr {
   generic_t *x;
   size_t  ssize;
   struct ints  *keys;
   struct dbls  *args;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
