/******************************************************************************
** $Id: rsc.h,v 2.13 1996/02/22 21:43:51 gerd Exp gerd $
**=============================================================================
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/


#ifdef RSC_INIT
#define RscNumeric(SYM,S,V,I) int    V = I;
#define RscString(SYM,S,V,I)  char * V = I;
#define RscBoolean(SYM,S,V,I) int    V = I;
#define RscByFct(SYM,S,FCT)   
#define DECLARE(TYPE,VAR,VAL) TYPE VAR = VAL
#else
#define RscNumeric(SYM,S,V,I) extern int    V;
#define RscString(SYM,S,V,I)  extern char * V;
#define RscBoolean(SYM,S,V,I) extern int    V;
#define RscByFct(SYM,S,FCT)   
#define DECLARE(TYPE,VAR,VAL) extern TYPE VAR
#endif

#include "resource.h"

DECLARE( int   , rsc_select       , FALSE  			);

DECLARE( char* , rsc_e_rsc        , RSC_BIBTOOL			);
DECLARE( char* , rsc_v_rsc        , RSC_BIBTOOL_DEFAULT		);


#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int load_rsc _ARG((char *name));		   /* rsc.c                  */
 int search_rsc _ARG((void));			   /* rsc.c                  */
 int set_rsc _ARG((char * name,char * val));	   /* rsc.c                  */
 void rsc_print _ARG((char *s));		   /* rsc.c                  */
 void use_rsc _ARG((char * s));			   /* rsc.c                  */
