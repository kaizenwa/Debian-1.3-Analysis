/******************************************************************************
** $Id: symbols.h,v 2.11 1996/02/22 21:43:51 gerd Exp gerd $
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

#ifndef SYMBOLS_H_LOADED
#define SYMBOLS_H_LOADED

#define symbol(X) sym_add(X,1)

/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

typedef struct STAB
  { char	*st_name;
    int		st_count, 
		st_flags;
    struct STAB *st_next;
  } *StringTab;

#define NextSymbol(S)	((S)->st_next)
#define SymbolCount(S)	((S)->st_count)
#define SymbolName(S)	((S)->st_name)
#define SymbolFlags(S)	((S)->st_flags)

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 StringTab new_string_tab _ARG((char *name,int count,int flags));/* symbols.c*/
 char * new_string _ARG((char * s));		   /* symbols.c              */
 char * sym_add _ARG((char *s,int count));	   /* symbols.c              */
 char * sym_extract _ARG((char *ap,char *ep,int count));/* symbols.c         */
 int sym_flag _ARG((void));			   /* symbols.c              */
 void init_symbols _ARG((void));		   /* symbols.c              */
 void sym_dump _ARG((void));			   /* symbols.c              */
 void sym_set_flag _ARG((int flags));		   /* symbols.c              */


#endif /* SYMBOLS_H_LOADED */
