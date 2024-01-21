/******************************************************************************
** $Id: symbols.c,v 2.14 1996/02/22 21:43:51 gerd Exp gerd $
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

#include "bibtool.h"
#include "symbols.h"
#include "error.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 StringTab new_string_tab _ARG((char *name,int count,int flags));/* symbols.c*/
 char * new_string _ARG((char * s));		   /* symbols.c              */
 char * sym_add _ARG((char *s,int count));	   /* symbols.c              */
 char * sym_extract _ARG((char *ap,char *ep,int count));/* symbols.c         */
 int hashindex _ARG((char *s));			   /* symbols.c              */
 int sym_flag _ARG((void));			   /* symbols.c              */
 void init_symbols _ARG((void));		   /* symbols.c              */
 void sym_dump _ARG((void));			   /* symbols.c              */
 void sym_set_flag _ARG((int flags));		   /* symbols.c              */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/


#ifdef HASH_TABLE_SIZE
#define HASHMAX HASH_TABLE_SIZE
#else
#define HASHMAX 307
#endif


/*****************************************************************************/
/***			Misc string allocation routine			   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	new_string()
** Purpose:	Allocate a space for a string and save the argument there.
** Arguments:
**	s	String to duplicate
** Returns:	
**___________________________________________________			     */
char * new_string(s)				   /*			     */
  register char * s;				   /*			     */
{ register char * t;				   /*			     */
  if ( (t=malloc((size_t)strlen(s)+1)) == NULL )   /*			     */
  { OUT_OF_MEMORY("string"); }	   		   /*			     */
  (void)strcpy(t,s);				   /*			     */
  return(t);					   /*			     */
}						   /*------------------------*/


/*****************************************************************************/
/***			     Symbol Table Section			   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	new_string_tab()
** Purpose:	Allocate a new StringTab structure and fill it with initial
**		values.
** Arguments:
**	name
**	count
**	flags
** Returns:	
**___________________________________________________			     */
StringTab new_string_tab(name,count,flags)	   /*			     */
  char		     *name;			   /*			     */
  int		     count;			   /*			     */
  int		     flags;			   /*			     */
{ register StringTab new;			   /*			     */
						   /*			     */
  if ( (new=(StringTab)malloc(sizeof(struct STAB))) == 0L )/*		     */
  { OUT_OF_MEMORY("StringTab"); }   		   /*			     */
  SymbolName(new)  = name;			   /*			     */
  SymbolCount(new) = count;			   /*			     */
  SymbolFlags(new) = flags;			   /*			     */
  NextSymbol(new)  = (StringTab)0;		   /*			     */
  return(new);					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	hashindex()
** Purpose:	Compute the sum of ASCII values modulo HASHMAX
**		to be used as an hashindex.
** Arguments:
**	s	string to be analyzed.
** Returns:	hashindex
**___________________________________________________			     */
int hashindex(s)				   /*                        */
  char *s;					   /*                        */
{ int	index=0;				   /*                        */
  while ( *s ) index = (index+*(s++)) % HASHMAX;   /*                        */
  return ( index < 0 ? -index : index );	   /*                        */
}						   /*------------------------*/

 static StringTab sym_tab[HASHMAX];

/*-----------------------------------------------------------------------------
** Function:	init_symbols()
** Purpose:	Initialize symbols.
**		The symbol table is cleared.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
void init_symbols()				   /*			     */
{ register int i;				   /*			     */
						   /*			     */
  for ( i=0; i<HASHMAX; i++ ) sym_tab[i] = NULL;   /*			     */
}						   /*------------------------*/


 static StringTab last_stp;	

/*-----------------------------------------------------------------------------
** Function:	sym_flag()
** Purpose:	Get the flags of the last sym_add() symbol.
** Arguments:
**	
** Returns:	
**___________________________________________________			     */
int sym_flag()					   /*			     */
{ return SymbolFlags(last_stp);			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sym_set_flag()
** Purpose:	Set the flags of the last sym_add() symbol.
** Arguments:
**	flags
** Returns:	nothing
**___________________________________________________			     */
void sym_set_flag(flags)			   /*			     */
  register int flags;				   /*			     */
{ SymbolFlags(last_stp) = flags;		   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	sym_add()
** Purpose:	Add a symbol to the global symbol table.
** Arguments:
**	s
**	count
** Returns:	
**___________________________________________________			     */
char * sym_add(s,count)				   /*			     */
  register char	     *s;			   /*			     */
  register int	     count;			   /*			     */
{ register StringTab *stp;			   /*			     */
						   /*			     */
  if ( s == NULL ) return(NULL);		   /* ignore dummies.	     */
 						   /*                        */
  for ( stp = &sym_tab[hashindex(s)];		   /*			     */
       *stp != NULL;		   		   /*			     */
        stp = &NextSymbol(*stp) )		   /*			     */
  { if ( strcmp(s,SymbolName(*stp)) == 0 )	   /*			     */
    { if ( count>0 ) SymbolCount(*stp) += count;   /*			     */
      last_stp = *stp;			   	   /*			     */
      return SymbolName(*stp);			   /*			     */
    }						   /*			     */
  }						   /*			     */
  if ( count<0 ) { count = 0;		  }	   /*			     */
  else		 { s	 = new_string(s); }	   /*			     */
  *stp	   = new_string_tab(s,count,0);		   /*			     */
  last_stp = *stp;				   /*			     */
  return SymbolName(*stp);			   /*			     */
}						   /*------------------------*/

#ifdef New
/*-----------------------------------------------------------------------------
** Function:	sym_extract()
** Purpose:	Extract a symbol from a string.
** Arguments:
**	ap
**	ep
**	count
** Returns:	
**___________________________________________________			     */
char * sym_extract(ap,ep,count)			   /*			     */
  register char *ap;				   /* pointer to first char  */
  register char *ep;				   /* pointer after last char*/
  register int	count;				   /*			     */
{ char c;					   /*			     */
						   /*			     */
  c   = *ep;					   /*			     */
  *ep = '\0';					   /*			     */
  ap  = sym_add(ap,count<0?0:count);		   /*			     */
  *ep = c;					   /*			     */
  return ap;					   /*			     */
}						   /*------------------------*/
#endif

#ifdef SYMBOL_DUMP
/*-----------------------------------------------------------------------------
** Function:	sym_dump()
** Purpose:	Dump the symbol table to the error stream.
** Arguments:
**	
** Returns:	nothing
**___________________________________________________			     */
void sym_dump()					   /*			     */
{ register int	     i,l;			   /*			     */
  register StringTab st;			   /*			     */
  register long	     len  = 0l;			   /*			     */
  register long	     cnt  = 0l;			   /*			     */
  register long	     used = 0l;			   /*			     */
						   /*			     */
  for ( i=0;i<HASHMAX;i++ )			   /*			     */
  { for ( st = sym_tab[i]; st; st=NextSymbol(st) ) /*			     */
    { ErrPrintF2("--- BibTool symbol %4d %s\n",	   /*			     */
		 (int)SymbolCount(st),		   /*			     */
		 SymbolName(st));		   /*			     */
      l     = strlen(SymbolName(st))+1;		   /*			     */
      len  += l;				   /*			     */
      used += l*SymbolCount(st);		   /*                        */
      ++cnt;					   /*			     */
    }						   /*			     */
  }						   /*			     */
  ErrPrintF2("--- BibTool symbol table: %ld bytes for %ld symbols\n",/*	     */
	     len, cnt);				   /*			     */
  ErrPrintF("--- BibTool symbol table emulates %ld bytes\n",/*		     */
	     used);				   /*			     */
}						   /*------------------------*/
#endif

