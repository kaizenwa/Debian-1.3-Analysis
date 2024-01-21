/******************************************************************************
** $Id: tex_aux.c,v 1.11 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "rsc.h"
#include "type.h"
#include "error.h"
#include "rewrite.h"
#include "symbols.h"
#include "sbuffer.h"
#include "wordlist.h"
#ifdef REGEX
#include "regex.h"
#endif

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#include "tex_aux.h"

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

 void save_input_file _ARG((char *file));	   /* main.c                 */

/*---------------------------------------------------------------------------*/

 static StringBuffer* aux_sb = (StringBuffer*)0;

 static int cite_star        = TRUE;

 static WordList cite[32]    = 
 { WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL,
   WordNULL, WordNULL, WordNULL, WordNULL
 };

/*-----------------------------------------------------------------------------
** Function:	save_ref()
** Purpose:	Store the cite key for selection.
**		If the cite key is "*" then all should be selected.
**		This is written e.g. by \nocite{*}
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void save_ref(s)				   /*                        */
  register char *s;				   /*                        */
{ 						   /*                        */
  if ( cite_star ) return;			   /*                        */
 						   /*                        */
  if ( *s == '*' && *(s+1) == '\0' )		   /*                        */
  { cite_star = TRUE; }				   /*                        */
  else { save_word(symbol(s),&cite[*s&31]); }	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	lookup_item()
** Purpose:	Decide if a given record has been selected.
** Arguments:
**	rec	
** Returns:	
**___________________________________________________			     */
int lookup_item(rec)				   /*                        */
  Record rec;				   	   /*                        */
{						   /*                        */
  register char *s;				   /*                        */
  s = *RecordHeap(rec);				   /*                        */
  if ( !is_selected(rec) ) return FALSE;	   /*                        */
 						   /*                        */
  return (  cite_star				   /*                        */
	 || find_word(s,cite[*s&31])  );	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	read_aux()
** Purpose:	Analyze an aux file.
** Arguments:
**	fname
**	verbose
** Returns:	nothing
**___________________________________________________			     */
void read_aux(fname,verbose)			   /*                        */
  char 	        *fname;				   /* aux file name          */
  int           verbose;			   /*                        */
{ FILE 	        *file;				   /*                        */
  int           c;				   /*                        */
  register char *s;				   /*                        */
 						   /*                        */
  cite_star = FALSE;				   /*                        */
  rsc_select = TRUE; 				   /*                        */
  					   	   /*                        */
  if ( (file=fopen(fname,"r") ) == NULL )	   /*                        */
  { ERROR3("aux File ",fname," not found.");	   /*                        */
    return;					   /*                        */
  }						   /*                        */
   						   /*                        */
  if ( verbose ) 				   /*                        */
  { VerbosePrint2("Analyzing ",fname); }	   /*                        */
 						   /*                        */
  if ( aux_sb == (StringBuffer*)0 )		   /*                        */
  { aux_sb = sbopen(); }			   /*                        */
 						   /*                        */
  while ( (c=getc(file)) != EOF )		   /*                        */
  { if ( c == '\\' ) 				   /*                        */
    {						   /*                        */
      c = getc(file);				   /*                        */
      while (is_alpha(c) || c == '@')		   /*                        */
      { (void)sbputchar(c,aux_sb); c = getc(file); }/*                       */
      s = sbflush(aux_sb);			   /*                        */
      sbrewind(aux_sb);				   /*                        */
 						   /*                        */
      if ( strcmp(s,"citation" )==0 )	   	   /*                        */
      { do					   /* Save a cite key.       */
	{ switch ( c=getc(file) )		   /*                        */
	  { case EOF: break;			   /*                        */
	    case ',':				   /*                        */
	    case '}':		   	   	   /*                        */
	      s = sbflush(aux_sb);		   /*                        */
	      sbrewind(aux_sb);			   /*                        */
	      save_ref(s);	   		   /*                        */
	      break;				   /*                        */
	    default:				   /*                        */
	      c = ToLower(c);  			   /*                        */
	      (void)sbputchar(c,aux_sb);	   /*                        */
	  }	   				   /*                        */
	} while ( c != '}' && c != EOF );	   /*                        */
      }						   /*                        */
      else if ( strcmp(s,"bibdata" )==0 )	   /*                        */
      { c = getc(file);				   /* Save a bib file name   */
	(void)sbputchar(c,aux_sb);		   /*                        */
	while ( c != '}' && c != EOF )		   /*                        */
	{ c = getc(file);			   /*                        */
	  if ( c == ',' ||  c == '}' )		   /*                        */
	  { s = sbflush(aux_sb);		   /*                        */
	    sbrewind(aux_sb);			   /*                        */
	    save_input_file(symbol(s));		   /*                        */
	  }					   /*                        */
	  else					   /*                        */
	  { (void)sbputchar(c,aux_sb); }	   /*                        */
	}					   /*                        */
      }						   /*                        */
      else if ( strcmp(s,"@input" )==0 )	   /* Read another aux file  */
      { while( (c=getc(file)) != '}' && c != EOF ) /*                        */
	{ (void)sbputchar(c,aux_sb); }		   /*                        */
	s = sbflush(aux_sb);			   /*                        */
	sbrewind(aux_sb);			   /*                        */
	    					   /*                        */
	read_aux(s,verbose);			   /*                        */
      }						   /*                        */
    }						   /*                        */
  }						   /*                        */
 						   /*                        */
  (void)fclose(file);				   /*                        */
 						   /*                        */
#ifdef DEBUG
  { register int i;				   /*                        */
 						   /*                        */
    for ( i=0; i<32; ++i )			   /*                        */
    { ErrPrintF("--- BibTool: %d\n",i);	   	   /*                        */
      list_words(cite[i]);			   /*                        */
    }		   	   			   /*                        */
  }						   /*                        */
#endif
}						   /*------------------------*/
