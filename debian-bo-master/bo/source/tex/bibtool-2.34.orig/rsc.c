/******************************************************************************
** $Id: rsc.c,v 2.17 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "error.h"
#include "symbols.h"
#include "macros.h"
#include "parse.h"
#include "tex_aux.h"
#include "tex_read.h"
#include "key.h"
#include "type.h"
#include "rewrite.h"
#include "s_parse.h"
#include "entry.h"

#define RSC_INIT
#include "rsc.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 int load_rsc _ARG((char *name));		   /* rsc.c		     */
 int search_rsc _ARG((void));			   /* rsc.c		     */
 int set_rsc _ARG((char * name,char * val));	   /* rsc.c		     */
 static int test_true _ARG((char * s));		   /* rsc.c		     */
 static void init_rsc _ARG((void));		   /* rsc.c		     */
 void rsc_print _ARG((char *s));		   /* rsc.c		     */
 void use_rsc _ARG((char * s));			   /* rsc.c		     */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

 extern void save_input_file _ARG((char *file));   /* main.c		     */
 extern void save_macro_file _ARG((char * file));  /* main.c		     */
 extern void save_output_file _ARG((char * file)); /* main.c		     */
 extern void usage _ARG((int full)); 		   /* main.c		     */

#ifndef __STDC__
#ifndef HAS_getenv
 extern char * getenv _ARG((char* name));	   /*			     */
#endif
#endif

/*---------------------------------------------------------------------------*/

#define RscNumeric(SYM,S,V,I) static char *S = NULL;
#define RscString(SYM,S,V,I)  static char *S = NULL;
#define RscBoolean(SYM,S,V,I) static char *S = NULL;
#define RscByFct(SYM,S,FCT)   static char *S = NULL;
#include "resource.h"

/*-----------------------------------------------------------------------------
** Function:	init_rsc()
** Purpose:	Resource Initializations.
**		Initialize the key words.
**		Tricky use of CPP. The header file resource.h is included 
**		for the second time. To avoid confusion in makedepend the 
**		hader file is only included if MAKEDPEND is not defined.
**		The same trick can be found again later.
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
static void init_rsc()				   /*			     */
{						   /*			     */
#define RscNumeric(SYM,S,V,I)  S = symbol(SYM);
#define RscString(SYM,S,V,I)   S = symbol(SYM);
#define RscBoolean(SYM,S,V,I)  S = symbol(SYM);
#define RscByFct(SYM,S,FCT)    S = symbol(SYM);
#ifndef MAKEDEPEND
#include "resource.h"
#endif
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	search_rsc()
** Purpose:	Try to open the resource file at different places:
**		- In the place indicated by the environment variable
**		  RSC_ENV_VAR
**		- In the HOME directory
**		- in the usual place for resource files.
** Arguments:	none
** Returns:	
**___________________________________________________			     */
int search_rsc()				   /*			     */
{ static char	*def = DefaultResourceFile;	   /*			     */
  register char *ap;				   /*			     */
  register char *fn;				   /*			     */
  int		l;				   /*			     */
						   /*			     */
#ifdef RSC_ENV_VAR
  if ( (ap=getenv(RSC_ENV_VAR)) != NULL		   /* Try to get the name    */
      && load_rsc(ap) ) return TRUE;		   /*  from the environment. */
#endif
						   /*			     */
#ifdef HOME_ENV_VAR
  if ( (ap=getenv(HOME_ENV_VAR)) != NULL )	   /* Try to get the resource*/
  { l = strlen(ap)+strlen(def)+2;		   /*  file from the home    */
    if ( (fn=malloc(l)) != NULL )		   /*  directory	     */
    { (void)strcpy(fn,ap);			   /*			     */
      (void)strcat(fn,DIR_SEP);			   /*			     */
      (void)strcat(fn,def);			   /*			     */
      l = load_rsc(fn);				   /*			     */
      free(fn);					   /*			     */
      if ( l ) return TRUE;			   /*			     */
    }						   /*			     */
  }						   /*			     */
#endif
						   /* if all fails then try  */
  return load_rsc(def);				   /*  to use a default.     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	load_rsc()
** Purpose:	Load a resource file.
** 		Perform initialization if required.
** 		The main job is done by read_rsc(). This function is located
** 		in parse.c since it shares subroutines with the parser.
** Arguments:
**	name
** Returns:	
**___________________________________________________			     */
int load_rsc(name)				   /*			     */
  register char *name;				   /*			     */
{						   /*			     */
  if ( r_v == NULL ) { init_rsc(); }		   /*			     */
  return ( name != NULL ? read_rsc(name) : 0 );	   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	test_true()
** Purpose:	A boolean resource can be set to true in different ways:
**		'true', 't', 'yes, 'on', '1'
**		all represent true. Each other value is interpreted as false.
**		The comparison is done case insensitive.
** Arguments:
**	s
** Returns:	
**___________________________________________________			     */
static int test_true(s)				   /*			     */
  register char * s;				   /*			     */
{						   /*			     */
  return (  case_cmp(s,"true")			   /*			     */
	 || case_cmp(s,"t")			   /*			     */
	 || case_cmp(s,"yes")			   /*			     */
	 || case_cmp(s,"on")			   /*			     */
	 || case_cmp(s,"1")			   /*			     */
	 );					   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	use_rsc()
** Purpose:	Entry point for command line options which set resources.
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void use_rsc(s)					   /*			     */
  char		*s;				   /*			     */
{ register char *name,				   /*			     */
		*value;				   /*			     */
						   /*			     */
  (void)sp_open(s);				   /*			     */
  if ( (name = SParseSymbol(&s)) == NULL ) return; /*			     */
						   /*			     */
  (void)SParseSkip(&s);				   /*			     */
						   /*			     */
  if ( (value = SParseValue(&s)) == NULL ) return; /*			     */
						   /*			     */
  if ( r_v == NULL ) { init_rsc(); }		   /*			     */
						   /*			     */
  (void)set_rsc(name,value);			   /*			     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	rsc_print()
** Purpose:	Print a string to the error stream.
**		Wrapper function used for resource print.
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void rsc_print(s)				   /*			     */
  register char *s;				   /*			     */
{ ErrPrintF("%s\n",s);				   /* print the string itself*/
			   			   /* followed by a newline  */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	set_rsc()
** Purpose:	Set the resource to a given value.
**		Both arguments are assumed to be symbols.
** Arguments:
**	name
**	val
** Returns:	
**___________________________________________________			     */
int set_rsc(name,val)				   /*			     */
  register char * name;				   /*			     */
  register char * val;				   /*			     */
{						   /*			     */
  if ( rsc_verbose )				   /*			     */
    VerbosePrint4("Resource ",name," = ",(val==NULL?"*NULL*":val));/*	     */
						   /*			     */
#define RscNumeric(SYM,S,V,I) if( name==S ) { V = atoi(val);	  return(0); }
#define RscString(SYM,S,V,I)  if( name==S ) { V = val;		  return(0); }
#define RscBoolean(SYM,S,V,I) if( name==S ) { V = test_true(val); return(0); }
#define RscByFct(SYM,S,FCT)   if( name==S ) { (void)FCT;	  return(0); }
#define RSC_FIRST(C)	      case C:
#define RSC_NEXT(C)	      break; case C:
						   /*			     */
  switch( *name )				   /*			     */
  {						   /*			     */
#ifndef MAKEDEPEND
#include "resource.h"
#endif
  }						   /*			     */
						   /*			     */
  ERROR3("Resource ",name," unknown.");		   /*			     */
  return(-1);					   /*			     */
}						   /*------------------------*/
