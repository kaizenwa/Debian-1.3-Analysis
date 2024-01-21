/******************************************************************************
** $Id: macros.c,v 2.18 1996/02/22 21:43:51 gerd Exp gerd $
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
#include "type.h"
#include "macros.h"
#include "sbuffer.h"

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 Macro new_macro _ARG((char *name,char *val,Macro next,int count));/* macros.c*/
 char * get_item _ARG((char * name,int type));	   /* macros.c               */
 char * get_key_name _ARG((char *symbol));	   /* macros.c               */
 char * look_macro _ARG((char *name,int add));	   /* macros.c               */
 static char * get_mapped_or_cased _ARG((char * name,Macro mac,int type));/* macros.c*/
 void def_field_type _ARG((char * s));		   /* macros.c               */
 void def_item _ARG((char * name,char * value));   /* macros.c               */
 void def_macro _ARG((char *name,char *val,int count));/* macros.c           */
 void dump_mac _ARG((char *fname,int allp));	   /* macros.c               */
 void init_macros _ARG((void));			   /* macros.c               */
 void save_key _ARG((char * symbol,char * key));   /* macros.c               */

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/

 static Macro macros = MacNull;			   /*                        */

/*-----------------------------------------------------------------------------
** Function:	new_macro()
** Purpose:	Allocate a new macro structure and fill it with initial values.
**		Upon failure exit() is called.
** Arguments:
**	name
**	val
**	next
**	count
** Returns:	The new Macro
**___________________________________________________			     */
Macro new_macro(name,val,next,count)		   /*                        */
  register char  *name;				   /*                        */
  register char  *val;				   /*                        */
  register int   count;				   /*                        */
  Macro          next;				   /*                        */
{ register Macro new;				   /*                        */
 						   /*                        */
  if ( (new=(Macro)malloc(sizeof(SMacro))) == MacNull )/*                    */
  { OUT_OF_MEMORY("Macro"); }      		   /*                        */
 						   /*                        */
  MacroName(new)  = name;			   /*                        */
  MacroVal(new)   = val ;			   /*                        */
  MacroCount(new) = count;			   /*                        */
  NextMacro(new)  = next;			   /*                        */
  return(new);					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	def_macro()
** Purpose:	Define a macro. Give a warning if it is already defined.
** Arguments:
**	name
**	val
**	count
** Returns:	nothing
**___________________________________________________			     */
void def_macro(name,val,count)			   /*                        */
  char           *name;				   /*                        */
  char           *val;				   /*                        */
  int		 count;				   /*                        */
{ register Macro *mp;				   /*                        */
 						   /*                        */
  name = symbol(name);				   /*                        */
  val  = symbol(val);				   /*                        */
 						   /*                        */
  for ( mp   = &macros;				   /*                        */
        *mp != MacNull;				   /*                        */
	mp   = &NextMacro(*mp)			   /*                        */
      )						   /*                        */
  { if ( MacroName(*mp) == name )		   /*                        */
    { WARNING3("Macro '",name,"' is already defined.");/*                    */
      MacroVal(*mp) = val;			   /*                        */
      return;					   /*                        */
    }						   /*                        */
  }						   /*                        */
 						   /*                        */
  macros = new_macro(name,val,macros,count);	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	look_macro()
** Purpose:	Return the value of a macro.
**		If the macro is undefined its name is returned.
** Arguments:
**	name
**	add
** Returns:	The value.
**___________________________________________________			     */
char * look_macro(name,add)			   /*                        */
  char           *name;				   /*                        */
  int		 add;				   /*                        */
{ register Macro *mp;				   /*                        */
						   /*                        */
  name = symbol(name);				   /*                        */
						   /*                        */
  for ( mp   = &macros;				   /*                        */
        *mp != MacNull;				   /*                        */
	mp   = &NextMacro(*mp)			   /*                        */
      )						   /*                        */
  { if ( MacroName(*mp) == name )		   /*                        */
    { if ( MacroCount(*mp) >= 0 )		   /*                        */
	MacroCount(*mp) += add;			   /*                        */
      return(MacroVal(*mp));			   /*                        */
    }						   /*                        */
  }						   /*                        */
  def_macro(name,name,add);			   /*                        */
  WARNING3("Macro '",name,"' is undefined.");	   /*                        */
  return(name);					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	dump_mac()
** Purpose:	Write macros to a file.
** Arguments:
**	fname	file name of the target file.
**	allp	if ==0 only the used macros are written.
** Returns:	nothing
**___________________________________________________			     */
void dump_mac(fname,allp)			   /*                        */
  char           *fname;			   /*                        */
  int            allp;				   /*                        */
{ FILE           *file;				   /*                        */
  register Macro mac;				   /*                        */
 						   /*                        */
  if ( *fname == '-' && *(fname+1) == '\0' ) 	   /*                        */
  { file = stdout; }				   /*                        */
  else if ( (file=fopen(fname,"w")) == NULL )      /*                        */
  { ERROR2("File could not be opened: ",fname);	   /*                        */
    return;					   /*                        */
  }					   	   /*                        */
 						   /*                        */
  for ( mac=macros; 				   /*                        */
	mac != MacNull; 			   /*                        */
	mac = NextMacro(mac) )			   /*                        */
  { if ( MacroCount(mac) > 0  ||		   /*                        */
         ( MacroCount(mac) >= 0 && allp) )	   /*                        */
    { if ( MacroName(mac) == MacroVal(mac) )
      { (void)fprintf(file,			   /*                        */
		      "_string{ %s = %s } used: %d\n",/*                     */
		      MacroName(mac),		   /*                        */
		      MacroVal(mac),		   /*                        */
		      MacroCount(mac));		   /*                        */
      }
      else
      { (void)fprintf(file,			   /*                        */
		      "@STRING{ %s = %s } used: %d\n",/*                     */
		      MacroName(mac),		   /*                        */
		      MacroVal(mac),		   /*                        */
		      MacroCount(mac));		   /*                        */
      }
    }						   /*                        */
  }						   /*                        */
 						   /*                        */
  if ( file != stdout ) (void)fclose(file);	   /*                        */
}						   /*------------------------*/

#define DeclareMacro(M) name = symbol(M); def_macro(name,name,-1)

/*-----------------------------------------------------------------------------
** Function:	init_macros()
** Purpose:	Initialize some macros from a table
** Arguments:	none
** Returns:	nothing
**___________________________________________________			     */
void init_macros()				   /*                        */
{ 						   /*                        */
#ifdef INITIALIZE_MACROS	
  register char *name;				   /*                        */
  register char**wp;				   /*			     */
  static char *word_list[] =			   /*                        */
  { INITIALIZE_MACROS, NULL };			   /*                        */
 						   /*                        */
  for ( wp=word_list; *wp!=NULL; ++wp )		   /*                        */
  { DeclareMacro(*wp); }			   /*			     */
#endif
}						   /*------------------------*/


/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

 static Macro items = MacNull;		

/*-----------------------------------------------------------------------------
** Function:	def_item()
** Purpose:	
**		
** Arguments:
**	name
**	value
** Returns:	nothing
**___________________________________________________			     */
void def_item(name,value)			   /*                        */
  register char * name;				   /*                        */
  register char * value;			   /*                        */
{						   /*                        */
  name  = symbol(name);				   /*                        */
  value = symbol(value);			   /*                        */
  items = new_macro(name,value,items,0);	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	def_field_type()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	nothing
**___________________________________________________			     */
void def_field_type(s)				   /*                        */
  char * s;					   /*                        */
{ char *name, *val, c;				   /*                        */
 						   /*                        */
  while ( *s && !is_allowed(*s) ) ++s;		   /*                        */
  if ( *s == '\0' ) return;			   /*                        */
  name = s;					   /*                        */
  while ( is_allowed(*s) ) ++s;			   /*                        */
  c = *s; *s = '\0'; val = new_string(name); *s = c;/*                       */
 						   /*                        */
  { register char * cp;				   /*                        */
    for ( cp=val; *cp; ++cp ) *cp = ToLower(*cp);  /*                        */
  }						   /*                        */
 						   /*                        */
  name = symbol(val);				   /*                        */
  free(val);					   /*                        */
  						   /*                        */
  while ( *s && !is_allowed(*s) ) ++s;		   /*                        */
  if ( *s == '\0' ) return;			   /*                        */
  val = s;					   /*                        */
  while ( is_allowed(*s) ) ++s;			   /*                        */
  c = *s; *s = '\0'; val = symbol(val); *s = c;	   /*                        */
 						   /*                        */
  def_item(name,val);				   /*                        */
}						   /*------------------------*/


/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	get_mapped_or_cased()
** Purpose:	
**		
**
** Arguments:
**	name
**	mac
**	type
** Returns:	
**___________________________________________________			     */
static char * get_mapped_or_cased(name,mac,type)   /*                        */
  register char * name;				   /*                        */
  int            type;				   /*                        */
  register Macro mac;				   /*                        */
{ static StringBuffer* sb = (StringBuffer*)NULL;   /*                        */
 						   /*                        */
  for ( ; mac != MacNull; mac=NextMacro(mac) )	   /*                        */
  { if ( name == MacroName(mac) )		   /*                        */
      return MacroVal(mac);			   /*                        */
  }						   /*                        */
 						   /*                        */
  if ( type != SYMBOL_TYPE_LOWER ) 	   	   /*                        */
  { 						   /*                        */
    if ( sb == NULL &&				   /*                        */
	(sb=sbopen()) == NULL )			   /*                        */
    { OUT_OF_MEMORY("get_item()"); } 		   /*                        */
    						   /*                        */
    switch ( type )			   	   /*                        */
    { case SYMBOL_TYPE_CASED:			   /*                        */
	while ( *name )				   /*                        */
	{ if ( is_alpha(*name) )		   /*                        */
	  { (void)sbputc(ToUpper(*name),sb);	   /*                        */
	    for ( ++name; is_alpha(*name); ++name) /*                        */
	    { (void)sbputc(*name,sb); }		   /*                        */
	  }					   /*                        */
	  else					   /*                        */
	  { (void)sbputc(*name,sb);		   /*                        */
	    ++name;				   /*                        */
	  }					   /*                        */
	}					   /*                        */
	break;					   /*                        */
      case SYMBOL_TYPE_UPPER:			   /*                        */
	for ( ; *name; ++name )			   /*                        */
	{ (void)sbputc(ToUpper(*name),sb); }	   /*                        */
	break;					   /*                        */
      }						   /*                        */
    name = sbflush(sb);				   /*                        */
    sbrewind(sb);				   /*                        */
  }						   /*                        */
  return name;					   /*                        */
}						   /*------------------------*/



/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

/*-----------------------------------------------------------------------------
** Function:	get_item()
** Purpose:	
**		
**
** Arguments:
**	name
**	type
** Returns:	
**___________________________________________________			     */
char * get_item(name,type)			   /*                        */
  register char * name;				   /*                        */
  int            type;				   /*                        */
{ return get_mapped_or_cased(name,items,type);	   /*                        */
}						   /*------------------------*/


/*****************************************************************************/
/***									   ***/
/*****************************************************************************/

 static Macro keys = MacNull;			   /*                        */

/*-----------------------------------------------------------------------------
** Function:	save_key()
** Purpose:	
**		
**
** Arguments:
**	s
**	key
** Returns:	nothing
**___________________________________________________			     */
void save_key(s,key)			   	   /*                        */
  char * s;				   	   /*                        */
  char * key;					   /*                        */
{						   /*                        */
  keys = new_macro(s,key,keys,1);		   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	get_key_name()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	
**___________________________________________________			     */
char * get_key_name(s)		   	   	   /*                        */
  register char *s;			   	   /*                        */
{ return get_mapped_or_cased(s,keys,SYMBOL_TYPE_LOWER);/*                    */
}						   /*------------------------*/
