/******************************************************************************
** $Id: pxfile.c,v 2.18 1996/02/22 21:43:51 gerd Exp gerd $
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
** 
** Purpose: Routines for extended file handling.
**	    Open files after search. 
**	    Patterns may be given which are used to determine the full file 
**	    name.
**	    A function is provided to allocate a pattern structure and
**	    fill it from a string specification.
**	    
******************************************************************************/

#include "config.h"
#include "pxfile.h"

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 static int absolute_file _ARG((char *n,char **base,char ***p));/* pxfile.c  */

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

#ifdef __STDC__

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#define SIZE_T		size_t

#else

 extern VoidPTR malloc();
 extern VoidPTR realloc();
#ifdef OLD_STYLE_STRINGS
 extern char * rindex();
#define strrchr(A,B) rindex(A,B)
#else
 extern char * strrchr();
#endif

#define SIZE_T		unsigned int

#endif

/*****************************************************************************/

 char	       *px_filename   = "" ;
 static SIZE_T px_len	      = 0  ;

 static char   * no_path[]    = { "."		 , NULL };
 static char   * no_pattern[] = { DEFAULT_PATTERN, NULL };

/*-----------------------------------------------------------------------------
** Function:	absolute_file()
** Purpose:	Check is the file is a absolute file name.
**		In this case make a new path array which contains the absolute
**		path only and its location in path. basename is the modified
**		file name. The return value is TRUE
**		Otherwise FALSE is returned and nothing changed.
**
**		Currently UN*X and MSDOS like file names are supported.
** Arguments:
**	name
**	basename
**	path
** Returns:	
**___________________________________________________			     */
static int absolute_file(name,basename,path)	   /*                        */
  char        *name;				   /*                        */
  char        **basename;			   /*                        */
  char        ***path;				   /*                        */
{ static char *absolut_path[2];			   /*                        */
  static int  first = (-1);			   /*                        */
  int         l;				   /*                        */
  char        *sp;				   /*                        */
#undef SEPARATOR
  						   /*                        */
#ifdef MSDOS
#define SEPARATOR '\\'
  if ( *name != SEPARATOR			   /* starting with root or  */
      && ( *name == '\0'			   /*                        */
	  || *(name+1) != ':'		   	   /*                        */
	  || *(name+2) != SEPARATOR ) )	   	   /*	with drive 	     */
#endif
#ifdef AMIGA
#define SEPARATOR '/'
  if ( strchr(name,':') == NULL )		   /* starting with root     */
#endif
#ifndef SEPARATOR
#define SEPARATOR '/'
  if ( *name != SEPARATOR )			   /* starting with root     */
#endif
  { return 0; }					   /*                        */
 						   /*                        */
  sp = strrchr(name,SEPARATOR);			   /* find last separator    */
  						   /* existence guarenteed!  */
#ifdef AMIGA
  if ( sp == NULL ) sp = strchr(name,':');	   /*                        */
#endif
  l = (int)(sp-name);				   /* length of directory.   */
  if ( first )				   	   /*                        */
  { first = 0;					   /*                        */
    absolut_path[1] = NULL;			   /* mark end of array.     */
    absolut_path[0] = malloc(l+1);		   /* allocate               */
  }						   /*  or                    */
  else						   /*  reallocate            */
  { absolut_path[0] = realloc(*absolut_path,l+1);  /*  space for the         */
  }						   /*  directory name        */
 						   /*                        */
  (void)strncpy(absolut_path[0],name,l);	   /* save directory name    */
  *(absolut_path[0]+l) = '\0';			   /* and mark the end.      */
 						   /*                        */
  *basename = name+l+1;				   /* return the base name   */
  *path = absolut_path;				   /* return the path array  */
  						   /*                        */
  return 1;					   /* finished.              */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	px_fopen()
** Purpose:	Open a file using path and pattern.
** Arguments:
**	name
**	mode
**	pattern
**	path
**	show
** Returns:	
**___________________________________________________			     */
FILE * px_fopen(name,mode,pattern,path,show)	   /*			     */
  char	* name;					   /*			     */
  char	* mode;					   /*			     */
  char	**pattern;				   /*			     */
  char	**path;					   /*			     */
  int	(*show)_ARG((char*));			   /*			     */
{ char	**fmt;					   /*			     */
  char	**pp;					   /*			     */
  FILE	* file;					   /*			     */
  SIZE_T len,plen;				   /*			     */
  char  **new_path;				   /*                        */
  char  *new_name;				   /*                        */
						   /*			     */
  if (	 path  == NULL )    path = no_path;	   /* use default path	     */
  if ( pattern == NULL ) pattern = no_pattern;	   /* use default pattern    */
						   /*			     */
  if ( absolute_file(name,&new_name,&new_path) )   /*			     */
  { name = new_name;				   /*                        */
    path = new_path;				   /*                        */
  }						   /*			     */
						   /*			     */
  for ( plen=0,fmt=pattern;*fmt;++fmt )		   /* compute longest	     */
  { len = strlen(*fmt);				   /*	pattern		     */
    if ( len > plen ) plen = len;		   /*			     */
  }						   /*			     */
  plen += 1+strlen(name);			   /* add length of name     */
						   /*	and 1 for '\0'	     */
  if ( px_len == 0 &&				   /* initial filename	     */
       (px_filename = malloc(plen))==NULL )	   /*	then allocate or     */
    return(NULL);				   /*	terminate	     */
  px_len = plen-1;				   /* usable is 1 less	     */
						   /*			     */
  for ( pp=path;*pp;++pp )			   /* use all paths	     */
  { len = strlen(*pp) + plen;			   /* required space	     */
    if ( px_len < len )				   /* if less present get it */
    { if ( (px_filename = realloc(px_filename,len))==NULL )/*		     */
	return(NULL);				   /*                        */
      px_len = len-1;				   /* usable is 1 less	     */
    }						   /*			     */
						   /*			     */
    for ( fmt=pattern;*fmt;++fmt )		   /* use all patterns	     */
    { (void)sprintf(px_filename,*fmt,*pp,name);	   /* construct new filename */
      if ( ( show == NULL ||			   /* if function present    */
	     (*show)(px_filename) ) &&		   /*	and it says yes	     */
	   (file=fopen(px_filename,mode)) != NULL )/*  and open succeeds     */
	return(file);				   /* then return file ptr   */
    }						   /*			     */
  }						   /*			     */
  *px_filename = '\0';				   /* clear filename	     */
  return(NULL);					   /* failure		     */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	px_s2p()
** Purpose:	Translate a path string specification into an array of the 
**		components.
**		The memory of the array is malloced and should be freed when
**		not used any longer.
** Arguments:
**	s
**	sep
** Returns:	The array of the components
**___________________________________________________			     */
char **px_s2p(s,sep)				   /*			     */
  char		  * s;				   /*			     */
  int		  sep;				   /*			     */
{ register char	  *cp;				   /*			     */
  register SIZE_T n, l;				   /*			     */
  char		  **pattern;			   /*			     */
						   /*			     */
  if ( s == NULL ) return(NULL);		   /* No string to analyze   */
						   /*			     */
  for ( n=2,l=1,cp=s; *cp; ++l,++cp )		   /* Count the lenght and   */
    if ( *cp == sep ) ++n;			   /*  number of fields	     */
						   /*			     */
  if ( (pattern=(char**)malloc( n*sizeof(char*)	   /*			     */
			       +l*sizeof(char)))   /*			     */
      == (char**)NULL )				   /* Try to allocate space  */
  { return(NULL); }				   /*			     */
						   /*			     */
  pattern[n-1] = 0L;				   /* Mark end		     */
  (void)strcpy((char*)&pattern[n],s);		   /* save the string spec   */
  s = (char *) &pattern[n];			   /*			     */
  pattern[0] = s;				   /*			     */
  for ( l=0,cp=s; *cp; ++cp )			   /*			     */
  { if ( *cp == sep )				   /*			     */
    { *cp = '\0'; pattern[++l] = cp+1; }	   /*			     */
  }						   /*			     */
						   /*			     */
  return(pattern);				   /*			     */
}						   /*------------------------*/
