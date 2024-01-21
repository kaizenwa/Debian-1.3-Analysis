/******************************************************************************
** $Id: error.c,v 2.13 1996/02/22 21:43:51 gerd Exp gerd $
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

/*****************************************************************************/
/* Internal Programs							     */
/*===========================================================================*/

/*****************************************************************************/
/* External Programs							     */
/*===========================================================================*/

 extern int rsc_quiet;

/*---------------------------------------------------------------------------*/

 char * empty = "";
 char * point = ".";
 char * oom = "Out of memory for ";

 FILE * err_file = stderr;

#define ErrNL		(void)fputc('\n',err_file)
#define ErrChar(C)	(void)fputc(C,err_file)
#define ErrS(S)		(void)fputs(S,err_file)

/*-----------------------------------------------------------------------------
** Function:	error()
** Purpose:	Generic Error Printing Routine.
**
**		Print an error message together with a optional filename,
**		the line number, the errorous line and a pointer to the
**		problematic position.
** Arguments:
**	type	Error type: Combination of
**		  ERR_WARNING ERR_ERROR ERR_POINT ERR_FILE ERR_EXIT 
**	s1	1st error message
**	s2	2nd error message
**	s3	3rd error message
**	line	current line when error occured
**	ep	error position in line
**	line_no	line number
**	file_name	file name
** Returns:	nothing
**___________________________________________________			     */
void error(type,s1,s2,s3,line,ep,line_no,file_name)/*			     */
  register int	  type;			   	   /* defined in error.h     */
  register char	  *s1;			   	   /* 1st error message	     */
  register char	  *s2;			   	   /* 2nd error message	     */
  register char	  *s3;			   	   /* 3rd error message	     */
  register U_CHAR *line;			   /* line_no string.	     */
  register U_CHAR *ep;			   	   /* error position in line */
  register int	  line_no;		   	   /* line number	     */
  register char	  *file_name;		   	   /* file name		     */
{						   /*			     */
  if ( (type&ERR_ERROR)==0 && rsc_quiet ) return;  /* anything less than an  */
						   /*  error is ignored.     */
  ErrNL;					   /*			     */
  if ( (type&ERR_POINT) && line != NULL )	   /*			     */
  { ErrS((char*)line);				   /* print the eror line.   */
    if ( line[strlen((char*)line)-1] != '\n' ) ErrNL;/*			     */
    for ( ; line<ep; ++line ) ErrChar('_');	   /*  and a pointer to the  */
    ErrChar('^'); ErrNL;			   /*  error position.	     */
  }						   /*			     */
						   /*			     */
  if	  ( type&ERR_ERROR ) ErrS("*** BibTool ERROR: "	 );/*		     */
  else if ( type&ERR_WARN  ) ErrS("*** BibTool WARNING: ");/*		     */
  else			     ErrS("*** BibTool: ");/*			     */
  if ( type&ERR_FILE	)			   /*			     */
  { (void)fprintf(err_file,			   /*			     */
		  " (line %d in %s): ",		   /*			     */
		  line_no,			   /*			     */
		  file_name );			   /*			     */
  }						   /*			     */
  if ( s1 ) { ErrS(s1); }		   	   /*			     */
  if ( s2 ) { ErrS(s2); }		   	   /*			     */
  if ( s3 ) { ErrS(s3); }		   	   /*			     */
  ErrNL;					   /*			     */
  if ( type&ERR_EXIT ) exit(-1);		   /*			     */
}						   /*------------------------*/
