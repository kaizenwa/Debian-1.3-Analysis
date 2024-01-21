/******************************************************************************
** $Id: error.h,v 2.15 1996/02/22 21:43:51 gerd Exp gerd $
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

#define ERR_WARN	1
#define ERR_WARNING	1
#define ERR_ERROR	2
#define ERR_POINT	4
#define ERR_FILE	8
#define ERR_EXIT      256

 typedef unsigned char U_CHAR;

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 void error _ARG((int type,char *s1,char *s2,char *s3,U_CHAR *line,U_CHAR *ep,int line_no,char *file_name));/* error.c*/

 extern char * empty;
 extern char * point;
 extern char * oom;

 extern FILE * err_file;

#define ERROR_EXIT(X)			\
	error(ERR_ERROR|ERR_EXIT,X,	\
	      (char*)0,(char*)0,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)
#define OUT_OF_MEMORY(X)		\
	error(ERR_ERROR|ERR_EXIT,oom,X,point,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)

#define ERROR(X)	\
	error(ERR_ERROR,X,(char*)0,(char*)0,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)
#define ERROR2(X,Y)	\
	error(ERR_ERROR,X,Y,(char*)0,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)
#define ERROR3(X,Y,Z)	\
	error(ERR_ERROR,X,Y,Z,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)

#define WARNING(X)	\
	error(ERR_WARN,X,(char*)0,(char*)0,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)
#define WARNING2(X,Y)	\
	error(ERR_WARN,X,Y,(char*)0,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)
#define WARNING3(X,Y,Z)	\
	error(ERR_WARN,X,Y,Z,(U_CHAR*)0,(U_CHAR*)0,0,(char*)0)

#define Err(S)			(void)fprintf(err_file,"*** BibTool: %s",S)
#define ErrC(CHAR)		(void)fputc(CHAR,err_file)
#define ErrPrintF(F,A)		(void)fprintf(err_file,F,A)
#define ErrPrintF2(F,A,B)	(void)fprintf(err_file,F,A,B)
#define FlushErr		(void)fflush(err_file)


#define VerbosePrint1(A)      (void)fprintf(err_file,"--- BibTool: %s\n",A)
#define VerbosePrint2(A,B)    (void)fprintf(err_file,"--- BibTool: %s%s\n",A,B)
#define VerbosePrint3(A,B,C)  (void)fprintf(err_file,"--- BibTool: %s%s%s\n",A,B,C)
#define VerbosePrint4(A,B,C,D) (void)fprintf(err_file,"--- BibTool: %s%s%s%s\n",A,B,C,D)

#ifdef DEBUG
#define DebugPrint1(A)        (void)fprintf(err_file,"+++ BibTool: %s\n",A)
#define DebugPrint2(A,B)      (void)fprintf(err_file,"+++ BibTool: %s%s\n",A,B)
#define DebugPrint3(A,B,C)    (void)fprintf(err_file,"+++ BibTool: %s%s%s\n",A,B,C)
#else
#define DebugPrint1(A)        
#define DebugPrint2(A,B)      
#define DebugPrint3(A,B,C)    
#endif

