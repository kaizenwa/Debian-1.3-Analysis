/*$Id: error.h,v 11.22 96/02/18 11:46:22 al Exp $ -*- C++ -*-
 * data for error and exception handling
 */
#ifndef bERROR
/*--------------------------------------------------------------------------*/
/* arg to error() (badness) to decide severity of exception */
#define	bNOERROR	0
#define bTRACE		1
#define bLOG		2
#define bDEBUG		3
#define bPICKY		4
#define	bWARNING	5
#define bDANGER		6
#define bERROR		7
#define bEXIT		8
#define bDISASTER	9
		void	  error(int,const char*,...);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
