/*$Id: io_trace.h,v 11.23 96/02/21 12:46:15 al Exp $ -*- C++ -*-
 * trace macros for model debugging
 */
/* allow multiple inclusions with different DO_TRACE */
#undef trace0
#undef trace1
#undef trace2
#undef trace3
#undef trace4
/*--------------------------------------------------------------------------*/
#ifdef DO_TRACE
#include "io.h"
#include "error.h"
#include "l_lib.h"
#define trace0(s) (::error(bTRACE, "@@%s\n", s))
#define trace1(s,x) (::error(bTRACE, "@@%s  %s=%s\n",\
	s, #x, trim(ftos(x, "", 5, 0))))
#define trace2(s,x,y) (::error(bTRACE, "@@%s  %s=%s  %s=%s\n",\
	s, #x, trim(ftos(x, "", 5, 0)), #y, trim(ftos(y, "", 5, 0))))
#define trace3(s,x,y,z) (::error(bTRACE, "@@%s  %s=%s  %s=%s  %s=%s\n",\
	s, #x, trim(ftos(x, "", 5, 0)), #y, trim(ftos(y, "", 5, 0)),\
	   #z, trim(ftos(z, "", 5, 0))))
#define trace4(s,w,x,y,z)(::error(bTRACE,"@@%s  %s=%s  %s=%s  %s=%s  %s=%s\n",\
	s, #w, trim(ftos(w, "", 5, 0)), #x, trim(ftos(x, "", 5, 0)),\
	   #y, trim(ftos(y, "", 5, 0)), #z, trim(ftos(z, "", 5, 0))))
#else
#define trace0(s)
#define trace1(s,x)
#define trace2(s,x,y)
#define trace3(s,x,y,z)
#define trace4(s,w,x,y,z)
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
