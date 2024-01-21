/*
Copyright (c) 1991, 1992, 1993, 1994 Xerox Corporation.  All Rights Reserved.

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

$Id: python.h,v 1.19 1996/07/09 22:57:51 janssen Exp $
*/

#ifdef __cplusplus
#undef ILU_NIL
#define ILU_NIL 0
#define BOOLEAN(x) (x ? ilu_TRUE : ilu_FALSE)
#else
#define BOOLEAN(x) x
#endif

/* Imports from Python */

#define HAVE_PROTOTYPES 1	/* We assume an ANSI C compiler */
#define HAVE_STDARG_PROTOTYPES	/* Again, ANSI C */

#include "pythonversion.h"

#if (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2)

#include <Python.h>
#include <traceback.h>
#include <pythonrun.h>

#else 

/* from Python */
#include "allobjects.h"
#include "rename1.h"

/* The following seem to be missing (as of Python 1.0.3) from rename1.h: */
#ifndef PyObject
typedef object PyObject;
typedef struct methodlist PyMethodDef;
#endif

#endif /* (ILUPYTHON_MAJOR_VERSION == 1 && ILUPYTHON_MINOR_VERSION >= 2) */

/* The following are for convenience. */
#define PyCallable_Check(obj) \
	(PyFunction_Check(obj) || PyCFunction_Check(obj) || PyMethod_Check(obj))

extern char * _ilupython_formErrDescription (char * /* buf into which to print the error */, ilu_Error *);
extern PyObject *_ilupython_GeneralError;
extern ilu_cardinal _ilupython_LangIndex;
