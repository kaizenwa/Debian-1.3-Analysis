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

$Id: iluclobject.h,v 1.7 1996/07/09 23:04:40 larner Exp $
*/
#ifdef __cplusplus
extern "C" {
#endif

/* ILU Class Record Object implementation */

typedef struct
{
	PyObject_HEAD
	ilu_Class		c;
	ilu_boolean		registered;
	ilu_boolean		optional;
	ilu_boolean		collectible;
	ilu_string		id;
	ilu_cardinal		nmethods;
	PyObject **		skeletons;
} IluclObject;

extern PyTypeObject	Ilucl_Type;

#define ilucl_Check(op)	((op)->ob_type == &Ilucl_Type)

extern PyObject *	ilucl_New(char *name, char *brand, char *uniqueId,
				char *singleton, int collectible, int optional,
				char *authentication,
				PyObject *methodTuple,
				PyObject *superclassTuple);

extern int		ilucl_RegisterSkeletons(IluclObject *cl,
				PyObject *skelTuple);

#ifdef __cplusplus
}
#endif
