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

$Id: ilusvobject.h,v 1.4 1996/07/09 23:06:43 larner Exp $
*/

#ifdef __cplusplus
extern "C" {
#endif

/* ILU Server Object implementation */

typedef struct
{
	PyObject_HEAD
	ilu_Server	kserver;
} IlusvObject;

extern PyTypeObject	Ilusv_Type;

#define ilusv_Check(op)	((op)->ob_type == &Ilusv_Type)

extern PyObject *	ilusv_FromServer(ilu_Server kserver);

#ifdef __cplusplus
}
#endif
