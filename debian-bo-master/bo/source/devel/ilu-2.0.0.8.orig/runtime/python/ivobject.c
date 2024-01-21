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

$Id: ivobject.c,v 1.14 1996/07/11 22:06:29 janssen Exp $
*/

#if defined(WIN32)
#include <process.h>	/* for _getpid() */
#else
#include <unistd.h>		/* for getpid() */
#endif

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ivobject.h"
#include "pythonthreads.h"

extern ilu_cardinal _ilupython_LangIndex;

static void
iluiv_dealloc(PyObject *o)
{
	IvObject *	iv	= (IvObject *) o;

	if (iv->kserver != 0 && iv->kclass != 0)
	{
		if (iv->publish_proof != 0)
		{
			CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
			/* Don't withdraw the object if it was created
			** by another process. */
			if (iv->kobj != 0 && iv->publish_proof != 0 &&
#if defined(WIN32)
				iv->creatorPid == _getpid())
#else
			    iv->creatorPid == getpid())
#endif
				{
				  CALL_KERNEL(ilupython_threaded_operation, ilu_WithdrawObject(iv->kobj, iv->publish_proof));
				}
			else
				ilu_ExitServer(iv->kserver, iv->kclass);
			PyMem_XDEL(iv->publish_proof);
			iv->publish_proof = 0;
		}

		CALL_KERNEL(ilupython_threaded_operation, ilu_EnterServer(iv->kserver, iv->kclass));
		if (iv->kobj != 0)
		{
			CALL_KERNEL(ilupython_threaded_operation, ilu_RegisterLanguageSpecificObject(iv->kobj, NULL, _ilupython_LangIndex));
			iv->kobj = 0;
		}
		ilu_ExitServer(iv->kserver, iv->kclass);
	}
	PyMem_DEL(o);
}

PyObject *
iv_New(void)
{
	IvObject *	v	= PyObject_NEW(IvObject, &Iv_Type);

	if (v == 0)
		return PyErr_NoMemory();
	v->kclass = 0;
	v->kserver = 0;
	v->kobj = 0;
	v->publish_proof = 0;
#if defined(WIN32)
	v->creatorPid = _getpid();
#else
	v->creatorPid = getpid();
#endif
	return (PyObject *) v;
}

static int
iluiv_print(PyObject *o, FILE *fp, int flags)
{
	IvObject *	iv	= (IvObject *) o;

	fprintf(fp, "<instvars:  kclass=0x%p  kserver=0x%p  kobj=0x%p%s>",
		iv->kclass, iv->kserver, iv->kobj,
		(iv->publish_proof != NULL) ? " P" : "");
	return 0;
}

PyTypeObject	Iv_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"instvars",
	sizeof(IvObject),
	0,
	iluiv_dealloc,		/*tp_dealloc*/
	iluiv_print,		/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
