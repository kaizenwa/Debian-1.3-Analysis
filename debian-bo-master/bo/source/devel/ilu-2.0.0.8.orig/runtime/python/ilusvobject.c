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

$Id: ilusvobject.c,v 1.8 1994/12/09 18:55:55 severson Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilusvobject.h"

static void
ilusv_dealloc(PyObject *o)
{
	PyMem_DEL(o);
}

PyObject *
ilusv_FromServer(ilu_Server kserver)
{
	IlusvObject *	v	= PyObject_NEW(IlusvObject, &Ilusv_Type);

	if (v == 0)
		return 0;
	v->kserver = kserver;
	return (PyObject *) v;
}

static int
ilusv_print(PyObject *self, FILE *fp, int flags)
{
	IlusvObject *	v	= (IlusvObject *) self;
	char *		id	= ilu_IDOfServer(v->kserver);

	fprintf(fp, "<ilu_Server:  id=\"%s\"  kserver=0x%p>",
		id ? id : "", v->kserver);
	return 0;
}

static PyObject *
ilusv_id(PyObject *self, PyObject *args)
{
	IlusvObject *	v	= (IlusvObject *) self;
	char *		id;

	if (!PyArg_Parse(args, ""))
		return 0;
	if ((id = ilu_IDOfServer(v->kserver)) == 0)
	{
		Py_INCREF(Py_None);
		return Py_None;
	}
	return PyString_FromString(id);
}

static PyMethodDef ilusv_methods[] =
{
	{ "id",			ilusv_id		},
	{ 0						}
};

static PyObject *
ilusv_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilusv_methods, self, name);
}

PyTypeObject	Ilusv_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Server",
	sizeof(IlusvObject),
	0,
	ilusv_dealloc,		/*tp_dealloc*/
	ilusv_print,		/*tp_print*/
	ilusv_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
