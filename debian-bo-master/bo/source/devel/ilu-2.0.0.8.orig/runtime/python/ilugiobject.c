/*
Copyright (c) 1996 Xerox Corporation.  All Rights Reserved.

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

$Id: ilugiobject.c,v 1.1 1996/05/11 02:59:09 janssen Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "ilugiobject.h"

static void
ilugi_dealloc(PyObject *o)
{
  IlugiObject * v	= (IlugiObject *) o;

  v->id = ILU_NIL;
  if (v->name != ILU_NIL)
    ilu_free(v->name);
  PyMem_DEL(o);
}

PyObject *
ilugi_FromIdentityInfo (ilu_IdentityInfo id)
{
  IlugiObject *	v	= PyObject_NEW(IlugiObject, &Ilugi_Type);

  if (v == 0)
    return 0;
  if (id->ii_type != ilu_GSSIdentity)
    return 0;
  v->id = id;
  v->name = ILU_NIL;
  v->localp = ilu_FALSE;
  return (PyObject *) v;
}

static int
ilugi_print(PyObject *o, FILE *fp, int flags)
{
  IlugiObject *v = (IlugiObject *) o;
  PyObject *retval;
  ilu_Error err;
  gss_name_t name;
  ilu_boolean localp;
  ilu_string s;

  fprintf (fp, "<ilu_GSSIdentity");
  if (v->id == ILU_NIL)
    {
      fprintf (fp, " (uninitialized)>");
      return 0;
    }
  else if (v->name == ILU_NIL)
    {
      if (!ilu_DecodeGSSIdentity (v->id, &name, &v->good_till, ILU_NIL, &v->localp, ILU_NIL, &err) ||
	  !(v->name = ilu_GSSNameToString (name, &err), ILU_ERROK(err)))
	{
	  fprintf (fp, " (error: %s)>", ILU_ERR_NAME(err));
	  ILU_HANDLED(err);
	  return 0;
	}
    }
  fprintf(fp, " %s name=%s ends=%lu.%lu>", v->localp ? "local" : "remote", v->name,
	  v->good_till.ft_s, v->good_till.ft_t);
  return 0;	  
}

PyTypeObject	Ilugi_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_GSSIdentityInfo",
	sizeof(IlugiObject),
	0,
	ilugi_dealloc,		/*tp_dealloc*/
	ilugi_print,		/*tp_print*/
	0,			/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
