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

$Id: iluclobject.c,v 1.17 1996/07/12 20:20:47 larner Exp $
*/

/* from ILU */
#include "iluxport.h"

/* local */
#include "python.h"
#include "iluclobject.h"

static void
ilucl_deallocSkeletons(IluclObject *cl)
{
	if (cl->skeletons)
	{
	  int	i;

	  for (i = 0; i < cl->nmethods; i++)
	    Py_XDECREF(cl->skeletons[i]);
	}
	PyMem_XDEL(cl->skeletons);
}

static void
ilucl_dealloc(PyObject *o)
{
	IluclObject *	cl = (IluclObject *) o;
	int		i;

	ilucl_deallocSkeletons(cl);
	ilu_free(cl->id);

	PyMem_DEL(o);
}

#define CLEAR(ptr, n)	memset((char *) ptr, 0, (n) * sizeof(*(ptr)))

static char *lstrdup(char *s)
{
  char *s2;

  if (s == ILU_NIL)
    return ILU_NIL;
  s2 = (char *) ilu_must_malloc(strlen(s) + 1);
  strcpy (s2, s);
  return s2;
}

static int
  setMethodEntry(ilu_Class cl, ilu_cardinal method, PyObject *t)
{
  ilu_Method	m;
  char *	name;
  char *	lname;
  int		id;
  char		cacheable;
  char		asynchronous;
  PyObject *	exceptionsTuple;
  ilu_Exception *	evec;
  int		i;
  ilu_Error	err = ILU_INIT_NO_ERR;

  if (!PyArg_Parse(t, "(sibbO)", &name, &id, &cacheable, &asynchronous,
		   &exceptionsTuple))
    return -1;

  lname = lstrdup(name);
  evec = PyMem_NEW(char *, PyTuple_Size(exceptionsTuple));
  for (i = 0;  i < PyTuple_Size(exceptionsTuple); i++)
    {
      PyObject *	e = PyTuple_GetItem(exceptionsTuple, i);

      if (PyString_Check(e))
	{
	  Py_INCREF(e);
	  evec[i] = ilu_DefineException (ILU_NIL, PyString_AsString(e), &err);
	  if (ILU_ERRNOK(err))
	    {
	      char errbuf[1000];

	      PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
	      ILU_HANDLED(err);
	      return -1;
	    }
	}
      else
	{
	  PyMem_XDEL(lname);
	  PyMem_XDEL(evec);
	  (void) PyErr_BadArgument();
	  return -1;
	}
    }
  ilu_DefineMethod (cl,
		    method,
		    lname,
		    id,
		    BOOLEAN(cacheable),
		    BOOLEAN(asynchronous),
		    PyTuple_Size(exceptionsTuple), evec,
		    &err);
  ILU_ERR_SWITCH(err) {
    ILU_SUCCESS_CASE ;
    ILU_ERR_CASE(no_memory, evp)
      {
	(void) PyErr_NoMemory();
	return -1;
      }
    ILU_ERR_ELSE
      {
	char errbuf[1000];

	PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
	ILU_HANDLED(err);
	return -1;
      }
  } ILU_ERR_ENDSWITCH;
  
  return 0;
}

PyObject *
ilucl_New(char *name, char *brand, char *uniqueId, char *singleton,
		int collectible, int optional, char *authentication,
		PyObject *methodsTuple, PyObject *superclassTuple)
{
  IluclObject *	cl = PyObject_NEW(IluclObject, &Ilucl_Type);
  ilu_Class	c;
  int		i;
  ilu_Error	err = ILU_INIT_NO_ERR;

  if (cl == 0)
    return 0;

  CLEAR(&cl->c, 1);
  cl->skeletons = 0;

  if ((c = ilu_FindClassFromID (uniqueId)) == ILU_NIL)
    {
      char **superclass_ids;

      ilu_EnterMutex (ilu_otmu, &err);
      ILU_MUST_BE_SUCCESS(err);

      superclass_ids = PyMem_NEW(char *, PyTuple_Size(superclassTuple));
      for (i = 0; i < PyTuple_Size(superclassTuple); i++)
	{
	  IluclObject *	sc	=
	    (IluclObject *) PyTuple_GetItem(superclassTuple, i);
		
	  if (!ilucl_Check(sc))
	    {
	      ilucl_dealloc((PyObject *) cl);
	      (void) PyErr_BadArgument();
	      return 0;
	    }
	  Py_INCREF(sc);
	  superclass_ids[i] = sc->id;
	}

      /* create new class struct */
      cl->c = ilu_DefineObjectType (name,
				    brand,
				    uniqueId,
				    singleton,
				    BOOLEAN(optional),
				    BOOLEAN(collectible),
				    authentication,
				    PyTuple_Size(methodsTuple),
				    PyTuple_Size(superclassTuple),
				    superclass_ids,
				    &err);
      ILU_ERR_SWITCH(err) {
	ILU_SUCCESS_CASE ;
	ILU_ERR_CASE(no_memory, evp)
	  {
	    ilucl_dealloc((PyObject *) cl);
	    (void) PyErr_NoMemory();
	    return 0;
	  }
	ILU_ERR_ELSE
	  {
	    char errbuf[1000];

	    ilucl_dealloc((PyObject *) cl);
	    PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
	    ILU_HANDLED(err);
	    return 0;
	  }
      } ILU_ERR_ENDSWITCH;

      for (i = 0; i < PyTuple_Size(methodsTuple); i++)
	{
	  if (setMethodEntry(cl->c, i,
			     PyTuple_GetItem(methodsTuple, i)) < 0)
	    {
	      ilucl_dealloc((PyObject *) cl);
	      return 0;
	    }
	}

      if (!ilu_ObjectTypeDefined(cl->c, &err))
	{
	  char errbuf[1000];

	  ilucl_dealloc((PyObject *) cl);
	  PyErr_SetString(_ilupython_GeneralError, _ilupython_formErrDescription(errbuf, &err));
	  ILU_HANDLED(err);
	  return 0;
	}
      ilu_ExitMutex (ilu_otmu, ilu_FALSE, &err);
      ILU_MUST_BE_SUCCESS(err);
    }
  else
    {
      cl->c = c;
    }
  cl->id = lstrdup(uniqueId);
  cl->collectible = BOOLEAN(collectible);
  cl->optional = BOOLEAN(optional);
  cl->nmethods = PyTuple_Size(methodsTuple);
  return (PyObject *) cl;
}

int
ilucl_RegisterSkeletons(IluclObject *cl, PyObject *skelTuple)
{
  int	i;
  int	count = PyTuple_Size(skelTuple);

  if (count != cl->nmethods)
    {
      PyErr_SetString(PyExc_TypeError, "skel tuple wrong length");
      return -1;
    }

  ilucl_deallocSkeletons(cl);
  cl->skeletons = PyMem_NEW(PyObject *, count);
  if (cl->skeletons == 0)
    {
      (void) PyErr_NoMemory();
      return -1;
    }
  CLEAR(cl->skeletons, count);

  for (i = 0; i < count; i++)
    {
      PyObject *	skel	 = PyTuple_GetItem(skelTuple, i);

      if (!PyCallable_Check(skel))
	{
	  ilucl_deallocSkeletons(cl);
	  PyErr_SetString(PyExc_TypeError,
			  "skel tuple contains noncallable");
	  return -1;
	}
      Py_INCREF(skel);
      cl->skeletons[i] = skel;
    }

  return 0;
}

static char *
booleanImage(int bool)
{
	return bool ? "TRUE" : "FALSE";
}

static char *
notPrefix(int bool)
{
	return bool ? "" : "not ";
}

static int
ilucl_print(PyObject *o, FILE *fp, int flags)
{
	IluclObject *	cl = (IluclObject *) o;
	int		i;
	ilu_string name, brand, id, authentication, singleton;
	ilu_boolean collectible, optional, cacheable, asynchronous;
	ilu_cardinal superclass_count, method_count, mid, ecount;
	ilu_Class *superclasses;
	ilu_Method	methods;
	ilu_Method	m;
	ilu_Exception	*evec;
	void (*stubproc) (ilu_Call);

	ilu_DataOfClass (cl->c, &name, &brand, &id, &singleton,
			 &collectible, &method_count,
			 &superclass_count, &superclasses, &optional,
			 &methods);
	fprintf(fp, "<ilu_Class at 0x%p\n", cl);
	fprintf(fp, "  name: '%s'\n", name);
	fprintf(fp, "  brand: '%s'\n", brand);
	fprintf(fp, "  unique_id: '%s'\n", id);

	fprintf(fp, "  singleton: ");
	if (singleton != ILU_NIL)
		fprintf(fp, "'%s'", singleton);
	else
		fprintf(fp, "null");
	putc('\n', fp);

	fprintf(fp, "  collectible: %s\n", booleanImage(collectible));
	fprintf(fp, "  optional: %s\n", booleanImage(optional));

	fprintf(fp, "  authentication: ");
	if (authentication)
		fprintf(fp, "'%s'", authentication);
	else
		fprintf(fp, "null");
	putc('\n', fp);

	for (i = 0; i < method_count; i++)
	{
	  ilu_DataOfMethod (methods + i, &name, &mid, &cacheable,
			    &asynchronous, &ecount, &evec, &stubproc);

	  if (i == 0)
	    fprintf(fp, "  methods:\n");
	  fprintf(fp, "    '%s', %lu, %scacheable, %sasync\n",
			name, (unsigned long) mid,
			notPrefix(cacheable),
			notPrefix(asynchronous));
	  if (ecount > 0)
	    {
	      int	j;

	      fprintf(fp, "      raises ");
	      for (j = 0; j < ecount; j++)
		{
		  fprintf(fp, "%s'%s'", j == 0 ? "" : ", ", evec[j]);
		}
	      putc('\n', fp);
	    }
	}

	for (i = 0; i < superclass_count; i++)
	{
	  ilu_string name2, id2;

	  ilu_DataOfClass (superclasses[i], &name2, ILU_NIL, &id2,
			   ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			   ILU_NIL, ILU_NIL, ILU_NIL);
	  if (i == 0)
	    fprintf(fp, "  superclasses:\n");
	  fprintf(fp, "    %s (%s)\n", name2, id2);
	}

	if (cl->skeletons)
	{
	  fprintf(fp, "  skeletons:\n");
	  for (i = 0; i < method_count; i++)
	    {
	      fprintf(fp, "    ");
	      PyObject_Print(cl->skeletons[i], fp, flags);
	      fprintf(fp, "\n");
	    }
	}

	fprintf(fp, "  >");
	return 0;
}

static PyObject *
ilucl_name(PyObject *self, PyObject *args)
{
	IluclObject *	cl	= (IluclObject *) self;
	ilu_string	name;

	if (!PyArg_Parse(args, ""))
		return 0;
	if (!ilu_DataOfClass (cl->c, &name, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL, ILU_NIL))
	  return 0;
	return PyString_FromString(name);
}

static PyObject *
ilucl_id(PyObject *self, PyObject *args)
{
	IluclObject *	cl	= (IluclObject *) self;
	ilu_string id;

	if (!PyArg_Parse(args, ""))
		return 0;
	if (!ilu_DataOfClass (cl->c, ILU_NIL, ILU_NIL, &id,
			      ILU_NIL, ILU_NIL, ILU_NIL, ILU_NIL,
			      ILU_NIL, ILU_NIL, ILU_NIL))
	    return 0;
	return PyString_FromString(id);
}

static PyMethodDef ilucl_methods[] =
{
	{ "name",		ilucl_name		},
	{ "id",			ilucl_id		},
	{ 0						}
};

static PyObject *
ilucl_getattr(PyObject *self, char *name)
{
	return Py_FindMethod(ilucl_methods, self, name);
}

PyTypeObject	Ilucl_Type =
{
	PyObject_HEAD_INIT(&PyType_Type)
	0,
	"ilu_Class",
	sizeof(IluclObject),
	0,
	ilucl_dealloc,		/*tp_dealloc*/
	ilucl_print,		/*tp_print*/
	ilucl_getattr,		/*tp_getattr*/
	0,			/*tp_setattr*/
	0,			/*tp_compare*/
	0,			/*tp_repr*/
	0,			/*tp_as_number*/
	0,			/*tp_as_sequence*/
	0,			/*tp_as_mapping*/
	0,			/*tp_hash*/
};
