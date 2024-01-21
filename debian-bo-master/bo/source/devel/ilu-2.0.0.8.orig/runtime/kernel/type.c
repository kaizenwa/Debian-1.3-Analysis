/*
 * Copyright (c) 1991-1995 Xerox Corporation.  All Rights
 * Reserved.
 * 
 * Unlimited use, reproduction, and distribution of this software is
 * permitted.  Any copy of this software must include both the
 * above copyright notice of Xerox Corporation and this paragraph.
 * Any distribution of this software must comply with all
 * applicable United States export control laws.  This software is
 * made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 * WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION
 * CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM THE
 * SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 * CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN
 * IF XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGES.
 */
/* $Id: type.c,v 1.68 1996/07/02 03:48:36 janssen Exp $ */
/* Last edited by Mike Spreitzer March 8, 1996 9:08 am PST */

#define _POSIX_SOURCE

#ifdef MACOS
#pragma segment ilu
#endif

#include "iluntrnl.h"

#include "object.h"
#include "server.h"
#include "type.h"
#include "vector.h"

#include <limits.h>		/* for ULONG_MAX */

/* L1, L2, Main unconstrained */

static void _ilu_HandleIsA (ilu_Call call);

static struct _ilu_Method_s _ilu_InternalMethods[] = {

  {"ILUGetTypes", 0xFF83,
     TRUE, FALSE, NIL, 0, _ilu_HandleGetTypes},

  {"ILURegisterGCInterest", 0xFF81,
     FALSE, FALSE, NIL, 0, _ilu_HandleGCInterestRegistration},
  {"ILUUnregisterGCInterest", 0xFF82,
     FALSE, FALSE, NIL, 0, _ilu_HandleGCInterestDeregistration},

  {"ILUPing", 0xFF84,
     FALSE, FALSE, NIL, 0, _ilu_HandlePing},

  {"-is-a", 0xFF85,		/* for CORBA support */
     FALSE, FALSE, NIL, 0, _ilu_HandleIsA}
};

ilu_Method      _ilu_GetTypesMethod = &_ilu_InternalMethods[0];
ilu_Method      _ilu_RegisterGCInterestMethod = &_ilu_InternalMethods[1];
ilu_Method      _ilu_UnregisterGCInterestMethod = &_ilu_InternalMethods[2];
ilu_Method      _ilu_PingMethod = &_ilu_InternalMethods[3];
ilu_Method      _ilu_IsAMethod = &_ilu_InternalMethods[4];

static const    nInternal = sizeof(_ilu_InternalMethods)
/ sizeof(struct _ilu_Method_s);

struct _ilu_Class_s _ilu_rootClass_s = {
  "ilu.Object",			/* name */
  "version 2",			/* brand */
  "ilu:root-object-type",	/* unique_id */
  NIL,				/* singleton? */
  TRUE,				/* collectible? */
  NIL,				/* authentication */
  _ilu_InternalMethods,		/* methods */
  sizeof(_ilu_InternalMethods) / sizeof(struct _ilu_Method_s),
  /* method_count */
  0,				/* superclass_count */
  NIL,				/* superclass_ids */
  NIL,				/* superclasses */
  FALSE,			/* shown */
  FALSE				/* optional? */
};

const ilu_Class _ilu_rootClass = &_ilu_rootClass_s;
const ilu_Class ilu_rootClass = &_ilu_rootClass_s;

static struct _ilu_Method_s nometh = {NIL, ULONG_MAX, ilu_FALSE,
ilu_FALSE, NIL, ULONG_MAX, NULLFN};

static int optstrcmp(const char *s1, const char *s2)
{
  if (s1 == s2)
    return 0;
  if (s1 == NIL)
    return -1;
  if (s2 == NIL)
    return 1;
  return strcmp(s1, s2);
}

ilu_boolean ilu_CollectibleP (ilu_Class c)
{
  return (c->cl_collectible);
}

ilu_Method ilu_MethodNOfClass (ilu_Class c, ilu_cardinal index)
{
  if (c == NIL || index >= c->cl_method_count)
    return NIL;
  return (c->cl_methods + index);
}

/* L1 >= {otmu} */
/* L2, Main unconstrained */

static HashTable ClassNameTable = NIL;
/* Object type name -> object type */

static HashTable ClassIDTable = NIL;
/* Object type id -> object type */

static HashTable UnknownTypeIDs = NIL;
/* Object type id -> ID_DAG */

static HashTable UnlinkedClasses = NIL;
/*
 * Mapping from object type id to vector of object types waiting
 * for id to be registered.
 */

static HashTable ifcErrors = NIL;
/* ifc name -> error table */

static HashTable corbaErrors = NIL;
/* corba rep -> NIL */


static void 
_ilu_RegisterClass(ilu_Class class)
{
  int             new;
  ilu_Class       p;
  register ilu_cardinal index;
  ilu_Vector      v;

  if (class != NIL AND((ilu_cardinal) (class->cl_singleton)) == 1) {
    ILU_ERRPRINTF("\
Fatal ILU error:  Stubs for type \"%s\" were generated\n\
by a pre-1.6.4-p8 stubber.  Please re-stub, re-compile, and re-link.\n",
	    class->cl_name);
    exit(1);
  }

  DEBUG(OBJECT_DEBUG,
	(stderr, "_ilu_RegisterClass:  Registering object type %s, id=%s, ilu_Class=%p.\n",
	 class->cl_name, class->cl_unique_id, class));
  
  if (ClassNameTable == NIL)
    ClassNameTable = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					    _ilu_hash_HashString,
					    _ilu_hash_StringCompare);
  if (ClassIDTable == NIL)
    ClassIDTable = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					  _ilu_hash_HashString,
					  _ilu_hash_StringCompare);
  if (UnlinkedClasses == NIL)
    UnlinkedClasses = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					     _ilu_hash_HashString,
					     _ilu_hash_StringCompare);

  if (_ilu_hash_FindInTable(ClassNameTable, class_name(class)) == NIL)
    _ilu_Assert((int) _ilu_hash_AddToTable(ClassNameTable, class_name(class),
				     class),
		"RegisterClass AddToTable ClassNameTable");
  new = NIL == _ilu_hash_FindInTable(ClassIDTable,
				      class_unique_id(class));
  if (new)
    _ilu_Assert((int) _ilu_hash_AddToTable(ClassIDTable,
				   class_unique_id(class), class),
		"RegisterClass AddToTable ClassIDTable");

  /*
   * this handles the case where additional class information is
   * loaded into a running program
   */
  if (new && UnknownTypeIDs != NIL
  && _ilu_hash_FindInTable(UnknownTypeIDs, class_unique_id(class))
      != NIL) {		/* This shouldn't happen! */
    ASSERT(0, buf,
	   (buf, "%s %s %s previously considered unknown.\n",
       "ilu_RegisterClass:  Config bug!  Registering object type",
	    class_name(class), class_unique_id(class)));
  }
  /*
   * this links up the superclass of "class" which have already
   * been registered
   */
  for (index = 0; index < class_superclass_count(class); index++) {
    if (class_superclass_id(class, index) != NIL) {
      p = (ilu_Class) _ilu_hash_FindInTable(ClassIDTable,
			       class_superclass_id(class, index));
      if (p == NIL) {
	v = (ilu_Vector) _ilu_hash_FindInTable(UnlinkedClasses,
			       class_superclass_id(class, index));
	if (v == NIL) {
	  v = _ilu_vector_new(1);
	  _ilu_Assert((int) _ilu_hash_AddToTable(UnlinkedClasses,
				class_superclass_id(class, index),
					   v),
		      "RegisterClass AddToTable UnlinkedClasses");
	}
	_ilu_vector_add(v, (ilu_refany) class);
      } else
	class_superclass(class, index) = p;
    }
  }

  /*
   * now link those classes which have been waiting for this
   * superclass
   */
  v = (ilu_Vector) _ilu_hash_FindInTable(UnlinkedClasses,
					 class_unique_id(class));
  if (v != NIL) {
    ilu_cardinal    size = _ilu_vector_size(v);
    ilu_Class      *elements = (ilu_Class *) _ilu_vector_elements(v);
    register ilu_cardinal j;

    _ilu_Assert(_ilu_hash_RemoveFromTable(UnlinkedClasses,
					  class_unique_id(class))
		== v,
		"RegisterClass RemoveFromTable UnlinkedClasses");

    for (index = 0; index < size; index++) {
      p = elements[index];
      for (j = 0; j < class_superclass_count(p); j++)
	if (strcmp(class_unique_id(class), class_superclass_id(p, j))
	    == 0) {
	  class_superclass(p, j) = class;
	  break;
	}
    }
    _ilu_vector_destroy(v, NULLFN);
  }
  return;
}

static ilu_Class ObjectTypeFromID(ilu_string id)
{
  if (strcmp(id, _ilu_rootClass->cl_unique_id) == 0)
    return _ilu_rootClass;
  else if (ClassIDTable != NIL)
    return ((ilu_Class) _ilu_hash_FindInTable(ClassIDTable, id));
  else
    return NIL;
}

ilu_Class
ilu_DefineObjectType(ilu_string cl_name,
		     ilu_string cl_brand,
		     ilu_string cl_unique_id,
		     ilu_string cl_singleton,
		     ilu_boolean cl_optional,
		     ilu_boolean cl_collectible,
		     ilu_string cl_authentication,
		     ilu_cardinal cl_method_count,
		     ilu_cardinal cl_scls_count,
		     ilu_string cl_scls_ids[],
		     ILU_ERRS((internal, no_memory)) * err)
{
  ilu_Class       ans = ObjectTypeFromID(cl_unique_id);
  unsigned        msize = sizeof(struct _ilu_Method_s);
  unsigned        i;
  if (ans == NIL) {
    ans = (ilu_Class) ilu_malloc(sizeof(*ans));
    if (ans == NIL)
      return ILU_ERR_CONS1(no_memory, err, nbytes, sizeof(*ans), NIL);
    ans->cl_name = _ilu_Strdup(cl_name);
    ans->cl_brand = _ilu_Strdup(cl_brand);
    ans->cl_unique_id = _ilu_Strdup(cl_unique_id);
    ans->cl_singleton = _ilu_Strdup(cl_singleton);
    ans->cl_collectible = cl_collectible;
    ans->cl_optional = cl_optional;
    ans->cl_authentication = _ilu_Strdup(cl_authentication);
    ans->cl_method_count = cl_method_count;
    ans->cl_methods = (ilu_Method) ilu_malloc(cl_method_count * msize);
    for (i = 0; i < cl_method_count; i++)
      ans->cl_methods[i] = nometh;
    ans->cl_scls_count = cl_scls_count;
    ans->cl_scls_ids = ((ilu_string *)
		   ilu_malloc(cl_scls_count * sizeof(ilu_string)));
    for (i = 0; i < cl_scls_count; i++)
      ans->cl_scls_ids[i] = _ilu_Strdup(cl_scls_ids[i]);
    ans->cl_sclses = ((ilu_Class *)
		    ilu_malloc(cl_scls_count * sizeof(ilu_Class)));
    for (i = 0; i < cl_scls_count; i++)
      ans->cl_sclses[i] = NIL;
    _ilu_RegisterClass(ans);
    ILU_CLER(*err);
    return ans;
  } else {
    if (strcmp(cl_name, ans->cl_name) ||
	optstrcmp(cl_brand, ans->cl_brand) ||
	optstrcmp(cl_singleton, ans->cl_singleton) ||
	optstrcmp(cl_authentication, ans->cl_authentication))
      goto no;
    if ((cl_collectible == 0) != (ans->cl_collectible == 0))
      goto no;
    if (cl_method_count != ans->cl_method_count ||
	cl_scls_count != ans->cl_scls_count)
      goto no;
    for (i = 0; i < cl_scls_count; i++)
      if (strcmp(ans->cl_scls_ids[i], cl_scls_ids[i]))
	goto no;
    ILU_CLER(*err);
    return ans;
no:
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeMismatch, NIL);
  }
}

ilu_Exception
ilu_DefineException(char *i, char *e,
		    ILU_ERRS((internal, no_memory)) * err)
{
  HashTable       errtab;
  ilu_Exception   ans;
  if (ifcErrors == NIL)
    ifcErrors = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
				       _ilu_hash_HashString,
				       _ilu_hash_StringCompare);
  if (corbaErrors == NIL)
    corbaErrors = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
					 _ilu_hash_HashString,
					 _ilu_hash_StringCompare);
  if (i == NIL)
    errtab = corbaErrors;
  else {
    errtab = (HashTable) _ilu_hash_FindInTable(ifcErrors, i);
    if (errtab == NIL) {
      errtab = _ilu_hash_MakeNewTable(CLASS_HASHTABLESIZE,
				      _ilu_hash_HashString,
				      _ilu_hash_StringCompare);
      _ilu_Assert((int) _ilu_hash_AddToTable(ifcErrors, i, errtab),
		  "ifcErrors");
    }
  }
  ans = (ilu_Exception) _ilu_hash_FindInTable(errtab, e);
  if (ans == NIL) {
    if (i == NIL)
      ans = _ilu_Strdup(e);
    else
      ans = _ilu_Strcat5("ilu:", i, ".", e, "");
    _ilu_Assert((int) _ilu_hash_AddToTable(errtab, e, ans), "errtab");
  }
  ILU_CLER(*err);
  return ans;
}

ilu_Method 
ilu_DefineMethod(ilu_Class c,
		 ilu_cardinal i,
		 ilu_string me_name,
		 ilu_cardinal me_id,
		 ilu_boolean me_cacheable,
		 ilu_boolean me_asynchronous,
		 ilu_cardinal me_exceptionCount,
		 ilu_Exception *me_exceptionVector,
		 ILU_ERRS((internal, no_memory)) *err)
{
  ilu_Method      m = c->cl_methods + i;
  ilu_cardinal    j;
  if (m->me_name == NIL) {
    m->me_name = _ilu_Strdup(me_name);
    m->me_id = me_id;
    m->me_cacheable = me_cacheable;
    m->me_asynchronous = me_asynchronous;
    m->me_exceptionCount = me_exceptionCount;
    m->me_exceptionVector = ((ilu_Exception *)
	    ilu_malloc(me_exceptionCount * sizeof(ilu_Exception)));
    for (j = 0; j < me_exceptionCount; j++) {
      if (me_exceptionVector[j] == NIL)
	ILU_ERR_CONS1(internal, err, minor, ilu_im_typeIncomplete,
		      FALSE);
      m->me_exceptionVector[j] = me_exceptionVector[j];
    }
    ILU_CLER(*err);
    return m;
  } else {
    if (strcmp(me_name, m->me_name) ||
	me_id != m->me_id ||
	(me_cacheable == 0) != (m->me_cacheable == 0) ||
	(me_asynchronous == 0) != (m->me_asynchronous == 0) ||
	me_exceptionCount != m->me_exceptionCount)
      goto no;
    for (j = 0; j < me_exceptionCount; j++)
      if (m->me_exceptionVector[j] != me_exceptionVector[j])
	goto no;
    ILU_CLER(*err);
    return m;
no:
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeMismatch,
			 NIL);
  }
}

ilu_boolean
ilu_ObjectTypeDefined(ilu_Class t,
		      ILU_ERRS((internal / typeIncomplete)) * err)
{
  unsigned        i;
  for (i = 0; i < t->cl_method_count; i++) {
    ilu_Method      m = t->cl_methods + i;
    ilu_Exception  *ev = m->me_exceptionVector;
    if (m->me_name == NIL)
      goto no;
  }
  return ILU_CLER(*err);
no:
  return ILU_ERR_CONS1(internal, err, minor, ilu_im_typeIncomplete,
		       FALSE);
}

ilu_boolean
  ilu_DataOfClass (ilu_Class c,
		   char **name,
		   char **brand,
		   char **id,
		   char **singleton,
		   ilu_boolean *collectible,
		   ilu_cardinal *method_count,
		   ilu_cardinal *superclass_count,
		   ilu_Class **superclasses,
		   ilu_boolean *optional,
		   ilu_Method *methods)
{
#define CLRETURN(p,f)	if ((p) != NIL) *(p) = (f)

  CLRETURN(name,c->cl_name);
  CLRETURN(brand,c->cl_brand);
  CLRETURN(id,c->cl_unique_id);
  CLRETURN(singleton,c->cl_singleton);
  CLRETURN(collectible,c->cl_collectible);
  CLRETURN(method_count,c->cl_method_count);
  CLRETURN(superclass_count,c->cl_scls_count);
  CLRETURN(superclasses,c->cl_sclses);
  CLRETURN(optional,c->cl_optional);
  CLRETURN(methods,c->cl_methods);

#undef CLRETURN
  return (ilu_TRUE);
}

ilu_boolean
  ilu_DataOfMethod (ilu_Method m,
		    ilu_string *name,
		    ilu_cardinal *id,
		    ilu_boolean *cacheable,
		    ilu_boolean *asynchronous,
		    ilu_cardinal *ecount,
		    ilu_Exception **evec,
		    ilu_StubProc *stubproc)
{
#define MERETURN(p,f)	if ((p) != NIL) *(p) = (f)

  MERETURN(name, m->me_name);
  MERETURN(id,m->me_id);
  MERETURN(cacheable,m->me_cacheable);
  MERETURN(asynchronous,m->me_asynchronous);
  MERETURN(ecount,m->me_exceptionCount);
  MERETURN(evec,m->me_exceptionVector);
  MERETURN(stubproc,m->me_stubproc);

#undef MERETURN

  return ilu_TRUE;
}

/* L1_sup < otmu */

static void 
PrintClassEntry(ilu_refany entry, ilu_refany junk)
{
  ilu_Class       p;

  if (entry != NIL) {
    p = (ilu_Class) entry;
    ILU_ERRPRINTF("    %s (%s)   %p\n", p->cl_name, p->cl_unique_id, p);
  } else
    ILU_ERRPRINTF("<null>\n");
}

void _ilu_EnumerateClasses (void (*proc) (ilu_Class c, ilu_refany rock), ilu_refany rock)
{
  _ilu_AcquireMutex(ilu_otmu);
  _ilu_hash_TableEnumerate (ClassNameTable, (void (*) (ilu_refany, ilu_refany)) proc, rock);
  _ilu_ReleaseMutex(ilu_otmu);
  return;
}

ilu_Class 
ilu_GetGcCallbackClass(void)
{
  _ilu_AcquireMutex(ilu_otmu);
  if (ClassIDTable == NIL ||
      _ilu_hash_FindInTable(ClassIDTable, _ilu_GcCallbackClass->cl_unique_id)
      == NIL)
    _ilu_RegisterClass(_ilu_GcCallbackClass);
  _ilu_ReleaseMutex(ilu_otmu);
  return _ilu_GcCallbackClass;
}

ilu_Class 
ilu_FindClassFromName(ilu_string classname)
{
  ilu_Class       c;
  _ilu_AcquireMutex(ilu_otmu);

#ifdef ENABLE_DEBUGGING

  if ((_ilu_DebugLevel & OBJECT_DEBUG) == OBJECT_DEBUG) {
    ILU_ERRPRINTF("ilu_FindClassFromName:  classname is %s, class table is %p:\n", classname,
	    ClassNameTable);
    _ilu_hash_TableEnumerate(ClassNameTable, (void (*) (ilu_refany, ilu_refany)) PrintClassEntry, NIL);
  }

#endif /* ENABLE_DEBUGGING */

  if (strcmp(classname, _ilu_rootClass->cl_name) == 0)
    c = _ilu_rootClass;
  else if (ClassNameTable != NIL)
    c = (ilu_Class) _ilu_hash_FindInTable(ClassNameTable, classname);
  else
    c = NIL;
  _ilu_ReleaseMutex(ilu_otmu);
  return (c);
}

ilu_Class 
ilu_FindClassFromID(ilu_string ID)
{
  ilu_Class       c;
  _ilu_AcquireMutex(ilu_otmu);

#ifdef ENABLE_DEBUGGING

  if ((_ilu_DebugLevel & OBJECT_DEBUG) == OBJECT_DEBUG) {
    ILU_ERRPRINTF("ilu_FindClassFromID:  id is %s, class table is %p:\n", ID,
	    ClassIDTable);
    _ilu_hash_TableEnumerate(ClassIDTable, (void (*) (ilu_refany, ilu_refany)) PrintClassEntry, NIL);
  }

#endif /* ENABLE_DEBUGGING */

  if (strcmp(ID, _ilu_rootClass->cl_unique_id) == 0)
    c = _ilu_rootClass;
  else if (ClassIDTable != NIL)
    c = (ilu_Class) _ilu_hash_FindInTable(ClassIDTable, ID);
  else
    c = NIL;
  _ilu_ReleaseMutex(ilu_otmu);
  return (c);
}

static ilu_boolean 
SeekAncestor(ilu_Class a, ilu_Class b)
{
  ilu_cardinal             j;
  ilu_Class       aa;
  if (a == b)
    return (TRUE);
  for (j = 0; j < class_superclass_count(a); j++) {
    aa = class_superclass(a, j);
    if (aa != NIL && SeekAncestor(aa, b))
      return (TRUE);
  }
  return (FALSE);
}

/* L1 > otmu */
/* To be used inside an _ilu_EnumerateClasses enumeration */
ilu_boolean 
_ilu_IsSubObjectType(ilu_Class a, ilu_Class b)
{
  ilu_boolean     ans;
  ans = (b == _ilu_rootClass) || SeekAncestor(a, b);
  return (ans);
}

ilu_boolean 
ilu_IsSubObjectType(ilu_Class a, ilu_Class b)
{
  ilu_boolean     ans;
  _ilu_AcquireMutex(ilu_otmu);
  ans = (b == _ilu_rootClass) || SeekAncestor(a, b);
  _ilu_ReleaseMutex(ilu_otmu);
  return (ans);
}

typedef struct _iluBuffer_s {
  /* L1 unconstrained */

  char           *buf;
  unsigned long   allocated;
  unsigned long   used;
}              *iluBuffer;
#define BUF_INCREMENT 100

/* L1 unconstrained */

static void 
addBytesToBuf(iluBuffer buf, char *string, ilu_cardinal len)
{
  if (buf->allocated - buf->used < len) {
    buf->allocated += BUF_INCREMENT;
    buf->buf = (char *) ilu_realloc(buf->buf, buf->allocated);
  }
  strncpy(buf->buf + buf->used, string, len);
  buf->used += len;
}

/* L1 >= {otmu} */

static void 
UnshowAncestors(ilu_Class c)
{
  ilu_cardinal             i;
  c->cl_shown = FALSE;
  for (i = 0; i < class_superclass_count(c); i++)
    UnshowAncestors(class_superclass(c, i));
  return;
}

static void 
AddTypeName(ilu_Class type, iluBuffer buf)
{
  ilu_cardinal             i;
  int             noco = 1;
  addBytesToBuf(buf, class_unique_id(type),
		strlen(class_unique_id(type)));
  if (type->cl_shown != TRUE) {
    type->cl_shown = TRUE;
    if (class_superclass_count(type) == 0)
      return;
    addBytesToBuf(buf, "(", 1);
    for (i = 0; i < class_superclass_count(type); i++) {
      if (noco)
	noco = 0;
      else
	addBytesToBuf(buf, ",", 1);
      AddTypeName(class_superclass(type, i), buf);
    }
    addBytesToBuf(buf, ")", 1);
  }
  return;
}

/* L1_sup < otmu */
static ilu_string 
ClassToString(ilu_Class class)
{
  struct _iluBuffer_s buf;
  buf.allocated = BUF_INCREMENT;
  buf.buf = ilu_malloc(BUF_INCREMENT);
  buf.used = 0;
  _ilu_AcquireMutex(ilu_otmu);
  UnshowAncestors(class);
  AddTypeName(class, &buf);
  addBytesToBuf(&buf, "\0", 1);	/* NUL-terminate */
  _ilu_ReleaseMutex(ilu_otmu);
  return (buf.buf);
}

/* L1 >= {otmu} */

typedef struct _ilu_ID_DAG_s ID_DAG_s, *ID_DAG;

typedef struct _ilu_ID_DAG_Cons {
  /* L1 >= {otmu} */

  ID_DAG          idd;
  struct _ilu_ID_DAG_Cons *next;
}               ID_DAG_Cons, *ID_DAG_List;

struct _ilu_ID_DAG_s {
  /* L1 >= {otmu} */

  char           *id;
  ilu_Class       it;		/* non-NIL if id registered */
  ID_DAG_List     supers;
  ilu_boolean     computed;	/* TRUE => mska, saak significant */
  ilu_Class       mska;		/* the one Most Specific Known
				 * Ancestor, or NIL if not
				 * well-defined */
  ilu_boolean     saak;		/* Some Ancestors Are Known */
  ilu_boolean     visited;	/* is this an ancestor of a counted
				 * type? */
  ID_DAG          anext;	/* threaded list of most spcfc
				 * known anc'rs */
  ID_DAG          aprev;	/* it's doubly-linked */
};

static ID_DAG_s mska_head;
/*
 * Head of threaded list of most spcfc known anc'rs. No one member
 * is an ancestor of another.
 */

/* L1_sup < otmu */
ilu_Class 
_ilu_FindMSKA(ilu_string tid)
{
  ID_DAG          n;
  ilu_Class       ans;
  _ilu_AcquireMutex(ilu_otmu);
  n = (ID_DAG) _ilu_hash_FindInTable(UnknownTypeIDs, tid);
  if (n != NIL && n->computed)
    ans = n->mska;
  else
    ans = NIL;
  _ilu_ReleaseMutex(ilu_otmu);
  return ans;
}

/*
 * spec ::== id [ "(" spec ("," spec)* ")" ] At entry, *names is at
 * the start of a spec; at exit, *names is at the first char past
 * that spec. Result is the ID_DAG having the id in the spec.
 * Caller owns storage for names.
 */
static ID_DAG 
ClassNamesToID_DAG(ilu_string * names)
{
  ID_DAG          n;
  char           *pl, *pr, *pc, *pe, cr;
  ilu_integer     len;

  DEBUG(OBJECT_DEBUG,
  (stderr, "(ClassNamesToID_DAG):  Called with \"%s\"\n", *names));

  if (*names == NIL OR ** names == '\0')
    return (NIL);
  len = strlen(*names);
  pl = strchr(*names, '(');
  pr = strchr(*names, ')');
  pc = strchr(*names, ',');
  if (pl == NIL)
    pl = (*names) + len;
  if (pr == NIL)
    pr = (*names) + len;
  if (pc == NIL)
    pc = (*names) + len;
  pe = (pr < pl) ? pr : pl;
  pe = (pc < pe) ? pc : pe;
  cr = *pe;
  *pe = '\0';
  n = (ID_DAG) _ilu_hash_FindInTable(UnknownTypeIDs, *names);
  if (n == NIL) {
    ilu_string      id = _ilu_Strdup(*names);
    n = (ID_DAG) ilu_malloc(sizeof(ID_DAG_s));
    _ilu_Assert((int) _ilu_hash_AddToTable(UnknownTypeIDs, id, n),
		"ClassNamesToID_DAG AddToTable UnknownTypeIDs");
    n->id = id;
    n->it = (ilu_Class) _ilu_hash_FindInTable(ClassIDTable, id);
    n->supers = NIL;
    n->mska = n->it;
    n->computed = n->saak = n->mska != NIL;
    n->anext = n->aprev = NIL;
    if (cr == '(') {
      ID_DAG_List    *pp = &(n->supers);
      *names = pl + 1;
      while ((**names) != ')') {
	ID_DAG          d2 = ClassNamesToID_DAG(names);
	ID_DAG_List     l = (ID_DAG_List) ilu_malloc(sizeof(ID_DAG_Cons));
	l->idd = d2;
	l->next = NIL;
	*pp = l;
	pp = &(l->next);
	if ((**names) == ',')
	  (*names)++;
      }
      (*names)++;
    } else
      *names = pe;
  } else if (pl < pc) {		/* check for consistency */
    ID_DAG_List     l = n->supers;
    *names = pl + 1;
    while ((**names) != ')') {
      ID_DAG          d2 = ClassNamesToID_DAG(names);
      ASSERT(d2 == l->idd, buf,
	     (buf, "ClassNamesToID_DAG:  %s %s: %s vs. %s.\n",
	      "Disagreement on superclasses of", n->id, d2->id,
	      l->idd->id));
      l = l->next;
      if ((**names) == ',')
	(*names)++;
    }
    (*names)++;
  } else
    *names = pe;
  *pe = cr;
  return (n);
}

static void 
ClearVisited(ID_DAG t)
{
  ID_DAG_List     p;
  t->visited = FALSE;
  for (p = t->supers; p != NIL; p = p->next)
    ClearVisited(p->idd);
  return;
}

/*
 * Before and after: every node in the mska list is visited; for
 * each visited node n: 1. if n is known then n is an ancestor of a
 * node in the mska list; 2. every ancestor of n is visited.
 */

/*
 * As above, plus... After: all of t's ancestors are visited, and
 * none are in the list.
 */
static void 
MarkAncestors(ID_DAG t)
{
  ID_DAG_List     p;
  for (p = t->supers; p != NIL; p = p->next) {
    ID_DAG          u = p->idd;
    if (u->anext != NIL) {
      /*
       * must already be visited, and have no proper ancestors in
       * the mska list; remove from list.
       */
      u->aprev->anext = u->anext;
      u->anext->aprev = u->aprev;
    } else if (!(u->visited && (u->it != NIL))) {
      MarkAncestors(u);
      u->visited = TRUE;
    }
  }
  return;
}

static void 
FindMostSpecificType(ID_DAG t)
{
  ID_DAG_List     p;
  DEBUG(OBJECT_DEBUG,
	(stderr, "(FindMostSpecificType):  at %s, %s visited.\n",
	 t->id, (t->visited == FALSE) ? "not" : ""));
  if (t->visited == TRUE)
    return;
  if (t->it != NIL) {
    MarkAncestors(t);
    t->anext = mska_head.anext;
    t->aprev = &mska_head;
    t->anext->aprev = t;
    t->aprev->anext = t;
  } else {
    for (p = t->supers; p != NIL; p = p->next)
      FindMostSpecificType(p->idd);
  }
  t->visited = TRUE;
  return;
}

/* L1_sup < otmu */
static ilu_Class 
StringToClass(ilu_string s)
{
  ID_DAG          t;
  char           *q = s;
  ilu_Class       ans;

  if (s == NIL)
    return (NIL);
  _ilu_AcquireMutex(ilu_otmu);
  if (UnknownTypeIDs == NIL)
    UnknownTypeIDs = (ilu_refany) _ilu_hash_MakeNewTable(
					      CLASS_HASHTABLESIZE,
			     (ilu_hashfnptr) _ilu_hash_HashString,
			 (ilu_compfnptr) _ilu_hash_StringCompare);
  t = ClassNamesToID_DAG(&q);
  if (t == NIL)
    return (NIL);
  DEBUG(OBJECT_DEBUG,
  (stderr, "(StringToClass):  Converted names <%s> to DAG.\n", s));
  if (t->computed) {
    DEBUG(OBJECT_DEBUG, (stderr, "(StringToClass):  Old problem.\n"));
  } else {
    ClearVisited(t);
    mska_head.anext = mska_head.aprev = &mska_head;
    FindMostSpecificType(t);
    t->saak = mska_head.anext != &mska_head;
    if (t->saak && (mska_head.anext->anext == &mska_head))
      t->mska = mska_head.anext->it;
    else
      t->mska = NIL;
    t->computed = TRUE;
  }
  ans = t->mska;
  _ilu_ReleaseMutex(ilu_otmu);
  if (ans == NIL) {
    DEBUG(OBJECT_DEBUG,
	  (stderr, 
	   "(StringToClass):  %s ancestors of type <%s> are known!",
	   t->saak ? "Multiple" : "No", s));
  } else {
    DEBUG(OBJECT_DEBUG,
	  (stderr, "(StringToClass):  Found class %s.\n",
	   class_name(ans)));
  }
  return (ans);
}

/* Main Invariant holds; L2 otherwise unconstrained */

static          ilu_boolean
ObtainTypes(ilu_Object o, ilu_string * types, ilu_cardinal * typeslen,
	    ILU_ERRS((bad_locks, inv_objref,
		      no_resources, IoErrs)) * err)
{
  ilu_Call_s      call_s;
  ilu_Call        call = &call_s;
  ilu_cardinal    reqSize;
  ilu_cardinal    estatus = 0;
  ilu_ProtocolException internal;
  ilu_Class       pclass = object_class(o);
  ilu_Server      s = object_server(o);
  ilu_Connection  newconn;
  ilu_boolean     ans;
  DEBUG(OBJECT_DEBUG, (stderr, "_ilu_FindClassViaRPC:  object %p...\n",
		       o));
  ans = ilu_StartCall(call, s, _ilu_rootClass, _ilu_GetTypesMethod, 0,
		      NIL, &newconn, err);
  if (newconn != NIL)
    _ilu_HandOffNewConnection(newconn, err);
  if (ILU_ERRNOK(*err))
    return FALSE;
  _ilu_AcquireMutex(server_lock(s));
  reqSize = ilu_SizeOfObjectID(call, o, TRUE, _ilu_rootClass, err);
  _ilu_ReleaseMutex(server_lock(s));
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_StartRequest(call, reqSize, err))
    goto faild;
  ilu_EnterServer(s, object_class(o));
  ilu_OutputObjectID(call, o, TRUE, _ilu_rootClass, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  if (!ilu_FinishRequest(call, err))
    goto faild;
  internal = ilu_GetReply(call, &estatus, err);
  if (internal == ilu_ProtocolException_Not)
    goto faild;
  if (internal != ilu_ProtocolException_Success) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  if (estatus != 0) {
    (void) ILU_ERR_CONS1(internal, err, minor, ilu_im_callFail, 0);
    goto faild;
  }
  ilu_InputString(call, types, typeslen, 0xFFFF, err);
  if (ILU_ERRNOK(*err))
    goto faild;
  ilu_ReplyRead(call, err);
  if (ILU_ERRNOK(*err))
    goto faild;
faild:
  ilu_FinishCall(call, err);
  if (ILU_ERRNOK(*err)) {
    ILU_HANDLED(*err);
    return FALSE;
  }
  return TRUE;
}

ilu_Class 
_ilu_FindClassViaRPC(ilu_Object o)
{
  ilu_string      types = NIL;
  ilu_cardinal    typeslen;
  ilu_Class       c = NIL;
  ilu_Class       pclass = object_class(o);
  ILU_ERRS((bad_locks, inv_objref,
		      no_resources, IoErrs)) lerr;

  if (class_singleton(pclass)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr,
	 "%s %s is singleton, not attempting GetTypes RPC call.\n",
	   "_ilu_FindClassViaRPC:  pclass", class_name(pclass)));
    return (NIL);
  }

  _ilu_Assert(!server_is_true(object_server(o)), "_ilu_FindClassViaRPC: called on true object");
  
  if (!ObtainTypes(o, &types, &typeslen, &lerr)) {
    DEBUG(OBJECT_DEBUG,
	  (stderr, "_ilu_FindClassViaRPC:  no types.\n"));
    DEBUG(OBJECT_DEBUG,
	  (stderr, "GetTypes call raised %s from %s:%d\n",
	   ILU_ERR_NAME(lerr), ilu_ErrorFile(&lerr),
	   ilu_ErrorLine(&lerr)));
  } else
    DEBUG(OBJECT_DEBUG,
	  (stderr, "_ilu_FindClassViaRPC:  typestring is <%s>...\n",
	   types));

  if (types != NIL) {
    c = StringToClass(types);
#ifdef ENABLE_DEBUGGING
    if (c == NIL) {
      ilu_DebugPrintf("ILU Error:  Unable to determine the object type of object <%s/%s>.\n",
		      server_id(object_server(o)), object_ih(o));
      ilu_DebugPrintf("            The specified type codes <%s> are unknown in this address space.\n",
		      types);
      ilu_DebugPrintf("            This may indicate either an ILU version mismatch, or an ISL interface version mismatch.\n");
    };
#endif
    FREETOKEN(types);
  }
  DEBUG(OBJECT_DEBUG,
	(stderr, "_ilu_FindClassViaRPC:  class is \"%s\".\n",
	 (c == NIL) ? "*unknown*" : class_name(c)));
  return (c);
}

/* L2    >=    {conn's callmu, iomu} before;
 * L2 disjoint {conn's callmu, iomu} after */
void 
_ilu_HandleGetTypes(ilu_Call call)
{
  ilu_Object      disc;
  ilu_string      names = NIL;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_InputObjectID(call, &disc, TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto dun;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
  }
  if (!ilu_RequestRead(call, &lerr))
    goto dun;
  if (disc != NIL && object_class(disc) != NIL)
    names = ClassToString(object_class(disc));
  if (names == NIL) {
    ilu_cardinal asize =
      ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto dun;
    if (!ilu_FinishException(call, &lerr))
      goto dun;
  } else {
    ilu_cardinal    alen, len = strlen(names);
    alen = ilu_BeginSizingReply (call, FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    alen += ilu_SizeOfString(call, names, len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_BeginReply(call, FALSE, alen, &lerr))
      goto dun;
    ilu_OutputString(call, names, len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
  }
dun:
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = _ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;
}

/* L2    >=    {conn's callmu, iomu} before;
 * L2 disjoint {conn's callmu, iomu} after */
static void 
_ilu_HandleIsA(ilu_Call call)
{
  ilu_Object      disc;
  ilu_string      typeid = NIL;
  ilu_cardinal    typeid_len = 0;
  ilu_Error       lerr = ILU_INIT_NO_ERR;
  ilu_Class       qtype;

  ilu_InputObjectID(call, &disc, TRUE, _ilu_rootClass, &lerr);
  if (ILU_ERRNOK(lerr))
    goto dun;
  if (disc != NIL) {
    lerr = _ilu_DeltaHolds(disc, 1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(object_server(disc), _ilu_rootClass);
    ilu_InputString(call, &typeid, &typeid_len, 0xFFFF, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
  }
  if (!ilu_RequestRead(call, &lerr))
    goto dun;

  if (disc == NIL || object_class(disc) == NIL || typeid == NIL) {
    ilu_cardinal asize =
      ilu_BeginSizingException (call, - (ilu_integer) ilu_ProtocolException_GarbageArguments, &lerr);
    if (ILU_ERRNOK(lerr)) goto dun;
    if (!ilu_BeginException(call, - (ilu_integer) ilu_ProtocolException_GarbageArguments,
			    asize, &lerr))
      goto dun;
    if (!ilu_FinishException(call, &lerr))
      goto dun;
  } else {
    ilu_cardinal    alen;
    ilu_boolean result = ((qtype = ilu_FindClassFromID(typeid)) != NIL &&
			  ilu_IsSubObjectType(object_class(disc), qtype));
    alen = ilu_BeginSizingReply (call, ilu_FALSE, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    alen += ilu_SizeOfBoolean(call, result, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_BeginReply(call, ilu_FALSE, alen, &lerr))
      goto dun;
    ilu_OutputBoolean(call, result, &lerr);
    if (ILU_ERRNOK(lerr))
      goto dun;
    if (!ilu_FinishReply(call, &lerr))
      goto dun;
  }
dun:
  if (typeid != NIL)
    ilu_free(typeid);
  ILU_HANDLED(lerr);
  if (disc != NIL) {
    ilu_Server      s = object_server(disc);
    ilu_Class       cl = object_class(disc);
    ilu_EnterServer(s, cl);
    lerr = _ilu_DeltaHolds(disc, -1);
    ILU_MUST_BE_SUCCESS(lerr);
    ilu_ExitServer(s, cl);
  }
  return;
}

