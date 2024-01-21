/*
 ** Copyright (c) 1991-1995 Xerox Corporation.  All Rights Reserved.
 **
 ** Unlimited use, reproduction, and distribution of this software is
 ** permitted.  Any copy of this software must include both the above
 ** copyright notice of Xerox Corporation and this paragraph.  Any
 ** distribution of this software must comply with all applicable United
 ** States export control laws.  This software is made available AS IS,
 ** and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 ** INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 ** AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 ** PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 ** THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 ** CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 ** XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 */
/* $Id: code.c,v 1.109 1996/06/20 17:57:25 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 20, 1996 10:43 am PDT */

#include "cstubber.h" 

struct double_s {
  Context         c;
  Type            t;
  unsigned int    id;
  Argument        default_arm;
};

static boolean  IncludeComma = FALSE;
static int mCount;

static char   *ArrayName = NULL;
static unsigned  ExceptionIndex = 0;
static unsigned  MethodIndex = 0;

static void classIDs (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "\t\"%s\",\n", ur_type(class)->uid);
}

static void generateClassTable (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  char           *tn;
  Class           o;

  if (type_basic_type(class) != object_Type)
    return;
  tn = c_type_name(class);
  o = class_object(class);

  fprintf(context->file, "ilu_Class _%s__ILUType = NULL;\n", tn);

}

static void 
generateExceptionEntry(refany elt, refany rock)
{
  Exception e = (Exception) elt;
  Context context = (Context) rock;
  if (e->interface == context->interface && e->import == NULL)
    fprintf(context->file,
	    "ILU_C_ExceptionCode _%s__Exception_%s = ILU_NIL;\n",
	    c_interface_name(e->interface), c_simple_name(e->name));
}

static void 
generateExceptionTable(Interface interface, Context context)
{
  list_enumerate(interface->exceptions, generateExceptionEntry,
		 context);
}

static void setExceptionValue (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  TypeKind        t;
  Type            ut = ur_type(e->type);

  if (e->type == NULL)
    return;
  t = type_basic_type(ut);
  fprintf(f, "  else if (stat->returnCode == ex_%s) {\n",
	  c_exception_name(e));
  fprintf(f, "    stat->ptr = (void *) ilu_must_malloc (sizeof (%s));\n",
	  c_type_name(ut));
  if (t == array_Type) {
    char           *rtn = c_return_type(ut);
    fprintf(f, "    memcpy ((void *)((%s)stat->ptr), (void *) va_arg(ap, %s), sizeof(%s));\n",
	    rtn, rtn, c_type_name(ut));
  } else
    fprintf(f, "    *(%s*)stat->ptr = %sva_arg (ap, %s);\n",
	    c_type_name(ut),
	    ((t == record_Type || t == union_Type
	      || (t == sequence_Type && !TypeIsEitherString(ut)))
	     ? "*" : ""),
	    c_parameter_type(ut, In));
  if (HasFreeRoutine(ut))
    fprintf(f, "    stat->freeRoutine = (void (*) (void *)) %s__Free;\n",
	    c_type_name(UltimateType(ut)));
  else
    fprintf(f, "    stat->freeRoutine = ((void (*) (void *)) 0);\n");
  fprintf(f, "  }\n");
}

static void generateSignalException (Context  context)
{
  fprintf(context->file, "#include <stdarg.h>\n\n");
  fprintf(context->file, "void %s__BindExceptionValue (ILU_C_ENVIRONMENT *stat, ilu_Exception exception, ...)\n",
	  c_interface_name(context->interface));
  fprintf(context->file, "{\n");
  fprintf(context->file, "  va_list ap;\n");
  fprintf(context->file, "  va_start (ap, exception);\n");
  fprintf(context->file, "  stat->_major = ILU_C_USER_EXCEPTION;\n");
  fprintf(context->file, "  stat->returnCode = exception;\n");
  fprintf(context->file, "  if (exception == NULL)\n");
  fprintf(context->file, "    /* no exception */;\n");
  list_enumerate(context->interface->exceptions, setExceptionValue,
		 context);
  fprintf(context->file, "  else\n");
  fprintf(context->file,
	  "    _ilu_Assert(0, \"bad exn given to %s__BindExceptionValue\");\n",
	  c_interface_name(context->interface));
  fprintf(context->file, "  va_end (ap);\n");
  fprintf(context->file, "}\n\n");
}

static void generateExceptionProcs (Context  context)
{
  if (list_size(context->interface->exceptions) > 0)
    generateSignalException (context);
}

static int DimStop;
static int DimCount;

static void dimHeader(refany elt, refany rock)
{
  long            d = (long) elt;
  Context         context = (Context) rock;
  if (DimStop >= 0 && DimCount >= DimStop)
    return;

  fprintf(context->file, "%*.*s{\n",
	  DimCount * 2 + 2, DimCount * 2 + 2, "");
  fprintf(context->file, "%*.*sregister int _i%u;\n",
	  DimCount * 2 + 4, DimCount * 2 + 4, "", DimCount);
  fprintf(context->file,
	  "%*.*sfor (_i%u = 0;  _i%u < %lu;  _i%u += 1)\n",
	  DimCount * 2 + 4, DimCount * 2 + 4, "",
	  DimCount, DimCount, d, DimCount);
  DimCount += 1;
}

static void dimRef(refany elt, refany rock)
{
  long            d = (long) elt;
  char           *buf = (char *) rock;
  if (DimStop >= 0 && DimCount >= DimStop)
    return;

  sprintf(buf + strlen(buf), "[_i%u]", DimCount);
  DimCount += 1;
}

static void dimFooter(refany elt, refany rock)
{
  long            d = (long) elt;
  Context         context = (Context) rock;
  DimCount -= 1;
  if (DimStop >= 0 && DimCount < DimStop)
    return;

  fprintf(context->file, "%*.*s}\n",
	  (DimCount - DimStop) * 2, (DimCount - DimStop) * 2, "");
}

static void outputRecordArg (Argument arg, Context context)
{
  char            buf[1000];
  Type            ut = ur_type(arg->type);
  TypeKind        t = type_basic_type(ut);

  sprintf(buf, "(%s_val->%s)",
	  ((t == record_Type || t == union_Type ||
	    (t == sequence_Type && !TypeIsEitherString(ut)))
	   ? "&" : ""),
	  c_argument_name(arg));
  MarshallValue(context, ut, buf, 2);
}

static void caseConst (refany elt, refany rock)
{
  ConstantValue   val = (ConstantValue) elt;
  struct double_s *s = (struct double_s *) rock;
  switch (val->type)
    {
    case integer_Type:
    case shortinteger_Type:
    case cardinal_Type:
    case shortcardinal_Type:
    case byte_Type:
      fprintf (s->c->file, "    case %s%ld:\n", (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
      break;

    case shortcharacter_Type:
      fprintf (s->c->file, "    case %s_%s:\n", c_interface_name (s->t->interface), val->val.s);
      break;

    case boolean_Type:
      fprintf (s->c->file, "    case ilu_%s:\n", val->val.b ? "TRUE" : "FALSE");
      break;

    default:       error ("illegal discriminator value\n");
    }
}

static void outputUnionType (Argument a, struct double_s * s)
{
  char            buffer[1000];
  int             mkReferent;
  Type            ut = ur_type(a->type);
  TypeKind        t = type_basic_type(ut);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));
  mkReferent = (t == record_Type || t == union_Type ||
		t == sequence_Type && !TypeIsEitherString(ut));
  sprintf(buffer, "%s_val->_u.%s", (mkReferent) ? "&" : "", name);
  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  MarshallValue(s->c, ut, buffer, 6);
  fprintf(s->c->file, "      break;\n");
}

static void ComputeTotalNumberOfElements(refany elt, refany rock)
{
  unsigned long   dim = (unsigned long) elt;
  unsigned long  *total = (unsigned long *) rock;
  *total = *total * dim;
}

static void FindZero(refany elt, refany rock)
{
  boolean        *z = (boolean *) rock;
  if (elt == 0)
    *z = TRUE;
}

static void generateOutputCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description (type);
  char buf[1000];

  fprintf(context->file,
	  "void _%s__Output (ilu_Call _call, %s _val, ilu_Error *_err)\n{\n",
	  c_type_name(type), c_parameter_type(type, In));
  if (t != optional_Type)
    fprintf (context->file, "  if (_val == NULL)\n    return;\n\n");

  if (t == array_Type)
    {
      unsigned long   size = 1;
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);
      boolean         zeroarray = FALSE;
      Type            et = d->structuredDes.array.type;
      TypeKind        etk = type_basic_type(et);

      list_enumerate(dims, FindZero, &zeroarray);

      list_enumerate(dims, ComputeTotalNumberOfElements, &size);

      if (etk == byte_Type OR etk == shortcharacter_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*silu_Output%s (_call,"
		  /* " (%s)" */
		  " (%s), %lu, _err);\n",
		  nDims * 2, nDims * 2, "",
		  (etk == byte_Type) ? "Opaque" : "StringVec",
		  /* (etk == byte_Type) ? "ilu_bytes" : "ilu_string", */
		  buf,
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims - 1;
	  DimStop = 0;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else if (etk == character_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*silu_OutputWStringVec (_call, _val, %lu, _err);\n",
		  nDims * 2 + 2, nDims * 2 + 2, "",
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims - 1;
	  DimStop = 0;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else
	{
	  fprintf (context->file, "  ilu_OutputArray (_call, %lu, _err);  if (ILU_ERRNOK(*_err)) return;\n", size);
	  DimStop = -1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf(buf, "%s_val",
		  ((etk == record_Type || etk == union_Type ||
		    etk == sequence_Type && !TypeIsEitherString(et)))
		  ? "&" : "");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  MarshallValue (context, ur_type(et), buf, 2*nDims+4);
	  DimCount = nDims;
	  list_enumerate(dims, dimFooter, context);
	}

      fprintf (context->file, "  ilu_EndArray (_call, _err);\n");
    }

  else if (t == sequence_Type)
    {
      Type            et = ur_type(d->structuredDes.sequence.type);
      TypeKind        st = type_basic_type(et);

      if (st == byte_Type)
	{
	  fprintf (context->file, "  ilu_OutputBytes (_call, _val->_buffer, _val->_length, %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else if (st == shortcharacter_Type)
	{
	  fprintf (context->file, "  ilu_OutputString (_call, _val, _ILU_C_SafeStrlen(_val), %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else if (st == character_Type)
	{
	  fprintf (context->file, "  ilu_OutputWString (_call, _val, _ILU_C_SafeWStrlen(_val), %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else
	{
	  fprintf (context->file, "  ilu_OutputSequence (_call, _val->_length, %lu, _err);\n",
		   d->structuredDes.sequence.limit);
	  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return;\n");
	  fprintf (context->file, "  {\n    %s *p;  int i;\n\n", c_type_name (et));
	  fprintf (context->file, "    for (p = _val->_buffer, i = 0;  i < _val->_length;  p++, i++)\n");
	  sprintf(buf, "%sp",
		  (st == record_Type || st == union_Type ||
		   st == sequence_Type && !TypeIsEitherString(et))
		  ? "" : "*");
	  MarshallValue (context, et, buf, 6);
	  fprintf (context->file, "  }\n  ilu_EndSequence (_call, _err);\n");
	}
    }
  else if (t == union_Type)
    {
      struct double_s  s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.id = 0;
      s.default_arm = d->structuredDes.uniond.default_arm;
      fprintf (context->file, "  ilu_OutputUnion (_call, _val->_d, sizeof(%s), _err);\n",
	       c_type_name(s.t));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return;\n");
      fprintf (context->file, "  switch (_val->_d) {\n");
      list_enumerate (d->structuredDes.uniond.types, (void (*)(void *,void *)) outputUnionType, &s);
      if (d->structuredDes.uniond.default_arm != NULL)
	; /* handled in outputUnionType */
      else if (d->structuredDes.uniond.others_allowed)
	{
	  fprintf (context->file, "    default:\n      break;\n");
	}
      else
	{
	  fprintf (context->file, "    default:\n");
	  fprintf (context->file, "      fprintf (stderr, \"_%s__Output:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n      break;\n",
		   c_type_name(type));
	}
      fprintf (context->file, "  };\n  ilu_EndUnion (_call, _err);\n");
    }

  else if (t == record_Type)
    {
      fprintf (context->file, "  ilu_OutputRecord (_call, _err);\n");
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return;\n");
      list_enumerate (d->structuredDes.record, (void (*)(void *,void *)) outputRecordArg, context);
      fprintf (context->file, "  ilu_EndRecord (_call, _err);\n");
    }
  else if (t == optional_Type)
    {
      Type            ut2 = ur_type(d->structuredDes.optional);
      int             indo, indp;
      while (type_kind(ut2) == optional_Type)
	ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      fprintf(context->file,
	 "  ilu_OutputOptional (_call, _val != ILU_NIL, _err);\n");
      fprintf(context->file,
	"  if (ILU_ERRNOK(*_err) || (_val == ILU_NIL)) return;\n");
      indo = !TypeIsPointer(ut2);
      indp = Indirectness(ut2, role_In, FALSE, 0);
      MarshallValue(context, ut2, (indo > indp) ? "*_val" : "_val", 2);
    }

  fprintf (context->file, " marshalError:\n  return;\n}\n\n");
}

static void inputRecordArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  sprintf(buf, "%s_val->%s",
	  Indirectness(arg->type, role_InOut, FALSE, 0) ? "&" : "",
	  c_argument_name(arg));
  UnmarshallValue(context, ur_type(arg->type), 0, buf, 2);
}

static void inputUnionCase(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;
  char            buffer[1000];
  Type            ut = ur_type(a->type);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));

  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  sprintf(buffer, "%s_val->_u.%s",
	  Indirectness(ut, role_InOut, FALSE, 0) ? "&" : "",
	  name);
  UnmarshallValue(s->c, ut, 0, buffer, 6);
  fprintf(s->c->file, "      break;\n");
}

/*
  We generate these for a small number of types:  arrays, records, sequences, unions,
  and optional.  Everything else is done explicitly.

  For arrays, the Input routine takes an optional argument of type ArrayType, and
  returns an element of type "BaseType *" (since arrays cannot be directly returned).
  For sequences, records, and unions, the Input routine takes an optional argument of
  type Type *, and returns an element of type Type *.  For optional, the Input
  routine takes an optional argument of type Type *, and returns a value of type
  Type.
*/

static void generateInputCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description(type);
  char           *name = c_type_name(type);
  char           *parm = c_parameter_type(type, InOut);
  char           *rettype = c_role_type(type, role_InpRet, FALSE);
  FILE           *f = context->file;

  context->class = type;

  fprintf(f,
      "%s _%s__Input (ilu_Call _call, %s _ref, ilu_Error *_err)\n",
	  rettype, name, parm);
  fprintf(f, "{\n  %s _val;\n\n", rettype);
  if (t == array_Type) {
    char            buf[1000];
    list            dims = d->structuredDes.array.dimensions;
    int             nDims = list_size(dims);
    boolean         zeroarray = FALSE;
    Type            type2 = ur_type(d->structuredDes.array.type);
    TypeKind        t2 = type_basic_type(type2);

    list_enumerate(dims, FindZero, &zeroarray);

    fprintf(f, "  if (_ref != NULL)\n");
    fprintf(f, "    _val = _ref;\n", rettype);
    fprintf(f, "  else\n");
    fprintf(f, "    _val = (%s) ilu_MallocE(sizeof(%s), _err);\n",
	    rettype, name);
    fprintf(f, "  if (_val == NULL)  /* malloc failure */\n");
    fprintf(f, "    return _val;\n");

    if (t2 == byte_Type || t2 == shortcharacter_Type) {
      DimStop = nDims - 1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "%s", "_val");
      list_enumerate(dims, dimRef, buf);
      fprintf(f, "%*.*s{\n", nDims * 2, nDims * 2, "");
      fprintf(f, "%*.*s%s _tmp = %s;\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (t2 == byte_Type) ? "ilu_opaque" : "ilu_string",
	      buf);
      fprintf(f, "%*.*silu_Input%s (_call, &_tmp, %lu, _err);\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (t2 == byte_Type) ? "Opaque" : "StringVec",
	      (cardinal) list_ref(dims, nDims - 1));
      fprintf(f, "%*.*s}\n", nDims * 2, nDims * 2, "");
      DimCount = nDims - 1;
      DimStop = 0;
      list_enumerate(dims, dimFooter, context);
      DimStop = -1;
    } else if (t2 == character_Type) {
      DimStop = nDims - 1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "%s", "_val");
      list_enumerate(dims, dimRef, buf);
      fprintf(f, "%*.*s{\n", nDims * 2, nDims * 2, "");
      fprintf(f, "%*.*silu_character *_tmp = %s;\n",
	      nDims * 2 + 2, nDims * 2 + 2, "", buf);
      fprintf(f,
	   "%*.*silu_InputWStringVec (_call, &_tmp, %lu, _err);\n",
	      nDims * 2 + 2, nDims * 2 + 2, "",
	      (cardinal) list_ref(dims, nDims - 1));
      fprintf(f, "%*.*s}\n", nDims * 2, nDims * 2, "");
      DimCount = nDims - 1;
      DimStop = 0;
      list_enumerate(dims, dimFooter, context);
      DimStop = -1;
    } else {
      fprintf(f, "  ilu_InputArray (_call, _err);\n");
      fprintf(f, "  if (ILU_ERRNOK(*_err)) return _val;\n");
      DimCount = 0;
      DimStop = -1;
      list_enumerate(dims, dimHeader, context);
      DimCount = 0;
      sprintf(buf, "%s_val",
	      Indirectness(type2, role_InOut, FALSE, 0) ? "&" : "",
	      name);
      list_enumerate(dims, dimRef, buf);
      UnmarshallValue(context, d->structuredDes.array.type,
		      type->def, buf, nDims * 2 + 2);
      DimCount = nDims;
      list_enumerate(dims, dimFooter, context);
      fprintf(f, "  ilu_EndArray (_call, _err);\n");
    }
  } else if (t == sequence_Type) {
    Type            ut2 = ur_type(d->structuredDes.sequence.type);
    TypeKind        t2 = type_basic_type(ut2);

    if (t2 == shortcharacter_Type) {
      fprintf(f, "  ilu_cardinal len = 0;\n");
      fprintf(f, "  ilu_InputString (_call, &_val, &len, %u, _err);\n",
	      d->structuredDes.sequence.limit);
      fprintf(f, "  if (_ref != ILU_NIL) *_ref = _val;\n");
    } else if (t2 == character_Type) {
      fprintf(f, "  ilu_cardinal len = 0;\n");
      fprintf(f, "  ilu_InputWString (_call, &_val, &len, %u, _err);\n",
	      d->structuredDes.sequence.limit);
      fprintf(f, "  if (_ref != ILU_NIL) *_ref = _val;\n");
    } else if (t2 == byte_Type) {
      fprintf(f, "  ilu_cardinal len;\n");
      fprintf(f, "  ilu_bytes b = NULL;\n\n");
      fprintf(f, "  if (_ref == NULL) {\n");
      fprintf(f, "    _val = (%s *) ilu_MallocE(sizeof(%s), _err);\n",
	      name, name);
      fprintf(f, "    if (_val == NULL)  /* malloc failure */\n");
      fprintf(f, "      return _val;\n");
      fprintf(f, "  } else\n    _val = _ref;\n");
      fprintf(f, "  ilu_InputBytes (_call, &b, &len, %lu, _err);\n",
	      d->structuredDes.sequence.limit);
      fprintf(f, "  _val->_maximum = len;\n");
      fprintf(f, "  _val->_length = len;\n");
      fprintf(f, "  _val->_buffer = b;\n");
    } else {
      fprintf(f, "  ilu_cardinal _count, _index;\n");
      fprintf(f, "  %s _tmp;\n\n", c_type_name(ut2));
      fprintf(f, "  ilu_InputSequence (_call, &_count, %lu, _err);\n",
	      d->structuredDes.sequence.limit);
      fprintf(f, "  if (ILU_ERRNOK(*_err))\n");
      fprintf(f, "    return _val;\n");
      fprintf(f, "  if (_ref != NULL) {\n");
      fprintf(f, "    _val = _ref;\n");
      fprintf(f, "    %s_Init(_val, 0, NULL);\n    }\n", name);
      fprintf(f, "  else {\n");
      fprintf(f, "    _val = (%s) ilu_MallocE(sizeof (%s), _err);\n",
	      parm, name);
      fprintf(f, "    if (_val == NULL)  /* malloc failure */\n");
      fprintf(f, "      return _val;\n");
      fprintf(f, "    _val->_maximum = 0;\n");
      fprintf(f, "    _val->_length = 0;\n");
      fprintf(f, "    _val->_buffer = NULL;\n  }\n");
      fprintf(f, "  for (_index = 0;  _index < _count;  _index++)\n");
      fprintf(f, "  {\n");
      UnmarshallValue(context, ut2, type->def,
		      (Indirectness(ut2, role_InOut, FALSE, 0)
		       ? "&_tmp" : "_tmp"),
		      4);
      fprintf(f, "    if (ILU_ERRNOK(*_err))\n");
      fprintf(f, "       return _val;\n");
      fprintf(f, "    %s_Append (_val, %s_tmp);\n",
	      c_type_name(type),
	      Indirectness(ut2, role_In, FALSE, 0) ? "&" : "");
      fprintf(f, "  }\n");
      fprintf(f, "  ilu_EndSequence (_call, _err);\n");
    }
  } else if (t == union_Type) {
    struct double_s s;
    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.id = 0;
    s.default_arm = d->structuredDes.uniond.default_arm;

    fprintf(f, "  ilu_cardinal tag;\n\n");
    fprintf(f, "  ilu_InputUnion (_call, &tag, sizeof(%s), _err);\n",
	    c_type_name(s.t));
    fprintf(f, "  if (ILU_ERRNOK(*_err))\n");
    fprintf(f, "    return _val;\n");
    fprintf(f, "  if (_ref != NULL)\n    _val = _ref; \n");
    fprintf(f, "  else\n");
    fprintf(f, "    _val = (%s) ilu_MallocE(sizeof (%s), _err);\n",
	    parm, name);
    fprintf(f, "  if (_val == NULL)  /* malloc failure */\n");
    fprintf(f, "    return _val;\n");
    fprintf(f, "  switch (tag) {\n");
    list_enumerate(d->structuredDes.uniond.types, inputUnionCase, &s);
    if (s.default_arm != NULL);
    else if (d->structuredDes.uniond.others_allowed)
      fprintf(f, "    default:\n      break;\n");
    else {
      fprintf(f, "    default:\n");
      fprintf(f, "      fprintf (stderr, \"_%s__Input:  %s\", %s);\n",
	      c_type_name(type),
	      "Bad value %lu received for discriminant.\\n",
	      "(unsigned long) tag");
      fprintf(f, "      break;\n");
    }
    fprintf(f, "  }\n");
    fprintf(f, "  _val->_d = (%s) tag;\n", c_type_name(s.t));
    fprintf(f, "  ilu_EndUnion (_call, _err);\n");
  } else if (t == record_Type) {
    fprintf(f, "  ilu_InputRecord (_call, _err);\n");
    fprintf(f, "  if (_ref != NULL)\n");
    fprintf(f, "    _val = _ref;\n  else\n");
    fprintf(f, "    _val = (%s) ilu_MallocE (sizeof (%s), _err);\n",
	    parm, name);
    fprintf(f, "  if (_val == NULL)  /* malloc failure */\n");
    fprintf(f, "    return _val;\n");
    list_enumerate(d->structuredDes.record, inputRecordArg, context);
    fprintf(f, "  ilu_EndRecord (_call, _err);\n");
  } else if (t == optional_Type) {
    Type            t2 = ur_type(d->structuredDes.optional);
    TypeKind        utk2;
    boolean         isptr;
    int             indv, indp;
    while (type_kind(t2) == optional_Type)
      t2 = ur_type(type_description(t2)->structuredDes.optional);
    utk2 = type_kind(t2);
    fprintf(f, "  %s _ptr = ILU_NIL;\n", name);
    fprintf(f, "  ilu_boolean _present;\n");
    fprintf(f, "  ilu_InputOptional (_call, &_present, _err);\n");
    fprintf(f, "  if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
    fprintf(f, "  if (_ref != ILU_NIL)\n    _val = _ref;\n");
    fprintf(f, "  else _val = (%s *) ilu_MallocE (sizeof (%s), _err);\n",
	    name, name);
    fprintf(f, "  if (_val == NULL)  /* malloc failure */\n");
    fprintf(f, "    return _val;\n");
    fprintf(f, "  if (_present)\n    {\n");
    if (isptr = TypeIsPointer(t2))
      0;
    else {
      fprintf(f, "    *_val = (%s*) ilu_MallocE (sizeof (%s), _err);\n",
	      c_type_name(t2), c_type_name(t2));
      fprintf(f, "    if (ILU_ERRNOK(*_err)) return ILU_NIL;\n");
    }
    indv = 1 + !isptr;
    indp = Indirectness(t2, role_InOut, FALSE, 0);
    UnmarshallValue(context, t2, 0, "**_val" + 2 - (indv - indp), 6);
    fprintf(f, "    }\n  else *_val = ILU_NIL;\n");
  } else {
    fprintf(stderr, "Bad type %s passed to generateInputCode\n",
	    c_type_name(type));
    exit(1);
  }
  fprintf(f, " marshalError:\n  return _val;\n}\n\n");
}

static void sizeUnionType (Argument a, struct double_s *s)
{
  char            buffer[1000];
  char           *name;
  Type            ut = ur_type(a->type);
  TypeKind        t = type_basic_type(ut);

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name (a->name);
  else
    name = (char *) c_string (type_name (a->type));

  if (a == s->default_arm)
    fprintf (s->c->file, "    default:\n");
  else
    list_enumerate (a->values, caseConst, s);

  sprintf (buffer, "_val->_u.%s", name);
  fprintf (s->c->file, "      size += ");
  SizeValue (s->c, ut, buffer);
  fprintf (s->c->file, ";\n      break;\n");
}

static void sizeRecordArg (Argument arg, Context context)
{
  char            buf[1000];
  Type            ut = ur_type(arg->type);
  TypeKind        t = type_basic_type(ut);

  sprintf(buf, "_val->%s", c_argument_name(arg));
  fprintf(context->file, "  size += ");
  SizeValue(context, ut, buf);
  fprintf(context->file, ";\n");
}

static void generateSizeofCode (Type type, TypeKind t, Context context)
{
  char buf[1000];
  TypeDescription d = type_description (type);

  fprintf (context->file, "ilu_cardinal _%s__SizeOf (ilu_Call _call, %s _val, ilu_Error *_err)\n{\n",
	   c_type_name(type), c_parameter_type (type, In));
  fprintf (context->file, "  ilu_cardinal size = 0;\n\n");
  if (t == array_Type)
    {
      unsigned long   size = 1;
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);
      boolean         zeroarray = FALSE;
      Type            et = d->structuredDes.array.type;
      TypeKind        etk = type_basic_type(et);

      list_enumerate(dims, FindZero, &zeroarray);
      list_enumerate(dims, ComputeTotalNumberOfElements, &size);

      if (etk == byte_Type OR etk == shortcharacter_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*ssize += ilu_SizeOf%s (_call,"
		  /* " (%s)" */
		  " (%s), %lu, _err);\n",
		  nDims * 2, nDims * 2, "",
		  (etk == byte_Type) ? "Opaque" : "StringVec",
		  /* (etk == byte_Type) ? "ilu_bytes" : "ilu_string", */
		  buf,
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims;
	  DimStop = 1;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else if (etk == character_Type)
	{
	  DimStop = nDims - 1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf (buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf(context->file,
		  "%*.*ssize += ilu_SizeOfWStringVec (_call, _val, %lu, _err);\n",
		  nDims * 2 + 2, nDims * 2 + 2, "",
		  (cardinal) list_ref(dims, nDims - 1));
	  DimCount = nDims;
	  DimStop = 1;
	  list_enumerate(dims, dimFooter, context);
	  DimStop = -1;
	}
      else
	{
	  fprintf (context->file, "  size = ilu_SizeOfArray (_call, %lu, _err);\n", size);
	  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
	  DimStop = -1;
	  DimCount = 0;
	  list_enumerate(dims, dimHeader, context);
	  sprintf(buf, "_val");
	  DimCount = 0;
	  list_enumerate (dims, dimRef, buf);
	  fprintf (context->file, "%*.*ssize += ", nDims*2+4, nDims*2+4, "");
	  SizeValue (context, ur_type(et), buf);
	  fprintf (context->file, ";\n");
	  DimCount = nDims;
	  list_enumerate(dims, dimFooter, context);
	}

      fprintf (context->file, "  ilu_EndArray (_call, _err);\n");
    }

  else if (t == sequence_Type)
    {
      Type            et = d->structuredDes.sequence.type;
      Type            ut = ur_type(et);
      TypeKind        st = type_basic_type(ut);

      if (st == byte_Type)
	{
	  fprintf (context->file, "  size = ilu_SizeOfBytes (_call, _val->_buffer, _val->_length, %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else if (st == shortcharacter_Type)
	{
	  fprintf (context->file, "  size = ilu_SizeOfString (_call, _val, _ILU_C_SafeStrlen(_val), %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else if (st == character_Type)
	{
	  fprintf (context->file, "  size = ilu_SizeOfWString (_call, _val, _ILU_C_SafeWStrlen(_val), %u, _err);\n",
		   d->structuredDes.sequence.limit);
	}
      else
	{
	  fprintf (context->file, "  size = ilu_SizeOfSequence (_call, _val->_length, %lu, _err);\n",         d->structuredDes.sequence.limit);
	  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
	  fprintf (context->file, "  {\n    %s *p;  int i;\n\n", c_type_name (et));
	  fprintf (context->file, "    for (p = _val->_buffer, i = 0;  i < _val->_length;  p++, i++)\n      size += ");
	  SizeValue (context, ut, "*p");
	  fprintf (context->file, ";\n  }\n  ilu_EndSequence (_call, _err);\n");
	}
    }

  else if (t == union_Type)
    {
      struct double_s  s;

      s.c = context;
      s.t = ur_type(d->structuredDes.uniond.discriminator_type);
      s.id = 0;
      s.default_arm = d->structuredDes.uniond.default_arm;

      fprintf (context->file, "  size = ilu_SizeOfUnion (_call, _val->_d, sizeof(%s), _err);\n",
	       c_type_name(s.t));
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      fprintf (context->file, "  switch (_val->_d) {\n");
      list_enumerate (d->structuredDes.uniond.types, (void (*)(void *,void *)) sizeUnionType, &s);
      if (s.default_arm != NULL)
	;
      else if (d->structuredDes.uniond.others_allowed)
	fprintf (context->file, "    default:\n      break;\n");
      else
	fprintf (context->file, "    default:\n      fprintf (stderr, \"_%s__SizeOf:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n      break;\n", c_type_name(type));
      fprintf (context->file, "  };\n  ilu_EndUnion (_call, _err);\n");
    }

  else if (t == record_Type)
    {
      fprintf (context->file, "  size = ilu_SizeOfRecord (_call, _err);\n");
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      list_enumerate (d->structuredDes.record, (void (*)(void *,void *)) sizeRecordArg, context);
      fprintf (context->file, "  ilu_EndRecord (_call, _err);\n");
    }
  else if (t == optional_Type)
    {
      int             indo;
      Type            ut2 = ur_type(d->structuredDes.optional);
      while (type_kind(ut2) == optional_Type)
	ut2 = ur_type(type_description(ut2)->structuredDes.optional);
      indo = !TypeIsPointer(ut2);
      fprintf(context->file,
	      "  size = ilu_SizeOfOptional (_call, _val != ILU_NIL, _err);\n");
      fprintf (context->file, "  if (ILU_ERRNOK(*_err)) return 0;\n");
      fprintf (context->file, "  if (_val != ILU_NIL)\n    size += ");
      SizeValue (context, ut2, indo ? "*_val" : "_val");
      fprintf (context->file, ";\n");
    }
  else if (t == object_Type)
    ; /* done elsewhere */
  else
    {};

  fprintf (context->file, "  return size;\n}\n\n");
}

static void generateSequenceCreateCode (Type  type,  Context  context)
{
  string  name = c_type_name (type);

  fprintf (context->file,     "%s %s_%s_Create (unsigned long count, unsigned char *bytes)\n",     c_return_type (type),     c_interface_name (type->interface),     c_simple_name (type->name));
  fprintf (context->file, "{\n  %s n;\n", name);
  fprintf (context->file,     "  n._length = count;\n");
  fprintf (context->file,     "  n._maximum = count;\n");
  fprintf (context->file,     "  n._buffer = bytes;\n  return (n);\n}\n\n");
}

static void FreeRecordField(refany elt, refany rock)
{
  Argument        field = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];

  sprintf(buf, "_val->%s", c_argument_name(field));
  FreeValue(field->type, buf, role_Val, context, 2);
}

static void FreeUnionField(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  struct double_s *s = (struct double_s *) rock;
  char            buffer[1000];
  Type            ut = ur_type(a->type);
  char           *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name(a->name);
  else
    name = (char *) c_string(type_name(a->type));
  sprintf(buffer, "_val->_u.%s", name);
  if (a == s->default_arm)
    fprintf(s->c->file, "    default:\n");
  else
    list_enumerate(a->values, caseConst, s);
  FreeValue(ut, buffer, role_Val, s->c, 6);
  fprintf(s->c->file, "      break;\n");
}

static void generateFreeCode (Type type, TypeKind t, Context context)
{
  TypeDescription d = type_description(type);
  /*
   * Generate the proc to go in the freeRoutine slot of a
   * CORBA_Environment.  The argument is of type c_role_type(type,
   * role_Exn, FALSE).
   */

  fprintf(context->file, "void %s__Free (%s _val)\n{\n",
	  c_type_name(type), c_role_type(type, role_Exn, FALSE));
  fprintf(context->file,
	  "  /* What you put in the freeRoutine member of a CORBA_Environment for an exception parameterized by a %s */\n",
	  c_type_name(type));
  fprintf(context->file,
	  "  /* frees allocated storage inside _val (if any), but does not free(_val) */\n");

  if (t == record_Type)
    list_enumerate(type_description(type)->structuredDes.record,
		   FreeRecordField, context);

  else if (t == union_Type) {
    struct double_s s;

    s.c = context;
    s.t = ur_type(d->structuredDes.uniond.discriminator_type);
    s.id = 0;
    s.default_arm = d->structuredDes.uniond.default_arm;

    fprintf(context->file, "  switch (_val->_d) {\n");
    list_enumerate(d->structuredDes.uniond.types,
		   (void (*) (void *, void *)) FreeUnionField, &s);
    if (d->structuredDes.uniond.default_arm != NULL)
      /* handled in FreeUnionField */
      ;
    else if (d->structuredDes.uniond.others_allowed) {
      fprintf(context->file, "    default:\n      break;\n");
    } else {
      fprintf(context->file, "    default:\n");
      fprintf(context->file, "      fprintf (stderr, \"_%s__Free:  Bad value %%lu in discriminant.\\n\", (unsigned long) _val->_d);\n", c_type_name(type));
      fprintf(context->file, "      break;\n");
    }
    fprintf(context->file, "  };\n");
  } else if (t == sequence_Type) {
    Type            st = ur_type(d->structuredDes.sequence.type);
    TypeKind        t2 = type_basic_type(st);
    if (t2 == shortcharacter_Type || t2 == character_Type) {
      fprintf(context->file, "  if (*_val != ILU_NIL)\n");
      fprintf(context->file, "    ilu_free(*_val);\n");
    } else {
      fprintf(context->file, "  if (_val->_buffer != NULL) {\n");
      if (HasFreeRoutine(st)) {
	fprintf(context->file, "    int i;\n");
	fprintf(context->file,
		"    for (i = 0;  i < _val->_length;  i++)\n");
	FreeValue(st, "_val->_buffer[i]", role_Val, context, 6);
      }
      fprintf(context->file, "    ilu_free(_val->_buffer);\n  };\n");
    }
  } else if (t == array_Type) {

    if (HasFreeRoutine(d->structuredDes.array.type)) {
      char            buf[1000];
      list            dims = d->structuredDes.array.dimensions;
      int             nDims = list_size(dims);

      DimStop = -1;
      DimCount = 0;
      list_enumerate(dims, dimHeader, context);
      sprintf(buf, "(*_val)");
      DimCount = 0;
      list_enumerate(dims, dimRef, buf);
      FreeValue(d->structuredDes.array.type, buf, role_Val, context,
		2 * nDims + 4);
      DimCount = nDims;
      list_enumerate(dims, dimFooter, context);
    } else {
      fprintf(context->file,
	      "  /* simple element type, nothing to free */\n");
    }
  } else if (t == optional_Type) {
    Type            st = ur_type(d->structuredDes.optional);
    int             indo;
    while (type_kind(st) == optional_Type)
      st = ur_type(type_description(st)->structuredDes.optional);
    indo = !TypeIsPointer(st);
    fprintf(context->file, "  /* subtype is %s */\n", c_type_name(st));
    fprintf(context->file, "  if (*_val != ILU_NIL) {\n");
    FreeValue(st, (indo ? "**_val" : "*_val"), role_Val, context, 4);
    if (indo)
      fprintf(context->file, "    ilu_free(*_val);\n");
    fprintf(context->file, "  }\n");
  } else if (t == object_Type) {
    fprintf(context->file, "  CORBA_Environment env = {0};\n");
    fprintf(context->file, "  CORBA_Object_release(*_val, &env);\n");
    fprintf(context->file, "  *_val = 0;\n");
  }
  fprintf(context->file, "}\n\n");
}

static void generateBufAllocCode (Type type, TypeKind t, Context context)
{
  char *s = c_type_name(type);

  fprintf (context->file, "%s *CORBA_sequence_%s_allocbuf (CORBA_unsigned_long _count)\n{\n",
	   s, (type->builtIn && strncmp(s, "CORBA_", 6) == 0) ? s + 6 : s);
  fprintf (context->file, "  %s *_p;\n  CORBA_unsigned_long _size = sizeof(%s) * _count;\n\n",
	   s, s);
  fprintf (context->file, "  if ((_p = (%s *) ilu_malloc(_size)) == ILU_NIL)\n", s);
  fprintf (context->file, "    { _ILU_C_MallocFailure(_size); return 0; }\n");
  fprintf (context->file, "  else\n    { memset((void *) _p, 0, _size);  return _p; }\n}\n\n");
}

static void generateAllocCode (Type type, TypeKind t, Context context)
{
  char *s = c_type_name(type);
  char *p = (type_kind(type) == array_Type) ? c_return_type(type) : c_parameter_type(type, InOut);

  fprintf (context->file, "%s %s__alloc ()\n{\n", p, s);
  fprintf (context->file, "  return ((%s) CORBA_sequence_%s_allocbuf(1));\n}\n\n", p,
	   (type->builtIn && strncmp(s, "CORBA_", 6) == 0) ? s + 6 : s);
}

static void generateTypeIoCode (Type type, Context context)
{
  TypeKind        t = type_basic_type(type);

  if (type->supertype != NULL
      OR type->builtIn
      OR type->interface != context->interface
      OR type->importInterfaceName != NULL)
    return;

  if (type->importInterfaceName == NULL AND
      (t == union_Type OR
       t == record_Type OR
       t == optional_Type OR
       (t == sequence_Type AND (!TypeIsString(type))) OR
       t == array_Type))
    {
      generateOutputCode (type, t, context);
      generateSizeofCode (type, t, context);
      generateInputCode (type, t, context);
    }

  if (HasFreeRoutine(type))
    generateFreeCode (type, t, context);

  generateBufAllocCode(type, t, context);

  if (HasAllocRoutine(type))
    generateAllocCode (type, t, context);
}

static void generateIosCode (Interface  interface,       Context  context)
{
  list_enumerate(interface->types,
		 (void (*) (void *, void *)) generateTypeIoCode,
		 context);
}

static void generateGlobalCode (Interface  interface,  Context  context)
{
  generateExceptionTable (interface, context);
  generateExceptionProcs (context);
  generateIosCode (interface, context);
}

static unsigned methodIdx = 47, exnIdx = 86;

static void setupExn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "    exns[%u] = ex_%s;\n",
	  exnIdx++, c_exception_name(e));
}

static void generateMethodDef (refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  fprintf(f, "  { ilu_Exception\t");
  if (list_size(m->exceptions) > 0) {
    fprintf(f, "exns[%u];\n", list_size(m->exceptions));
    exnIdx = 0;
    list_enumerate(m->exceptions, setupExn, context);
  } else
    /* Don't try C's patience with a 0-length array */
    fprintf(f, "*exns = NULL;\n");
  fprintf(f, "    m = ilu_DefineMethod(cl, %u,\n", methodIdx++);
  fprintf(f, "\t\"%s\",\t/*name*/\n", procedure_name(m));
  fprintf(f, "\t%u,\t/*id*/\n", m->id);
  fprintf(f, "\t%u,\t/*functional*/\n", m->functional != 0);
  fprintf(f, "\t%u,\t/*asynch*/\n", m->asynch != 0);
  fprintf(f, "\t%u,\t/*n exns*/\n", list_size(m->exceptions));
  fprintf(f, "\texns, &lerr);\n");
  fprintf(f, "    if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "      goto fail2;\n");
  fprintf(f, "  }\n");
  return;
}

static void RegisterClass (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  char           *tn;
  Class           o;
  if ((class->builtIn && class->importInterfaceName == NULL)
      || (type_basic_type(class) != object_Type))
    return;
  tn = c_type_name(class);
  o = class_object(class);
  fprintf(f, "  { ilu_string supers[] = {\n");
  list_enumerate(o->superclasses, classIDs, context);
  fprintf(f, "\tNULL};\n");
  fprintf(f, "    cl = ilu_DefineObjectType(\"%s.%s\",\t/*name*/\n",
	  name_base_name(context->interface->name),
	  name_base_name(class->name));
  fprintf(f, "\t\"%s\",\t/*brand*/\n", o->brand == NULL ? "" : o->brand);
  fprintf(f, "\t\"%s\",\t/*uid*/\n", class->uid);
  if (o->singleton == NULL)
    fprintf(f, "\tNULL,\t/*singleton*/\n");
  else
    fprintf(f, "\t\"%s\",\t/*singleton*/\n", o->singleton);
  fprintf(f, "\t%s,\t/* optional */\n\t%s,\t/* collectible */\n",
	  o->optional ? "ilu_TRUE" : "ilu_FALSE",
	  o->collectible ? "ilu_TRUE" : "ilu_FALSE");
  if (o->authentication == NULL)
    fprintf(f, "\tNULL,\t/*auth*/\n");
  else
    fprintf(f, "\t\"%s\",\t/*auth*/\n", o->authentication);
  fprintf(f, "\t%u,\t/*n methods*/\n", list_size(o->methods));
  fprintf(f, "\t%u,\t/*n supers*/\n",
       (o->superclasses == NULL) ? 0 : list_size(o->superclasses));
  fprintf(f, "\tsupers, &lerr);\n");
  fprintf(f, "    if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "      goto fail2;\n");
  fprintf(f, "    _%s__ILUType = cl;\n", tn);
  fprintf(f, "  }\n");
  methodIdx = 0;
  list_enumerate(o->methods, generateMethodDef, context);
  return;
}

static boolean AddGCCallback (Type type, Context context)
{
  if ((type->builtIn && type->importInterfaceName == NULL)
      OR (type_basic_type(type) != object_Type)
      OR (NOT class_object(type)->collectible))
    return FALSE;
  fprintf(context->file, "  _ILU_C_EnsureGcClient ();\n");
  return TRUE;
}

static void InitializeImportedInterfaces (refany elt, refany rock)
{
  Imported        i = (Imported) elt;
  Context         c = (Context) rock;
  Interface       i2 = GetInterface(i->name, i->filename);
  if (i2 == NULL) {
    fprintf(stderr, "Can't find interface <%s>\n", i->name);
    return;
  } else if (strcmp(i->name, "ilu") == 0);
  else
    fprintf(c->file, "  _%s__GeneralInitialization();\n",
	    c_interface_name(i2));
}

static void genExnDef(refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  FILE           *f = context->file;
  if (e->interface != context->interface || e->import != NULL)
    return;
  fprintf(f, "  _%s__Exception_%s = ilu_DefineException(",
	  c_interface_name(e->interface), c_simple_name(e->name));
  if (e->corba_rep_id == NIL)
    fprintf(f, "\"%s\", \"%s\"", name_base_name(e->interface->name),
	    name_base_name(e->name));
  else
    fprintf(f, "ILU_NIL, \"%s\"", e->corba_rep_id);
  fprintf(f, ", &lerr);\n");
  fprintf(f, "  if (ILU_ERRNOK(lerr))\n");
  fprintf(f, "    goto fail1;\n");
  return;
}

static void
generateClientRegistrationCode(Interface interface, Context context)
{
  char           *interface_name = (char *) c_interface_name(interface);
  FILE           *f = context->file;

  fprintf(f,
	  "void _%s__GeneralInitialization (void)\n{\n",
	  interface_name);
  fprintf(f, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(f, "  ilu_Error lerr = ILU_INIT_NO_ERR;\n");
  fprintf(f, "  ilu_Class cl = ILU_NIL;\n");
  fprintf(f, "  ilu_Method m = ILU_NIL;\n");
  fprintf(f, "  ilu_Mutex otmu = ilu_GetOTMutex();\n");
  fprintf(f, "  if (initialized)\n    return;\n");
  fprintf(f, "  initialized = ilu_TRUE;\n");
  list_enumerate(interface->imports, InitializeImportedInterfaces,
		 context);
  fprintf(f, "  if (!ilu_EnterMutex(otmu, &lerr))\n");
  fprintf(f, "    goto fail1;\n");
  list_enumerate (interface->exceptions, genExnDef, context);
  list_enumerate(interface->classes, RegisterClass, context);
  fprintf(f, "  if (!ilu_ExitMutex(otmu, ilu_TRUE, &lerr))\n");
  fprintf(f, "    goto fail2;\n");
  (void) list_find(interface->classes, (FindProc) AddGCCallback, context);
  fprintf(f, "fail1:\n");
  fprintf(f, "fail2:\n");
  fprintf(f, "  ILU_MUST_BE_SUCCESS(lerr);\n");
  fprintf(f, "}\n\n");
}

static void generateSequenceCode (refany elt, refany rock)
{
  Type            seq = (Type) elt;
  Context         context = (Context) rock;
  boolean         hasReferent;
  char           *rtn;
  Type            eltType;
  char           *st;
  TypeKind        t;
  char           *tn;
  FILE           *f = context->file;

  if (type_basic_type(seq) != sequence_Type OR
      seq->importInterfaceName != NULL OR
      seq->builtIn)
    return;
  eltType = type_description(seq)->structuredDes.sequence.type;
  t = type_basic_type(ur_type(eltType));
  if (t == shortcharacter_Type || t == character_Type) {
    tn = c_type_name(seq);
    st = c_parameter_type(eltType, In);
    rtn = c_type_name(eltType);

    fprintf(f,
	    "void %s_Every (%s *h, void (*f)(%s *, void *), void * data)\n{\n",
	    tn, tn, rtn);
    fprintf(f, "  %s *p;\n", rtn);
    fprintf(f, "  for (p = *h;  *p != 0;  p++) (*f)(p, data);\n}\n\n");

    fprintf(f, "void %s_Append (%s *h, %s item)\n{\n", tn, tn, rtn);
    fprintf(f,
	 "  _ILU_C_Extend%sString (h, item, (CORBA_boolean) 1);\n",
	    (t == shortcharacter_Type) ? "" : "W");
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Push (%s *h, %s item)\n{\n", tn, tn, rtn);
    fprintf(f,
	 "  _ILU_C_Extend%sString (h, item, (CORBA_boolean) 0);\n",
	    (t == shortcharacter_Type) ? "" : "W");
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Pop (%s *h, %s *item)\n{\n", tn, tn, rtn);
    fprintf(f, "  _ILU_C_Pop%sString (h, item);\n}\n\n",
	    (t == shortcharacter_Type) ? "" : "W");

    fprintf(f, "CORBA_unsigned_long %s_Length (%s *h)\n{\n", tn, tn);
    fprintf(f, "  if (h == ILU_NIL || *h == ILU_NIL) return 0;\n");
    if (t == shortcharacter_Type)
      fprintf(f, "  return (strlen((char *)(*h)));\n");
    else
      fprintf(f, "  return _ILU_C_SafeWStrlen((ilu_character *)(*h));\n");
    fprintf(f, "}\n\n");

    fprintf(f, "%s * %s_Nth (%s *h, CORBA_unsigned_long n)\n{\n",
	    rtn, tn, tn);
    fprintf(f, "  if (h == ILU_NIL || *h == ILU_NIL) return ILU_NIL;\n");
    /* cast because of sign fuckups */
    fprintf(f, "  if (n >= %s((%s*)(*h)))\n",
            (t == shortcharacter_Type) ? "strlen" : "_ILU_C_SafeWStrlen",
	    (t == shortcharacter_Type) ? "char" : "ilu_character");
    fprintf(f, "    return ILU_NIL;\n");
    fprintf(f, "else return &((*h)[n]);\n");
    fprintf(f, "}\n\n");

    fprintf(f, "%s %s_Create (CORBA_unsigned_long sz, %s *p)\n{\n",
	    tn, tn, rtn);
    fprintf(f, "  %s s;\n\n  if (p == ILU_NIL)\n", tn);
    fprintf(f, "    {\n      s = ilu_malloc(sz * sizeof(%s));\n", rtn);
    fprintf(f, "      if (s == ILU_NIL) {");
    fprintf(f, "        _ILU_C_MallocFailure(sz * sizeof(%s));", rtn);
    fprintf(f, "        return ILU_NIL; }\n");
    fprintf(f, "      memset((void *) s, 0, sz * sizeof(%s));\n", rtn);
    fprintf(f, "      return s;\n");
    fprintf(f, "    }\n  else\n    return p;\n}\n\n");

    fprintf(f, "void %s_Init (%s *s, CORBA_unsigned_long sz)\n",
	    tn, tn, rtn);
    fprintf(f, "{\n  return;\n}\n\n");
  } else {
    tn = c_type_name(seq);
    st = c_parameter_type(eltType, In);
    rtn = c_type_name(eltType);
    hasReferent = (t == record_Type || t == union_Type ||
		   (t == sequence_Type &&
		    !TypeIsEitherString(ur_type(eltType))));
    fprintf(f,
    "void %s_Every (%s *h, void (*f)(%s%s, void *), void * data)\n",
	    tn, tn, st, hasReferent ? "" : " *");
    fprintf(f, "{\n");
    fprintf(f, "  _ILU_C_EveryElement (%s, %s, sizeof(%s), %s);\n}\n\n",
	    "(ILU_C_Sequence) h", "(void (*)(void *, void *)) f",
	    rtn, "(void *) data");

    fprintf(f, "void %s_Append (%s *h, %s item)\n{\n", tn, tn, st);
    fprintf(f, "  _ILU_C_AppendGeneric (%s, %s, sizeof(%s));\n",
	    "(ILU_C_Sequence) h",
	    (hasReferent) ? "(char *) item" : "(char *) &item",
	    rtn);
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Push (%s *h, %s item)\n{\n", tn, tn, st);
    fprintf(f, "  _ILU_C_PushGeneric (%s, %s, sizeof(%s));\n",
	    "(ILU_C_Sequence) h",
	    (hasReferent) ? "(char *) item" : "(char *) &item",
	    rtn);
    fprintf(f, "}\n\n");

    fprintf(f, "void %s_Pop (%s *h, %s item)\n{\n", tn, tn, st);
    fprintf(f, "  _ILU_C_PopGeneric (%s, %s, sizeof(%s));\n}\n\n",
	    "(ILU_C_Sequence) h",
	    (hasReferent) ? "(char *) item" : "(char *) &item",
	    rtn);

    fprintf(f, "CORBA_unsigned_long %s_Length (%s *h)\n{\n", tn, tn);
    fprintf(f, "  if (h == ILU_NIL)\n    return 0;\n");
    fprintf(f, "  else return h->_length;\n}\n\n");

    fprintf(f, "%s * %s_Nth (%s *h, CORBA_unsigned_long n)\n{\n",
	    rtn, tn, tn);
    fprintf(f, "  if (h == ILU_NIL || (n >= h->_length))\n");
    fprintf(f, "    return ILU_NIL;\n");
    fprintf(f, "  else return &(h->_buffer[n]);\n}\n\n");

    fprintf(f, "%s * %s_%s_Create (CORBA_unsigned_long sz, %s %sp)\n",
	    tn, c_interface_name(context->interface),
	    c_simple_name(seq->name), st, hasReferent ? "" : "*");
    fprintf(f, "{\n  %s *s;\n", tn);
    fprintf(f, "  s = (%s *) ilu_malloc(sizeof(%s));\n", tn, tn);
    fprintf(f, "  if (s == ILU_NIL) { _ILU_C_MallocFailure(sizeof(%s)); return ILU_NIL; };\n", tn);
    fprintf(f, "  s->_maximum = sz;\n");
    fprintf(f, "  s->_length = (sz > 0 && p != ILU_NIL) ? sz : 0;\n");
    fprintf(f, "  s->_buffer = (p != ILU_NIL) ? p : ((sz > 0) ? ((%s *) ilu_malloc(sz * sizeof(%s))) : ILU_NIL);\n",
	    rtn, rtn);
    fprintf(f, "  if ((s->_buffer == ILU_NIL) && sz > 0 && p == ILU_NIL) {\n");
    fprintf(f, "    _ILU_C_MallocFailure(sz * sizeof(%s));  ilu_free(s);  return ILU_NIL; };\n", rtn);
    fprintf(f, "  return s;\n}\n\n");

    fprintf(f,
       "void %s_%s_Init (%s *s, CORBA_unsigned_long sz, %s %sp)\n",
	    c_interface_name(context->interface),
       c_simple_name(seq->name), tn, st, (hasReferent ? "" : "*"));
    fprintf(f, "{\n");
    fprintf(f, "  if (sz == 0 && p != NULL)\n    return;\n");
    fprintf(f, "  if (sz > 0)\n    s->_maximum = sz;\n");
    fprintf(f, "  else\n    s->_maximum = 0;\n");
    fprintf(f, "  if (sz > 0 && p != NULL)\n    s->_length = sz;\n");
    fprintf(f, "  else\n    s->_length = 0;\n");
    fprintf(f, "  if (sz > 0 && p == NULL) {\n");
    fprintf(f, "    s->_buffer = (%s *) ilu_malloc (sz * sizeof (%s));\n",
	    rtn, rtn);
    fprintf(f, "    if (s->_buffer == ILU_NIL) {\n");
    fprintf(f, "      s->_length = 0;\n");
    fprintf(f, "      s->_maximum = 0;\n");
    fprintf(f, "      _ILU_C_MallocFailure(sz * sizeof(%s)); }}\n", rtn);
    fprintf(f, "  else\n    s->_buffer = p;\n");
    fprintf(f, "  return;\n}\n\n");
  }
}

static void listArgumentNames (Argument arg, Context context)
{
  fprintf (context->file, ", %s", c_argument_name(arg));
}

static void generateDispatchClassMethod(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         c = (Context) rock;
  int             ndx;
  TypeKind        t = type_basic_type(ur_type(m->returnType));

  /*
   * this stuff is trying to ensure that we dont generate the same
   * method more than once. this can happen if the current class
   * has more than one super classes and each of those is a
   * subclass of the same superclass. got it!
   * [Probably irrelevant now --- MJS 1/13/95]
   */

  if (methodInList(c_simple_name(m->name))) {
    MethodRecordID++;
    return;
  }
  addMethodToList(c_simple_name(m->name));
  generateProcHeader(m, c, TRUE);
  fprintf(c->file, "  %s (*_f)(%s",
	  (t == void_Type) ? "void" : c_return_type(m->returnType),
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *);\n");
  fprintf(c->file, "  _f = (%s (*)(%s",
	  (t == void_Type) ? "void" : c_return_type(m->returnType),
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  ndx = methodNdxOf(m, m->object);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *)) _ILU_C_FindMethod (_handle, _%s__ILUType, %d);\n",
	  c_type_name(ur_type(c->class)),
  /* This would be the wrong ilu_Class for an inherited method! */
	  ndx);
  fprintf(c->file, "  _status->_major = ILU_C_NO_EXCEPTION;\n");
  fprintf(c->file, "  _status->ptr = ILU_NIL;\n");
  fprintf(c->file, "  _status->freeRoutine = (void(*)(void *)) 0;\n");
  fprintf(c->file, "  _status->returnCode = (ilu_Exception) 0;\n");
  if (t != void_Type)
    fprintf(c->file, "  return (");
  else
    fprintf(c->file, "  ");
  fprintf(c->file, "(*_f)(_handle");
  list_enumerate(m->arguments, (iluparser_EnumProc) listArgumentNames, c);
  fprintf(c->file, ", _status");
  if (t != void_Type)
    fprintf(c->file, ")");
  fprintf(c->file, ");\n}\n\n");
}

static void generateDispatchMethods (refany elt, refany rock)
{
  Type            pclass = (Type) elt;
  Context         context = (Context) rock;
  Class           od = class_object(ur_type(pclass));
  static int      recursing = 0;

#if 0
  /*
   * Here's some wild stuff. What's happening is that we always
   * want to retain the highest class level as we recurse through
   * the class tree thats why we need to retain this nasty
   * recursing flag so we can know when to reset the context->class
   */

  if (od->superclasses) {
    if (!recursing)
      context->class = ur_type(pclass);
    recursing++;
    list_enumerate(od->superclasses, generateDispatchMethods, context);
    recursing--;
  }
#endif
  /*
   * MJS 1/13/95: Why recurse at all?  We don't want generic
   * functions for inherited methods!  (At least not while their
   * implementations are buggy!)
   */
  if (!recursing)
    context->class = ur_type(pclass);
  if (list_size(od->methods) > 0)
    list_enumerate(od->methods, generateDispatchClassMethod, context);
}

static void generateDispatch (refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  if (type_basic_type(class) != object_Type)
    return;
  MethodRecordID = 0;
  clearMethodList ();
  generateDispatchMethods (class, context);
}

static void generateUserDataAccessors (refany elt, refany rock)
{
  Type            type = (Type) elt;
  Context         context = (Context) rock;
  char           *tn;

  if (type_basic_type(type) != object_Type)
    return;

  tn = c_type_name(type);
  fprintf (context->file, "void %s__SetUserData (%s self, void *userData)\n", tn, tn);
  fprintf (context->file, "{\n  ((ILU_C_Object *) self)->data = userData;\n}\n\n");
  fprintf (context->file, "void *%s__GetUserData (%s self)\n", tn, tn);
  fprintf (context->file, "{\n  return(((ILU_C_Object *) self)->data);\n}\n\n");
}

void generateCommonCode (Interface interface, FILE *file)
{
  struct context_s context;
  char           *pc_interfacename;

  context.file = file;
  context.interface = interface;

  /*
   * get any translation of what the header file for the interface
   * is
   */
  pc_interfacename = interface_header_name(c_interface_name(interface));

  fprintf(file, "#include <stdio.h>\n");
  fprintf(file, "#include <string.h> /* used for error statements */\n");
  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  list_enumerate(interface->classes, generateClassTable, &context);
  list_enumerate(interface->classes, generateDispatch, &context);
  list_enumerate(interface->classes, generateUserDataAccessors,
		 &context);
  generateGlobalCode(interface, &context);
  list_enumerate(interface->types, generateSequenceCode, &context);
  generateClientRegistrationCode(interface, &context);
}
