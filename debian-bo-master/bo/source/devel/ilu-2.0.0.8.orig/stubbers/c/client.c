/*
 ** Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.
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
/* $Id: client.c,v 1.77 1996/06/21 01:12:56 janssen Exp $ */

#include "cstubber.h" 

static void generateInstanceFromSBH (Type type, Context context)
{
  char           *tn = c_type_name(context->class);
  fprintf(context->file,
     "%s %s__CreateFromSBH (char *sbh, ILU_C_ENVIRONMENT * env)\n",
	  tn, tn);
  fprintf(context->file, "{\n");
  fprintf(context->file,
    "  return (%s) (ILU_C_SBHToObject(sbh, _%s__ILUType, env));\n",
	  tn, tn);
  fprintf(context->file, "}\n\n");
}

static void generateInstanceFromURL (Type type, Context context)
{
  char           *tn = c_type_name(context->class);
  fprintf(context->file,
     "%s %s__CreateFromURL (char *url, ILU_C_ENVIRONMENT * env)\n",
	  tn, tn);
  fprintf(context->file, "{\n");
  fprintf(context->file,
    "  return (%s) (ILU_C_SBHToObject(url, _%s__ILUType, env));\n",
	  tn, tn);
  fprintf(context->file, "}\n\n");
}

static void declareCallerReturnValue(Type type, Context context)
{
  if  (type_basic_type (type) != void_Type) {
    fprintf (context->file, "  %s _retvalue;\n", c_return_type (type));
  }
}

static void listArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  fprintf(context->file, ", %s %s",
	  c_parameter_type(arg->type, arg->direction),
	  c_argument_name(arg));
}

void generateProcHeader (Procedure m, Context c, boolean external)
{
  fprintf(c->file, "%s%s %s%s_%s (%s _handle",
	  external ? "" : "static ",
	  ((type_basic_type(m->returnType) == void_Type)
	   ? "void" : c_return_type(m->returnType)),
	  external ? "" : "_",
	  c_type_name(c->class),
	  c_simple_name(m->name), c_type_name(c->class));
  list_enumerate(m->arguments, listArgument, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *_status)\n{\n");
}

int methodNdxOf (Procedure p, Type t)
{
  list m;
  int  mNdx;
  listElement *ptr;

  mNdx = 0;
  m =  (class_object (t))->methods;
  if  (m == NULL || m->count < 1)
    return (0);
  ptr = m->head;
  while (ptr) {
    if  (ptr->data == p)
      return (mNdx);
    mNdx++;
    ptr = ptr->next;
  }
  return (mNdx);
}

static void sizeArgument (Argument arg, Context context)
{
  char            b[1000];
  Type            ut = ur_type(arg->type);
  TypeKind        t = type_basic_type(ut);

  if (arg->direction == Out)
    return;
  sprintf(b, "%s%s",
	  (t != array_Type &&
	   (arg->direction == InOut || t == union_Type ||
	    t == record_Type || NonStringSequence(ut)))
	  ? "*" : "",
	  c_argument_name(arg));
  fprintf(context->file, "\n     +");
  SizeValue(context, ut, b);
}

static void outputArgument (Argument arg, Context context)
{
  char b[1000];
  Type ut = ur_type(arg->type);

  if  (arg->direction == Out)
    return;
  else if (arg->direction == InOut)
    {
      enum PrimitiveTypes t;

      t = type_basic_type(ut);
      if (TypeIsString(ut) OR
	  TypeIsWString(ut) OR
	  (t != sequence_Type AND
	   t != union_Type AND
	   t != array_Type AND
	   t != record_Type))
	{
	  *b = '*';
	  strcpy (b+1, c_argument_name(arg));
	}
      else
	strcpy (b, c_argument_name(arg));
    }
  else
    strcpy (b, c_argument_name(arg));
  MarshallValue (context, ut, b, 2);
}

static void TypeIsObj (Type t, boolean *objarg)
{
  if (type_basic_type(t) == object_Type)
    *objarg = TRUE;
}

static void scanForObjectArg (Argument a, boolean *objarg)
{
  type_recurse (a->type, (void (*) (Type, refany)) TypeIsObj, objarg);
}

static void listArg (Context c, char *op, Type t, char *name)
{
  TypeKind        tk = type_ur_kind(t);
  fprintf(c->file, ",\n                       %s", name);
  if (!(ur_type(t)->builtIn OR
	TypeIsEitherString(t) OR
	(type_ur_kind(t) == object_Type) OR
	(type_ur_kind(t) == enumeration_Type)))
    fprintf(c->file, ", _%s__%s", c_type_name(ur_type(t)), op);
  if (tk == object_Type)
    fprintf(c->file, ", _%s__ILUType", c_type_name(ur_type(t)));
  if (TypeIsEitherString(t))
    fprintf(c->file, ", (ilu_cardinal) 0x%lx",
	    (unsigned long) (type_description(t)->
			     structuredDes.sequence.limit));
}

static void listSizeOfArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == Out)
    return;
  listArg (context, "SizeOf", a->type, c_argument_name(a));
}

static void listOutputArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == Out)
    return;
  listArg (context, "Output", a->type, c_argument_name(a));
}

static void listInputArgs (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  if (a->direction == In)
    return;
  listArg (context, "Input", a->type, c_argument_name(a));
}

void codeType (Type t, char **p, Role role)
{
  char a;

  switch (type_ur_kind (t)) {
  case integer_Type:     a = 'b'; break;
  case cardinal_Type:     a = 'e'; break;
  case shortinteger_Type:     a = 'a'; break;
  case shortcardinal_Type:     a = 'd'; break;
  case longinteger_Type:     a = 'c'; break;
  case longcardinal_Type:     a = 'f'; break;
  case character_Type:     a = 'k'; break;
  case shortcharacter_Type:     a = 'j'; break;
  case real_Type:     a = 'h'; break;
  case shortreal_Type:     a = 'g'; break;
  case longreal_Type:     a = 'i'; break;
  case byte_Type:     a = 'm'; break;
  case boolean_Type:     a = 'n'; break;
  case enumeration_Type:     a = 'o'; break;
  case object_Type:     a = 'p'; break;
  case sequence_Type:
    {
      TypeKind        ek = type_ur_kind(type_description(t)->
				      structuredDes.sequence.type);
      if (ek == shortcharacter_Type)
	a = 'q';
      else if (ek == character_Type)
	a = 'r';
      else if (role == role_Out || role == role_Return)
	a = 'y';
      else
	a = 'z';
    }
    break;
  case array_Type:
    a = (role == role_Out && VariableLength(t) ||
	 role == role_Return) ? 'y' : 'z';
    break;
  case record_Type:
  case union_Type:
    a = ((role == role_Out || role == role_Return) &&
	 VariableLength(t)) ? 'y' : 'z';
    break;
  case optional_Type:	a = 'z'; break;
  case void_Type:     a = '*'; break;
  default:     fatal ("Error: Bad parameter type %s.\n", c_type_name (t));
  }

  if (role == role_InOut)
    a = toupper(a);

  **p = a;
  (*p)++;
}

static void codeInputArg (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  char          **p = (char **) rock;
  if (a->direction != Out)
    codeType (a->type, p, adRole[a->direction]);
}

static void codeOutputArg (refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  char          **p = (char **) rock;
  if (a->direction != In)
    codeType (a->type, p, adRole[a->direction]);
}

static char *genParmDescription (Procedure m)
{
  int len = list_size(m->arguments) * 2 + 3;
  char *p, *q;

  p = (char *) malloc(len);
  q = p;
  list_enumerate (m->arguments, codeInputArg, &q);
  *q++ = ':';
  if (m->returnType != NULL && type_ur_kind(m->returnType) != void_Type)
    codeType (m->returnType, &q, role_Return);
  list_enumerate (m->arguments, codeOutputArg, &q);
  *q = 0;
  return p;
}

static void addExceptionInputFn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  Exception       ure = e->import ? e->import : e;
  Type            ut = ur_type(ure->type);
  TypeKind        tk = type_kind(ut);
  FILE           *f = context->file;

  fprintf(context->file, "%s\n    { ", context->counter == 0 ? "" : ",");
  context->counter += 1;

  if (tk == void_Type)
    fprintf(f, "0, ");
  else
    fprintf(f, "sizeof(%s), ", c_type_name(ut));

  if (tk == object_Type)
    fprintf(f, "_%s__ILUType, ", c_type_name(ut));
  else
    fprintf(f, "ILU_NIL, ");

  switch (tk) {
  case integer_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputInteger, 0");
    break;
  case cardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputCardinal, 0");
    break;

  case shortinteger_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortInteger, 0");
    break;
  case shortcardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortCardinal, 0");
    break;

  case longinteger_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongInteger, 0");
    break;
  case longcardinal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongCardinal, 0");
    break;

  case character_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputCharacter, 0");
    break;
  case shortcharacter_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortCharacter, 0");
    break;

  case real_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputReal, 0");
    break;
  case shortreal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputShortReal, 0");
    break;
  case longreal_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputLongReal, 0");
    break;

  case byte_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputByte, 0");
    break;
  case boolean_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ILU_C_InputBoolean, 0");
    break;

  case enumeration_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) ilu_InputEnum, 0");
    break;

  case object_Type:
    fprintf(f, "0, 0, 0, 0");
    break;

  case array_Type:
  case record_Type:
  case union_Type:
  case sequence_Type:
  case optional_Type:
    fprintf(f, "0, 0, (ILU_C_InputFn) _%s__Input, ", c_type_name(ut));
    if (HasFreeRoutine(ut))
      fprintf(f, "(ILU_C_FreeFn) %s__Free", c_type_name(ut));
    else
      fprintf(f, "0");
    break;

  case void_Type:
    fprintf(f, "0, 0, 0, 0");
    break;

  default:
    fatal("Error: Bad parameter type %s.\n", c_type_name(ure->type));
  }
  fprintf(f, " }");
}

static void declareExceptionValInputFns (list exceptions, Context context)
{
  if (exceptions == NULL || list_size(exceptions) < 1)
    return;
  fprintf(context->file,
      "  static struct _ILU_C_ExceptionDescription_s _evec[] = {");
  context->counter = 0;
  list_enumerate(exceptions, addExceptionInputFn, context);
  fprintf(context->file, "};\n\n");
}

static void generateMethodCode (Procedure m, Context context)
{
  char           *p;
  TypeKind        t = type_basic_type(ur_type(m->returnType));

  if (methodInList(c_simple_name(m->name))) {
    MethodRecordID++;
    return;
  }
  addMethodToList(c_simple_name(m->name));

  generateProcHeader(m, context, FALSE);

  /* Declare var to hold return value, if any. */
  declareCallerReturnValue(m->returnType, context);
  /*
   * Declare var to hold possible exception value unmarshalling
   * routines, if any.
   */
  declareExceptionValInputFns(m->exceptions, context);

  /*
   * If method is functional, see if it is cached,     and if so,
   * return the cached value.
   */
  if (IsCacheableMethod(m)) {
    /* eventually... */
  }
  /*
   * If return type is an array, we need to allocate a new copy of
   * it, as GenericCall will be expecting the space to be
   * pre-allocated.  Arrays are the only type for which this is
   * necessary.
   */

  /* generate call to _ILU_C_GenericCall */

  fprintf(context->file,
	  "  _ILU_C_GenericCall (_%s__ILUType, &_%s__ILUType->cl_methods[%d],\n",
	  c_type_name(m->object), c_type_name(m->object),
	  methodNdxOf(m, m->object));
  p = genParmDescription(m);
  fprintf(context->file,
	  "                       %s, _handle, _status, \"%s\"",
	  list_size(m->exceptions) < 1 ? "ILU_NIL" : "_evec", p);
  free(p);
  list_enumerate(m->arguments, listSizeOfArgs, context);
  list_enumerate(m->arguments, listOutputArgs, context);
  if (t != void_Type) {
    listArg(context, "Input", m->returnType, "&_retvalue");
  }
  list_enumerate(m->arguments, listInputArgs, context);
  fprintf(context->file, "\n                      );\n");

  /*
   * If we returned an error, release the storage allocated for an
   * array.
   */

  if (t == array_Type) {
    fprintf(context->file, "  if (!ILU_C_SUCCESSFUL(_status))\n");
    fprintf(context->file, "    ilu_free(_retvalue);\n");
  }
  if (t != void_Type)
    fprintf(context->file, "  return _retvalue;\n");
  fprintf(context->file, "}\n\n");
}

static void mkPrototype (Procedure m, Context c)
{
  enum PrimitiveTypes t = type_basic_type (m->returnType);

  fprintf (c->file,
	   "%s _%s_%s (%s", c_return_type(m->returnType),
	   c_type_name(m->object), c_simple_name(m->name), c_type_name(m->object));
  list_enumerate (m->arguments, listArgumentTypes, c);
  fprintf (c->file, ", ILU_C_ENVIRONMENT *);\n");
}

static void generatePrototypes (    Type        t,     Context     context)
{
  Class       c;

  if  (t == NULL ||
       type_basic_type (t) != object_Type ||
       (c = class_object (t)) == NULL)
    return;
  list_enumerate (c->methods, (void (*)(refany, refany)) mkPrototype, context);
}

static void generateMethodPtrs(refany elt, refany rock)
{
  Procedure       p = (Procedure) elt;
  Context         c = (Context) rock;
  fprintf(c->file, "    (void(*)()) _%s_%s,\n",
	  c_type_name(p->object), c_simple_name(p->name));
}

static void generateClsPtrs (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses, generateClsPtrs, c);
  fprintf(c->file,
	  "  (_ILU_C_MethodBlock *) &_%s__SurrogateMethodBlock,\n",
	  c_type_name(ur_type(t)));
}

static void generateMiMethodPtrs(Type t, Context c)
{
#if 0
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses, generateMiMethodPtrs,
		   c);
  if (methodInList(c_type_name(t)))
    return;
  addMethodToList(c_type_name(t));
#endif
  if (OriginalInterface(t) != c->interface)
    return;
  generatePrototypes(t, c);
  fprintf(c->file,
       "struct _%s__MethodBlock_s _%s__SurrogateMethodBlock = {\n",
	  c_type_name(t), c_type_name(t));
  fprintf(c->file, "  ILU_NIL /* will be set to _%s__ILUType */",
	  c_type_name(t));
  if (list_size(class_object(t)->methods) > 0) {
    fprintf(c->file, ",\n  {\n");
    list_enumerate(class_object(t)->methods, generateMethodPtrs, c);
    fprintf(c->file, "  }\n");
  }
  fprintf(c->file, "};\n\n");
}

static void generateClassTable (Type t, Context c)
{
  char           *tn = c_type_name(t);

  clearMethodList();
  generateMiMethodPtrs(t, c);
  fprintf(c->file,
     "static _ILU_C_MethodBlock *_%s__SurrogateTypeVector[] = {\n",
	  tn);

  /* This class's data must be first in the list. */
  /* Some runtime machinery depends on this. */

  fprintf(c->file,
	  "  (_ILU_C_MethodBlock *) &_%s__SurrogateMethodBlock,\n",
	  tn);
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses, generateClsPtrs, c);
  fprintf(c->file, "  ILU_NIL\n};\n\n");
}

static void surrMethBlkInitPerClass(refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (OriginalInterface(t) != c->interface)
    return;
  fprintf(c->file, "  _%s__SurrogateMethodBlock.c = _%s__ILUType;\n",
	  c_type_name(t), c_type_name(t));
}

static void generateMethods (    Type class,     Context context)
{
  if  (!class)
    return;

  list_enumerate(class_object(class)->methods,  (iluparser_EnumProc) generateMethodCode, context);
}

static void generateClassCode(refany elt, refany rock)
{
  Type            type = (Type) elt;
  Context         context = (Context) rock;
  context->class = type;
  MethodRecordID = 0;
  clearMethodList();
  generateMethods(type, context);
  generateClassTable(type, context);
  generateInstanceFromSBH(type, context);
}

static void RegisterSurrogateTypes(refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (OriginalInterface(t) == c->interface) {
    fprintf(c->file,
	    "  _ILU_C_RegisterSurrogateCType (_%s__ILUType, _%s__SurrogateTypeVector);\n",
	    c_type_name(t), c_type_name(t));
  }
}

static void InitializeImportedInterfaces(refany elt, refany rock)
{
  Imported        i = (Imported) elt;
  Context         c = (Context) rock;
  Interface       i2 = GetInterface(i->name, i->filename);
  if (i2 == NULL) {
    fprintf(stderr, "Can't find interface <%s>\n", i->name);
    return;
  } else if (strcmp(i->name, "ilu") == 0);
  else
    fprintf(c->file, "  %s__Initialize();\n",
	    c_interface_name(i2));
}

void generateClientCode(Interface interface, FILE *file)
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

  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  clearMethodList();
  list_enumerate(interface->classes, generateClassCode, &context);

  fprintf(file, "void %s__Initialize(void)\n{\n",
	  c_interface_name(interface));
  fprintf(file, "  extern void _%s__GeneralInitialization(void);\n\n",
	  c_interface_name(interface));
  fprintf(file, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(file, "  if (initialized) return;\n");
  fprintf(file, "  initialized = ilu_TRUE;\n\n");
  list_enumerate(interface->imports, InitializeImportedInterfaces,
		 &context);
  fprintf(file, "  _%s__GeneralInitialization();\n",
	  c_interface_name(interface));
  list_enumerate(interface->classes, surrMethBlkInitPerClass, &context);
  list_enumerate(interface->classes, RegisterSurrogateTypes, &context);
  fprintf(file, "  return;\n");
  fprintf(file, "}\n");
}
