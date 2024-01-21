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
/* $Id: server.c,v 1.74 1996/06/19 03:08:18 janssen Exp $ */
/* Last edited by Mike Spreitzer April 3, 1996 9:01 am PST */

#include "cstubber.h"

static boolean  FirstClass = TRUE;

static void listArgument(refany elt, refany rock)
{
  Argument        a = (Argument) elt;
  Context         context = (Context) rock;
  TypeKind        t = type_basic_type(ur_type(a->type));
  int             indv, indp;
  indv = Indirectness(a->type, adRole[a->direction], TRUE, 0);
  indp = Indirectness(a->type, adRole[a->direction], FALSE, 0);
  fprintf(context->file, ", %s%s",
	  ((indp > indv) ? "&" : ""),
	  c_argument_name(a));
}

void 
FreeValue(Type type, string val, Role role, Context context,
	  int indent)
{
  Type            ut = ur_type(type);
  TypeKind        t = type_ur_kind(type);
  int             indv = Indirectness(ut, role, TRUE, 1);
  int             indp = Indirectness(ut, role_Exn, FALSE, 1);

  if (HasFreeRoutine(ut)) {
    if (Sliced(ut, role)) {
      if (indv != indp) {
	fprintf(stderr, "Bug at %s:%d: %d != %d, for %s, %d.\n",
		__FILE__, __LINE__, indv, indp,
		c_type_name(ut), role);
	fprintf(context->file,
		"%*.*s_ilu_Assert(0, \"FreeValue(%s, %d)\");\n",
		indent, indent, "",
		c_type_name(ut), role);
      }
      fprintf(context->file, "%*.*s%s__Free ((%s)%s);\n",
	      indent, indent, "", c_type_name(ut),
	      c_role_type(ut, role_Exn, FALSE), val);
    } else
      fprintf(context->file, "%*.*s%s__Free (%s%s);\n",
	      indent, indent, "",
	      c_type_name(ut),
	      ((indp > indv) ? "&" : ""),
	      val);
  }
  if (indv)
    fprintf(context->file, "  CORBA_free(%s);\n", val);
}

static void freeArgument(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  FreeValue(arg->type, c_argument_name(arg), adRole[arg->direction],
	    context, 2);
}

static void freeReturnValue (Type type, Context context)
{
  FreeValue (type, "_retvalue", role_Return, context, 2);
}

static void sizeArgument(char *name, Type ut, Role role, Context context)
{
  char            buf[1000];
  TypeKind        t = type_ur_kind(ut);
  int             ind;
  ind = Indirectness(ut, role, TRUE, 0);
  sprintf(buf, "%s%s", (ind ? "*" : ""), name);
  fprintf(context->file, "        _size += ");
  SizeValue(context, ut, buf);
  fprintf(context->file, ";\n");
  fprintf(context->file, "      if (ILU_ERRNOK(*_err)) goto errexit;\n");
}

static void sizeArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  if (arg->direction == In)
    return;
  sizeArgument(c_argument_name(arg), arg->type, adRole[arg->direction],
	       context);
}

static void outputArgument (char *name, Type ut, Role role, Context context)
{
  char            buf[1000];
  int             indp, indv;
  indp = Indirectness(ut, role_In, FALSE, 0);
  indv = Indirectness(ut, role, TRUE, 0);
  sprintf(buf, "%s%s",
	  ((indp < indv) ? "*" : (indp > indv) ? "&" : ""),
	  name);
  MarshallValue(context, ut, buf, 6);
}

static void outputArg(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  if (arg->direction != In)
    outputArgument(c_argument_name(arg), arg->type,
		   adRole[arg->direction], (Context) rock);
}

static void inputArgument (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  char            buf[1000];
  int             indp, indv;
  if (arg->direction == Out)
    return;
  indv = Indirectness(arg->type, adRole[arg->direction], TRUE, 0);
  indp = Indirectness(arg->type, role_InOut, FALSE, 0);
  sprintf(buf, "%s%s", (indp > indv) ? "&" : "", c_argument_name(arg));
  UnmarshallValue(context, ur_type(arg->type), arg->def, buf, 2);
}

static void declareArg (refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  Type            type = arg->type;
  string          name = c_argument_name(arg);
  fprintf(context->file, "  %s %s = {0};\n",
	  c_role_type(type, adRole[arg->direction], TRUE), name);
}

static void initType(char *name, Type type, Context context, Role role)
{
  TypeKind        t = type_ur_kind(type);
  int             ind = Indirectness(type, role, TRUE, 1);

  if (ind > 0) {
    fprintf(context->file, "  %s = (%s) 0;\n", name,
	    c_role_type(type, role, TRUE));
  } else if (t == array_Type) {
    fprintf(context->file, "  memset ((void *) %s, 0, sizeof(%s));\n",
	    name, c_type_name(type));
  } else if (t == record_Type || t == union_Type ||
	     NonStringSequence(type)) {
    fprintf(context->file, "  memset ((void *) &%s, 0, sizeof(%s));\n",
	    name, c_type_name(type));
  } else {
    fprintf(context->file, "  %s = (%s) 0;\n",
	    name, c_type_name(type));
  }
}

static void initArgument(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  if (arg == NULL || arg->type == NULL ||
      (type_kind(arg->type) == void_Type))
    return;
  initType(c_argument_name(arg), arg->type, (Context) rock,
	   adRole[arg->direction]);
}

static void initReturnValue (Type t, Context context)
{
  if (t == NIL || type_kind(t) == void_Type)
    return;
  initType ("_retvalue", t, context, role_Return);
}

static void addExceptionOutputFn (refany elt, refany rock)
{
  Exception       e = (Exception) elt;
  Context         context = (Context) rock;
  Exception       ure = e->import ? e->import : e;
  Type            ut = ur_type(ure->type);
  TypeKind        tk = type_kind(ut);
  char            a;
  char           *p = &a;
  FILE           *f = context->file;

  codeType (ut, &p, role_In);
  fprintf(context->file, "%s\n    { (unsigned int) '%c', ",
	  context->counter == 0 ? "" : ",", a);
  context->counter += 1;

  if (tk == object_Type)
    fprintf (f, "_%s__ILUType, (ILU_C_SizeFn) ", c_type_name(ut));
  else
    fprintf (f, "(ilu_Class) 0, (ILU_C_SizeFn) ");

  switch (tk)
    {
    case integer_Type:
      fprintf (f, "ilu_SizeOfInteger, (ILU_C_OutputFn) ilu_OutputInteger");
      break;
      
    case cardinal_Type:
      fprintf (f, "ilu_SizeOfCardinal, (ILU_C_OutputFn) ilu_OutputCardinal");
      break;
      
    case shortinteger_Type:
      fprintf (f, "ilu_SizeOfShortInteger, (ILU_C_OutputFn) ilu_OutputShortInteger");
      break;
      
    case shortcardinal_Type:
      fprintf (f, "ilu_SizeOfShortCardinal, (ILU_C_OutputFn) ilu_OutputShortCardinal");
      break;
      
    case longinteger_Type:
      fprintf (f, "ilu_SizeOfLongInteger, (ILU_C_OutputFn) ilu_OutputLongInteger");
      break;
      
    case longcardinal_Type:
      fprintf (f, "ilu_SizeOfLongCardinal, (ILU_C_OutputFn) ilu_OutputLongCardinal");
      break;

    case character_Type:
      fprintf (f, "ilu_SizeOfCharacter, (ILU_C_OutputFn) ilu_OutputCharacter");
      break;

    case shortcharacter_Type:
      fprintf (f, "ilu_SizeOfShortCharacter, (ILU_C_OutputFn) ilu_OutputShortCharacter");
      break;

    case real_Type:
      fprintf (f, "ilu_SizeOfReal, (ILU_C_OutputFn) ilu_OutputReal");
      break;

    case shortreal_Type:
      fprintf (f, "ilu_SizeOfShortReal, (ILU_C_OutputFn) ilu_OutputShortReal");
      break;

    case longreal_Type:
      fprintf (f, "ilu_SizeOfLongReal, (ILU_C_OutputFn) ilu_OutputLongReal");
      break;

    case byte_Type:
      fprintf (f, "ilu_SizeOfByte, (ILU_C_OutputFn) ilu_OutputByte");
      break;

    case boolean_Type:
      fprintf (f, "ILU_C_SizeOfBoolean, (ILU_C_OutputFn) ILU_C_OutputBoolean");
      break;

    case enumeration_Type:
      fprintf (f, "ilu_SizeOfEnum, (ILU_C_OutputFn) ilu_OutputEnum");
      break;

    case object_Type:
      fprintf(f, "0, 0");
      break;

    case array_Type:
    case record_Type:
    case union_Type:
    case sequence_Type:
    case optional_Type:
      fprintf(f,
	      "(ILU_C_SizeFn) _%s__SizeOf, (ILU_C_OutputFn) _%s__Output",
	      c_type_name(ut), c_type_name(ut));
      break;

    case void_Type:
      fprintf (f, "0, 0");
      break;

  default:
    fatal ("Error: Bad parameter type %s.\n", c_type_name (ure->type));
  }
  fprintf (f, ", 0, 0 }");
}

static void declareExceptionValOutputFns (list exceptions, Context context)
{
  if (exceptions == NULL || list_size(exceptions) < 1)
    return;

  fprintf (context->file, "  static struct _ILU_C_ExceptionDescription_s _evec[] = {");
  context->counter = 0;
  list_enumerate (exceptions, addExceptionOutputFn, context);
  fprintf (context->file, "};\n\n");
}

static void serverMethods(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         context = (Context) rock;
  fprintf(context->file, "    (void(*)(void)) server_%s_%s,\n",
	  c_type_name(context->class), c_simple_name(m->name));
}

static void generateCalleeStub (Procedure  m, Context  context)
{
  enum PrimitiveTypes t = type_basic_type(ur_type(m->returnType));

  if (methodInList(c_simple_name(m->name))) {
    MethodRecordID++;
    return;
  }
  addMethodToList(c_simple_name(m->name));

  fprintf(context->file, "static void _%s_%s__truestub(ilu_Call _call, ilu_Error *_err)\n{\n",
	  c_type_name(context->class), c_simple_name(m->name));
  fprintf(context->file, "  %s _h;\n", c_type_name(context->class));
  fprintf(context->file, "  ILU_C_ENVIRONMENT _status;\n");
  list_enumerate(m->arguments, declareArg, context);
  if (type_ur_kind(m->returnType) != void_Type)
    fprintf(context->file, "  %s _retvalue = {0};\n",
	    c_role_type(m->returnType, role_Return, TRUE));
  declareExceptionValOutputFns (m->exceptions, context);
  fprintf(context->file, "\n");

#if 0
  list_enumerate(m->arguments, initArgument, context);
  initReturnValue(m->returnType, context);
#endif
  fprintf(context->file, "  ILU_CLER(*_err);\n");
  fprintf(context->file, "  _h = (%s) ",
	  c_type_name(context->class));
  if (SINGLETON(context->class))
    fprintf(context->file, "_ILU_C_GetServerSingleton (_call, _err);\n");
  else
    fprintf(context->file,
	    "_ILU_C_InputObject (_call, _%s__ILUType, ilu_TRUE, _err);\n",
	    c_type_name(context->class));
  fprintf (context->file, "  if (ILU_ERRNOK(*_err)) goto errexit;\n");
  list_enumerate(m->arguments, inputArgument, context);
  fprintf(context->file, "\n");
  fprintf(context->file,
	  "  if (_ILU_C_FinishParameters (_call, _h, _err)) {\n");
  fprintf(context->file, "    _status.returnCode = ILU_NIL;\n");
  fprintf(context->file, "    _status.callerPassport = ilu_CallerPassportOfCall(_call);\n    ");
  if (t != void_Type)
    fprintf(context->file, "_retvalue = ");
  fprintf(context->file, "%s_%s (_h",
	  c_type_name(context->class), c_simple_name(m->name));
  list_enumerate(m->arguments, listArgument, context);
  fprintf(context->file, ", &_status);\n\n");
  if (m->asynch) {
    fprintf(context->file, "    /* asynchronous method -- no reply */\n");
    fprintf(context->file, "  }\n");
    fprintf(context->file, "  if (!_ILU_C_NoReply(_call, _err)) goto errexit;\n");
  } else {
    fprintf(context->file, "    /* check for errors */\n");
    fprintf(context->file, "    if (_status.returnCode == ILU_NIL) {\n");
    fprintf(context->file, "      ilu_cardinal _size = 0;\n");
    fprintf(context->file, "      if (ilu_CallNeedsSizing(_call)) {\n");
    fprintf(context->file, "        _size = ilu_BeginSizingReply(_call, %s, _err);\n",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    fprintf(context->file, "        if (ILU_ERRNOK(*_err))\n");
    fprintf(context->file, "          goto errexit;\n");
    if (t != void_Type) {
      sizeArgument ("_retvalue", ur_type(m->returnType), role_Return, context);
    }
    list_enumerate(m->arguments, sizeArg, context);
    fprintf(context->file, "      };\n");
    fprintf(context->file,
	    "      if (!_ILU_C_BeginReply (_call, %s, _size, _err)) goto errexit;\n",
	(list_size(m->exceptions) > 0) ? "ilu_TRUE" : "ilu_FALSE");
    if (type_basic_type(ur_type(m->returnType)) != void_Type)
      outputArgument("_retvalue", ur_type(m->returnType), role_Return,
		     context);
    list_enumerate(m->arguments, outputArg, context);
    fprintf(context->file,
	    "      if (! _ILU_C_FinishReply (_call, _err)) goto errexit;\n");
    fprintf(context->file, "    }\n");
    fprintf(context->file, "    else {\n");
    if (list_size(m->exceptions) > 0) {
      fprintf(context->file,
	      "      _ILU_C_SendException (_call, _evec, &_status, _err);\n",
	      c_interface_name(m->interface));
      fprintf (context->file, "      if (ILU_ERRNOK(*_err)) goto errexit;\n");
    } else {
      fprintf(context->file,
	      "      (void) ILU_ERR_CONS1(bad_param, _err, minor, ilu_bpm_some_raise, 6);\n");
    }
    fprintf(context->file, "    }\n");
    fprintf(context->file, "  }\n\n");
  }
  fprintf (context->file, " marshalError:\n errexit:\n");
  fprintf(context->file, "  _ILU_C_FinishServingCall(_call, _err);\n");
  if (t != void_Type)
    freeReturnValue(m->returnType, context);
  list_enumerate(m->arguments, freeArgument, context);
  fprintf(context->file, "  return;\n}\n\n");
}

static void classTable (Type class, Context context)
{
  Class od;

  if  (class == NULL || 
       type_basic_type (class) != object_Type || 
       (od = class_object (class)) == NULL)
    return;
  if  (list_size (od->methods) > 0)
    list_enumerate (od->methods, serverMethods, context);
}


static void generateServerMiMethodPtrs(refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  Type            ut = ur_type(class);

  if (class_object(ut)->superclasses != NULL)
    list_enumerate(class_object(ut)->superclasses,
		   generateServerMiMethodPtrs, context);
  if (methodInList(c_type_name(ut)))
    return;
  addMethodToList(c_type_name(ut));
  fprintf(context->file,
  "static struct _%s__MethodBlock_s _%s_%s__TrueMethodBlock = {\n",
	  c_type_name(ut), c_type_name(context->class),
	  c_type_name(ut));
  fprintf(context->file, "  NULL /* will be set to _%s__ILUType */",
	  c_type_name(ut));
  if (list_size(class_object(ut)->methods) > 0) {
    fprintf(context->file, ",\n  {\n");
    classTable(ut, context);
    fprintf(context->file, "  }\n");
  }
  fprintf(context->file, "};\n");
  return;
}

static void generateServerClsPtrs (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses,
		   generateServerClsPtrs, c);
  fprintf(c->file,
	  "  (_ILU_C_MethodBlock *) &_%s_%s__TrueMethodBlock,\n",
	  c_type_name(c->class), c_type_name(ur_type(t)));
}

static void generateServerClassTable (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (type_basic_type(t) != object_Type)
    return;

  c->class = t;
  clearMethodList();
  generateServerMiMethodPtrs(t, c);
  fprintf(c->file,
	  "static _ILU_C_MethodBlock *_%s__TrueTypeVector[] = {\n",
	  c_type_name(t));
  fprintf(c->file,
	  "  (_ILU_C_MethodBlock *) &_%s_%s__TrueMethodBlock,\n",
	  c_type_name(c->class), c_type_name(t));
  if (class_object(t)->superclasses != NULL)
    list_enumerate(class_object(t)->superclasses, generateServerClsPtrs,
		   c);
  fprintf(c->file, "  ILU_NIL\n};\n\n");
}

static void trueMethBlkInitsPerIluType(refany elt, refany rock)
{
  Type            class = (Type) elt;
  Context         context = (Context) rock;
  Type            ut = ur_type(class);

  if (class_object(ut)->superclasses != NULL)
    list_enumerate(class_object(ut)->superclasses,
		   trueMethBlkInitsPerIluType, context);
  if (methodInList(c_type_name(ut)))
    return;
  addMethodToList(c_type_name(ut));
  fprintf(context->file, "  _%s_%s__TrueMethodBlock.c = _%s__ILUType;\n",
	  c_type_name(context->class), c_type_name(ut),
	  c_type_name(ut));
}

static void trueMethBlkInitsPerCType (refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         c = (Context) rock;
  if (type_basic_type(t) != object_Type)
    return;
  c->class = t;
  clearMethodList();
  trueMethBlkInitsPerIluType(t, c);
}

static void classSetupStubs (Type  class, Context  context)
{
  Class  od;

  if  (class == NULL || 
       type_basic_type (class) != object_Type || 
       (od = class_object (class)) == NULL)
    return;
  if  (list_size (od->methods) > 0)
    list_enumerate (od->methods, (void (*)(refany, refany)) generateCalleeStub, context);
}


static void generateServerCodeForClass (Type class, Context context)
{
  if (type_basic_type(class) != object_Type)
    return;

  context->class = class;
  clearMethodList ();
  classSetupStubs (class, context);
}

static void initializeStubPointer (Procedure  method, Context  context)
{
  fprintf (context->file, "  _%s__ILUType->cl_methods[%ld].me_stubproc = (void (*)(ilu_Call)) _%s_%s__truestub;\n",
	   c_type_name (context->class), MethodRecordID++,
	   c_type_name (context->class /*method->object*/),
	   c_simple_name (method->name));
}

static void setupServerStubsInMethodTable (Type  type, Context  context)
{
  Class od;
  
  if  (type == NULL || 
       type_basic_type (type) != object_Type || 
       (od = class_object (type)) == NULL)
    return;
  fprintf (context->file, "\n");
  list_enumerate (od->methods, (void (*)(refany, refany)) initializeStubPointer, context);
}

static void setupServerStubsTable (Type  type, Context  context)
{
  MethodRecordID = 0;
  FirstClass = True;
  context->class = type;
  setupServerStubsInMethodTable (type, context);
}

static void generateHandles (Type  type, Context  context)
{
  fprintf (context->file, "ILU_C_OBJECT %s__CreateTrue  (ilu_string instance_handle, ilu_Server server, void * data)\n{\n", c_type_name(type));
  fprintf (context->file, "  return (_ILU_C_CreateTrueObject (_%s__TrueTypeVector, instance_handle, server, data, ilu_FALSE));\n}\n\n", c_type_name (type));
  fprintf (context->file, "ILU_C_OBJECT %s__OTCreateTrue  (ilu_string instance_handle, ilu_Server server, void * data)\n{\n", c_type_name(type));
  fprintf (context->file, "  if (server == ILU_NIL || instance_handle == ILU_NIL) return ILU_NIL;\n");
  fprintf (context->file, "  return (_ILU_C_CreateTrueObject (_%s__TrueTypeVector, instance_handle, server, data, ilu_TRUE));\n}\n\n", c_type_name (type));
}

static void 
generateServerRegistrationCode(Interface interface, Context context)
{
  FILE           *f = context->file;
  char           *interface_name = (char *) c_interface_name(interface);

  list_enumerate(interface->classes,
	     (void (*) (refany, refany)) generateHandles, context);
  fprintf(f, "void %s__InitializeServer(void)\n{\n",
	  interface_name);
  fprintf(f, "  extern void _%s__GeneralInitialization(void);\n",
	  c_interface_name(interface));
  fprintf(f, "  static ilu_boolean initialized = ilu_FALSE;\n");
  fprintf(f, "  if (initialized) return;\n  initialized = ilu_TRUE;\n\n");
  fprintf(f, "  _%s__GeneralInitialization ();\n", interface_name);
  list_enumerate(interface->classes, trueMethBlkInitsPerCType, context);
  list_enumerate(interface->classes,
       (void (*) (refany, refany)) setupServerStubsTable, context);
  fprintf(f, "}\n\n");
}

void generateServerCode (Interface parse, FILE *file)
{
  struct context_s context;
  char           *pc_interfacename;

  context.file = file;
  context.interface = parse;

  /*
   * get any translation of what the header file for the interface
   * is
   */
  pc_interfacename = interface_header_name(c_interface_name(parse));

  fprintf(file, "#include <stdio.h>\n\n");
  fprintf(file, "#include \"%s.h\"\n\n", pc_interfacename);

  list_enumerate(parse->classes, generateServerClassTable, &context);

  list_enumerate(parse->classes,
	    (void (*) (refany, refany)) generateServerCodeForClass,
		 &context);

  generateServerRegistrationCode(parse, &context);
}
