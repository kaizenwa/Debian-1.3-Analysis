/*
 ** Copyright (c) 1991-1994 Xerox Corporation. All Rights Reserved.
 **
 ** Unlimited use, reproduction, and distribution of this software is
 ** permitted. Any copy of this software must include both the above
 ** copyright notice of Xerox Corporation and this paragraph. Any
 ** distribution of this software must comply with all applicable United
 ** States export control laws. This software is made available AS IS,
 ** and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
 ** INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
 ** AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
 ** PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
 ** THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
 ** CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
 ** XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 */
/* $Id: cheaders.c,v 1.87 1996/04/04 04:05:40 janssen Exp $ */
/* Last edited by Mike Spreitzer April 2, 1996 11:03 pm PST */


#include "cstubber.h"

#include <ctype.h>

#define IsHexDigit(x) ((((x)>='0')&&((x)<='9'))||(((x)>='a')&&((x)<='f'))||(((x)>='A')&&((x)<='F')))
#define HexValue(x) (((x)<='9')?((x)-'0'):(((x)<='F')?((x)-'A'):(((x)<='f')?((x)-'a'):0)))

  struct double_s {
    Type discriminant_type;
    Context context;
    unsigned long id;
  };

static void sortTypesForDeclaration (Type, list);

const char hyphens[] = "--------------------------------------------------------------------------------";

static void convertStringForm (char *buf, string str)
{
  char *p;
  char *q;

  for (p = str, q = buf; *p != '\0';)
    {
      if (!isgraph(*p) OR (*p == '"') OR (*p == '\\'))
	{
	  *q++ = '\\';
	  *q++ = '0' + ((((unsigned char) *p) >> 6) & 0x7);
	  *q++ = '0' + ((((unsigned char) *p) >> 3) & 0x7);
	  *q++ = '0' + (((unsigned char) *p) & 0x7);
	  p++;
	}
      else
	*q++ = *p++;
    }
  *q = '\0';
}

static void declareConstant (Constant c, Context context)
{
  enum PrimitiveTypes t = type_ur_kind (c->type);

  if (c->interface != context->interface)
    return;
  if ((t == cardinal_Type || t == shortcardinal_Type || t == byte_Type)
      && (c->value->type == integer_Type)) {
    fprintf (context->file, "static const %s %s = 0x%lx;\n", (t == cardinal_Type) 
	     ? "unsigned long" 
	     : ((t == byte_Type) 
		? "unsigned char" 
		: "unsigned short"), c_constant_name (c), c->value->val.i.value);
  }
  else if ((t == integer_Type || t == shortinteger_Type)
	   && (c->value->type == integer_Type)) {
    fprintf (context->file, "static const %s %s = %s%lu;\n",
	     (t == integer_Type) ? "long int" : "short int",
	     c_constant_name (c),
	     (c->value->val.i.sign < 0) ? "-" : "", c->value->val.i.value);
  }
  else if ((t == real_Type || t == shortreal_Type)
	   && (c->value->type == real_Type)) {
    fprintf (context->file, "static const %s %s = %s%s.%se%ld;\n",
	     (t == real_Type) ? "double" : "float",
	     c_constant_name (c),
	     (c->value->val.r.sign < 0) ? "-" : "",
	     c->value->val.r.value,
	     (c->value->val.r.fraction == NULL) ? "0" : c->value->val.r.fraction,
	     c->value->val.r.exponent);
  }
  else if (t == shortcharacter_Type)
    {
      if (c->value->type == shortcharacter_Type)
	{
	  char buf[ 1000 ];

	  convertStringForm (buf, c->value->val.s);
	  fprintf (context->file, "static const %s %s = '%s';\n",
		   c_type_name(c->type), c_constant_name(c), buf);
	}
      else
	fprintf (context->file, "static const %s %s = 0x%lx;\n",
		 c_type_name(c->type), c_constant_name(c),
		 c->value->val.i.value);
    }
  else if (TypeIsString(c->type))
    {
      if (c->value->type == shortcharacter_Type)
	{
	  char buf[ 1000 ];

	  convertStringForm (buf, c->value->val.s);
	  fprintf (context->file, "static const %s %s = \"%s\";\n",
		   c_type_name(c->type), c_constant_name(c), buf);
	}
      else
	fprintf (context->file, "static const char %s = 0x%lx;\n",
		 c_constant_name (c), c->value->val.i.value);
    }
  else if (t == boolean_Type AND c->value->type == boolean_Type)
    fprintf (context->file, "static const %s %s = %s;\n",
	     c_type_name(c->type), c_constant_name(c),
	     c->value->val.b ? "ilu_TRUE" : "ilu_FALSE");
  else
    error ("Invalid constant, %s, encountered.\n", name_base_name (c->name));
}

static void declareException (Exception e, Context context)
{
  if (e->import)
    return;
  fprintf(context->file,
	  "extern ILU_C_ExceptionCode _%s__Exception_%s;\n",
	  c_interface_name(context->interface),
	  c_simple_name(e->name));
  fprintf(context->file, "#define ex_%s _%s__Exception_%s\n",
	  c_exception_name(e),
	  c_interface_name(context->interface),
	  c_simple_name(e->name));
}

void listArgumentTypes(refany elt, refany rock)
{
  Argument        arg = (Argument) elt;
  Context         context = (Context) rock;
  fprintf(context->file, ", %s",
	  c_parameter_type(arg->type, arg->direction));
}


static void listException (Exception e, Context c)
{
  fprintf (c->file, " %s", exception_name(e));
}

static void mkClientPrototype (Procedure m, Context c)
{
  fprintf(c->file, "extern %s", c_return_type(m->returnType));
  if (list_size(m->exceptions) > 0) {
    fprintf(c->file, "  /* exceptions: ");
    list_enumerate(m->exceptions, (iluparser_EnumProc) listException, c);
    fprintf(c->file, " */");
  }
  fprintf(c->file, "\n");

  fprintf(c->file, "  %s_%s (%s", c_type_name(c->class),
	  c_simple_name(m->name), c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *);\n\n");
}

static void mkServerPrototype(refany elt, refany rock)
{
  Procedure       m = (Procedure) elt;
  Context         c = (Context) rock;
  fprintf(c->file, "extern %s server_%s_%s (%s",
	  c_return_type(m->returnType),
	  c_type_name(c->class), c_simple_name(m->name),
	  c_type_name(c->class));
  list_enumerate(m->arguments, listArgumentTypes, c);
  fprintf(c->file, ", ILU_C_ENVIRONMENT *);\n");
}

static void generateClientPrototypes (Type t, Context context)
{
  Class c;

  if (t == NULL ||
      type_basic_type(t) != object_Type ||
      (c = class_object(t)) == NULL)
    return;
  list_enumerate(c->methods, (iluparser_EnumProc) mkClientPrototype, context);
}

static void generateServerPrototypes(refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         context = (Context) rock;
  Class           c;
  if (t == NULL || type_ur_kind(t) != object_Type ||
      (c = class_object(t)) == NULL)
    return;
  list_enumerate(c->superclasses, generateServerPrototypes, context);
  list_enumerate(c->methods, mkServerPrototype, context);
}

static void declareClassType (Type t, Context context)
{
  Class c;

  if (t == NULL ||
      type_basic_type (t) != object_Type ||
      (c = class_object (t)) == NULL)
    return;
  context->class = t;
  fprintf (context->file, "typedef ILU_C_OBJECT %s;\n", c_type_name (t));
}

static void declareCClass(refany elt, refany rock)
{
  Type            t = (Type) elt;
  Context         context = (Context) rock;
  Class           c;
  char *	  tn;
  int             pad;

  if (t == NULL ||
      type_basic_type(t) != object_Type ||
      (c = class_object(t)) == NULL)
    return;
  context->class = t;
  tn = c_type_name(t);
  pad = 71 - 23 - strlen(tn);

  fprintf (context->file, "/*-----------------------------------------------------------------------*/\n");
  fprintf (context->file, "/*-----  object type \"%s\"  %*.*s*/\n\n",
	   tn, pad, pad, hyphens);

  fprintf(context->file, "#define %s__MSType _%s__ILUType\n\n", tn, tn);

  fprintf(context->file, "%s\n", tn);
  fprintf(context->file,
	  "  %s__CreateFromSBH (char * /* sbh */, ILU_C_ENVIRONMENT * /* env */);\n\n",
	  tn);

  fprintf (context->file, "/* for true module only: */\n");
  fprintf(context->file,
	  "%s\n  %s__CreateTrue (ilu_string /* instance-handle */, ilu_Server /* server */, void * /* user data */);\n",
	  tn, tn);
  fprintf (context->file, "/* only for use within an object table's `ot_object_of_ih' method */\n");
  fprintf(context->file,
	  "%s\n  %s__OTCreateTrue (ilu_string /* instance-handle */, ilu_Server /* server */, void * /* user data */);\n",
	  tn, tn);
  fprintf(context->file,
	  "void\n  %s__SetUserData (%s /* self */, void * /* userData */);\n",
	  tn, tn);
  fprintf(context->file, "void *\n  %s__GetUserData (%s /* self */);\n\n", tn, tn);
  fprintf (context->file, "/*------- methods ------------------------------------------------------*/\n\n");
  generateClientPrototypes(t, context);
  fprintf (context->file, "\n\n");
}

void declareCClassJunk (Type t, Context context)
{
  char           *tn;
  int             i;
  Class           c;

  if (t == NULL ||
      type_basic_type(t) != object_Type ||
      (c = class_object(t)) == NULL)
    return;

  tn = c_type_name(t);
  context->class = t;

  fprintf(context->file, "\n");
  generateServerPrototypes(t, context);
  fprintf(context->file, "\n");
  fprintf(context->file, "extern ilu_Class _%s__ILUType;\n", tn);
  fprintf(context->file, "struct _%s__MethodBlock_s {\n", tn);
  fprintf(context->file, " ilu_Class c;\n");
  if ((i = list_size(c->methods)) > 0)
    fprintf(context->file, " void (*methods[%u])(void);\n", i);
  fprintf(context->file, "};\n");
  fprintf(context->file,
     "extern struct _%s__MethodBlock_s _%s__SurrogateMethodBlock;",
	  tn, tn);
}

static boolean matchPointer (refany p1, refany p2)
{
  return (p1 == p2);
}

static void generateRecordField (Argument a, Context context)
{
  fprintf (context->file, "\t%s %s;\n",
	   ((type_basic_type (a->type) == object_Type) ? c_return_type (a->type) : c_type_name (a->type)),
	   c_simple_name (a->name));
}

static void generateRecordDeclaration (Type t, Context context)
{
  fprintf (context->file, "/* record \"%s\" */\n", c_type_name(t));

  fprintf (context->file, "struct %s {\n", c_type_name (t));
  list_enumerate (type_description (t)->structuredDes.record, (iluparser_EnumProc) generateRecordField, context);
  fprintf (context->file, "};\n");
}

static void generateUnionField (Argument a, Context context)
{
  char *name;

  if (a->name->base_name != NULL)
    name = (char *) c_simple_name (a->name);
  else
    name = (char *) c_string (type_name (a->type));
  fprintf (context->file, "\t\t%s %s;\n", c_type_name (a->type), name);
}

static void generateUnionTypeDiscriminator (Argument a, struct double_s *s)
{
  fprintf (s->context->file, "#define %s__%s ", c_type_name (s->context->class), c_string (type_name (a->type)));
  if (list_size(a->values) == 0)
    fprintf (s->context->file, "%lu\n", s->id++);
  else
    {
      ConstantValue val = list_car(a->values);
      switch (val->type)
	{
	case integer_Type:
	case shortinteger_Type:
	case cardinal_Type:
	case shortcardinal_Type:
	case byte_Type:
	  fprintf (s->context->file, "%s%lu\n", (val->val.i.sign < 0) ? "-" : "", val->val.i.value);
	  break;
	case shortcharacter_Type:
	  fprintf (s->context->file, "%s_%s\n", c_interface_name(s->discriminant_type->interface), val->val.s);
	  break;
	case boolean_Type:
	  fprintf (s->context->file, "ilu_%s\n", val->val.b ? "TRUE" : "FALSE");
	  break;
	default:
	  error ("illegal discriminator value\n");
	}
    }
}

static void generateUnionDeclaration (Type t, Context context)
{
  Type d = type_description (t)->structuredDes.uniond.discriminator_type;
  list e = type_description (t)->structuredDes.uniond.types;
  struct double_s s;
  
  s.discriminant_type = ur_type(type_description (t)->structuredDes.uniond.discriminator_type);
  s.id = 0;
  s.context = context;
  context->class = t;

  fprintf (context->file, "/* union \"%s\" */\n", c_type_name(t));

  list_enumerate (e, (iluparser_EnumProc) generateUnionTypeDiscriminator, &s);

  fprintf (context->file, "struct _%s_union {\n", c_type_name (t));
  fprintf (context->file, "\t%s _d;\n\tunion {\n", c_type_name (d));
  list_enumerate (e, (iluparser_EnumProc) generateUnionField, context);
  fprintf (context->file, "\t} _u;\n};\n");
}

static void PrintEnumField (EnumField e, Context context)
{
  fprintf (context->file, ", %s_%s", c_interface_name (context->interface), c_string (e->name));
  if (e->id >= 0)
    fprintf (context->file, " = %d", e->id); 
}

static void PrintFirstEnumField (EnumField e, Context context)
{
  fprintf (context->file, "%s_%s", c_interface_name (context->interface), c_string (e->name));
  if (e->id >= 0)
    fprintf (context->file, " = %d", e->id); 
}

static void generateEnumerationTypedefDeclaration (Type t, Context context)
{
  list e;
  EnumField ef;

  fprintf (context->file, "typedef enum {");
  e = (type_description (t))->structuredDes.enumeration;
  ef = (EnumField) list_car (e);
  context->class = t;
  PrintFirstEnumField (ef, context);
  if (list_size (e) > 1)
    list_enumerate (list_cdr (e), (iluparser_EnumProc) PrintEnumField, context);
  fprintf (context->file, "} %s;\n", c_type_name (t));
}

static void generateSequenceDeclaration(Type t, Context context)
{
  TypeDescription d = type_description(t);
  TypeKind        tk = type_ur_kind(d->structuredDes.sequence.type);

  if (tk == shortcharacter_Type || tk == character_Type)
    return;
  fprintf(context->file, "/* sequence \"%s\" */\n", c_type_name(t));
  fprintf(context->file, "struct _%s__sequence {\n", c_type_name(t));
  fprintf(context->file, " unsigned long _maximum;\n");
  fprintf(context->file, " unsigned long _length;\n");
  fprintf(context->file, " %s *_buffer;\n};\n",
	  c_type_name(d->structuredDes.sequence.type));

}

static void AliasMethod (Procedure p, Context context)
{
  fprintf (context->file, "#define %s_%s %s_%s\n",
	   c_type_name(context->class), c_simple_name(p->name),
	   c_type_name(ur_type(context->class)), c_simple_name(p->name));
}


static void AliasMethodsOfClass (Type t, Context context)
{
  Class c;

  if (t == NULL ||
      type_basic_type (ur_type(t)) != object_Type ||
      (c = class_object (t)) == NULL)
    return;
  if (c->superclasses != NULL)
    list_enumerate (c->superclasses, (iluparser_EnumProc) AliasMethodsOfClass, context);
  list_enumerate (c->methods, (iluparser_EnumProc) AliasMethod, context);
}

static void generateAliasSupport (Type t, Context context)
{
  /* in general, nothing to be done, except in the case of objects and sequences */
  Type ut;

  if (type_basic_type(t) != alias_Type)
    return;

  ut = ur_type(t);

  context->class = t;

  fprintf (context->file, "/* \"%s\" alias for \"%s\" */\n", c_type_name(t), c_type_name(ut));

  switch (type_basic_type(ut))
    {
    case object_Type:
      fprintf (context->file, "#define %s__CreateTrue %s__CreateTrue\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__OTCreateTrue %s__OTCreateTrue\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__CreateFromSBH %s__CreateFromSBH\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__SetUserData %s__SetUserData\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__GetUserData %s__GetUserData\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__MSType _%s__ILUType\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__Free %s__Free\n", c_type_name(t), c_type_name(ut));
      AliasMethodsOfClass (t, context);
      break;

    case sequence_Type:
      fprintf (context->file, "#define %s_Every %s_Every\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s_Append %s_Append\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s_Push %s_Push\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s_Pop %s_Pop\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s_Init %s_Init\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s_Create %s_Create\n", c_type_name(t), c_type_name(ut));
      fprintf (context->file, "#define %s__alloc %s__alloc\n", c_type_name(t), c_type_name(ut));
      break;

    case union_Type:
      {
	struct double_s s;
  
	s.discriminant_type = ur_type(type_description(t)->structuredDes.uniond.discriminator_type);
	s.id = 0;
	s.context = context;

	list_enumerate (type_description(t)->structuredDes.uniond.types,
			(iluparser_EnumProc) generateUnionTypeDiscriminator, &s);
      }
      break;

    default:
      /* nothing to do for these alias types */
      break;
    }

  {
    char *a = c_type_name(t);
    char *b = c_type_name(ut);
    fprintf (context->file, "#define CORBA_sequence_%s_allocbuf CORBA_sequence_%s_allocbuf\n",
	     (t->builtIn && strncmp(a, "CORBA_", 6) == 0) ? a + 6 : a,
	     (ut->builtIn && strncmp(b, "CORBA_", 6) == 0) ? b + 6 : b);
  }
}

static void generateBufAlloc (Type t, Context c)
{
  char *s = c_type_name(t);

  fprintf (c->file, "%s *\n  CORBA_sequence_%s_allocbuf (CORBA_unsigned_long /* count */);\n",
	   s, (t->builtIn && strncmp(s, "CORBA_", 6) == 0) ? s + 6 : s);
}

static void generateSeqProto (Type t, Context c);

static void declareType (Type t, Context context)
{
  if (t->builtIn)
    return;

  if (t->importInterfaceName != NULL)
    return;	/* defined in some other interface */

  switch (type_basic_type (t)) {
  case void_Type:
  case integer_Type:
  case cardinal_Type:
  case shortinteger_Type:
  case shortcardinal_Type:
  case longinteger_Type:
  case longcardinal_Type:
  case character_Type:
  case shortcharacter_Type:
  case real_Type:
  case shortreal_Type:
  case longreal_Type:
  case byte_Type:
  case array_Type:
  case boolean_Type:
  case object_Type:
    break;

  case alias_Type:
    generateAliasSupport (t, context);
    break;

  case record_Type:
    generateRecordDeclaration (t, context);
    break;

  case union_Type:
    generateUnionDeclaration (t, context);
    break;

  case sequence_Type:
    generateSequenceDeclaration (t, context);
    generateSeqProto (t, context);
    break;

  case enumeration_Type:
    /* already done in typedef */
    break;

  case optional_Type:
    /* already done */
    break;

  default:
    fatal ("Error: Can't cope with declaration of type %s yet.\n", c_type_name (t));
  }

  if (type_basic_type(t) != alias_Type)
    generateBufAlloc(t, context);

  fprintf (context->file, "\n");
}

static void generateUnionTypedef (Type t, Context context)
{

  fprintf (context->file, "typedef struct _%s_union %s;\n", c_type_name (t), c_type_name (t));
}

static void OutputDim (long d, Context context)
{
  fprintf (context->file, "[%lu]", d);
}

static void generateArrayTypedef (Type t, Context context, boolean slice)
{
  TypeDescription d = type_description (t);
  Type et = d->structuredDes.array.type;
  fprintf (context->file, "typedef ");
  switch (type_ur_kind(et)) {
  case void_Type:
  case integer_Type:
  case cardinal_Type:
  case shortinteger_Type:
  case shortcardinal_Type:
  case longinteger_Type:
  case longcardinal_Type:
  case character_Type:
  case shortcharacter_Type:
  case real_Type:
  case shortreal_Type:
  case longreal_Type:
  case byte_Type:
  case array_Type:
  case boolean_Type:
  case object_Type:
  case optional_Type:
  case enumeration_Type:
    fprintf (context->file, "%s", c_type_name(et));
    break;

  case record_Type:
    fprintf (context->file, "struct %s", c_type_name(et));
    break;

  case union_Type:
    fprintf (context->file, "struct _%s_union", c_type_name(et));
    break;

  case sequence_Type:
    {
      Type eet = type_description(et)->structuredDes.sequence.type;
      TypeKind        t = type_ur_kind(eet);
      if (t == shortcharacter_Type)
	fprintf (context->file, "CORBA_char *");
      else if (t == character_Type)
	fprintf (context->file, "ilu_character *");
      else
	fprintf (context->file, "struct _%s__sequence", c_type_name(et));
      break;
    }

  default:
    fatal("Error: Can't cope with declaration of type %s yet.\n",
	  c_type_name(et));
  }

  fprintf (context->file, " %s%s", c_type_name (t), (slice) ? "_slice" : "");
  if (slice)
    list_enumerate (list_cdr(d->structuredDes.array.dimensions), (iluparser_EnumProc) OutputDim, context);
  else
    list_enumerate (d->structuredDes.array.dimensions, (iluparser_EnumProc) OutputDim, context);
  fprintf (context->file, ";\n");
}

static void generateSequenceTypedef(Type t, Context context)
{
  TypeDescription d = type_description(t);
  Type            et = d->structuredDes.sequence.type;
  TypeKind        etk = type_ur_kind(et);

  if (etk == shortcharacter_Type)
    fprintf(context->file, "typedef CORBA_char * %s;\n",
	    c_type_name(t));
  else if (etk == character_Type)
    fprintf(context->file, "typedef ilu_character * %s;\n",
	    c_type_name(t));
  else {
    fprintf(context->file, "typedef struct _%s__sequence %s;\n",
	    c_type_name(t), c_type_name(t));
  }
}

static void generateRecordTypedef (Type t, Context context)
{
  fprintf (context->file, "typedef struct %s %s;\n", c_type_name (t), c_type_name (t));
}

static void generateOptionalTypedef(Type t, Context context)
{
  Type            t2 = ur_type(type_description(t)->structuredDes.optional);
  while (type_kind(t2) == optional_Type)
    t2 = ur_type(type_description(t2)->structuredDes.optional);
  fprintf(context->file, "typedef %s %s%s;\n",
	  c_type_name(t2),
	  TypeIsPointer(t2) ? "" : "*",
	  c_type_name(t));
}

static void generateAliasTypedef (Type t, Context context)
{
  fprintf (context->file, "typedef %s %s;\n", c_type_name(ur_type(t)), c_type_name(t));
}

static void typedefType (Type t, Context context)
{
  if (t->builtIn)
    return;
  if (t->importInterfaceName != NULL)
    return;

  switch (type_basic_type (t)) {
  case void_Type:
    break;
  case integer_Type:
    fprintf (context->file, "typedef CORBA_long %s;\n", c_type_name (t));
    break;
  case cardinal_Type:
    fprintf (context->file, "typedef CORBA_unsigned_long %s;\n", c_type_name (t));
    break;
  case shortinteger_Type:
    fprintf (context->file, "typedef CORBA_short %s;\n", c_type_name (t));
    break;
  case shortcardinal_Type:
    fprintf (context->file, "typedef CORBA_unsigned_short %s;\n", c_type_name (t));
    break;
  case longinteger_Type:
    fprintf (context->file, "typedef ilu_longinteger %s;\n", c_type_name (t));
    break;
  case boolean_Type:
    fprintf (context->file, "typedef CORBA_boolean %s;\n", c_type_name (t));
    break;
  case longcardinal_Type:
    fprintf (context->file, "typedef ilu_longcardinal %s;\n", c_type_name (t));
    break;
  case real_Type:
    fprintf (context->file, "typedef CORBA_double %s;\n", c_type_name (t));
    break;
  case shortreal_Type:
    fprintf (context->file, "typedef CORBA_float %s;\n", c_type_name (t));
    break;
  case longreal_Type:
    fprintf (context->file, "typedef ilu_longreal %s;\n", c_type_name (t));
    break;
  case byte_Type:
    fprintf (context->file, "typedef CORBA_octet %s;\n", c_type_name (t));
    break;
  case character_Type:
    fprintf (context->file, "typedef ilu_character %s;\n", c_type_name (t));
    break;
  case shortcharacter_Type:
    fprintf (context->file, "typedef CORBA_char %s;\n", c_type_name (t));
    break;
  case alias_Type:
    generateAliasTypedef (t, context);
    break;
  case optional_Type:
    generateOptionalTypedef (t, context);
    break;
  case array_Type:
    generateArrayTypedef (t, context, FALSE);
    generateArrayTypedef (t, context, TRUE);
    break;
  case record_Type:
    generateRecordTypedef (t, context);
    break;
  case object_Type:
    break;
  case union_Type:
    generateUnionTypedef (t, context);
    break;
  case sequence_Type:
    generateSequenceTypedef (t, context);
    break;
  case enumeration_Type:
    generateEnumerationTypedefDeclaration (t, context);
    break;
  default:
    fatal ("Error: Can't cope with typedef of type %s yet.\n", c_type_name (t));
  }
}

static void generateSeqProto (Type t, Context c)
{
  enum PrimitiveTypes bt;
  int nr;
  char *rtn;
  Type seq;
  char *st;
  char *tn;

  if (type_basic_type (t) != sequence_Type)
    return;
  seq = type_description (t)->structuredDes.sequence.type;
  bt = type_basic_type (ur_type(seq));
  tn = c_type_name (t);
  st = c_parameter_type (seq, In);
  rtn = c_type_name (seq);
  nr = (bt == record_Type OR bt == union_Type OR
   (bt == sequence_Type AND NOT TypeIsEitherString(ur_type(seq))));
  if (bt == shortcharacter_Type || bt == character_Type)
    {
      /* these are a bit different */
      fprintf (c->file, "void %s_Every (%s *h, void (*f)(%s *, void *), void *);\n", tn, tn, rtn);
      fprintf (c->file, "void %s_Append (%s *, %s);\n", tn, tn, rtn);
      fprintf (c->file, "void %s_Push (%s *, %s);\n", tn, tn, rtn);
      fprintf (c->file, "void %s_Pop (%s *, %s *);\n", tn, tn, st);
      fprintf (c->file, "CORBA_unsigned_long %s_Length (%s *);\n", tn, tn);
      fprintf (c->file, "%s * %s_Nth (%s *, CORBA_unsigned_long);\n", rtn, tn, tn);
      fprintf (c->file, "%s %s_Create (CORBA_unsigned_long, %s *);\n", tn, tn, rtn);
      fprintf (c->file, "void %s_Init (%s *, CORBA_unsigned_long);\n", tn, tn);
    } else {
      fprintf (c->file, "void %s_Every (%s *h, void (*f)(%s%s, void *), void *);\n", tn, tn, st, nr ? "" : "*");
      fprintf (c->file, "void %s_Append (%s *h, %s item);\n", tn, tn, st);
      fprintf (c->file, "void %s_Push (%s *h, %s item);\n", tn, tn, st);
      fprintf (c->file, "void %s_Pop (%s *h, %s item);\n", tn, tn, st);
      fprintf (c->file, "CORBA_unsigned_long %s_Length (%s *);\n", tn, tn);
      fprintf (c->file, "%s * %s_Nth (%s *, CORBA_unsigned_long);\n", rtn, tn, tn);
      fprintf (c->file, "%s * %s_Create (CORBA_unsigned_long /* size */, %s %s /* init val */);\n", tn, tn, st, (nr) ? "" : "*");
      fprintf (c->file, "void %s_Init (%s * /* seq */, CORBA_unsigned_long /* size */, %s %s /* init val */);\n", tn, tn, st, (nr) ? "" : "*");
    }
}

static void generateNormalIncludes (Context c)
{
  fprintf (c->file, "#ifndef __ilu_c_h_\n");
  fprintf (c->file, "#include \"iluchdrs.h\"\n");
  fprintf (c->file, "#endif\n");
}

static void 
generateInputPrototype(Type type, TypeKind t, Context context)
{
  string          parm = c_parameter_type(type, InOut);
  string          rettype = c_role_type(type, role_InpRet, FALSE);
  context->class = type;
  fprintf(context->file,
	  "extern %s _%s__Input (ilu_Call, %s, ilu_Error *);\n",
	  rettype, c_type_name(type), parm);
}

static void generateFreePrototype (Type type, enum PrimitiveTypes t, Context context)
{
  fprintf(context->file, "extern void %s__Free (%s);\n",
	  c_type_name(type),
	  c_role_type(type, role_Exn, FALSE));
}

static void generateOutputPrototype (Type type, enum PrimitiveTypes t, Context context)
{
  fprintf (context->file, "extern void _%s__Output (ilu_Call, %s, ilu_Error *);\n", c_type_name(type),
	   c_parameter_type (type, In));
}

static void generateSizeOfPrototype (Type type, enum PrimitiveTypes t, Context context)
{
  fprintf (context->file, "extern ilu_cardinal _%s__SizeOf (ilu_Call, %s, ilu_Error *);\n",
	   c_type_name(type), c_parameter_type (type, In));
}

static void generateAllocHeader (Type type, enum PrimitiveTypes t, Context context)
{
  char *s = c_type_name(type);
  char *p = (type_kind(type) == array_Type) ? c_return_type(type) : c_parameter_type(type, InOut);

  fprintf (context->file, "%s %s__alloc ();\n", p, s);
}

static void generateTypeIoProto (Type type, Context context)
{
  TypeKind        t = type_basic_type(type);

  if (type->supertype != NULL
      || type->interface != context->interface
      || type->importInterfaceName != NULL)
    return;

  if (type->importInterfaceName == NULL &&
      (t == union_Type ||
       t == record_Type ||
       (t == sequence_Type && (!TypeIsString(type))) ||
       t == array_Type ||
       t == optional_Type)) {
    generateOutputPrototype(type, t, context);
    generateSizeOfPrototype(type, t, context);
    generateInputPrototype(type, t, context);
  }
  if (HasFreeRoutine(type))
    generateFreePrototype(type, t, context);
  if (HasAllocRoutine(type))
    generateAllocHeader(type, t, context);
}

static Interface SortContextInterface = NULL;

static void sortArgTypes (Argument arg, list sorted)
{
  sortTypesForDeclaration (arg->type, sorted); 
}

static void sortMethodTypes (Procedure m, list sorted)
{
  if (!m->returnOptional)
    sortTypesForDeclaration (m->returnType, sorted);
  list_enumerate (m->arguments, (iluparser_EnumProc) sortArgTypes, sorted);
}

static void sortTypesForDeclaration (Type type, list sorted)
{
  enum PrimitiveTypes t;
  static list pending = NULL;

  if (type == NULL)
    return;
  if (pending == NULL)
    pending = new_list ();
  t = type_basic_type (type);
  if (type->interface != SortContextInterface OR type->importInterfaceName != NULL)
    return;
  if (list_find (sorted, matchPointer, type) != NULL)
    return;
  if (list_find (pending, matchPointer, type) != NULL)
    {
      list_insert (sorted, type);
      list_remove (pending, type);
      return;
    }
  else {
    list_insert (pending, type);

    if (type->importInterfaceName == NULL &&
	(t == record_Type || 
	 t == alias_Type ||
	 t == optional_Type ||
	 t == array_Type || 
	 t == union_Type || 
	 t == sequence_Type || 
	 t == object_Type)) {
      switch (t) {

      case record_Type:
	list_enumerate (type_description (type)->structuredDes.record, (iluparser_EnumProc) sortArgTypes, sorted);
	break;

      case alias_Type:
	sortTypesForDeclaration (type->supertype, sorted);
	break;

      case optional_Type:
/*
	printf ("*** %s => optional %s\n", type_name(type), type_name(type_description(type)->structuredDes.optional));
	printf ("pending is\n-------------------\n");
	list_enumerate (pending, (iluparser_EnumProc) printSortedTypes, NULL);
	printf ("--------------------\n");
*/
	sortTypesForDeclaration (type_description(type)->structuredDes.optional, sorted);
	break;

      case array_Type:
	if (!type_description (type)->structuredDes.array.optional)
	  sortTypesForDeclaration (type_description (type)->structuredDes.array.type, sorted);
	break;

      case union_Type:
	list_enumerate (type_description (type)->structuredDes.uniond.types, (iluparser_EnumProc) sortArgTypes, sorted);
	break;

      case sequence_Type:
	/* normal (non-string) sequence types always have pointers */
	break;

      case object_Type:
	list_enumerate (class_object (type)->superclasses, (iluparser_EnumProc) sortTypesForDeclaration, sorted);
	list_enumerate (class_object (type)->methods, (iluparser_EnumProc) sortMethodTypes, sorted);
	break;

      default:
	break;
      }
    }
    if (list_find (sorted, matchPointer, type) == NULL)
      list_insert (sorted, type);
    list_remove (pending, type);
  }
}


void generateCHeaders (Interface interface, FILE *file)
{
  struct context_s context;
  list sorted = new_list ();
  int pad = 72 - strlen(interface_name(interface)) - 10;
  int pad1, pad2;

  context.file = file;
  context.interface = interface;
  context.class = NULL;
  fprintf (file, "#ifndef __%s_h_\n", c_interface_name (interface));
  fprintf (file, "#define __%s_h_\n\n", c_interface_name (interface));
  generateNecessaryIncludes (&context);
  generateNormalIncludes (&context);
  fprintf (file, "\n");

  fprintf (file, "/************************************************************************/\n");
  fprintf (file, "/************************************************************************/\n");
  if (pad < 0)
    {
      pad1 = 0;
      pad2 = 0;
    }
  else if (pad > 1)
    {
      pad1 = pad/2;
      pad2 = pad - pad1;
    }
  else
    {
      pad1 = 0;
      pad2 = pad - pad1;
    }
  
  fprintf (file, "/***  %*.*s%s%*.*s  ***/\n",
	   pad1, pad1, "", interface_name(interface), pad2, pad2, "");
  fprintf (file, "/************************************************************************/\n");
  fprintf (file, "/************************************************************************/\n\n");

  fprintf (file, "extern void %s__Initialize(void);\n", c_interface_name(interface));
  fprintf (file, "extern void %s__InitializeServer(void);\n\n\n", c_interface_name(interface));


  if (list_size(interface->constants) > 0)
    { fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*=========================== constants ================================*/\n\n");
      list_enumerate (interface->constants, (iluparser_EnumProc) declareConstant, &context);
      fprintf (file, "\n\n");
    }
  if (list_size(interface->classes) > 0)
    { fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*=================== object type declarations =========================*/\n\n");
      list_enumerate (interface->classes, (iluparser_EnumProc) declareClassType, &context);
      fprintf (file, "\n\n");
    }

  SortContextInterface = interface;
  list_enumerate (interface->types, (iluparser_EnumProc) sortTypesForDeclaration, sorted);
/*
  list_enumerate (sorted, (iluparser_EnumProc) printSortedTypes, NULL);
  fprintf (file, "\n");
*/
  if (list_size(sorted) > 0)
    { fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*========================= typedefs ===================================*/\n\n");
      list_enumerate (sorted, (iluparser_EnumProc) typedefType, &context);
      fprintf (file, "\n\n");

      fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*=============== non-object type declarations =========================*/\n\n");
      list_enumerate (sorted, (iluparser_EnumProc) declareType, &context);
      fprintf (file, "\n\n");
    }

  if (list_size(interface->exceptions) > 0)
    { fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*======================== exceptions ==================================*/\n\n");
      list_enumerate (interface->exceptions, (iluparser_EnumProc) declareException, &context);
      fprintf (file, "\n\n");
    }

  if (list_size(interface->classes) > 0)
    { fprintf (file, "/*======================================================================*/\n");
      fprintf (file, "/*================= object type declarations ===========================*/\n\n");
      list_enumerate (interface->classes, declareCClass, &context);
      fprintf (file, "\n\n");
    }

  fprintf (file, "/*======================================================================*/\n");
  fprintf (file, "/*================= miscellaneous internal declarations ================*/\n");
  fprintf (file, "/*======================================================================*/\n");
  fprintf (file, "/*================= Humans never have to read this stuff ===============*/\n");
  fprintf (file, "/*======================================================================*/\n\n");

  fprintf (file, "\nextern void %s__BindExceptionValue (ILU_C_ENVIRONMENT *, ilu_Exception, ...);\n", c_interface_name(interface));
  fprintf (file, "\nextern void _%s_CatchException (ilu_Call, ILU_C_ENVIRONMENT *, ilu_cardinal);\n", c_interface_name(interface));
  fprintf (file, "extern void _%s_SendException (ilu_Call, ILU_C_ENVIRONMENT *, ilu_Error *);\n", c_interface_name(interface));
  fprintf (file, "\n");
  list_enumerate (interface->types, (iluparser_EnumProc) generateTypeIoProto, &context);
  list_enumerate (interface->classes, (iluparser_EnumProc) declareCClassJunk, &context);
  fprintf (file, "\n");
  fprintf (file, "extern void _%s__GeneralInitialization(void);\n\n", c_interface_name(interface));
  fprintf (file, "#endif /* ifndef __%s_h_ */\n", c_interface_name(interface));
}

