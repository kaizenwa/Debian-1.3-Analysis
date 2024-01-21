/*
** Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.
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
/* $Id: common.c,v 1.58 1996/06/20 17:57:25 spreitze Exp $ */
/* Last edited by Mike Spreitzer June 20, 1996 10:25 am PDT */
 

#include "cstubber.h"

static Interface	CurrentInterface = NULL;
static list 		InterfacesToInclude = NULL;
cardinal 		MethodRecordID;

boolean IsCacheableMethod (Procedure m)
{
    enum PrimitiveTypes         t = type_ur_kind (m->returnType);
 
    return (m->functional && list_size (m->arguments) == 0
          &&  (t == object_Type || t == enumeration_Type || t == byte_Type
              || t == integer_Type || t == shortinteger_Type
              || t == cardinal_Type || t == shortcardinal_Type
              || t == real_Type || t == shortreal_Type
              || t == character_Type || t == shortcharacter_Type));
}
 
Interface OriginalInterface (Type type)
{
  return (ur_type(type)->interface);
}

Type UltimateType (Type type)
{
  return (ur_type(type));
}

static boolean FindVarField (Argument field)
{
  return (VariableLength(field->type));
}

static boolean HasVarField (list fields)
{
  return (list_find(fields, (iluparser_FindProc) FindVarField, NULL) != NULL);
}

boolean VariableLength (Type type)
{
  TypeKind t = type_ur_kind(type);
  TypeDescription d = type_description(type);

  return (t == sequence_Type ||
	  (t == array_Type && VariableLength(d->structuredDes.array.type)) ||
	  (t == record_Type && HasVarField(d->structuredDes.record)) ||
	  (t == union_Type && HasVarField(d->structuredDes.uniond.types)));
}

boolean IsSequenceObject(
    Type 	type,
    Interface 	i)
{
    return ((i == NULL || type->interface == i)
	  && type->importInterfaceName == NULL
	  && type_ur_kind(type) == sequence_Type
	  &&  (type_ur_kind (type_description (type)->
		structuredDes.sequence.type) != shortcharacter_Type)
	  &&  (type_ur_kind (type_description (type)->
		structuredDes.sequence.type) != byte_Type));
}

boolean IsPipeObject(
    Type 	type,
    Interface 	i)
{
    return ((i == NULL || type->interface == i)
	  && type->importInterfaceName == NULL
	  && type_ur_kind (type) == pipe_Type);
}

boolean PassedByRef (Type type)
{
    enum PrimitiveTypes t;

    if (type == NULL)
        return (False);
    t = type_ur_kind (type);
    return  (t == record_Type
	  || t == union_Type
	  || t == array_Type
	  || t == object_Type
	  || t == pipe_Type
	  || t == sequence_Type
	  || t == longcardinal_Type
	  || t == longinteger_Type);
}

boolean TypeIsNonObjectStruct(Type type)
{
  TypeKind        t;
  if (type == NULL)
    return (False);
  t = type_ur_kind(type);
  return (t == record_Type || t == union_Type);
}

boolean TypeIsArray(Type type)
{
  if (type == NULL)
    return (False);
  return (type_ur_kind(type) == array_Type);
}

boolean TypeIsString(Type type)
{
  Type            u = ur_type(type);
  return (type_kind(u) == sequence_Type &&
	  (type_ur_kind(type_description(u)
			->structuredDes.sequence.type)
	   == shortcharacter_Type));
}

boolean TypeIsWString(Type type)
{
  Type            u = ur_type(type);
  return (type_kind(u) == sequence_Type &&
	  (type_ur_kind(type_description(u)
			->structuredDes.sequence.type)
	   == character_Type));
}

boolean TypeIsEitherString(Type type)
{
  Type            u = ur_type(type);
  TypeKind        k;
  return (type_kind(u) == sequence_Type &&
	  (k = type_ur_kind(type_description(u)
			    ->structuredDes.sequence.type),
	   k == character_Type || k == shortcharacter_Type));
}

boolean NonStringSequence (Type type)
{
  Type            u = ur_type(type);
  TypeKind        k;
  return (type_kind(u) == sequence_Type && !
	  (k = type_ur_kind(type_description(u)
			    ->structuredDes.sequence.type),
	   k == character_Type || k == shortcharacter_Type));
}

boolean TypeIsPointer(Type type)
{
  return (type_ur_kind(type) == object_Type || TypeIsEitherString(type));
}

boolean HasFreeRoutine (Type type)
{
  TypeKind        t = type_ur_kind(type);

  return (type->importInterfaceName == NULL AND
	  (t == union_Type || t == record_Type ||
	   t == optional_Type || t == object_Type ||
	   t == sequence_Type || t == array_Type));
}

boolean HasAllocRoutine (Type type)
{
  enum PrimitiveTypes t = type_ur_kind(type);

  return (type->importInterfaceName == NULL AND
	  (t == union_Type OR
	   t == record_Type OR
	   t == optional_Type OR
	   t == sequence_Type OR
	   t == array_Type)); 
}

void SizeValue (Context context, Type type, string name)
{
  TypeDescription d = type_description(type);
  enum PrimitiveTypes t = type_basic_type(type);
  FILE           *f = context->file;

  if (t == integer_Type)
    fprintf(f, "ilu_SizeOfInteger(_call, %s, _err)", name);
  else if (t == enumeration_Type)
    fprintf(f, "ilu_SizeOfEnum(_call, (ilu_shortcardinal) %s, _err)",
	    name);
  else if (t == cardinal_Type)
    fprintf(f, "ilu_SizeOfCardinal(_call, %s, _err)", name);
  else if (t == shortinteger_Type)
    fprintf(f, "ilu_SizeOfShortInteger(_call, %s, _err)", name);
  else if (t == shortcardinal_Type)
    fprintf(f, "ilu_SizeOfShortCardinal(_call, %s, _err)", name);
  else if (t == longinteger_Type)
    fprintf(f, "ilu_SizeOfLongInteger(_call, %s, _err)", name);
  else if (t == boolean_Type)
    fprintf(f, "ILU_C_SizeOfBoolean(_call, %s, _err)", name);
  else if (t == longcardinal_Type)
    fprintf(f, "ilu_SizeOfLongCardinal(_call, %s, _err)", name);
  else if (t == character_Type)
    fprintf(f, "ilu_SizeOfCharacter(_call, %s, _err)", name);
  else if (t == shortcharacter_Type)
    fprintf(f, "ilu_SizeOfShortCharacter(_call, %s, _err)", name);
  else if (t == real_Type)
    fprintf(f, "ilu_SizeOfReal(_call, %s, _err)", name);
  else if (t == shortreal_Type)
    fprintf(f, "ilu_SizeOfShortReal(_call, %s, _err)", name);
  else if (t == longreal_Type)
    fprintf(f, "ilu_SizeOfLongReal(_call, %s, _err)", name);
  else if (t == byte_Type)
    fprintf(f, "ilu_SizeOfByte(_call, %s, _err)", name);
  else if (t == alias_Type)
    SizeValue(context, ur_type(type), name);
  else if (TypeIsString(type))
    fprintf(f, "_ilu_CString__SizeOf(_call, %s, _err)",
	    name);
  else if (t == record_Type ||
	   t == optional_Type ||
	   t == union_Type ||
	   t == sequence_Type ||
	   t == array_Type)
    fprintf(f, "_%s__SizeOf (_call, %s%s, _err)",
	    c_type_name(type),
	    (t == record_Type || t == union_Type ||
	     NonStringSequence(type)) ? "&" : "",
	    name);
  else if (t == object_Type)
    fprintf(f,
	    "_ILU_C_SizeOfObject(_call, (%s) %s, _%s__ILUType, ilu_FALSE, _err)",
	    c_return_type(type), name, c_type_name(type));
  else if (t == void_Type)
    fprintf(f, "0");
  else
    fatal("Error:  Can't figure size of argument of type %s (line %d) yet.\n",
	  type_name(type), type->def);
}

void MarshallValue (Context context, Type type, string name, int indent)
{
  enum PrimitiveTypes t = type_basic_type(type);
  TypeDescription d = type_description(type);
  FILE           *f = context->file;

  fprintf(f, "%*.*s", indent, indent, "");

  if (t == integer_Type)
    fprintf(f, "ilu_OutputInteger (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == enumeration_Type)
    fprintf(f, "ilu_OutputEnum (_call, (ilu_shortcardinal) %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    name);
  else if (t == cardinal_Type)
    fprintf(f, "ilu_OutputCardinal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == shortinteger_Type)
    fprintf(f, "ilu_OutputShortInteger (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == shortcardinal_Type)
    fprintf(f, "ilu_OutputShortCardinal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == boolean_Type)
    fprintf(f, "ILU_C_OutputBoolean(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == longinteger_Type)
    fprintf(f, "ilu_OutputLongInteger(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == longcardinal_Type)
    fprintf(f, "ilu_OutputLongCardinal(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == character_Type)
    fprintf(f, "ilu_OutputCharacter(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == shortcharacter_Type)
    fprintf(f, "ilu_OutputShortCharacter(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == real_Type)
    fprintf(f, "ilu_OutputReal(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == shortreal_Type)
    fprintf(f, "ilu_OutputShortReal(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == longreal_Type)
    fprintf(f, "ilu_OutputLongReal(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == byte_Type)
    fprintf(f, "ilu_OutputByte(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", name);
  else if (t == alias_Type)
    MarshallValue(context, ur_type(type), name, 0);
  else if (TypeIsString(type))
    fprintf(f, "_ilu_CString__Output (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    name);
  else if (t == record_Type OR
	   t == union_Type OR
	   t == sequence_Type OR
	   t == optional_Type OR
	   t == array_Type)
    fprintf(f, "_%s__Output(_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    c_type_name(type), name);
  else if (t == object_Type)
    fprintf(f,
	    "_ILU_C_OutputObject(_call, (%s) %s, _%s__ILUType, ilu_FALSE, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    c_type_name(type), name, c_type_name(type));
  else if (t == void_Type);
  else
    fatal("Error:  Can't cope with argument of type %s yet.\n",
	  c_type_name(type));
}

void encodeArgument(Argument arg, Context context)
{
  char b[1000];
  enum PrimitiveTypes t = type_basic_type (ur_type(arg->type));

  if (arg->direction == Out)
    return;
  b[ 0 ] = 0;
  b[ 1 ] = 0;
  if (arg->direction == InOut) {
    if ( (t != array_Type) && 
	(t != sequence_Type) && 
	(t != record_Type) && 
	(t != union_Type)) {
      strcat (b, "*");
    }
  }
  strcat (b, (char *) c_argument_name (arg));
  MarshallValue (context, ur_type(arg->type), b, 2);
}

void UnmarshallValue (Context	 context,
		      Type	 type,
		      LineNumber line,
		      string	 val,
		      int	 indent)
{
  TypeKind        t = type_basic_type(type);
  TypeDescription d = type_description(type);
  FILE           *f = context->file;

  if (indent > 0)
    fprintf(f, "%*.*s", indent, indent, "");

  if (t == integer_Type)
    fprintf(f, "ilu_InputInteger (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == cardinal_Type)
    fprintf(f, "ilu_InputCardinal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == shortinteger_Type)
    fprintf(f, "ilu_InputShortInteger (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == shortcardinal_Type)
    fprintf(f, "ilu_InputShortCardinal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == boolean_Type)
    fprintf(f, "ILU_C_InputBoolean (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == longinteger_Type)
    fprintf(f, "ilu_InputLongInteger (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == longcardinal_Type)
    fprintf(f, "ilu_InputLongCardinal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == character_Type)
    fprintf(f, "ilu_InputCharacter (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == shortcharacter_Type)
    fprintf(f, "ilu_InputShortCharacter (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == real_Type)
    fprintf(f, "ilu_InputReal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == shortreal_Type)
    fprintf(f, "ilu_InputShortReal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == longreal_Type)
    fprintf(f, "ilu_InputLongReal (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == byte_Type)
    fprintf(f, "ilu_InputByte (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n", val);
  else if (t == alias_Type)
    UnmarshallValue(context, ur_type(type), line, val, 0);
  else if (t == enumeration_Type) {
    fprintf(f, "{\n%*.*silu_shortcardinal _xxx;\n",
	    indent + 2, indent + 2, "");
    fprintf(f, "%*.*silu_InputEnum (_call, &_xxx, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    indent + 2, indent + 2, "");
    fprintf(f, "%*.*s*%s = (%s) _xxx;\n%*.*s};\n",
	    indent + 2, indent + 2, "",
	    val, c_type_name(type), indent, indent, "");
  } else if (TypeIsString(type)) {
    fprintf(f,
	"(void) _ilu_CString__Input (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
    /* cast because some of sign fuckups */
	    val, c_type_name(type));
  } else if (t == array_Type ||
	     t == sequence_Type ||
	     t == optional_Type ||
	     t == record_Type ||
	     t == union_Type) {
    fprintf(f, "(void) _%s__Input (_call, %s, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    c_type_name(ur_type(type)), val);
  } else if (t == object_Type)
    fprintf(f,
      "*%s = _ILU_C_InputObject (_call, _%s__ILUType, 0, _err); if (ILU_ERRNOK(*_err)) goto marshalError;\n",
	    val, c_type_name(type));
  else if (t == void_Type);
  else
    fatal("Error:  Can't cope with procedure of type %s (line %d) yet.\n",
	  c_type_name(type), line);
}

boolean BlockingCall (Procedure proc)
{
  return (!(proc->asynch)
	  || type_basic_type(proc->returnType) != void_Type
	  || list_size(proc->exceptions) > 0);
}

static boolean matchString(
    char 	*s1, 
    char	*s2)
{
    return (strcmp (s1,s2) == 0);
}

static void PrintInclude(
    char 	*interfacename,
    Context 	context)
{
 /* get any translation of what the header file for the interface is */
 interfacename = interface_header_name(interfacename);

 fprintf (context->file, "#include \"%s.h\"\n", interfacename);
}


static void listIncludes(
    Imported 	i,
    list 	l)
{
    Interface 	imported;

    if ((imported = GetInterface (i->name, i->filename)) != NULL
	&& imported != CurrentInterface
	&& (strcmp(i->name, "ilu") != 0)
	&& (list_find (l, (iluparser_FindProc) matchString, c_simple_name(imported->name)) == NULL))
      list_insert (l, c_simple_name (imported->name));    
}

void generateNecessaryIncludes(
    Context 	context)
{
  if (InterfacesToInclude == NULL)
    InterfacesToInclude = (list) new_list();
  list_clear (InterfacesToInclude, FALSE);
  CurrentInterface = context->interface;
  list_enumerate (context->interface->imports, (void (*)(refany, refany)) listIncludes, InterfacesToInclude);
  list_enumerate (InterfacesToInclude, (void (*)(refany, refany)) PrintInclude, context);
}

