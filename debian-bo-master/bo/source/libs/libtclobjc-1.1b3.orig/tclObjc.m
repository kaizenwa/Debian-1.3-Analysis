/* Implementation for Objective-C Tcl interpreter functions
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   With NeXT runtime compatibility incorporated by:
   Robert Stabl <stabl@informatik.uni-muenchen.de>
   Comp. Sci. Inst., U. of Munich, Leopoldstr. 11B D-80802 Muenchen

   This file is part of the Tcl/Objective-C interface library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

/*****************************************************************
  in Tcl type:
    set tclObjcDebug 1
  to see debugging information at each message send.
*******************************************************************/

/* Choose between:
   1. Each time an object is returned, it is defined as a tcl command.
   2. Messages to Objective-C objects are caught in 'unknown'.
   I think 2. is better. */
#define OBJECTS_AS_TCL_COMMANDS 0

#ifdef NeXT
#include "objc-gnu2next.h"
#include <objc/objc-class.h>
#include <objc/objc-runtime.h>
#endif

#include "tclObjc.h"
#include "objc-malloc.h"
#include <tcl.h>

#if (NeXT)

#include <objc/List.h>		/* for special case hack */

#else /* not NeXT */

#include <objc/objc-api.h>
#include <objc/encoding.h>
#include "coll/List.h"		/* for special case hack */
extern int method_get_sizeof_arguments (struct objc_method* mth);
#if (HAVE_LIBCOLL)
#include <coll/Array.h>		/* for special case hack */
#endif /* HAVE_LIBCOLL */

#endif /* NeXT */

#define XSTR(s) STR(s)
#define STR(s) #s
const char coll_version[] = XSTR(TCLOBJC_VERSION);

int (*tclObjc_eventHook)();

Tcl_Interp *_TclObject_interp;

#ifdef NeXT
static BOOL object_is_instance(id object)
{
  BOOL ret1, ret2;
  
  ret1 = (object!=nil);
  ret2 = CLS_ISCLASS(object->isa);
  return ((object!=nil) && CLS_ISCLASS(object->isa));
}
#endif

char *tclObjc_objectToName(id obj)
{
  /* Fix this messiness */
  static char name[512];
  if (obj)
    {
      sprintf(name, "%s@0x%x", obj->class_pointer->name, (unsigned)obj);
      return name;
    }
  return "nil";
}

extern char *strchr();

/* Return TCLOBJC_NO_OBJ if name is no good */
id tclObjc_nameToObject(const char *name)
{
  id object;
  unsigned long ul;
  const char *p = name;
  while (*p != '@' && *p != '\0') p++;
  if ((*p) && (sscanf(p+3, "%lx", &ul) == 1))
    {
      return (id)ul;
    }
  else if ((!strcmp(name, "nil")) 
	   || (!strcmp(name, "Nil"))
	   || (!strcmp(name, "0x0")))
    {
      return nil;
    }
  else if ((object = (id)objc_lookup_class(name)))
    {
      return object;
    }
  return TCLOBJC_NO_OBJ;
}


int tclObjc_msgSendToClientData(ClientData clientData, Tcl_Interp *interp,
				int argc, char *argv[])
{
  char resultString[1024];
  char methodName[100];
  BOOL argvIsMethodArg[256];
  id self;
  SEL sel;
  Method_t method;
  int i;

  if (argc < 2)
    {
      interp->result = "no method specified.";
      return TCL_ERROR;
    }

  argvIsMethodArg[0] = NO;
  argvIsMethodArg[1] = NO;
  strcpy(methodName, argv[1]);
  for (i = 2; i < argc; i++)
    {
      if (argv[i][strlen(argv[i])-1] == ':')
	{
	  strcat(methodName, argv[i]);
	  argvIsMethodArg[i] = NO;
	}
      else
	{
	  argvIsMethodArg[i] = YES;
	}
    }

  self = (id)clientData;

#if (HAVE_LIBCOLL)
  /* special case hack for getting Arrays of id's to tcl.
     Send the message "contents" to an Array object.
   */
  if (!strcmp("contents", methodName) 
      && [self isKindOf:[Array class]]
      && !strcmp("@",[self contentEncoding]))
    {
      int i;

      Tcl_ResetResult(interp);
      for (i = 0; i < [self count]; i++)
	{
	  Tcl_AppendElement(interp, tclObjc_objectToName([self elementAtIndex:i].id_u));
	}
      return TCL_OK;
    }
#endif /* HAVE_LIBCOLL */

  /* special case hack for getting List contents to tcl.
     Send the message "contents" to a List object.
   */
  if (!strcmp("contents", methodName) 
      && [self isKindOf:[List class]])
    {
      int i;

      Tcl_ResetResult(interp);
      for (i = 0; i < [self count]; i++)
	{
	  Tcl_AppendElement(interp, tclObjc_objectToName([self objectAt:i]));
	}
      return TCL_OK;
    }

  /* special case hack for sending message to a TclObject */
  if (self->class_pointer == [TclObject class])
    {
      static Tcl_DString command;
      static char *cmd;
      int i;
      int code;

      Tcl_DStringInit(&command);
      Tcl_DStringAppend(&command, ((TclObject*)self)->_tclName, -1);
      Tcl_DStringAppend(&command, " ", -1);
      Tcl_DStringAppend(&command, methodName, -1);
      for (i = 2; i < argc; i++)
	{
	  if (argvIsMethodArg[i]) continue;
	  Tcl_DStringAppendElement(&command, argv[i]);
	  Tcl_DStringAppend(&command, " ", -1);
	}
      cmd = Tcl_DStringAppend(&command, "\n", -1);
      if (!(((TclObject*)self)->_interp))
	{
	  fprintf(stderr, "TclObject interp not yet set\n");
	  return TCL_ERROR;
	}
      code = Tcl_Eval(((TclObject*)self)->_interp, cmd);
      if (code != TCL_OK)
	{
	  char *msg;
	  msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
	  if (msg == NULL) {
	    msg = interp->result;
	  }
	  fprintf(stderr, "(tclObjc: messaging a TclObject:) %s\n", msg);
	  fprintf(stderr, "while evaluating: %s\n", cmd);
	}
      Tcl_DStringFree(&command);
      return code;
    }

  sel = sel_get_uid(methodName);
  if (![self respondsTo:sel])
    {
      printf("%s does not respond to method %s\n", 
	     [self name], methodName);
      Tcl_SetResult(interp, "object does not respond to method", 
		    TCL_STATIC);    
      return TCL_ERROR;
    }

  if (object_is_instance(self))
    method = class_get_instance_method(self->class_pointer, sel);
  else
    method = class_get_class_method(self->class_pointer, sel);

  if (!method)
    {
      printf("class %s doesn't have method %s\n", 
	     self->class_pointer->name, methodName);
      Tcl_SetResult(interp, "method is NULL", TCL_STATIC); 
      return TCL_ERROR;
    }


  {
#ifdef NeXT
    arglist_t argframe;
    int datum;
    int realnum;
    id retframe;
#else
    arglist_t argframe = __builtin_apply_args();
    char *datum;
    void *retframe;
#endif
    int argsize = method_get_sizeof_arguments(method);
    char argptr_buffer[argsize];
    unsigned int bargframe = (unsigned int)argptr_buffer;
    int argnum;
    const char *type;
    int tmpint;
    int tmpuint;
    char *objcdebug;
    BOOL debug_printing;
#if OBJECTS_AS_TCL_COMMANDS
    Tcl_CmdInfo cmdInfo;
#endif

#ifdef NeXT
    argframe = argptr_buffer;
#else
    argframe->arg_ptr = argptr_buffer;
#endif

    objcdebug = Tcl_GetVar(interp, "tclObjcDebug", TCL_GLOBAL_ONLY);
    if (objcdebug) 
      debug_printing = YES;
    else 
      debug_printing = NO;
    /* Perhaps later we could add different levels of debugging 
       depending on the contents of objcdebug */

    if (debug_printing)
      printf("c %d, name %s, argsize %d, method types: '%s'\n", 
	     argc, methodName, argsize, method->method_types);
    
#ifdef NeXT
	method_getArgumentInfo(method, 0, &type, &datum);
	*(marg_getRef(argframe, datum, id)) = self;
	method_getArgumentInfo(method, 1, &type, &datum);
	*(marg_getRef(argframe, datum, SEL)) = sel;
	type = (char *)1;
	realnum = 1;
#else
    datum = method_get_first_argument(method, argframe, &type);
    *(id*)datum = self;
    datum = method_get_next_argument(argframe, &type);
    *(SEL*)datum = sel;
#endif

#ifdef NeXT
    for (argnum = 2; argnum < argc && type; argnum++)
#else
    for (argnum = 2,
	 datum = method_get_next_argument(argframe, &type);
	 datum;
	 datum = method_get_next_argument(argframe, &type),
	 ({argnum++; while (datum && !argvIsMethodArg[argnum]) argnum++;}))
#endif
      {
#ifdef NeXT
	if (!argvIsMethodArg[argnum])
		continue;
	realnum++;
	method_getArgumentInfo(method, realnum, &type, &datum);
	if (!type || !datum)
		break;
	if (debug_printing)
	{
		fprintf(stderr, "datum=%x type=%s\n", 
			(unsigned int)
				(marg_getRef(argframe, datum, id))-bargframe,
			type);
		fprintf(stderr, "argv[%d] = %s type=%s\n", realnum, 
			argv[argnum], type);
	}
#else /* NeXT */

#define marg_getRef(margs, offset, type) ( (type *)offset )

	unsigned flags = objc_get_type_qualifiers(type);
	type = objc_skip_type_qualifiers(type);
	flags = flags;
	if (debug_printing)
	  {
	    printf("datum=%x type=%s\n", 
		   (unsigned int)datum - bargframe, type);
	    printf("argv[%d] = %s type=%s\n", argnum, argv[argnum], type);
	  }
#endif /* NeXT */
	switch (*type)
	  {
#ifdef NeXT
	  case _C_SEL:
		realnum--;
		break;
#endif /* NeXT */
	  case _C_ID:
	    *(marg_getRef(argframe, datum, id)) = 
	    	tclObjc_nameToObject(argv[argnum]);
	    if (*(marg_getRef(argframe, datum, id)) == TCLOBJC_NO_OBJ)
	      {
		sprintf(interp->result, 
			"Expected objc object, got %s instead.\n", 
			argv[argnum]);
		return TCL_ERROR;
	      }
	    break;
	  case _C_PTR:
	    sscanf(argv[argnum], "0x%x", 
			(marg_getRef(argframe, datum, unsigned int)));
	    break;
	  case _C_INT:
	    sscanf(argv[argnum], "%d", 
			(marg_getRef(argframe, datum, int)));
	    break;
	  case _C_UINT:
	    sscanf(argv[argnum], "%u",  
			(marg_getRef(argframe, datum, unsigned int)));
	    break;
	  case _C_LNG:
	    sscanf(argv[argnum], "%ld", 
			(marg_getRef(argframe, datum, long)));
	    break;
	  case _C_ULNG:
	    sscanf(argv[argnum], "%lu",
			(marg_getRef(argframe, datum, unsigned long)));
	    break;
	  case _C_SHT:
	    sscanf(argv[argnum], "%d", &tmpint);
	    *(marg_getRef(argframe, datum, short)) = (short)tmpint;
	    break;
	  case _C_USHT:
	    sscanf(argv[argnum], "%u", &tmpuint);
	    *(marg_getRef(argframe, datum, unsigned short)) = 
	    	(unsigned short)tmpuint;
	    break;
	  case _C_CHR:
	    sscanf(argv[argnum], "%c",
	    		(marg_getRef(argframe, datum, char)));
	    break;
	  case _C_UCHR:
	    sscanf(argv[argnum], "%d", &tmpuint);
	    *(marg_getRef(argframe, datum, unsigned char)) = 
	    	(unsigned char)tmpuint;
	    break;
	  case _C_CHARPTR:
	    *(marg_getRef(argframe, datum, char*)) = argv[argnum];	    
	    break;
	  case _C_FLT:
	    sscanf(argv[argnum], "%f",
	    		(marg_getRef(argframe, datum, float)));
	    break;
	  case  _C_DBL:
	    sscanf(argv[argnum], "%lf",
	    		(marg_getRef(argframe, datum, double)));
	    break;
	  default:
	    {
	      fprintf(stderr, "Tcl can't handle arg type %s", type);
	      sprintf(resultString, "Tcl can't handle arg type %s", type);
	      Tcl_SetResult(interp, resultString, TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	  }
      }

#ifdef NeXT
    retframe = 
    	objc_msgSendv(self, sel, method_getSizeOfArguments(method), 
				argframe);
# define GET_RETVAL(TYPE,VAL) ((TYPE)VAL)

#else
    retframe = __builtin_apply((apply_t)method->method_imp, 
			       (void*)argframe, 
			       argsize);

# define GET_RETVAL(TYPE,VAL) ({                      \
  TYPE __r (void* __rv) { __builtin_return (__rv); } \
  __r (VAL); })

#endif

    if (debug_printing)
      {
	printf("retframe unsigned int 0x%x\n", 
	       GET_RETVAL(unsigned int,retframe));
#if 0
	printf("retframe retfloat %g\n", 
	       *GET_RETVAL(float*, retframe));
	printf("retframe retdouble %g\n", 
	       *GET_RETVAL(double*, retframe));
#endif
      }
    type = method->method_types;
    switch (*type)
      {
      case _C_ID:
	sprintf(resultString, tclObjc_objectToName(GET_RETVAL(id, retframe)));
#if OBJECTS_AS_TCL_COMMANDS
	if (!Tcl_GetCommandInfo(interp, resultString, &cmdInfo))
	  Tcl_CreateCommand(interp, resultString, tclObjc_msgSendToClientData,
			    *(id*)retframe, 0);
#else /* messages caught and forwarded by tcl proc "unknown" */
#endif
	break;
      case _C_PTR:
	sprintf(resultString, "0x%x", (unsigned)GET_RETVAL(void*, retframe));
	break;
      case _C_INT:
	sprintf(resultString, "%d", GET_RETVAL(int, retframe));
	break;
      case _C_UINT:
	sprintf(resultString, "%u", GET_RETVAL(unsigned int, retframe));
	break;
      case _C_SHT:
	sprintf(resultString, "%d", (short)GET_RETVAL(short, retframe));
	break;
      case _C_USHT:
	sprintf(resultString, "%u", (unsigned short)GET_RETVAL(unsigned short, retframe));
	break;
      case _C_LNG:
	sprintf(resultString, "%ld", GET_RETVAL(long, retframe));
	break;
      case _C_ULNG:
	sprintf(resultString, "%lx", GET_RETVAL(unsigned long, retframe));
	break;
      case _C_CHR:
	sprintf(resultString, "%d", (char)GET_RETVAL(char, retframe));
	break;
      case _C_UCHR:
	sprintf(resultString, "%d", (unsigned char)GET_RETVAL(unsigned char, retframe));
	break;
      case _C_CHARPTR:
        /* Yuck.  Clean this up. */
        Tcl_SetResult(interp, GET_RETVAL(char*,retframe), TCL_VOLATILE);
        return TCL_OK;
      case _C_FLT:
	sprintf(resultString, "%g", *GET_RETVAL(float*, retframe));
	break;
      case _C_DBL:
	sprintf(resultString, "%g", *GET_RETVAL(double*, retframe));
	break;
      case _C_VOID:
        resultString[0] = '\0';
        break;
      default:
	{
	  fprintf(stderr, "Tcl can't handle ret type %s", type);
	  sprintf(resultString, "Tcl can't handle ret type %s", type);
	  Tcl_SetResult(interp, resultString, TCL_VOLATILE);
	  return TCL_ERROR;
	}
      }

    Tcl_SetResult(interp, resultString, TCL_VOLATILE);

    if (*tclObjc_eventHook)
      (*tclObjc_eventHook)();
    return TCL_OK;
    
  }

}

void tclObjc_registerObjectWithName(Tcl_Interp *interp, 
				    id object, const char *name)
{
  Tcl_CreateCommand(interp, (char *)name, tclObjc_msgSendToClientData,
		    object, 0);
}

void tclObjc_unregisterObjectNamed(Tcl_Interp *interp,
				   const char *name)
{
  Tcl_DeleteCommand(interp, (char *)name);
}

void tclObjc_registerClassnames(Tcl_Interp *interp)
{
#ifdef NeXT
 Class thisClass;
 NXHashTable *classes = objc_getClasses();
 NXHashState  state = NXInitHashState(classes); 

 while (NXNextHashState(classes, &state, (void **)&thisClass))
 {
  tclObjc_registerObjectWithName(interp, thisClass, 
				 object_getClassName((id)thisClass));
 }
#else
  id class; 
  void *es = NULL;
  while ((class = objc_next_class(&es)))
    tclObjc_registerObjectWithName(interp, class, [class name]);
#if 0
  node_ptr node = NULL;

  /* register all class names with tcl */
  while ((node = hash_next(__objc_class_hash, node)))
    {
      //      printf("registering %s\n", (char *)node->key);
      tclObjc_registerObjectWithName(interp, node->value, node->key);
    }
#endif
#endif
}


int tclObjc_msgSendToArgv1(ClientData clientData, Tcl_Interp *interp,
			   int argc, char *argv[])
{
  id obj;

  if ((obj = tclObjc_nameToObject(argv[1])) != TCLOBJC_NO_OBJ)
    {
      return tclObjc_msgSendToClientData((ClientData)obj, interp, 
					argc-1, &(argv[1]));
    }
  else
    {
      sprintf(interp->result, 
	      "tclObjc: %s not recognized as an object", argv[1]);
      return TCL_ERROR;
    }
}


@implementation TclObject

+ newName: (char *)objectName
{
  TclObject *newTclObject = class_create_instance(self);
  OBJC_MALLOC(newTclObject->_tclName, char, strlen(objectName)+1);
  strcpy(newTclObject->_tclName, objectName);
  /* Fix this ugliness!!! */
  newTclObject->_interp = _TclObject_interp;
  return newTclObject;
}

- free
{
  OBJC_FREE(_tclName);
  return object_dispose(self);
}  

- (BOOL) respondsTo: (SEL)aSel
{
  Tcl_CmdInfo cmdInfo;
  char selString[128];
  sprintf(selString, "%s%s", _tclName, sel_get_name(aSel));
  return (((object_is_instance(self)
           ?class_get_instance_method(self->ISA, aSel)
	    :class_get_class_method(self->ISA, aSel))!=METHOD_NULL)
	  || Tcl_GetCommandInfo(_interp, selString, &cmdInfo));
}

- forward: (SEL)aSel : (arglist_t)argframe
{
  return [self performv: aSel :argframe];
}


- performv:(SEL)aSel :(arglist_t)argframe
{
#ifdef NeXT
  int datum;
  int argnum;
#else
  char *datum;
#endif
  const char *type;
  char *objcdebug;
  BOOL debug_printing;
  Method_t method = 0;
  char argString[256];
  Tcl_DString command;
  char *cmd;
  int tmpint;
  unsigned int tmpuint;

  if (_interp == NULL)
    {
      fprintf(stderr, "interp not set yet, %s\n", sel_get_name(aSel));
      return self;
    }

  objcdebug = Tcl_GetVar(_interp, "objcdebug", TCL_GLOBAL_ONLY);
  if (objcdebug) 
    debug_printing = YES;
  else 
    debug_printing = NO;

  Tcl_DStringInit(&command);
  Tcl_DStringAppend(&command, _tclName, -1);
  //  Tcl_DStringAppend(&command, " ", -1);
  Tcl_DStringAppend(&command, (char *)sel_get_name(aSel), -1);
  Tcl_DStringAppend(&command, " ", -1);

  if (debug_printing)
    printf("selector: %s\n", sel_get_name(aSel));

  /* search all classes for the method */
#ifdef NeXT
  {
   Class thisClass;
   NXHashTable *classes = objc_getClasses();
   NXHashState  state = NXInitHashState(classes); 

   while (NXNextHashState(classes, &state, (void **)&thisClass)
   	  && (!(method = 
	  	class_getInstanceMethod(object_getClassName((id)thisClass), 
					aSel)))
	  && (!(method = 
		class_getClassMethod(object_getClassName((id)thisClass), 
				     aSel))))
   {
	;
   }
  }
#else
  {
    id class;
    void *es = NULL;

    while ((class = objc_next_class(&es))
	   && (!(method = class_get_instance_method(class, aSel)))
	   && (!(method = class_get_class_method(class, aSel))))
      ;
  }
#endif
#if 0
  {
    node_ptr node = NULL;
    while ((node = hash_next(__objc_class_hash, node))
           && (!(method = class_get_instance_method(node->value, aSel)))
           && (!(method = class_get_class_method(node->value, aSel))))
      ;
  }
#endif
  if (!method)
    {
      fprintf(stderr, "method not found, %s\n", sel_get_name(aSel));
      return self;
    }

#ifdef NeXT
  for ( argnum = 2,
	method_getArgumentInfo(method, argnum, &type, &datum);
	type;
	method_getArgumentInfo(method, argnum, &type, &datum))
#else
  /* self */
  datum = method_get_first_argument(method, argframe, &type);
  /* SEL */
  datum = method_get_next_argument(argframe, &type);
  for (datum = method_get_next_argument(argframe, &type);
       datum;
       datum = method_get_next_argument(argframe, &type))
#endif
    {
#ifndef NeXT
      unsigned flags = objc_get_type_qualifiers(type);
      type = objc_skip_type_qualifiers(type);
      flags = flags;
#endif
      switch (*type)
	{
	case _C_PTR:
	  sprintf(argString, "0x%x", 
	  	*(unsigned int*)(marg_getRef(argframe, datum, unsigned int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_ID:
	  strcpy(argString, tclObjc_objectToName(
	  	*(id*)(marg_getRef(argframe, datum, id))));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_INT:
	  sprintf(argString, "%d", 
	  	*(int*)(marg_getRef(argframe, datum, int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_UINT:
	  sprintf(argString, "%u", 
	  	*(unsigned int*)(marg_getRef(argframe, datum, unsigned int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_SHT:
	  tmpint = 
	  	*(short*)(marg_getRef(argframe, datum, short));
	  sprintf(argString, "%d", tmpint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_USHT:
	  tmpuint = 
	  	*(unsigned short*)(marg_getRef(argframe, datum, unsigned short));
	  sprintf(argString, "%u", tmpuint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_CHR:
	  sprintf(argString, "%c", 
	  	*(char*)(marg_getRef(argframe, datum, char)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_UCHR:
	  tmpuint = 
	  	*(unsigned char*)(marg_getRef(argframe, datum, unsigned char));
	  sprintf(argString, "%u", tmpuint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_CHARPTR:
	  Tcl_DStringAppendElement(&command, 
	  	*(char**)(marg_getRef(argframe, datum, char *)));
	  break;
	case _C_FLT:
	  sprintf(argString, "%f", 
	  	*(float*)(marg_getRef(argframe, datum, float)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case  _C_DBL:
	  sprintf(argString, "%f", 
	  	*(double*)(marg_getRef(argframe, datum, double)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	default:
	  {
	    fprintf(stderr, "TclObject can't handle arg type %s", type);
	    return self;
	  }
	}
    }
  cmd = Tcl_DStringAppend(&command, "\n", -1);
  Tcl_GlobalEval(_interp, cmd);
  // I should interpret returned string and return it!;
  return self;
}


@end


/*****************************************************************/

static char tclObjcInitCmd[] =
"if {[llength [info procs unknown]]} { \n\
   rename unknown unknown_pre_tclObjc \n\
 } \n\
 proc unknown {name args} {\n\
   if {[string match *@0x* $name]} {\n\
     return [uplevel tclObjc_msg_send $name $args]\n\
   } else {\n\
     if {[llength [info procs unknown_pre_tclObjc]]} {\n\
       unknown_pre_tclObjc $name $args\n\
     } else {\n\
       error \"in unknown: invalid command name: $name\"\n\
     }\n\
   }\n\
 }\n";


int TclObjc_Init(Tcl_Interp *interp)
{
#if ! OBJECTS_AS_TCL_COMMANDS
  int code;
#endif

  /* Fix this ugliness!!! */
  _TclObject_interp = interp;
  tclObjc_registerClassnames(interp);
  Tcl_CreateCommand(interp, "tclObjc_msg_send", 
		    tclObjc_msgSendToArgv1, 0, 0);
#if ! OBJECTS_AS_TCL_COMMANDS
  code = Tcl_Eval(interp, tclObjcInitCmd);
  if (code != TCL_OK)
    {
      fprintf(stderr, "tclObjc: Error during TclObjc_Init:\n");
      fprintf(stderr, interp->result);
    }
#endif
  return TCL_OK;
}

