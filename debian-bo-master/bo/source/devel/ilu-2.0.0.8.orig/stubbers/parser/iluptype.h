/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

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

$Id: iluptype.h,v 1.55 1996/07/03 02:19:24 janssen Exp $
*/
/* Last edited by Mike Spreitzer November 10, 1994 4:27 pm PST */

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#else
#include <iluconf.h>
#endif

typedef int boolean;
#define TRUE 1
#define FALSE 0

#define ILU_NIL	((void *) 0)
#define NIL ILU_NIL
#define NULLFN 0
#define NULLCH 0

typedef unsigned long cardinal;

typedef char * string;
typedef unsigned char * bytes;
typedef unsigned char byte;
typedef char character;
typedef void *refany;

typedef struct list_element_s {
  refany data;
  struct list_element_s *next;
} listElement;

typedef struct list_s {
  listElement *head;
  listElement *tail;
  cardinal count;
} *list;

typedef list set;

typedef struct usagename_s {
  char *lang;
  char *name;
} * usagename;

typedef struct name_s {
  char *base_name;
  set	langnames;	/* set of usagename pairs */
} * Name;

typedef enum {In, Out, InOut} ArgDirection;

/***********************************************************************
  Types needed by ILU parser

***********************************************************************/

typedef struct ilu_interface_s	*	Interface;
typedef struct ilu_type_s	*	Type;
typedef struct ilu_proc_s	*	Procedure;
typedef struct ilu_argument_s	*	Argument;
typedef struct ilu_exception_s	*	Exception;
typedef struct ilu_typedes_s	*	TypeDescription;
typedef struct ilu_imported_s	*	Imported;
typedef int				ProtocolId;
typedef struct enumerationField_s *	EnumField;
typedef cardinal			LineNumber;
typedef struct ilu_constant_s	*	Constant;
typedef struct ilu_constantvalue_s *	ConstantValue;
typedef struct ilu_class_s	*	Class;

enum PrimitiveTypes {
  invalid_Type, void_Type,
  byte_Type, boolean_Type,
  character_Type, shortcharacter_Type,
  shortinteger_Type, integer_Type, longinteger_Type,
  shortcardinal_Type, cardinal_Type, longcardinal_Type,
  real_Type, shortreal_Type, longreal_Type,
  object_Type, pipe_Type, optional_Type, alias_Type,
  union_Type, sequence_Type, record_Type, array_Type, enumeration_Type };

typedef enum PrimitiveTypes TypeKind;

struct ilu_imported_s {
  string		name;
  string		filename;
};

struct enumerationField_s {
  string		name;
  int			id;
};

struct ilu_interface_s {
  Name			name;
  string		brand;
  set			types;		/* set of Type */
  set			classes;	/* set of Type */
  set			exceptions;	/* set of Exception */
  set			imports;	/* set of Imported */
  set			constants;	/* set of Constants */
  LineNumber		def;
  string		filename;	/* file in which interface is defined (if any) */
};

struct ilu_type_s {		/* A type-reference (in refman
				 * terms) AKA type_stamp (in
				 * ilu.bison terms). */
  Name            name;
  TypeDescription description;
  /* When a type_stamp is defined in its file, this is the def. */
  Type            supertype;
  /*
   * When a type_stamp is an alias, or an external reference, this
   * is what it refers to; otherwise NULL.
   */
  set             refs;		/* set of LineNumber */
  LineNumber      def;
  boolean         builtIn;
  string          importInterfaceName;	/* The first component of a
					 * type_stamp that's an
					 * external ref. */
  Interface       interface;	/* in which this type_stamp occurs */
  TypeDescription cached_des;	/* cache for type_basic_type() */
  boolean         marked;	/* mark for recursive type search */
  string          uid;
};

struct ilu_proc_s {
  Name			name;
  list			arguments;	/* list of Argument */
  boolean		returnOptional;	/* may be NULL */
  Type			returnType;
  set			exceptions;	/* set of Exception */
  LineNumber		def;
  ProtocolId		id;
  Type			object;		/* for use with methods */
  Interface		interface;
  boolean		functional;
  boolean		asynch;
  boolean		marked;		/* for use with recursive sweep */
  char *		authentication_type;
  char *		doc_string;
};

struct ilu_argument_s {
  Name			name;
  Type			type;
  ArgDirection		direction;
  boolean		sibling;
  list			values;		/* used only for Unions */
  LineNumber		def;
};

struct ilu_exception_s {
  Name			name;
  Type			type;
  boolean		valueOptional;	/* T if value may be NULL */
  LineNumber		def;
  set			refs;		/* set of LineNumber */
  boolean		builtIn;
  ProtocolId		id;
  string		importInterfaceName;	/* if imported, name of interface */
  Exception		import;			/* imported exception */
  Interface		interface;
  boolean		marked;
  string		corba_rep_id;	/* CORBA RepositoryID, if any */
  char *		doc_string;
};

struct ilu_constant_s {
  Name			name;
  LineNumber		def;
  Interface		interface;
  Type			type;
  string		importInterfaceName;
  Constant		import;
  ConstantValue		value;
};

struct ilu_constantvalue_s {

  /* Constant values are represented in this struct in the following
     manner (where "ctk" is the ur_type_kind of the "type" in the
     ilu_constant_s struct pointing to this one, and "vtk" is the
     "type" field of this struct):

     integer constant:

     (ctk == byte_Type, {short,long}{integer,cardinal}_Type &&
      vtk == integer_Type) && "i" arm valid

     real constant:

     (ctk == {short,long}real_Type && vtk == real_Type) &&
     "i.sign" field is valid && "r" arm valid ("r.fraction" may be NULL)
     OR
     (ctk == {short,long}real_Type && vtk == integer_Type) &&
     "i" arm valid

     boolean constant:

     (ctk == boolean_Type && vtk == boolean_Type) && "b" arm valid

     ilu.CString constant:

     (ctk == sequence_Type && vtk == shortcharacter_Type) &&
     "s" arm valid

     enum constant:

     (ctk == enum_Type && vtk == shortcharacter_Type) &&
     "s" arm valid
  */

  TypeKind type;
  union {
    boolean b;
    struct {
      int sign;
      unsigned long value;
    } i;
    struct {
      int sign;
      string value;
      string fraction;
      long exponent;
    } r;
    string s;
  } val;
};

struct ilu_class_s {
  list		superclasses;
  string	brand;
  string	singleton;
  boolean	collectible;
  boolean	optional;
  string	authentication;
  string	corba_rep_id;	/* CORBA RepositoryID, if any */
  list		methods;	/* list of Procedure */
  string	doc_string;
};

typedef struct {
  Type            discriminator_type;
  list            types;	/* list of Argument */
  Argument        default_arm;
  boolean         others_allowed;	/* other discriminant
					 * values allowed? */
}               UnionDescription;

struct ilu_typedes_s {
  TypeKind type;
  union
    {
      struct {
	Type type;
	boolean optional;
	cardinal limit;
      } sequence;

      struct {
	Type type;
	boolean sink_p;
	boolean optional;
      } pipe;

      struct {
	Type type;
	list dimensions;	/* list of dimensions */
	boolean optional;
	} array;

      list record;		/* list of Argument */

      list enumeration;		/* list of EnumField */

      UnionDescription  uniond;

      Class object;

      Type optional;		/* non-optional type of the optional type */

    } structuredDes;
};

/* various exported functionality */
extern list ParseFile (string filename);	/* list of Interface found in file */
extern Interface GetInterface (string interfacename, string filename);
extern char *iluparser_CanonicalPathname (char *file);
extern char *iluparser_GetProgramName (char *short_name);
extern void iluparser_GenerateBoilerplate (FILE *file, Interface parse, char *programName, char *prefixes[2]);
extern void iluparser_MultipleInterfaceBoilerplate (FILE *file, list interfaces, char *programName, char *prefixes[2]);
extern char *iluparser_GetILUVersionString(void);
extern void iluparser_RegisterInterfaceDirectories(list /* of directory names */);
extern boolean iluparser_IsKeyword(string potential_keyword);
extern void iluparser_ClearMarks(void);	/* clear mark bits on all known types */

/* memory management */
extern void *iluparser_Malloc(unsigned long size);
extern void iluparser_Free(void *ptr);
extern void *iluparser_Realloc (void *ptr, unsigned long size);
extern char * ilu_strdup (char *);
extern int ilu_strcasecmp (char *s1, char *s2);	/* -n if s1 < s2, 0 if s1 == s2, n if s1 > s2 */

/* list primitives */

typedef void (*iluparser_EnumProc) (refany element, refany rock);
/* do something with element and rock */

typedef boolean (*iluparser_FindProc) (refany element, refany rock);
/* return TRUE if element matches the find criteria */

typedef boolean (*iluparser_CompareProc) (refany element1, refany element2);
/* return TRUE if element1 and element2 should be swapped */

extern cardinal list_size (list l);
extern refany list_ref (list l, cardinal index);
extern list list_cdr (list l);
extern refany list_car (list l);
extern void list_insert (list, refany new_element);
extern boolean list_remove (list, refany element);
extern void list_clear (list, boolean freeElements);
extern void list_enumerate (list, iluparser_EnumProc proc, refany rock);
extern refany list_find (list, iluparser_FindProc proc, refany rock);
extern list iluparser_new_list(void);
extern void list_push(list, refany new_element);
#define new_list iluparser_new_list
extern void list_sort (list, iluparser_CompareProc);

/* type primitives */
extern TypeDescription type_description(Type);
extern Type     ur_type(Type);
extern Type     under_type(Type);
extern TypeKind type_kind(Type);
extern TypeKind type_basic_type(Type);	/* = type_kind */
extern TypeKind type_ur_kind(Type);	/* type_kind(ur_type(..)) */
extern string   type_name(Type);
extern Interface type_interface(Type);
extern void type_recurse(Type,
			 void (*proc) (Type, refany rock),
			 refany rock);
extern Class    class_object(Type t);
extern Type	exception_type(Exception e);

/* name primitives */
extern void name_set_base_name (Name n, string name);
extern void name_set_lang_name (Name n, string lang, string name);
extern string name_lang_name (Name n, string lang);
extern string name_base_name (Name n);

/* accessors for various kinds of names */
extern string exception_name(Exception e);
extern string interface_name(Interface i);
extern string argument_name (Argument a);
extern string procedure_name (Procedure);
extern string constant_name (Constant);
