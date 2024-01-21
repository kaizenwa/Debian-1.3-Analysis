%{
/*
Copyright (c) 1991-1996 Xerox Corporation.  All Rights Reserved.  

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

$Id: ilu.bison,v 1.132 1996/07/15 23:42:58 janssen Exp $
*/

/*
 * portability warnings:  this code is highly dependent upon the list
 * data type for storing lists of data.  This code makes the assumption
 * that a pointer and integer are the same size, as it does list_insert()
 * of integers at numerous points in the code, as part of the bison
 * actions associated with grammar productions.
 *
 * The list data type should eventually be updated to use a more general
 * (and portable) union mechanism;  For now casts have been added to coerce
 * integers into pointers of the appropriate type for the corresponding
 * call.
 */
#include <stdio.h>

#include "iluptype.h"

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <ctype.h>		/* for isgraph() */
#include <stdlib.h>
#include <string.h>

#if (!((defined(WIN32) || defined(WIN16)) && defined(_WINIO)))
#include <errno.h>		/* for errno, ERANGE */
#include <limits.h>		/* for ULONG_MAX */
#endif /* (!((defined(WIN32) || defined(WIN16)) && defined(_WINIO))) */

#if (defined(WIN32) || defined(WIN16))
#include <malloc.h>	/* needed because of include of alloca */
#endif

#ifdef _IS_POSIX
#include <unistd.h>
#endif

/*
#ifndef __GNUC__
extern void * alloca(unsigned long);
#endif
*/

#define AND		&&
#define OR		||
#define NOT		!

#define KEYWORD(x)	static char x[1]

#define YYSTYPE	refany

#define NEWLINE	'\n'

#ifndef DEFAULT_INTERFACE_PATH

#if (defined(WIN32) || defined(WIN16))
#define DEFAULT_INTERFACE_PATH	".;" ILUHOME
#else
#define DEFAULT_INTERFACE_PATH	".:" ILUHOME
#endif /* if (def... */

#endif /* ndef DEFAULT_INTERFACE_PATH */

#include "version.h"	/* defines ILU_VERSION */

static const char * _ilu_id = "$" "Id: ILU version " ILU_VERSION_STRING ".  Copyright 1990-1996 Xerox Corporation.  All Rights Reserved. $" ;

#define YYDEBUG 1

static void ilutokenprint (FILE *, int, YYSTYPE);

#define YYPRINT ilutokenprint

KEYWORD(o_Brand);
KEYWORD(o_Singleton);
KEYWORD(o_Superclasses);
KEYWORD(o_Collectible);
KEYWORD(o_Authentication);
KEYWORD(o_RepositoryID);
KEYWORD(o_Optional);
KEYWORD(o_Methods);
KEYWORD(o_Documentation);
KEYWORD(m_Functional);
KEYWORD(m_Asynchronous);

typedef struct keyword_s {
  char * keyword;
  refany value;
} Attribute;

static Attribute O_Brand = { o_Brand, 0 };
static Attribute O_Singleton = { o_Singleton, 0 };
static Attribute O_Superclasses = { o_Superclasses, 0 };
static Attribute O_Collectible = { o_Collectible, 0 };
static Attribute O_Optional = { o_Optional, 0 };
static Attribute O_Authentication = { o_Authentication, 0};
static Attribute O_RepositoryID = { o_RepositoryID, 0};
static Attribute O_Methods = { o_Methods, 0 };
static Attribute O_Documentation = { o_Documentation, 0 };

extern int iluparsedebug;

int iluparseparse(void);
static int iluparselex (refany *lvalp);
extern void FigureTypeUID(Type t);

struct parse_s {
  list interfaces;
  Interface interface;
  FILE *input;
  string filename;
  cardinal line;
  struct parse_s *next;
};

static struct parse_s *CurrentParse = NULL;

static struct list_s iluparser_DefaultUnionArm = { NULL, NULL, 0 };

static long int ilu_atoi (string s)
{
  cardinal base = 10;
  int sign = 1;

  if (*s == '-')
    {
      s++;
      sign = -1;
    }
  else if (*s == '+')
    {
      s++;
    }

  if (*s == '0')
    {
      switch (*++s)
	{
	case 'b':
	case 'B':
	  ++s;
	  base = 2;
	  break;

	case 'x':
	case 'X':
	  ++s;
	  base = 16;
	  break;

	case 'd':
	case 'D':
	  ++s;
	  base = 10;
	  break;

	case 'o':
	case 'O':
	  ++s;
	  base = 8;
	  break;

	default:
	  --s;
	}
    }

  return (sign * strtol(s, NULL, base));
}  

char *ilu_strdup(char *src)
{
  char *dst=(char *) iluparser_Malloc(strlen(src)+1);

  strcpy(dst,src);
  return dst;
}

/* a strcasecmp(), since we don't have one with ANSI */
int ilu_strcasecmp (char *a, char *b)
{
  register unsigned char *p1 = ( unsigned char *) a;
  register unsigned char *p2 = ( unsigned char *) b;
  unsigned char c1, c2;

  if (p1 == p2)
    return 0;

  do
    {
      c1 = tolower (*p1++);
      c2 = tolower (*p2++);
      if (c1 == '\0')
        break;
    }
  while (c1 == c2);

  return c1 - c2;
}

typedef struct {
  string          name;
  string          interface;
}               matchname_s;

static boolean matchTypeName (Type type, matchname_s *name)
{
  return ((strcmp(type_name(type), name->name) == 0) AND
	  (((name->interface == NULL) && (type->importInterfaceName == NULL)) OR
	   (name->interface != NULL AND type->interface != NULL AND
	    strcmp(name->interface, name_base_name(type->interface->name)) == 0)));
}

string constant_name (Constant c)
{
  return (name_base_name(c->name));
}

static boolean matchConstantName (Constant constant, matchname_s *name)
{
  return (strcmp(constant_name(constant), name->name) == 0 AND
	  ((name->interface == NULL AND constant->importInterfaceName == NULL) OR
	   (name->interface != NULL AND constant->importInterfaceName != NULL AND
	    strcmp(name->interface, constant->importInterfaceName) == 0)));
}
    
static boolean matchExceptionName (Exception exception, matchname_s *e)
{
  return (strcmp(exception_name(exception), e->name) == 0 AND
	  ((e->interface == NULL AND exception->importInterfaceName == NULL) OR
	   (e->interface != NULL AND exception->importInterfaceName != NULL AND
	    strcmp(e->interface, exception->importInterfaceName) == 0)));
}

/***********************************************************************
	List functions
***********************************************************************/

#ifdef new_list
#undef new_list
#endif

list iluparser_new_list (void)
{
  list ptr;

  ptr = (list) iluparser_Malloc (sizeof(struct list_s));
  ptr->head = NULL;
  ptr->tail = NULL;
  ptr->count = 0;
  return (ptr);
}

#define new_list iluparser_new_list

void list_insert (list l, refany element)
{
  listElement *new;

  if (l == NULL)
    return;

  new = (listElement *) iluparser_Malloc (sizeof(listElement));

  new->data = element;
  new->next = NULL;
  if (l->tail != NULL)
    l->tail->next = new;
  l->tail = new;
  if (l->head == NULL)
    l->head = new;
  l->count += 1;
}

void list_push (list l, refany element)
{
  listElement *new;

  if (l == NULL)
    return;

  new = (listElement *) iluparser_Malloc (sizeof(listElement));

  new->data = element;
  new->next = l->head;
  l->head = new;
  if (l->tail == NULL)
    l->tail = new;
  l->count += 1;
}

boolean list_remove (list l, refany element)
{
  listElement *next, *last;
  boolean wasThere = FALSE;

  if (l == NULL)
    return (FALSE);

  for (next = l->head, last = NULL;  next != NULL;  last = next, next = next->next)
    {
      if (next->data == element)
	{
	  wasThere = TRUE;
	  if (last == NULL)
	    l->head = next->next;
	  else
	    last->next = next->next;
	  if (next == l->tail)
	    l->tail = last;
	  next->next = NULL;
	  iluparser_Free (next);
	  l->count -= 1;
	  next = last;
	  if (next == NULL)
	    break;
	}
    }
  return (wasThere);
}

void list_clear (list l, boolean freeElements)
{
  listElement *p, *last;

  for (p = l->head, last = NULL;  p != NULL;  p = last)
    {
      last = p->next;
      if (freeElements && p->data != NULL)
	iluparser_Free(p->data);
      p->data = NULL;
      p->next = NULL;
      iluparser_Free(p);
    }
  l->head = NULL;
  l->tail = NULL;
  l->count = 0;
}

void list_enumerate (list l, iluparser_EnumProc proc, refany rock)
{
  listElement *ptr;

  if (l == NULL || l->count < 1)
    return;

  for (ptr = l->head;  ptr != NULL;  ptr = ptr->next)
    (*proc)(ptr->data, rock);
}

refany list_find (list l, iluparser_FindProc proc, refany rock)
{
  listElement *ptr;

  if (l == NULL)
    return (NULL);

  for (ptr = l->head;  ptr != NULL;  ptr = ptr->next)
    if ((*proc)(ptr->data, rock))
      return (ptr->data);
  return (NULL);
}

cardinal list_size (list l)
{
  if (l == NULL)
    return (0);

  return (l->count);
}

refany list_ref (list l, cardinal index)
{
  listElement *ptr;
  cardinal i;

  if (l == NULL)
    return (NULL);

  for (ptr = l->head, i = 0;  ptr != NULL && i <= index;  ptr = ptr->next, i++)
    if (i == index)
      return (ptr->data);
  return (NULL);
}

list list_cdr (list l)
{
  list ptr;

  if (l == NULL || l->count < 1)
    return (NULL);
      
  ptr = (list) iluparser_Malloc (sizeof(struct list_s));
  ptr->head = l->head->next;
  ptr->tail = l->tail;
  ptr->count = l->count - 1;
  return (ptr);
}

refany list_car (list l)
{
  if (l == NULL || l->count == 0)
    return (NULL);
  else
    return (l->head->data);      
}

void list_sort (list l, iluparser_CompareProc fn)
{
  listElement *current, *next, *prev;
  int count = list_size(l);
  int index;
  boolean stable = FALSE;

  /* bubblesort list */
  while (!stable && count > 2)
    {
      count -= 1;
      for (stable = TRUE, prev = NULL, current = NULL, next = l->head, index = 0;
	   next != NULL && index <= count;
	   prev = current, current = next, next = current->next, index++)
	{
	  if (current != NULL)
	    {
	      if ((*fn)(current->data, next->data))
		{
		  if (prev == NULL)
		    l->head = next;
		  else
		    prev->next = next;
		  current->next = next->next;
		  next->next = current;
		  current = next;
		  next = next->next;
		  stable = FALSE;
		}
	    }
	}
      count += 1;
    }
}

/**********************************************************************
	Name methods
**********************************************************************/

static Name new_Name(void)
{
  Name new = (Name) iluparser_Malloc (sizeof(struct name_s));
  new->base_name = NULL;
  new->langnames = (set) new_list();
  return (new);
}

string name_base_name (Name n)
{
  if (n == NULL)
    return (NULL);
  return (n->base_name);
}

void name_set_base_name (Name n, string name)
{
  if (n == NULL)
    return;
  if (n->base_name != NULL)
    iluparser_Free (n->base_name);
  n->base_name = ilu_strdup(name);
}

static boolean MatchNameLang (usagename u, string lang)
{
  return (u != NULL && strcmp (u->lang, lang) == 0);
}

string name_lang_name (Name n, string lang)
{
  usagename p;

  if (n == NULL)
    return (NULL);
  p = (usagename) list_find (n->langnames, (boolean (*)(refany, refany)) MatchNameLang, lang);
  if (p != NULL)
    return (p->name);
  else
    return (NULL);
}

void name_set_lang_name (Name n, string lang, string name)
{
  usagename new;

  if (n == NULL)
    return;
  
  if ((new = (usagename) list_find (n->langnames, (boolean (*)(refany, refany)) MatchNameLang, lang)) != NULL)
    {
      iluparser_Free (new->name);
      new->name = ilu_strdup (name);
    }
  else
    {
      new = (usagename) iluparser_Malloc (sizeof(struct usagename_s));
      new->lang = ilu_strdup(lang);
      new->name = ilu_strdup(name);
      list_insert (n->langnames, new);
    }
}

/**********************************************************************
	Type constructors
**********************************************************************/

static Type new_Type(void)
{
  Type new = (Type) iluparser_Malloc (sizeof(struct ilu_type_s));
  new->name = (Name) new_Name();
  new->description = NULL;
  new->supertype = NULL;
  new->refs = new_list();
  new->def = 0;
  new->builtIn = FALSE;
  new->importInterfaceName = NULL;
  new->interface = NULL;
  new->cached_des = NULL;
  new->marked = FALSE;
  new->uid = NULL;
  return (new);
}

string type_name (Type t)
{
  if (t == NULL || t->name == NULL)
    return ((string) "void");
  else
    return (name_base_name(t->name));
}

Type ur_type (Type t)
{
  Type            p;
  if (t == NULL)
    return t;
  /*
   * The following circumlocution is better for setting breakpoints
   * than the straightfoward formulation.
   */
  for (p = t; 1; p = p->supertype)
    if (p->supertype == NULL)
      return p;
}

Type under_type (Type t)
{
  Type            ans = t;
  if (ans->supertype != NULL) {
    ans = ans->supertype;
    if (ans->importInterfaceName != NULL)
      ans = ans->supertype;
  }
  return ans;
}

TypeDescription type_description (Type t)
{
  Type p;

  if (t == NULL)
    return (NULL);
  if (t->cached_des == NULL)
    {
      for (p = t;  p->description == NULL; )
	{
	  if (p->supertype != NULL)
	    p = p->supertype;
	  else
	    return (NULL);
	}
      t->cached_des = p->description;
    }
  return (t->cached_des);
}

TypeKind type_kind (Type t)
{
  TypeDescription td;

  if (t == NULL)
    return (void_Type);
  if (t->supertype != NULL)
    return (alias_Type);
  td = type_description(t);
  if (td == NULL)
    return (invalid_Type);
  return (td->type);
}

TypeKind type_basic_type (Type t)
{
  return type_kind(t);
}

TypeKind type_ur_kind(Type t)
{
  Type            u = ur_type(t);
  return type_kind(u);
}

typedef struct {
  void (*action)(Type, refany);
  refany actionrock;
} typeRecurseData;

static void recurseRecordFields (Argument field, typeRecurseData *data)
{
  if (field->type != NULL AND NOT field->type->marked)
    type_recurse (field->type, data->action, data->actionrock);
}

void type_recurse (Type t, void (*action) (Type, refany),
		   refany actionrock)
{
  TypeKind        tk;
  if (t == NULL OR t->marked)
    return;
  t->marked = TRUE;
  tk = type_kind(t);

  /* check types that contain other types */
  if (tk == alias_Type)
    type_recurse(t->supertype, action, actionrock);
  else if (tk == record_Type) {
    typeRecurseData data;

    data.action = action;
    data.actionrock = actionrock;

    list_enumerate(type_description(t)->structuredDes.record,
		   (void (*) (refany, refany)) recurseRecordFields,
		   &data);
  } else if (tk == optional_Type) {
    type_recurse(type_description(t)->structuredDes.optional,
		 action, actionrock);
  } else if (tk == sequence_Type) {
    type_recurse(type_description(t)->structuredDes.sequence.type,
		 action, actionrock);
  } else if (tk == array_Type) {
    type_recurse(type_description(t)->structuredDes.array.type,
		 action, actionrock);
  } else if (tk == union_Type) {
    typeRecurseData data;

    data.action = action;
    data.actionrock = actionrock;

    list_enumerate(type_description(t)->structuredDes.uniond.types,
		   (void (*) (refany, refany)) recurseRecordFields,
		   &data);
  }
  (*action) (t, actionrock);
  t->marked = FALSE;
}

Interface type_interface (Type type)
{
  Type t;

  for (t = type;  t->importInterfaceName != NULL;  t = t->supertype)
    ;
  return (t->interface);
}

static Argument new_Argument(void)
{
  Argument new = (Argument) iluparser_Malloc (sizeof(struct ilu_argument_s));
  new->name = new_Name();
  new->type = NULL;
  new->values = NULL;
  new->def = 0;
  new->sibling = FALSE;
  new->direction = In;
  return (new);
}

string argument_name (Argument a)
{
  if (a == NULL)
    return (NULL);
  return (name_base_name(a->name));
}

static Exception new_Exception(void)
{
  Exception new = (Exception) iluparser_Malloc (sizeof(struct ilu_exception_s));
  new->name = new_Name();
  new->type = NULL;
  new->valueOptional = FALSE;
  new->refs = new_list();
  new->def = 0;
  new->id = 0;
  new->builtIn = FALSE;
  new->interface = NULL;
  new->importInterfaceName = NULL;
  new->import = NULL;
  new->marked = FALSE;
  new->corba_rep_id = NULL;
  new->doc_string = NULL;
  return (new);
}

string exception_name (Exception e)
{
  if (e == NULL)
    return (NULL);
  return (name_base_name(e->name));
}

Type exception_type (Exception e)
{
  Exception ep = e;

  while (ep->import != NULL)
    ep = ep->import;
  return (ep->type);
}

static Procedure new_Procedure(void)
{
  Procedure new = (Procedure) iluparser_Malloc (sizeof(struct ilu_proc_s));
  new->name = new_Name();
  new->object = NULL;
  new->arguments = NULL;
  new->returnType = NULL;
  new->returnOptional = FALSE;
  new->functional = FALSE;
  new->asynch = FALSE;
  new->exceptions = NULL;
  new->def = 0;
  new->id = -1;
  new->interface = NULL;
  new->marked = FALSE;
  return (new);
}

string procedure_name (Procedure p)
{
  return (name_base_name(p->name));
}

static void SetObjectSlotOfMethod (refany element, refany rock)
{
  Procedure m = (Procedure) element;
  Type o = (Type) rock;
  if (m != NULL)
    m->object = o;
}

static TypeDescription new_TypeDescription(void)
{
  TypeDescription new = (TypeDescription) iluparser_Malloc(sizeof(struct ilu_typedes_s));
  new->type = invalid_Type;
  memset((char *) new, 0, sizeof(struct ilu_typedes_s));
  return (new);
}

static boolean FindInterfaceName (string name, string *interfacename, string *typename)
{
  string p;

  if ((p = strchr(name, '.')) == NULL)
    return (FALSE);
  name = ilu_strdup(name);
  p = strchr(name, '.');
  *p = (char) 0;
  *interfacename = name;
  *typename = p + 1;
  return (TRUE);
}

static Type find_or_create_type (list l, char *name, Interface interface)
{
  Type hit;
  matchname_s stype;

/*
  printf ("%s, %u:  finding type \"%s\"...\n",
	  (CurrentParse == NULL) ? "" : CurrentParse->filename,
	  (CurrentParse == NULL) ? 0 : CurrentParse->line, name);
*/

  stype.name = NULL;
  stype.interface = NULL;
  FindInterfaceName (name, &stype.interface, &stype.name);
  if (stype.name == NULL) stype.name = name;

  hit = (Type) list_find (l, (iluparser_FindProc) matchTypeName, &stype);

  if (hit == NULL)
    {
      hit = new_Type();
      hit->interface = interface;
      hit->importInterfaceName = stype.interface;
      name_set_base_name(hit->name, ilu_strdup(stype.name));
      list_push (l, hit);
      if (stype.interface != NULL AND interface != NULL
	  AND strcmp(stype.interface, name_base_name(interface->name)) != 0)
	{
	  name_set_lang_name(hit->name, "import", stype.name);
	}
/*
      printf ("added type <%s> to interface <%s>\n", name, (interface == NULL) ? "*" : name_base_name(interface->name));
*/
    }
  
  return (hit);
}

#define FIND_OR_CREATE_TYPE(name) (find_or_create_type (CurrentParse->interface->types, (name), CurrentParse->interface))

static Exception find_or_create_exception (list l, char *name, Interface interface)
{
  string typename = NULL, interfacename = NULL;
  matchname_s e;
  Exception hit;

  FindInterfaceName(name, &interfacename, &typename);

  if (typename == NULL)
    typename = name;

  e.name = typename;
  e.interface = interfacename;
  hit = (Exception) list_find (l, (boolean (*)(refany, refany)) matchExceptionName, &e);

  if (hit == NULL)
    {
      hit = new_Exception();
      hit->interface = interface;
      name_set_base_name(hit->name, ilu_strdup(typename));
      list_insert (l, hit);
      hit->importInterfaceName = interfacename;
      if (interfacename != NULL)
	name_set_lang_name (hit->name, "import", typename);
    }

  return (hit);
}

#define FIND_OR_CREATE_EXCEPTION(name) (find_or_create_exception (CurrentParse->interface->exceptions, (name), CurrentParse->interface))

static Constant find_or_create_constant (list l, char *name, Interface interface)
{
  string typename = NULL, interfacename = NULL;
  Constant hit;
  matchname_s ndata;

  FindInterfaceName(name, &interfacename, &typename);

  if (typename == NULL)
    typename = name;

  ndata.name = typename;
  ndata.interface = interfacename;
  hit = (Constant) list_find (l, (boolean (*)(refany, refany)) matchConstantName, &ndata);

  if (hit == NULL)
    {
      hit = (Constant) iluparser_Malloc(sizeof(struct ilu_constant_s));
      hit->type = NULL;
      hit->def = 0;
      hit->value = NULL;
      hit->name = new_Name();
      hit->interface = interface;
      hit->importInterfaceName = interfacename;
      name_set_base_name(hit->name, ilu_strdup(typename));
      if (interfacename != NULL)
	name_set_lang_name (hit->name, "import", typename);
      list_insert (l, hit);
    }

  return (hit);
}

#define FIND_OR_CREATE_CONSTANT(name) (find_or_create_constant (CurrentParse->interface->constants, (name), CurrentParse->interface))

#define ADD_PRIMITIVE_TYPE(l,i,n,t) {Type new = find_or_create_type((l),(n),(i));if(new->description==NULL){TypeDescription d = new_TypeDescription();d->type=(t);new->description=d;new->builtIn=TRUE;};}

static void AddPredefinedTypes (list l, Interface i)
{
  ADD_PRIMITIVE_TYPE (l, i, "integer", integer_Type);
  ADD_PRIMITIVE_TYPE (l, i, "cardinal", cardinal_Type);
  ADD_PRIMITIVE_TYPE (l, i, "real", real_Type);
  ADD_PRIMITIVE_TYPE (l, i, "shortinteger", shortinteger_Type);
  ADD_PRIMITIVE_TYPE (l, i, "shortcardinal", shortcardinal_Type);
  ADD_PRIMITIVE_TYPE (l, i, "shortreal", shortreal_Type);
  ADD_PRIMITIVE_TYPE (l, i, "longinteger", longinteger_Type);
  ADD_PRIMITIVE_TYPE (l, i, "longcardinal", longcardinal_Type);
  ADD_PRIMITIVE_TYPE (l, i, "longreal", longreal_Type);
  ADD_PRIMITIVE_TYPE (l, i, "byte", byte_Type);
  ADD_PRIMITIVE_TYPE (l, i, "boolean", boolean_Type);
  ADD_PRIMITIVE_TYPE (l, i, "character", character_Type);
  ADD_PRIMITIVE_TYPE (l, i, "shortcharacter", shortcharacter_Type);
}

static list KnownInterfaces = NULL;

static void ClearExcnMark (Exception e, void *junk)
{
  e->marked = FALSE;
}

static void ClearProcMark (Procedure p, void *junk)
{
  p->marked = FALSE;
  if (p->exceptions != NULL)
    list_enumerate (p->exceptions, (iluparser_EnumProc) ClearExcnMark, NULL);
}

static void ClearTypeMarks (Type t, void *unused)
{
  t->marked = FALSE;
  if (type_ur_kind(t) == object_Type)
    {
      Class c = class_object(t);
      if (c->methods != NULL)
	list_enumerate (c->methods, (iluparser_EnumProc) ClearProcMark, NULL);
    }
}

static void ClearInterfaceMarks(Interface s, void *unused)
{
  list_enumerate (s->types, (iluparser_EnumProc) ClearTypeMarks, NULL);
  list_enumerate (s->exceptions, (iluparser_EnumProc) ClearExcnMark, NULL);
}

void iluparser_ClearMarks (void)
{
  list_enumerate (KnownInterfaces, (iluparser_EnumProc) ClearInterfaceMarks, NULL);
}

static boolean FindNamedInterface (Interface s, string name)
{
  return (strcmp (interface_name(s), name) == 0);
}

static Exception FindExceptionInInterface (string interfacename, string ename)
{
  Interface s;
  Exception e;
  matchname_s edata;

  if (KnownInterfaces == NULL)
    return (NULL);
  if ((s = (Interface) list_find (KnownInterfaces, (boolean (*)(refany, refany)) FindNamedInterface, interfacename)) == NULL)
    return (NULL);
  edata.name = ename;
  edata.interface = NULL;
  if ((e = (Exception) list_find (s->exceptions, (boolean (*)(refany, refany)) matchExceptionName, &edata)) == NULL)
    return (NULL);
  return (e);
}

static Type FindTypeInInterface (string interfacename, string tname)
{
  Interface s;
  Type t;
  matchname_s stype;

  if (KnownInterfaces == NULL
      || ((s = (Interface) list_find (KnownInterfaces, (boolean (*)(refany, refany)) FindNamedInterface, interfacename)) == NULL))
    {
      fprintf (stderr, "(FindTypeInInterface):  Can't find interface \"%s\".\n", interfacename);
      return (NULL);
    }
  stype.name = tname;
  stype.interface = interfacename;
  if ((t = (Type) list_find (s->types, (iluparser_FindProc) matchTypeName, &stype)) == NULL)
    {
      fprintf (stderr, "(FindTypeInInterface):  Interface \"%s\" doesn't seem to contain a type \"%s\".\n",
	       interfacename, tname);
      return (NULL);
    }
  return (t);
}

static Imported new_Imported(string name, string filename);

static Interface new_Interface (string name)
{
  Interface new = (Interface) iluparser_Malloc (sizeof(struct ilu_interface_s));
  new->name = new_Name();
  name_set_base_name(new->name, name);
  new->types = (set) new_list();
  new->classes = (set) new_list();
  new->imports = NULL;
  new->exceptions = (set) new_list();
  new->constants = (set) new_list();
  new->brand = NULL;
  new->def = 0;
  AddPredefinedTypes (new->types, new);
  return (new);
}

static boolean FindImport (Imported a, char *b)
{
  return (strcmp(a->name, b) == 0);
}

static boolean has_imported (list imports, char *newimport)
{
  return (list_find(imports, (iluparser_FindProc) FindImport, newimport) != NULL);
}

string interface_name(Interface i)
{
  return ((string) ((i == NULL) ? NULL : name_base_name(i->name)));
}

static Imported new_Imported (string name, string filename)
{
  Imported new = (Imported) iluparser_Malloc (sizeof(struct ilu_imported_s));
  new->name = name;
  new->filename = filename;
  return (new);
}

/**********************************************************************
  Globals and code
**********************************************************************/

#ifdef MACOS
static string FigureFilename (name)
     string name;
{
/*
 * for the Mac. port, all of this code is very pointless:  If we can't find
 * it on the fopen() call, then too bad.
 */
	return ilu_strdup(name);
}
#else
static list SearchList = NULL;

static void AddILUPATH (list sl)
{
  char *prefix;
  char *p;
  char *ourstorage;

  if ((prefix = getenv ("ILUPATH")) == NULL)
    prefix = DEFAULT_INTERFACE_PATH;
  p = ourstorage = ilu_strdup(prefix);
  while (p != NULL AND *p != '\0')
    {
      list_insert (sl, p);
#if (defined(WIN32) || defined(WIN16))
	/* use common ; path convention for windows platforms */
      p = strchr(p, ';');
#else
      p = strchr(p, ':');
#endif
      if (p != NULL)
	*p++ = '\0';
    }
}

void iluparser_RegisterInterfaceDirectories (list directories)
{
  if (SearchList != NULL)
    {
      list_clear (SearchList, FALSE);
    }
  SearchList = directories;
  AddILUPATH (SearchList);
}

static list GetSearchList (void)
{
  if (SearchList == NULL)
    {
      SearchList = new_list();
#if (defined(WIN32) || defined(WIN16))
      list_insert (SearchList, ilu_strdup("."));
#endif
      AddILUPATH (SearchList);
    }
  return (SearchList);
}

static boolean FindFileInDir (char *dir, char *name)
{
  char buf[1000];
  char *canonical_name;
  boolean status;

#if (defined(WIN32) || defined(WIN16))
  sprintf (buf, "%s\\%s", dir, name);
#else
  sprintf (buf, "%s/%s", dir, name);
#endif /* (defined(WIN32) || defined(WIN16)) */

  canonical_name = iluparser_CanonicalPathname (buf);

#ifdef _IS_POSIX
  status = (access (canonical_name, R_OK) == 0);
#else
  {
    FILE *f = fopen(canonical_name, "r");
    if (f != NULL)
      fclose(f);
    status = (f != NULL);
  }
#endif
  iluparser_Free(canonical_name);
  return status;
}

static string FigureFilename (string name, string extension)
{
  char buf[1000];
  char nbuf[1000];
  char *testname;
  char *dir;
  char *ext;
  
  if ((ext = strrchr(name, '.')) == NULL)
    {
      sprintf (nbuf, "%s%s", name, (extension == NULL) ? ".isl" : extension);
      testname = nbuf;
    }
  else
    testname = name;

#if (defined(WIN32) || defined(WIN16))
  if (*testname == '.' || *testname == '\\')
#else
  if (*testname == '.' || *testname == '/')
#endif /* (defined(WIN32) || defined(WIN16)) */
    return (iluparser_CanonicalPathname(testname));

  if ((dir = list_find(GetSearchList(), (iluparser_FindProc) FindFileInDir, testname)) != NULL)
    {  
#if (defined(WIN32) || defined(WIN16))
      sprintf (buf, "%s\\%s", dir, testname);
#else
      sprintf (buf, "%s/%s", dir, testname);
#endif /* (defined(WIN32) || defined(WIN16)) */
      return (iluparser_CanonicalPathname(buf));
    }
  else
    return NULL;
}
#endif	/* MACOS */

/* used to return an Interface; changed to be ANSI-conformant for
 * only use in call to list_enumerate().
 */
static void GetImportedInterface (Imported s)
{
  GetInterface (s->name, s->filename);
}

static string type_import_name (Type t)
{
  return ((string) ((t == NULL) ? NULL : name_lang_name (t->name, "import")));
}

static void FixUpImportedType (Type t)
{
  if (t->importInterfaceName != NULL) {
    Type            ref = FindTypeInInterface(t->importInterfaceName,
					      type_import_name(t));
    t->supertype = ref;
    if (strchr(name_base_name(t->name), '.') != NULL)
      t->interface = GetInterface(t->importInterfaceName, NULL);
  }
  return;
}

static string exception_import_name (Exception e)
{
  return ((string) ((e == NULL) ? NULL : name_lang_name (e->name, "import")));
}

static void AssignEnumValue(EnumField ef, long int *count)
{
    if ( ef->id < 0 ) {
        ef->id = *count;
        *count += 1;
    }
    else
	*count = ef -> id + 1;
}

static void AssignEnumerationIDs(Type t)
{
    long count = 0;

    if ( t->importInterfaceName == NULL && 
		type_kind(t) == enumeration_Type ) {
        list_enumerate (type_description(t)->structuredDes.enumeration, 
			(void (*)(refany, refany)) AssignEnumValue, &count );
    }
}

static void AssignArmSpecificValue (Argument arg, int *count)
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = integer_Type;
  new->val.i.sign = 1;
  new->val.i.value = *count;

  arg->values = new_list();
  list_insert(arg->values, new);
  *count += 1;
}

static void CountUnvaluedArms (Argument arm, int *count)
{
  if (arm->values == NULL)
    *count += 1;
}

static boolean FindDefaultArm (Argument arg, Argument *default_arm)
{
  if (arg->values == &iluparser_DefaultUnionArm)
    {
      if (*default_arm != NULL)
	return (TRUE);
      else
	*default_arm = arg;
    }
  return (FALSE);
}

static void FigureUnionIDs (Type t)
{
  long            count = 0;
  unsigned int    arms_without_specific_values = 0;
  TypeDescription td;
  Type            tagt;
  TypeKind        tagtb;
  list            arms;

  if (t->importInterfaceName != NULL ||
      type_kind(t) != union_Type)
    return;

  td = type_description(t);
  arms = td->structuredDes.uniond.types;
  tagt = ur_type(td->structuredDes.uniond.discriminator_type);
  tagtb = type_kind(tagt);

  if (NOT(tagtb == integer_Type OR tagtb == shortinteger_Type
	  OR tagtb == cardinal_Type OR tagtb == shortcardinal_Type
	  OR tagtb == boolean_Type OR tagtb == byte_Type
	  OR tagtb == enumeration_Type)) {
    fprintf(stderr,
	    "Error:  Type \"%s\" specified as tag type for a union.  Only enumerations, [SHORT]CARDINAL, [SHORT]INTEGER, BYTE, or BOOLEAN are allowed.\n",
	    type_name(tagt));
    exit(1);
  }
  td->structuredDes.uniond.default_arm = NULL;
  if (list_find(arms, (iluparser_FindProc) FindDefaultArm,
		&td->structuredDes.uniond.default_arm)
      != NULL) {
    fprintf(stderr,
	    "Two arms of union \"%s\" declared as default!\n",
	    type_name(t));
    exit(1);
  }
  if (td->structuredDes.uniond.default_arm != NULL
      AND td->structuredDes.uniond.others_allowed) {
    fprintf(stderr, "Union \"%s\":  the keyword OTHERS should not be used with a union type that has a default arm.\n", type_name(t));
    exit(1);
  }
  list_enumerate(arms, (iluparser_EnumProc) CountUnvaluedArms,
		 &arms_without_specific_values);
  if (arms_without_specific_values > 0
      AND list_size(arms) > arms_without_specific_values) {
    fprintf(stderr, "If any arm of a union such as \"%s\" is assigned particular case values or defaulted, all arms must be assigned values.\n",
	    type_name(t));
    exit(1);
  }
  if (arms_without_specific_values > 0)
    list_enumerate(arms, (iluparser_EnumProc) AssignArmSpecificValue,
		   &count);
}

static void FixUpImportedException (Exception e)
{
  if (e->importInterfaceName != NULL)
    {
      Exception ref = FindExceptionInInterface (e->importInterfaceName, exception_import_name(e));
      e->import = ref;
      if (ref == NULL)
	fprintf (stderr, "Can't find referred-to exception \"%s\" or perhaps interface \"%s\".\n",
		 exception_import_name(e), e->importInterfaceName);
    }
}

static void AssignMethodID (Procedure method, ProtocolId *val)
{
  if (method->id < 0)
    method->id = ++*val;
}

static void FindHighestMethodID (Procedure method, ProtocolId *val)
{
  if (method->id > *val)
    *val = method->id;
}

Class class_object (Type t)
{
  Type rt = ur_type(t);
  if (type_kind(rt) != object_Type)
    return (NULL);
  if (rt->description != NULL)
    return (rt->description->structuredDes.object);
  return NULL;
}

static void AssignMethodIDs (refany element, refany rock)
{
  Type            type = (Type) element;
  Class           od;
  ProtocolId      HighestMethodID;

  if (type == NULL || type_kind(type) != object_Type ||
      (od = class_object(type)) == NULL)
    return;
  list_enumerate(od->superclasses, AssignMethodIDs, NULL);
  if (list_size(od->methods) > 0) {
    HighestMethodID = 0;
    list_enumerate(od->methods,
		   (void (*) (refany, refany)) FindHighestMethodID,
		   &HighestMethodID);
    list_enumerate(od->methods,
		   (void (*) (refany, refany)) AssignMethodID,
		   &HighestMethodID);
  }
}

static boolean ParsingConstant = FALSE;
static boolean ParsingNonRealConstant = FALSE;

static char *ErrorCheckMsg;
static char *ErrorCheckTname;
static boolean ErrorCheckErrors = FALSE;
static char *ErrorCheckFilename;

static void PrintRef (long int ref, char *file)
{
  fprintf (stderr, "%s:%ld:  %s \"%s\".\n", file, ref, ErrorCheckMsg, ErrorCheckTname);
}

static void FindDifferentlyCasedSameException (Exception e1, Exception e2)
{
  if (e1->interface == e2->interface && e1->def != 0 && e2->def != 0
      && ilu_strcasecmp(exception_name(e1), exception_name(e2)) == 0
      && strcmp(exception_name(e1), exception_name(e2)) != 0)
    {
      fprintf (stderr, "%s:%ld:  exception \"%s\" redefined as \"%s\"\n",
	       ErrorCheckFilename, e2->def, exception_name(e1), exception_name(e2));
      ErrorCheckErrors = TRUE;
    }
}

static void FindDifferentlyCasedSameType (Type t1, Type t2)
{
  if (t1->interface == t2->interface && t1->def != 0 && t2->def != 0
      && ilu_strcasecmp(type_name(t1), type_name(t2)) == 0 && strcmp(type_name(t1), type_name(t2)) != 0)
    {
      fprintf (stderr, "%s:%ld:  type \"%s\" redefined as \"%s\"\n",
	       ErrorCheckFilename, t2->def, type_name(t1), type_name(t2));
      ErrorCheckErrors = TRUE;
    }
}

static void CheckTypeDefs (Type type, char *file)
{
  ErrorCheckFilename = file;
  if (!type->builtIn && type->def == 0 && type->importInterfaceName == NULL)
    {
      fprintf (stderr, "%s:*:  undefined type \"%s\".\n", file, name_base_name(type->name));
      ErrorCheckTname = type_name(type);
      ErrorCheckMsg = "undefined type";
      list_enumerate (type->refs, (void (*)(refany, refany)) PrintRef, file);
      ErrorCheckErrors = TRUE;
    }
  else
    {
      if (type->interface != NULL)
	list_enumerate (type->interface->types, (void (*)(refany, refany)) FindDifferentlyCasedSameType, type);
    }
}

static void CheckExceptionDefs (Exception e, char *file)
{
  ErrorCheckFilename = file;
  if (!e->builtIn && e->def == 0 && e->importInterfaceName == NULL)
    {
      ErrorCheckTname = exception_name(e);
      ErrorCheckMsg = "undefined exception";
      fprintf (stderr, "%s:  undefined exception %s\n", file, ErrorCheckTname);
      list_enumerate (e->refs, (void (*)(refany, refany)) PrintRef, file);
      ErrorCheckErrors = TRUE;
    }
  if (e->interface != NULL)
    list_enumerate (e->interface->exceptions, (void (*)(refany, refany)) FindDifferentlyCasedSameException, e);
}

static void FigureCollectibility2 (Type type, boolean *v)
{
  Class od;

  if (type == NULL || type_kind(type) != object_Type || (od = class_object(type)) == NULL)
    return;
  if (!od->collectible)
    {
      *v = FALSE;
      return;
    }
  else
    list_enumerate(od->superclasses, (void (*) (refany, refany)) FigureCollectibility2, v);
}

static void FigureCollectibility (Type type, char *file)
{
  Class od;
  boolean was_collectible;

  if (type == NULL || type_kind(type) != object_Type || (od = class_object(type)) == NULL)
    return;
  was_collectible = od->collectible;
  if (!od->collectible)
    return;
  else
    list_enumerate(od->superclasses, (void (*) (refany, refany)) FigureCollectibility2, &od->collectible);
  if (was_collectible AND NOT od->collectible)
    {
      fprintf (stderr, "%s:%ld:  collectible type %s inherits from non-collectible superclasses.\n",
	       type_interface(type)->filename, type->def, type_name(type));
      ErrorCheckErrors = TRUE;
    }
}

static void FigureOptionality2 (Type type, boolean *v)
{
  Class od;

  if (type == NULL || type_kind(type) != object_Type || (od = class_object(type)) == NULL)
    return;
  if (!od->collectible)
    {
      *v = FALSE;
      return;
    }
  else
    list_enumerate(od->superclasses, (void (*) (refany, refany)) FigureOptionality2, v);
}

static void FigureOptionality (Type type, char *file)
{
  Class od;
  boolean was_collectible;

  if (type == NULL || type_kind(type) != object_Type || (od = class_object(type)) == NULL)
    return;
  was_collectible = od->collectible;
  if (!od->collectible)
    return;
  else
    list_enumerate(od->superclasses, (void (*) (refany, refany)) FigureOptionality2, &od->collectible);
  if (was_collectible AND NOT od->collectible)
    {
      fprintf (stderr, "%s:%ld:  optional class %s inherits from non-optional superclasses.\n",
	       type_interface(type)->filename, type->def, type_name(type));
      ErrorCheckErrors = TRUE;
    }
}

static void CheckConstants (Constant c, char *file)
{
  TypeKind ctk, vtk;
  char *cn = name_base_name(c->name);
  char *tn = type_name(c->type);

  ctk = type_ur_kind(c->type);
  vtk = c->value->type;

  switch (ctk)
    {
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
      if (vtk != integer_Type) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must have an associated integer value.\n",
		 file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      else if (ctk == shortinteger_Type && (c->value->val.i.value > 0x7FFF || c->value->val.i.value < -0x8000)) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must be between %ld and %ld.\n",
		 file, c->def, cn, tn, - 0x8000, 0x7FFF);
	ErrorCheckErrors = TRUE;
      }
      else if (ctk == integer_Type && (c->value->val.i.value > 0xFFFFFFFF || c->value->val.i.value < -0xFFFFFFFF)) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must be between %ld and %ld.\n",
		 file, c->def, cn, tn, - 0xFFFFFFFF, 0xFFFFFFFF);
	ErrorCheckErrors = TRUE;
      }
      break;

    case byte_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
      if (vtk != integer_Type) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must have an associated integer value.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      else if (c->value->val.i.sign < 0) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" may not be negative.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      else if (ctk == byte_Type && c->value->val.i.value > 255) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must be less than 255.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      else if (ctk == shortcardinal_Type && c->value->val.i.value > 0xFFFF) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must be less than %lu.\n", file, c->def, cn, tn, 0xFFFF);
	ErrorCheckErrors = TRUE;
      }
      else if (ctk == cardinal_Type && c->value->val.i.value > 0xFFFFFFFF) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must be less than %lu.\n", file, c->def, cn, tn, 0xFFFFFFFF);
	ErrorCheckErrors = TRUE;
      }
      break;

    case real_Type:
    case shortreal_Type:
    case longreal_Type:
      if (vtk != real_Type && vtk != integer_Type) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must have an associated real or integer value.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      break;

    case boolean_Type:
      if (vtk != boolean_Type) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must have an associated boolean value.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      break;

    case sequence_Type:
      if (vtk != shortcharacter_Type) {
	fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" must have an associated string value.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      break;

    case enumeration_Type:
      if (vtk != shortcharacter_Type) {
	fprintf (stderr, "*** Constants of type \"%s\" must have an associated string value.\n", file, c->def, cn, tn);
	ErrorCheckErrors = TRUE;
      }
      break;

    default:
      fprintf (stderr, "%s:%ld:  %s: constants of type \"%s\" not allowed.\n", file, c->def, cn, tn);
      ErrorCheckErrors = TRUE;
      break;
    }
}

static boolean ErrorCheck (Interface s, char *file)
{
  boolean         stat;

  ErrorCheckErrors = FALSE;
  list_enumerate(s->types, (void (*) (refany, refany)) CheckTypeDefs,
		 file);
  list_enumerate(s->exceptions,
	    (void (*) (refany, refany)) CheckExceptionDefs, file);
  list_enumerate(s->classes,
	  (void (*) (refany, refany)) FigureCollectibility, file);
  list_enumerate(s->classes,
	     (void (*) (refany, refany)) FigureOptionality, file);
  list_enumerate(s->constants,
		 (iluparser_EnumProc) CheckConstants, file);
  stat = ErrorCheckErrors;
  ErrorCheckErrors = FALSE;
  return (stat);
}

static boolean has_suffix(string filename, string suffix)
{
  int offset = strlen(filename) - strlen(suffix);

  return offset >= 0 && strcmp(filename + offset, suffix) == 0;
}

static FILE *idl2isl(char *filename)
{
#ifdef _IS_POSIX
  static char idl2isl_cmd[] = "idl2isl";
  char command_buffer[1000];
  char name_buffer[L_tmpnam];
  char *isl_name = tmpnam(name_buffer);
  FILE *file = NULL;

  sprintf(command_buffer, "%s/%s %s > %s", IDL2ISL_BINDIR, idl2isl_cmd, filename, isl_name);
  if (system(command_buffer) != 0)
      fprintf (stderr, "IDL to ISL translation failed on file %s.\n", filename);
  else if ((file = fopen(isl_name, "r")) == NULL)
      fprintf (stderr, "Error opening file %s for read.\n", isl_name);
  unlink(isl_name);
  return file;
#else	/* is *not* POSIX */
  printf("ILU parser:  OMG IDL parsing is only supported on POSIX UNIX platforms.\n");
  exit(1);
  return (0);	/* shut up compiler warning about no return value */
#endif /* _IS_POSIX */
}

static void AddNewInterface (Interface interface, struct parse_s *new)
{
  interface->filename = ilu_strdup(new->filename);

  if (KnownInterfaces == NULL)
    KnownInterfaces = new_list();
  if (list_find(KnownInterfaces,
		(boolean(*) (refany, refany)) FindNamedInterface,
		name_base_name(interface->name))
      == NULL)
    list_insert(KnownInterfaces, interface);
}

static void IdentifyClasses (Type t, list classes)
{
  if (type_kind(t) == object_Type && t->importInterfaceName == NULL)
    list_insert(classes, t);
}

static boolean FixupInterface (Interface interface, struct parse_s *new)
{
  list_enumerate(interface->types, (iluparser_EnumProc) IdentifyClasses, interface->classes);

  /* Now go and get any imported interfaces. */
  list_enumerate(interface->imports,
		 (iluparser_EnumProc) GetImportedInterface, NULL);
  list_enumerate(interface->types,
		 (iluparser_EnumProc) FixUpImportedType, NULL);
  list_enumerate(interface->exceptions,
	       (iluparser_EnumProc) FixUpImportedException, NULL);

  /* assign Enumeration IDs */
  list_enumerate(interface->types,
	  (void (*) (refany, refany)) AssignEnumerationIDs, NULL);

  /* assign Union discriminators */
  list_enumerate(interface->types, (iluparser_EnumProc) FigureUnionIDs,
		 NULL);

  /* assign Method IDs and unique class ids */
  list_enumerate(interface->classes, AssignMethodIDs, NULL);
  iluparser_ClearMarks();
  list_enumerate(interface->types, (iluparser_EnumProc) FigureTypeUID, NULL);

  /* check for errors and remove if necessary */
  if (ErrorCheck(interface, new->filename)) {
    list_remove(KnownInterfaces, interface);
    return (TRUE);
  } else
    return (FALSE);
}

list ParseFile (string filename)
{
  struct parse_s *new;
  int stat;
  list val = NULL;

  iluparsedebug = ((getenv("ISLDEBUG") != NULL) ? 1 : 0);

  if (KnownInterfaces == NULL)
    KnownInterfaces = new_list();

  new = (struct parse_s *) iluparser_Malloc (sizeof(struct parse_s));
  new->filename = FigureFilename (filename, ".isl");
  new->interfaces = NULL;
  new->interface = NULL;
  new->input = NULL;
  new->line = 0;
  new->next = NULL;

  if (new->filename == NULL)
    {
      fprintf (stderr, "ParseFile:  couldn't figure full filename for partial name \"%s\".\n", filename);
      iluparser_Free(new);
      return (NULL);
    }
  
  new->next = CurrentParse;
  new->interface = NULL;
  new->line = 1;
  
  if (has_suffix(new->filename, ".idl"))
    {
      if ((new->input = idl2isl(new->filename)) == NULL)
	{
	  fprintf (stderr, "ParseFile:  couldn't create ISL from IDL file \"%s\".\n", new->filename);
	  iluparser_Free (new->filename);
	  iluparser_Free (new);
	  return (NULL);
	}
    }
  else if ((new->input = fopen(new->filename, "r")) == NULL)
    {
      fprintf (stderr, "ParseFile:  Error opening file \"%s\" for read.\n", new->filename);
      iluparser_Free (new->filename);
      iluparser_Free (new);
      return (NULL);
    }

  CurrentParse = new;
  ParsingConstant = FALSE;
  if (iluparsedebug)
    printf ("parsing %s\n", filename);
  stat = iluparseparse();
  if (iluparsedebug)
    printf ("finished %s\n", filename);
  fclose (new->input);

  CurrentParse = new->next;

  if (stat == 0 AND new->interfaces != NULL)
    {
      Interface invalid_interface;

      list_enumerate (new->interfaces, (iluparser_EnumProc) AddNewInterface, new);

      while ((invalid_interface = (Interface) list_find (new->interfaces, (boolean (*)(refany, refany)) FixupInterface, new)) != NULL)
	list_remove (new->interfaces, invalid_interface);      
      if (list_size(new->interfaces) == 0)
	{
	  iluparser_Free (new->interfaces);
	  val = NULL;
	}
      else
	val = new->interfaces;
      iluparser_Free (new->filename);
      iluparser_Free (new);
    }
  else
    {
      fprintf (stderr, "%s:%ld:  parse error %d\n", new->filename, new->line, stat);
      iluparser_Free (new->filename);
      iluparser_Free (new);
      val = NULL;
    }
  return (val);
}

Interface GetInterface (string interfacename, string filename)
{
  Interface s;

  if (interfacename == NULL)
    return (NULL);

  if (KnownInterfaces == NULL
      || interfacename == NULL
      || (s = (Interface) list_find (KnownInterfaces, (boolean (*)(refany, refany)) FindNamedInterface, interfacename)) == NULL)
    {
      char buf[1000];

      if (filename == NULL)
	{
	  sprintf (buf, "%s.isl", interfacename);
	  ParseFile (buf);
	}
      else
	ParseFile (filename);
    }
  if (KnownInterfaces == NULL
      || (s = (Interface) list_find (KnownInterfaces, (boolean (*)(refany, refany)) FindNamedInterface, interfacename)) == NULL)
    return (NULL);
  else
    return (s);      
}

static boolean MyError = FALSE;

static int iluparseerror (string s)
{
  if (!MyError && (strcmp("parse error", s) == 0) && (!iluparsedebug))
    return 0;

  fprintf (stderr, "%s:%ld:  %s\n", CurrentParse->filename, CurrentParse->line, s);
  return (0);
}

static int iluwarn (string s)
{
  fprintf (stderr, "%s:%ld:  WARNING -- %s\n", CurrentParse->filename, CurrentParse->line, s);
  return 0;
}

static void iluerror (string s)
{
  MyError = TRUE;
  iluparseerror (s);
  MyError = FALSE;
}

static void iluerror1 (string s, refany a)
{
  char buf[1000];

  sprintf (buf, s, a);
  iluerror (buf);
}

static Argument argument_Create (char *name, Type type, boolean sibling,
	ArgDirection direction, long int line_def, list value)
{
  Argument new = new_Argument();
  if ( name )
      name_set_base_name (new->name, name);
  new->def = line_def;
  new->type = type;
  new->values = value;
  new->direction = direction;
  new->sibling = sibling;
  return (new);
}

static Procedure procedure_Create (char *name, boolean functional, boolean asynch, list args, Type returnType, list exceptions, long int id, long int def_line, Interface interface, Type object, char *authentication_type, char *doc_string)
{
  Procedure new = new_Procedure();
  name_set_base_name (new->name, name);
  new->functional = functional;
  new->asynch = asynch;
  new->arguments = args;
  new->returnType = returnType;
  new->returnOptional = FALSE;
  new->exceptions = exceptions;
  new->id = id;
  new->def = def_line;
  new->interface = interface;
  new->object = object;
  new->authentication_type = authentication_type;
  new->doc_string = doc_string;
  return (new);
}

static void AssignClassAttributes (Attribute *a, Class c)
{
  if (a->keyword == o_Brand)
    c->brand = (string) a->value;
  else if (a->keyword == o_Collectible)
    c->collectible = (boolean) a->value;
  else if (a->keyword == o_Optional)
    c->optional = (boolean) a->value;
  else if (a->keyword == o_Singleton)
    c->singleton = (string) a->value;
  else if (a->keyword == o_Superclasses)
    c->superclasses = (list) a->value;
  else if (a->keyword == o_Authentication)
    c->authentication = (string) a->value;
  else if (a->keyword == o_RepositoryID)
    c->corba_rep_id = (string) a->value;
  else if (a->keyword == o_Methods)
    c->methods = (list) a->value;
  else if (a->keyword == o_Documentation)
    c->doc_string = (char *) a->value;
  else
    {
      yyerror ("Bad keyword for object type\n");
    }
  a->value = 0;
}

static boolean CheckForSelf (Imported i, string name)
{
  return (strcmp(i->name, name) == 0);
}

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

%}

%pure_parser

%expect 1

%start interface_list

%token K_INTERFACE K_EXCEPTION K_TYPE K_CONSTANT K_IMPORTS K_FROM K_END K_BRAND K_TRUE
%token K_INTEGER K_CARDINAL K_REAL K_CHARACTER K_BYTE K_LONG K_SHORT K_BOOLEAN K_FALSE
%token K_EQUALS K_COLON K_LEFTPAREN K_RIGHTPAREN K_SEMICOLON K_COMMA K_IN K_OUT K_INOUT
%token K_RECORD K_UNION K_ARRAY K_OF K_ENUMERATION K_SEQUENCE K_OPTIONAL K_DEFAULT
%token K_OBJECT K_SUPERCLASS K_SUPERTYPES K_AUTHENTICATION K_FUNCTIONAL K_ASYNCH K_DOCUMENTATION
%token K_METHODS K_RAISES K_SIBLING K_SINGLETON K_COLLECTIBLE K_SOURCE K_SINK K_TYPEID
%token T_STRING T_NUMBER K_EXPONENT K_PERIOD K_HYPHEN K_PLUS K_LIMIT K_OTHERS

%%	/* beginning of rules section */

interface_list :	interface
{
  CurrentParse->interfaces = new_list();
  list_insert(CurrentParse->interfaces, $1);
  $$ = (refany) CurrentParse->interfaces;
}
			| interface_list interface
{
  list_insert ((list) $1, $2);
  $$ = $1;
}
			;

interface :		interface_declaration declaration_list
{
  $$ = $1;
}
			| error
{
  iluerror ("Bad interface description.");
  yyerrok;
}
			;

declaration_list :	simple_declaration

			|

			declaration_list simple_declaration
			;

simple_declaration :	type_declaration
{
  $$ = $1;
}
			| exception_declaration
{
  $$ = $1;
}
			| constant_declaration
{
  $$ = $1;
}
			| error K_SEMICOLON
{
  iluerror ("Bad statement.");
  yyerrok;
  ParsingConstant = FALSE;
  ParsingNonRealConstant = FALSE;
  $$ = $1;
}
  			;

interface_declaration :	K_INTERFACE identifier optional_brand interface_end
{
  CurrentParse->interface = new_Interface($2);
  CurrentParse->interface->brand = (string) $3;
  CurrentParse->interface->def = CurrentParse->line;
  if (!has_imported((list) $4, "ilu") && strcmp((string) $2, "ilu") != 0)
    list_insert ((list) $4, new_Imported("ilu", NULL));
  CurrentParse->interface->imports = (list) $4;
  /* check for import of self */
  if (list_find((list) $4, (iluparser_FindProc) CheckForSelf, (refany) $2) != NULL)
    {
      char buf[1000];
      sprintf(buf, "Interface \"%s\" may not import itself.", (string) $2);
      iluerror(buf);
      YYERROR;
    }
  $$ = (refany) CurrentParse->interface;
}
			;

optional_brand :	/* empty */
{
  $$ = NULL;
}
			| K_BRAND T_STRING
{
  $$ = (refany) $2;
}
			;

interface_end :		K_SEMICOLON
{
  list new = new_list();
  $$ = (refany) new;
}
			|
			import_list K_SEMICOLON
{
  $$ = $1;
}
			;


import_list :		K_IMPORTS import_list2 K_END
{
  $$ = $2;
}
			| K_IMPORTS import_list2 K_SEMICOLON error
{
  iluerror ("Missing END statement in import list.");
  yyerrok;
  $$ = $2;
}
			;

import_list2 :		import_statement
{
  list new = new_list();

  list_insert(new, $1);
  $$ = (refany) new;
}
			| import_list2 K_COMMA import_statement
{
  if (!has_imported((list) $1, ((Imported) $3)->name))
    list_insert ($1, $3);
  else
    free($3);
  $$ = $1;
}
			;

import_statement :	identifier
{
  $$ = (refany) new_Imported ($1, NULL);
}
			| identifier K_FROM T_STRING
{
  $$ = (refany) new_Imported ($1, $3);
}
			;

type_declaration :	K_TYPE identifier K_EQUALS type_description K_SEMICOLON
{
  Type            new = FIND_OR_CREATE_TYPE($2);
  if (new->def != 0) {
    char            buf[500];
    sprintf(buf, "type \"%s\" already defined on line %ld\n",
	    type_name(new), new->def);
    iluerror(buf);
    YYERROR;
  } else {
    new->description = (TypeDescription) $4;
    new->def = CurrentParse->line;
    if (type_kind(new) == object_Type)
      list_enumerate(new->description->structuredDes.object->methods,
		     SetObjectSlotOfMethod,
		     new);
  }
  $$ = (refany) new;
}
			| K_TYPE identifier K_EQUALS type_stamp K_SEMICOLON
{
  Type            new = FIND_OR_CREATE_TYPE($2);
  if (new->def != 0) {
    char            buf[500];
    sprintf(buf, "type \"%s\" already defined on line %ld\n",
	    type_name(new), new->def);
    iluerror(buf);
    YYERROR;
  } else {
    new->supertype = (Type) $4;
    new->def = CurrentParse->line;
  }
  $$ = (refany) new;
}
			;


type_description :	array_description
{
  $$ = (refany) $1;
}
			| sequence_description
{
  $$ = (refany) $1;
}
			| pipe_description
{
  $$ = (refany) $1;
}
			| record_description
{
  TypeDescription new = new_TypeDescription();
  new->type = record_Type;
  new->structuredDes.record = (list) $1;
  $$ = (refany) new;
}
			| union_description
{
  $$ = (refany) $1;
}
			| optional_description
{
  TypeDescription new = new_TypeDescription();
  new->type = optional_Type;
  new->structuredDes.optional = (Type) $1;
  $$ = (refany) new;
}
			| enumeration_description
{
  $$ = (refany) $1;
}
			| object_description
{
  $$ = (refany) $1;
}
			;

optional_description :	K_OPTIONAL type_stamp
{
  $$ = (refany) $2;
}
			;

primitive_size :	/* empty */
{
  $$ = (refany) 0;
}
			| K_LONG
{
  $$ = (refany) K_LONG;
}
			| K_SHORT
{
  $$ = (refany) K_SHORT;
}
			;

primitive_type :	primitive_size K_INTEGER
{
  $$ = (refany) ((((int)$1) == 0) ? integer_Type : ((((int)$1) == K_SHORT) ? shortinteger_Type : longinteger_Type));
}
			| primitive_size K_CARDINAL
{
  $$ = (refany) ((((int)$1) == 0) ? cardinal_Type : ((((int)$1) == K_SHORT) ? shortcardinal_Type : longcardinal_Type));
}
			| primitive_size K_REAL
{
  $$ = (refany) ((((int)$1) == 0) ? real_Type : ((((int)$1) == K_SHORT) ? shortreal_Type : longreal_Type));
}
			| primitive_size K_CHARACTER
{
  $$ = (refany) ((((int)$1) == 0 || ((int)$1) == K_LONG) ? character_Type : shortcharacter_Type);
}
			| K_BOOLEAN
{
  $$ = (refany) boolean_Type;
}
			| K_BYTE
{
  $$ = (refany) byte_Type;
}
			;

type_stamp :		primitive_type
{
  matchname_s s;
#define BTN(sss,nnn,iii)	((sss).name=(nnn),(sss).interface=(iii),&sss)

  if ((TypeKind) $1 == integer_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			    (iluparser_FindProc) matchTypeName, BTN(s, "integer", NULL));
  else if ((TypeKind) $1 == cardinal_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			 (iluparser_FindProc) matchTypeName, BTN(s, "cardinal", NULL));
  else if ((TypeKind) $1 == real_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			    (iluparser_FindProc) matchTypeName, BTN(s, "real", NULL));
  else if ((TypeKind) $1 == shortinteger_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
		     (iluparser_FindProc) matchTypeName, BTN(s, "shortinteger", NULL));
  else if ((TypeKind) $1 == shortcardinal_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
		    (iluparser_FindProc) matchTypeName, BTN(s, "shortcardinal", NULL));
  else if ((TypeKind) $1 == shortreal_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			(iluparser_FindProc) matchTypeName, BTN(s, "shortreal", NULL));
  else if ((TypeKind) $1 == longinteger_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
		      (iluparser_FindProc) matchTypeName, BTN(s, "longinteger", NULL));
  else if ((TypeKind) $1 == longcardinal_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
		     (iluparser_FindProc) matchTypeName, BTN(s, "longcardinal", NULL));
  else if ((TypeKind) $1 == longreal_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			 (iluparser_FindProc) matchTypeName, BTN(s, "longreal", NULL));
  else if ((TypeKind) $1 == character_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			(iluparser_FindProc) matchTypeName, BTN(s, "character", NULL));
  else if ((TypeKind) $1 == shortcharacter_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
		   (iluparser_FindProc) matchTypeName, BTN(s, "shortcharacter", NULL));
  else if ((TypeKind) $1 == boolean_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			  (iluparser_FindProc) matchTypeName, BTN(s, "boolean", NULL));
  else if ((TypeKind) $1 == byte_Type)
    $$ = (refany) list_find(CurrentParse->interface->types,
			    (iluparser_FindProc) matchTypeName, BTN(s, "byte", NULL));
}
			| scoped_identifier
{
  $$ = (refany) FIND_OR_CREATE_TYPE($1);
}
			;

array_description :	K_ARRAY K_OF number_list type_stamp
{
  TypeDescription new = new_TypeDescription();
  new->type = array_Type;
  new->structuredDes.array.type = (Type) $4;
  new->structuredDes.array.optional = FALSE;
  list_insert (((Type) $4)->refs, (refany) CurrentParse->line);
  new->structuredDes.array.dimensions = (list) $3;
  $$ = (refany) new;
}
			| K_ARRAY number_list K_OF type_stamp
{
  TypeDescription new = new_TypeDescription();
  new->type = array_Type;
  new->structuredDes.array.type = (Type) $4;
  new->structuredDes.array.optional = FALSE;
  list_insert (((Type) $4)->refs, (refany) CurrentParse->line);
  new->structuredDes.array.dimensions = (list) $2;
  $$ = (refany) new;
}
			;

number_list :		T_NUMBER
{
  list new = new_list();
  list_insert (new, (refany) ilu_atoi($1));
  $$ = (refany) new;
}
			| number_list K_COMMA T_NUMBER
{
  list_insert ($1, (refany) ilu_atoi($3));
  $$ = (refany) $1;
}
			;

sequence_description :	primitive_size K_SEQUENCE K_OF type_stamp possible_limit
{
  TypeDescription new = new_TypeDescription();
  new->type = sequence_Type;
  new->structuredDes.sequence.type = (Type) $4;
  new->structuredDes.sequence.optional = FALSE;
  new->structuredDes.sequence.limit = (cardinal) $5;
  if (new->structuredDes.sequence.limit > 0xFFFF && ((int)$1) == K_SHORT)
    iluwarn ("Both a limit and SHORT are used on sequence declaration.  SHORT dominates.\n");
  if (((int)$1) == K_SHORT)
    new->structuredDes.sequence.limit = 0xFFFF;
  list_insert (((Type) $4)->refs, (refany) CurrentParse->line);
  $$ = (refany) new;
}
			;

possible_limit :	/* empty */
{
  $$ = (refany) 0;
}
			| K_LIMIT T_NUMBER
{
  $$ = (refany) ilu_atoi($2);
}
			;

pipe_description :	pipe_side K_OF type_stamp
{
  TypeDescription new = new_TypeDescription();
  new->type = pipe_Type;
  new->structuredDes.pipe.type = (Type) $3;
  new->structuredDes.pipe.optional = FALSE;
  new->structuredDes.pipe.sink_p = (boolean) $1;
  list_insert (((Type) $3)->refs, (refany) CurrentParse->line);
  $$ = (refany) new;
}
			;

pipe_side :		K_SOURCE
{
  $$ = (refany) FALSE;
}
			| K_SINK
{
  $$ = (refany) TRUE;
}
			;

record_description :	K_RECORD record_field_list K_END
{
  $$ = $2;
}
			| K_RECORD error K_END
{
  iluerror ("Error on record field definitions.");
  yyerrok;
  $$ = (refany) new_list();
}
			| K_RECORD record_field_list error
{
  iluerror( "Missing 'End' in record definition" );
  yyerrok;
  $$ = ( refany ) new_list( );
}
			;
			
record_field_list :	record_field_description
{
  list new = new_list();
  list_insert (new, $1);
  $$ = (refany) new;
}
			|
  			record_field_list K_COMMA record_field_description
{
  list_insert ($1, $3);
  $$ = $1;
}
			;

record_field_description :	identifier K_COLON type_stamp
{
  list_insert (((Type)$3)->refs, (refany) CurrentParse->line);
  $$ = (refany) argument_Create ((string) $1, (Type) $3, FALSE, In, CurrentParse->line, NULL);
}
			;

union_description :	union_prefix union_type_list K_END possible_others
{
  TypeDescription new = new_TypeDescription();
  new->type = union_Type;
  new->structuredDes.uniond.discriminator_type = $1;
  new->structuredDes.uniond.types = $2;
  new->structuredDes.uniond.others_allowed = (boolean) $4;
  $$ = (refany) new;
}
			| union_prefix union_type_list error
{
  iluerror ("missing 'END' in Union description.");
  yyerrok;
}

			;

union_prefix :		possible_type_stamp K_UNION
{
  if ($1 != NULL)
    $$ = $1;
  else
    $$ = (refany) FIND_OR_CREATE_TYPE("shortinteger");
}
			;

possible_type_stamp:	/* empty */
{
  $$ = NULL;
}
			| type_stamp
{
  $$ = $1;
}
			;

union_type_list :	union_type
{
  list new;
  new = new_list();
  list_insert(new, $1);
  $$ = (refany) new;
}
			| union_type_list K_COMMA union_type
{
  list_insert((list) $1, $3);
  $$ = $1;
}
			;
			
union_type:		type_stamp possible_value
{
  list_insert(((Type) $1)->refs, (refany) CurrentParse->line);
  $$ = argument_Create((string) NULL, (Type) $1, FALSE, In,
		       CurrentParse->line, (list) $2);
}
			| identifier K_COLON type_stamp possible_value
{
  list_insert(((Type) $3)->refs, (refany) CurrentParse->line);
  $$ = argument_Create((string) $1, (Type) $3, FALSE, In, CurrentParse->line,
		       (list) $4);
}
			;

possible_value:		/* empty */
{
  $$ = (refany) NULL;
}
			| K_EQUALS 
                          { ParsingConstant = TRUE;
			    ParsingNonRealConstant = TRUE; }
                          union_value_specifier
{ 
  $$ = $3;
  ParsingNonRealConstant = FALSE;
  ParsingConstant = FALSE;
}
			;

union_value_specifier:	K_DEFAULT
{ 
  $$ = (refany) &iluparser_DefaultUnionArm;
}
			| constant_value_list K_END
{
  $$ = $1;
}
			;

possible_others		: /* empty */
{
  $$ = (refany) 0;
}
			| K_OTHERS
{
  $$ = (refany) 1;
}
			;

enumeration_description : enumeration_prefix K_END
{
  TypeDescription new = new_TypeDescription();
  new->type = enumeration_Type;
  new->structuredDes.enumeration = (list) $1;
  $$ = (refany) new;
}
			| enumeration_prefix error
{
  iluerror ("Missing 'END' in enumeration element list.");
  yyerrok;
}
			;

enumeration_prefix :	K_ENUMERATION enumeration_field
{
  list new = new_list();
  list_insert (new, $2);
  $$ = (refany) new;
}
			| enumeration_prefix K_COMMA enumeration_field
{
  list_insert ((list) $1, $3);
  $$ = $1;
}
			;

enumeration_field :	identifier
{
  EnumField new = (EnumField) iluparser_Malloc(sizeof(struct enumerationField_s));
  new->name = $1;
  new->id = -1;
  $$ = (refany) new;
}
			| identifier K_EQUALS T_NUMBER
{
  EnumField new = (EnumField) iluparser_Malloc(sizeof(struct enumerationField_s));
  new->name = $1;
  new->id = ilu_atoi($3);
  $$ = (refany) new;
}
			;

exception_declaration :	K_EXCEPTION identifier exception_type possible_id possible_documentation K_SEMICOLON
{
  Exception new = FIND_OR_CREATE_EXCEPTION ($2);
  if (new->def != 0)
    {
      char buf[500];
      sprintf (buf, "exception \"%s\" already defined on line %ld\n", exception_name(new), new->def);
      iluerror (buf);
    }
  else
    {
      new->valueOptional = FALSE;
      new->type = (Type) $3;
      new->def = CurrentParse->line;
      new->corba_rep_id = (char *) $4;
      new->doc_string = (char *) $5;
    }
  $$ = (refany) new;
}
			;

corba_rep_id:		K_TYPEID T_STRING
{
  $$ = (refany) $2;
}
			;

possible_id :		/* empty */
{
  $$ = (refany) 0;
}
			| corba_rep_id
{
  $$ = (refany) $1;
}
			;

exception_type :	/* empty */
{
  $$ = (refany) NULL;
}
			| K_COLON type_stamp
{
  list_insert (((Type) $2)->refs, (refany) CurrentParse->line);
  $$ = $2;
}
			;

arguments :		K_LEFTPAREN argument_list K_RIGHTPAREN
{
  $$ = $2;
}
			| K_LEFTPAREN K_RIGHTPAREN
{
  $$ = (refany) new_list();
}
			| K_LEFTPAREN error K_RIGHTPAREN
{
  yyerrok;
  $$ = (refany) NULL;
}
  			;

argument_list :		argument
{
  list new = new_list();
  list_insert (new, $1);
  $$ = (refany) new;
}
			| argument_list K_COMMA argument
{
  list_insert ($1, $3);
  $$ = $1;
}
			;

direction_decl : 	/* empty */
{
  $$ = (refany) In;
}
			| K_IN
{
  $$ = (refany) In;
}
			| K_OUT
{
  $$ = (refany) Out;
}
			| K_INOUT
{
  $$ = (refany) InOut;
}
			;

argument :		direction_decl identifier K_COLON sibling_decl type_stamp
{
  /* name, type, sibling, direction, line_def */
  Argument new = argument_Create ($2, $5, (boolean) $4, (ArgDirection) $1, CurrentParse->line, NULL);
  list_insert (new->type->refs, (refany) CurrentParse->line);
  $$ = (refany) new;
}
			;

sibling_decl :		/* empty */
{
  $$ = (refany) FALSE;
}
			| K_SIBLING
{
  $$ = (refany) TRUE;
}
			;

exception_list :	scoped_identifier
{
  list l = new_list();
  Exception new = FIND_OR_CREATE_EXCEPTION ($1);
  list_insert (l, new);
  list_insert (new->refs, (refany) CurrentParse->line);
  $$ = (refany) l;
}
			| exception_list K_COMMA scoped_identifier
{
  Exception new = FIND_OR_CREATE_EXCEPTION ($3);
  list_insert ($1, new);
  list_insert (new->refs, (refany) CurrentParse->line);
  $$ = (refany) $1;
}
			;

object_description :	K_OBJECT object_keywords
/* object_brand object_collectible object_optional object_singleton object_superclasses object_authentication object_corba_rep_id object_methods object_documentation */
{
  TypeDescription new = new_TypeDescription();

  new->type = object_Type;
  new->structuredDes.object = (Class) iluparser_Malloc (sizeof(struct ilu_class_s));
  new->structuredDes.object->brand = NULL;
  new->structuredDes.object->corba_rep_id = NULL;
  new->structuredDes.object->collectible = FALSE;
  new->structuredDes.object->optional = FALSE;
  new->structuredDes.object->singleton = NULL;
  new->structuredDes.object->superclasses = NULL;
  new->structuredDes.object->authentication = NULL;
  new->structuredDes.object->methods = NULL;
  new->structuredDes.object->doc_string = NULL;

  list_enumerate ((list) $2, (void (*)(refany, refany)) AssignClassAttributes, new->structuredDes.object);

  if (new->structuredDes.object->superclasses == NULL)
    new->structuredDes.object->superclasses = new_list();
  if (new->structuredDes.object->methods == NULL)
    new->structuredDes.object->methods = new_list();

  list_clear ((list) $2, FALSE);
  iluparser_Free((void *) $2);

  $$ = (refany) new;
}
			;

object_keywords :	/* empty */
{
  $$ = (refany) new_list();
}
			| object_keywords object_brand
{
  O_Brand.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Brand);
  $$ = $1;
}
			| object_keywords object_collectible
{
  O_Collectible.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Collectible);
  $$ = $1;
}
			| object_keywords object_optional
{
  O_Optional.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Optional);
  $$ = $1;
}
			| object_keywords object_singleton
{
  O_Singleton.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Singleton);
  $$ = $1;
}
			| object_keywords object_superclasses
{
  O_Superclasses.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Superclasses);
  $$ = $1;
}
			| object_keywords object_authentication
{
  O_Authentication.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Authentication);
  $$ = $1;
}
			| object_keywords corba_rep_id
{
  O_RepositoryID.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_RepositoryID);
  $$ = $1;
}
			| object_keywords object_methods
{
  O_Methods.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Methods);
  $$ = $1;
}
			| object_keywords object_documentation
{
  O_Documentation.value = (refany) $2;
  list_insert ((list) $1, (refany) &O_Documentation);
  $$ = $1;
}
			;

object_singleton :	K_SINGLETON T_STRING
{
  $$ = (refany) $2;
}
			;

object_collectible :	K_COLLECTIBLE
{
  $$ = (refany) TRUE;
}
			;

object_optional :	K_OPTIONAL
{
  $$ = (refany) TRUE;
}
			;

object_superclasses :	K_SUPERCLASS scoped_identifier
{
  list l = new_list();
  list_insert(l, FIND_OR_CREATE_TYPE($2));
  $$ = (refany) l;
}
			| K_SUPERTYPES class_list K_END
{
  $$ = $2;
}
			;

class_list :		type_stamp
{
  list l = new_list();
  list_insert(l, (Type) $1);
  $$ = (refany) l;
}
			| class_list K_COMMA type_stamp
{
  list_insert((list) $1, $3);
  $$ = $1;
}
			;

object_authentication :	K_AUTHENTICATION T_STRING
{
  $$ = (refany) $2;
}
			;

object_brand :		K_BRAND T_STRING
{
  $$ = (refany) $2;
}
			;

object_documentation :	K_DOCUMENTATION T_STRING
{
  $$ = (refany) $2;
}
			;

object_methods :	K_METHODS object_method_list K_END
{
  $$ = (refany) $2;
}
			| K_METHODS object_method_list K_COMMA K_END error
{
  iluerror ("Trailing comma in list of methods.");
  yyerrok;
  $$ = (refany) $2;
}
			| K_METHODS error K_COMMA
{
  iluerror ("Bad method definition.");
  yyerrok;
  $$ = (refany) $2;
}
			;

object_method_list :	object_method
{
  list new = new_list();
  if ($1 != NULL)
    list_insert (new, $1);
  $$ = (refany) new;
}
			| object_method_list K_COMMA object_method
{
  if ($3 != NULL)
    list_insert ($1, $3);
  $$ = $1;
}
			| object_method_list error K_COMMA
{
  iluerror ("Bad method definition, possible missing comma.");
  yyerrok;
  $$ = $1;
}
			;

object_method :		possible_asynch_or_functional identifier arguments method_type method_exceptions method_id possible_authentication possible_documentation
{
  if ((((char *) $1) == m_Asynchronous) && ($4 != NULL || $5 != NULL))
    {
      iluerror1 ("method \"%s\":  ASYNCHRONOUS methods cannot have return type or exceptions.\n", (char *) $2);
      $$ = (refany) NULL;
    }
  else if ($3 == NULL)
    {
      iluerror1 ("method \"%s\":  Bad argument list.\n", (char *) $2);
      $$ = (refany) NULL;
    }
  else
    {
      /* name, functional, async, arguments, returnType, exceptions, id, def_line, interface, object_type, authentication_type, doc_string */
      Type t = ($4 != NULL) ? ((Argument) $4)->type : NULL;

      Procedure new = procedure_Create ($2,
					((char *) $1 == m_Functional),
					((char *) $1 == m_Asynchronous),
					$3, t, $5, (int) $6,
					CurrentParse->line,
					CurrentParse->interface, NULL,
					(char *) $7, (char *) $8);
      $$ = (refany) new;
    }
}
			;

method_type :		/* empty */
{
  $$ = NULL;
}
			| K_COLON type_stamp
{
  $$ = (refany) argument_Create (ilu_strdup(""), $2, FALSE, In, CurrentParse->line, NULL);
  list_insert(((Type) $2)->refs, (refany) CurrentParse->line);
}
			;

possible_asynch_or_functional :	/* empty */
{
  $$ = (refany) FALSE;
}
			| K_FUNCTIONAL
{
  $$ = (refany) m_Functional;
}
			| K_ASYNCH
{
  $$ = (refany) m_Asynchronous;
}
			;

possible_documentation :	/* empty */
{
  $$ = (refany) NULL;
}
			| T_STRING
{
  $$ = (refany) $1;
}
			;

method_exceptions :	/* empty */
{
  $$ = NULL;
}
			| K_RAISES exception_list K_END
{
  $$ = $2;
}
			| K_RAISES error K_COMMA
{
  iluerror ("Missing END on exception list");
  $$ = NULL;
}
			;

method_id :	/* empty */
{
  $$ = (refany) -1;
}
			| K_EQUALS T_NUMBER
{
  $$ = (refany) ilu_atoi($2);
}
			;

possible_authentication :	/* empty */
{
  $$ = (refany) NULL;
}
			| K_AUTHENTICATION T_STRING
{
  $$ = (refany) ilu_strdup((char *) $2);
}
			;

constant_declaration : 	K_CONSTANT identifier K_COLON type_stamp K_EQUALS
					{ ParsingConstant = TRUE; }
			constant_value
					{ ParsingConstant = FALSE; }

			K_SEMICOLON
{
  Constant new = FIND_OR_CREATE_CONSTANT((string) $2);
  ParsingConstant = FALSE;

  if (new->def != 0)
    {
      char buf[500];
      sprintf (buf, "constant \"%s\" already defined on line %ld\n", (char *) $2, new->def);
      iluerror (buf);
    }
  else
    {
      new->type = (Type) $4;
      new->value = (ConstantValue) $7;
      new->def = CurrentParse->line;
    }
  $$ = (refany) new;
}
			;

constant_value:		real_constant_value
{
  $$ = $1;
}
			| nonreal_constant_value
{
  $$ = $1;
}
			;

nonreal_constant_value:	sign T_NUMBER
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = integer_Type;
  new->val.i.sign = ($1 == 0) ? 1 : -1;
  new->val.i.value = ilu_atoi ($2);
  $$ = (refany) new;
}
			| T_STRING
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = shortcharacter_Type;
  new->val.s = (string) $1;
  $$ = (refany) new;
}
			| K_TRUE
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = boolean_Type;
  new->val.b = TRUE;
  $$ = (refany) new;
}
			| K_FALSE
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = boolean_Type;
  new->val.b = FALSE;
  $$ = (refany) new;
}
			;

real_constant_value:	sign T_NUMBER real_exponent
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = real_Type;
  new->val.r.sign = ($1 == 0) ? 1 : -1;
  new->val.r.value = (string) $2;
  new->val.r.fraction = NULL;
  new->val.r.exponent = (long) $3;
  $$ = (refany) new;
}
			| sign T_NUMBER K_PERIOD T_NUMBER real_exponent
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = real_Type;
  new->val.r.sign = ($1 == 0) ? 1 : -1;
  new->val.r.value = (string) $2;
  new->val.r.fraction = (string) $4;
  new->val.r.exponent = (long) $5;
  $$ = (refany) new;
}
			| sign T_NUMBER K_PERIOD T_NUMBER
{
  ConstantValue new = (ConstantValue) iluparser_Malloc (sizeof(struct ilu_constantvalue_s));
  new->type = real_Type;
  new->val.r.sign = ($1 == 0) ? 1 : -1;
  new->val.r.value = (string) $2;
  new->val.r.fraction = (string) $4;
  new->val.r.exponent = 0;
  $$ = (refany) new;
}
			;

constant_value_list: constant_value_list K_COMMA constant_value
{
    if ( $3 != NULL )
        list_insert( $1, $3 );
    $$ = $1;
}
			| constant_value
{
    list	new = new_list( );

    if ( $1 != NULL )
        list_insert( new, $1 );
    $$ = ( refany ) new;
}
			;
real_exponent:	
			 K_EXPONENT T_NUMBER
{
  $$ = (refany) ilu_atoi($2);
}
			| K_EXPONENT K_HYPHEN T_NUMBER
{
  $$ = (refany) ( - ilu_atoi( $3 ));
}
			| K_EXPONENT K_PLUS T_NUMBER
{
  $$ = (refany) (( cardinal ) ilu_atoi( $3 ));
}
			;

sign:			K_HYPHEN
{
  $$ = (refany) 1;
}
			| K_PLUS
{
  $$ = 0;
}
			| /* empty */
{
  $$ = 0;
}
			;

identifier:		T_STRING
{
  register char *p;
  if (!isalpha(*((string)$1)))
    {
      char buf[500];
      sprintf (buf, "Identifier \"%s\" does not begin with alphabetic character.", (string) $1);
      iluerror (buf);
      YYERROR;
    }
  for (p = (string) $1;  *p != '\0';  p++)
    {
      if (!(isalnum(*p) OR (*p == '-')))
	{
	  char buf[500];
	  sprintf (buf, "Identifier \"%s\" contains invalid character %c.", (string) $1, *p);
	  iluerror (buf);
	  YYERROR;
	}
    }
  $$ = $1;
}
			;

scoped_identifier:	T_STRING
{
  register char *p;
  if (!isalpha(*((string)$1)))
    {
      char buf[500];
      sprintf (buf, "Identifier \"%s\" does not begin with alphabetic character.", (string) $1);
      iluerror (buf);
      YYERROR;
    }
  for (p = (string) $1;  *p != '\0';  p++)
    {
      if (!(isalnum(*p) OR (*p == '-') OR (*p == '.')))
	{
	  char buf[500];
	  sprintf (buf, "Scoped identifier \"%s\" contains invalid character %c.", (string) $1, *p);
	  iluerror (buf);
	  YYERROR;
	}
    }
  $$ = $1;
}
			;

%%	/* start of program */


static void GetQuotedString(FILE *file, character match, string buffer, int buffersize);
static int GetChar (FILE *file);

static int EatComment (FILE *file)
{
  boolean possibleEndOfComment, possibleNestedComment, endOfComment;
  register int c;

  /* eat input until end-of-comment.  Handle nested comments. */
  for (c = GetChar(file), possibleNestedComment = FALSE, possibleEndOfComment = FALSE, endOfComment = FALSE;
       c != EOF;  c = GetChar(file))
    {
      if (c == NEWLINE)
	CurrentParse->line += 1;

      if (endOfComment)
	return (c);

      if (possibleNestedComment)
	{
	  if (c == '*')
	    c = EatComment (file);
	  possibleNestedComment = FALSE;
	}
      else if (possibleEndOfComment)
	{
	  if (c == ')')
	    endOfComment = TRUE;
	  possibleEndOfComment = FALSE;
	}

/*
      if (c == '"')
	GetQuotedString (file, c, buffer, sizeof(buffer));
      else
*/
      if (c == '(')
	possibleNestedComment = TRUE;
      else if (c == '*')
	possibleEndOfComment = TRUE;
    }
  return (c);
}

/* ungetc() only provides one character of putback.  Extend this. */

struct putback_char {
  FILE *file;
  char *chars;
  unsigned long allocated;
  unsigned long used;
  struct putback_char *next;
};

static struct putback_char *Putbacks = NULL;

static void PutbackChar (char c, FILE *file)
{
  struct putback_char *p;

  for (p = Putbacks;  p != NULL;  p = p->next)
    if (p->file == file)
      break;
  if (p == NULL)
    {
      p = (struct putback_char *) iluparser_Malloc(sizeof(struct putback_char));
      p->file = file;
      p->chars = iluparser_Malloc(20);
      p->allocated = 20;
      p->used = 0;
      p->next = Putbacks;
      Putbacks = p;
    }
  if (p->used >= p->allocated)
    {
      p->allocated = p->used + 20;
      p->chars = iluparser_Realloc(p->chars, p->allocated);
    }
  p->chars[p->used] = c;
  p->used += 1;
  if (c == NEWLINE)
    {
      CurrentParse->line -= 1;
    }
}

static int GetChar (FILE *file)
{
  struct putback_char *p;

  for (p = Putbacks;  p != NULL;  p = p->next)
    if (p->file == file)
      break;
  if (p == NULL || p->used < 1)
    {
      return (getc(file));
    }
  else
    {
      p->used -= 1;
      return (p->chars[p->used]);
    }
}

static int NextChar (FILE *file)
{
  int c;

  c = GetChar(file);
  if (c == NEWLINE)
    {
      CurrentParse->line += 1;
    }

  while (c == '(')
    {
      c = GetChar(file);
      if (c == NEWLINE)
	CurrentParse->line += 1;
      if (c == '*')
	{
	  c = EatComment (file);
	}
      else
	{
	  PutbackChar ((character) c, file);
	  c = '(';
	  break;
	}
    }
  return (c);
}

static void GetQuotedString (FILE *file, character match, string buffer, int buffersize)
{
/*
  Copy string into buffer, stopping when char matching string[0]
  is encountered.  Backslash is quote character.
*/
  register int c;
  register character *output;
  register cardinal size;

#define IsHexDigit(x) ((((x)>='0')&&((x)<='9'))||(((x)>='a')&&((x)<='f'))||(((x)>='A')&&((x)<='F')))
#define HexValue(x) (((x)<='9')?((x)-'0'):((x)<='F')?((x)-'A'+10):((x)<='f')?((x)-'a'+10):0)

  for (output = buffer, c = NextChar(file), size = 1;  c != EOF && size < (cardinal) buffersize;  c = NextChar(file))
    {
      if (c == match)
	break;
      else
	{
	  if (c == '#')
	    {
	      c = NextChar(file);
	      if (IsHexDigit(c))
		{
		  character oldc = c;
		  unsigned int num = HexValue(c);
		  c = NextChar(file);
		  if (IsHexDigit(c))
		    *output++ = num * 16 + HexValue(c);
		  else
		    {
		      PutbackChar((character) c, file);
		      *output++ = oldc;
		    }
		}
	      else if (c == 'n')
		*output++ = '\n';
	      else if (c == 'r')
		*output++ = '\r';
	      else
		*output++ = c;
	    }
	  else
	    *output++ = c;
	  size += 1;
	}
    }
  *output = (character) 0;
  return;
}

static struct key {
  string k;
  int type;
  cardinal len;
  string trans;
} isl_keywords[] = {

    { "interface",		K_INTERFACE,	9, NULL },
    { "imports",		K_IMPORTS,	7, NULL },
    { "from",			K_FROM,		4, NULL },

    { "type",			K_TYPE,		4, NULL },
    { "short",			K_SHORT,	5, NULL },
    { "long",			K_LONG,		4, NULL },
    { "integer",		K_INTEGER,	7, NULL },
    { "cardinal",		K_CARDINAL,	8, NULL },
    { "real",			K_REAL,		4, NULL },
    { "byte",			K_BYTE,		4, NULL },
    { "boolean",		K_BOOLEAN,	7, NULL },
    { "character",		K_CHARACTER,	9, NULL },
    { "enumeration",		K_ENUMERATION,  11, NULL },
    { "union",			K_UNION,	5, NULL },
    { "default",		K_DEFAULT,	7, NULL },
    { "others",			K_OTHERS,	6, NULL },
    { "array",			K_ARRAY,	5, NULL },
    { "sequence",		K_SEQUENCE,	8, NULL },
    { "limit",			K_LIMIT,	5, NULL },
    { "of",			K_OF,		2, NULL },
    { "record",			K_RECORD,	6, NULL },
    { "optional",		K_OPTIONAL,	8, NULL },
    { "end",			K_END,		3, NULL },
    { "source",			K_SOURCE,	6, NULL },
    { "sink",			K_SINK,		4, NULL },

    { "exception",		K_EXCEPTION,	9, NULL },
    { "typeid",			K_TYPEID,	7, NULL },

    { "class",			K_OBJECT,	5, NULL },
    { "object",			K_OBJECT,	6, NULL },
    { "brand",			K_BRAND,	5, NULL },
    { "superclass",		K_SUPERCLASS,	10, NULL },
    { "superclasses",		K_SUPERTYPES,	12, NULL },
    { "supertypes",		K_SUPERTYPES,	10, NULL },
    { "authentication",		K_AUTHENTICATION, 14, NULL },
    { "functional",		K_FUNCTIONAL,	10, NULL },
    { "asynchronous",		K_ASYNCH,	12, NULL },
    { "methods",		K_METHODS,	7, NULL },
    { "in",			K_IN,		2, NULL },
    { "out",			K_OUT,		3, NULL },
    { "inout",			K_INOUT,	5, NULL },
    { "sibling",		K_SIBLING,	7, NULL },
    { "singleton",		K_SINGLETON,	9, NULL },
    { "collectible",		K_COLLECTIBLE,	11, NULL },
    { "raises",			K_RAISES,	6, NULL },
    { "documentation",		K_DOCUMENTATION,13, NULL },

    { "constant",		K_CONSTANT,	8, NULL },
    { "true",			K_TRUE,		4, NULL },
    { "false",			K_FALSE,	5, NULL },

    { NULL, 0, 0} };

static boolean find_keyword (string buffer, cardinal *type, cardinal *length, string *translation)
{
  struct key *p;
/*
  printf ("checking <%s> for keywordness\n", buffer);
*/

  if (buffer == NULL)
    return FALSE;
  for (p = isl_keywords;  p->k != NULL;  p += 1)
    if (ilu_strcasecmp(buffer, p->k) == 0)
      {
	if (type != NULL)
	  *type = p->type;
	if (length != NULL)
	  *length = p->len;
	if (translation != NULL)
	  *translation = p->trans == NULL ? p->k : p->trans;
	return (TRUE);
      }
  return (FALSE);		 
}

boolean iluparser_IsKeyword (char *str)
{
  return (find_keyword (str, NULL, NULL, NULL));
}

static boolean IsBaseDigit (cardinal base, unsigned char digit)
{
  return ((base <= 10 && (((cardinal)(digit - '0')) < base))
	  || ((base > 10) && ((((cardinal)(digit - '0')) < 10)
			      || ((digit >= 'a') && (((cardinal) (digit - 'a' + 10)) < base))
			      || ((digit >= 'A') && (((cardinal) (digit - 'A' + 10)) < base)))));
}

static int GetToken (FILE *file, string buffer, int buffersize, cardinal *type)
{
  static string chars = "();:,=";
  static cardinal types[] = { K_LEFTPAREN, K_RIGHTPAREN, K_SEMICOLON, K_COLON, K_COMMA, K_EQUALS };
  static string funnychars = "&|!()<>\"';:,";
  static string bases = "bBdDoOxX";
  static cardinal base[] = { 2, 2, 10, 10, 8, 8, 16, 16 };
  int length;
  character *q;
  int c;
  string trans;
  string bufferbase = buffer;

#define loop while(1)

  c = NextChar(file);
  while (!isgraph(c) && c != EOF)
    c = NextChar(file);
  if (c == EOF)
    return (EOF);

  if (c == '"' || c == '\'')
    {
      GetQuotedString (file, (character) c, buffer, buffersize);
      *type = T_STRING;
    }
  else if ((q = (char *) strchr(chars, c)) != NULL)
    {
      *buffer = c;
      buffer[1] = (char) 0;
      *type = types[q - chars];
    }
  else if (ParsingConstant && c == '-')
    {
      *type = K_HYPHEN;
    }
  else if (ParsingConstant && c == '+')
    {
      *type = K_PLUS;
    }
  else if (ParsingConstant && c == '.')
    {
      *type = K_PERIOD;
    }
  else if (ParsingConstant && (!ParsingNonRealConstant)
	   && (c == 'e' || c == 'E'))
    {
      *type = K_EXPONENT;
    }
  else if (isdigit(c))
    {
      cardinal CurrentBase = 10;

      *buffer++ = c;
      if (c == '0')
	{
	  c = NextChar(file);
	  if ((q = (char *) strchr(bases, c)) != NULL)
	    {
	      *buffer++ = c;
	      CurrentBase = base[q - bases];
	    }
	  else
	    PutbackChar((character) c, file);
	}
      while ((c = NextChar(file)) != EOF && IsBaseDigit(CurrentBase, (unsigned char) c))
	*buffer++ = c;
      PutbackChar((character) c, file);
      *buffer = 0;
      *type = T_NUMBER;
/*
      if (iluparsedebug)
	fprintf (stderr, "(number)  %s   (%s, %u)\n", bufferbase, CurrentParse->filename, CurrentParse->line);
*/
    }	  
  else
    {
      string orig = buffer;
      loop
	{
	  if (isgraph(c) && (strchr(funnychars, c) == NULL))
	    { *buffer++ = c;  c = NextChar(file); }
	  else
	    {
	      PutbackChar((character) c, file);
	      *buffer = 0;
	      *type = T_STRING;
	      break;
	    }
	}
      if (find_keyword (orig, type, (cardinal *) &length, &trans))
	{
	  strcpy (orig, trans);
	  orig[strlen(trans)] = (char) 0;
/*
	  if (iluparsedebug) 
	    fprintf (stderr, "(keyword)  <%s>   (%s, %u)\n", orig, CurrentParse->filename, CurrentParse->line);
*/
	}
/*
      else if (iluparsedebug)
	fprintf (stderr, "(string)  \"%s\"   (%s, %u)\n", bufferbase, CurrentParse->filename, CurrentParse->line);
*/
    }
  return (c);
}

/*
	"iluparselex"
*/
static int iluparselex (refany *lvalp)
{	/* lexical analysis routine */
  cardinal type;
  char buffer[1000];
  int c;

  if (CurrentParse->input == NULL)
    return (-1);

  buffer[0] = '\0';
  c = GetToken (CurrentParse->input, buffer, sizeof(buffer), &type);
  if (c == EOF)
    return (-1);

  *lvalp = (YYSTYPE) ilu_strdup(buffer);

  return ((int) type);
}

static void ilutokenprint (FILE *where, int tokentype, YYSTYPE token)
{
  if (tokentype == T_STRING)
    fprintf (where, " \"%s\"", (string) token);
  else if (tokentype == T_NUMBER)
    fprintf (where, " %s", (string) token);
}
