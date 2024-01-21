#ifndef _list_
#define _list_

#ifdef __cplusplus
extern "C" {
#endif


/*
I want to be able to deal with different data types on the same
list so I need some typing information.
Each cell has an element called Type that relates to one of the
types below.
*/
#define LIST_BASE 1000
#define LIST_UNSIGNEDCHAR LIST_BASE+0 
#define LIST_CHAR LIST_BASE+1
#define LIST_ENUM LIST_BASE+2
#define LIST_UNSIGNEDINT LIST_BASE+3
#define LIST_SHORTINT LIST_BASE+4
#define LIST_INT LIST_BASE+5
#define LIST_UNSIGNEDLONG LIST_BASE+6
#define LIST_LONG LIST_BASE+7
#define LIST_FLOAT LIST_BASE+8
#define LIST_DOUBLE LIST_BASE+9
#define LIST_LONGDOUBLE LIST_BASE+10
#define LIST_NEAR LIST_BASE+11
#define LIST_FAR LIST_BASE+12


typedef struct _list LIST;
typedef struct _list *PLIST;
typedef struct _cell POSITION;	/* For readability */
typedef struct _list STACK;		/* For readability */
typedef struct _cell CELL; 
typedef void LIST_ATOM;		/* For readability */

struct _list {
        int Length;		/* Number of cells in list */
        CELL *Head, *Tail;	/* Pointers to head and tail of list */
};
 
struct _cell {
        LIST_ATOM *Atom;	/* Any data type you want */
	int Type;	/* Optional type information */
        CELL *Next;	/* Next cell in list */
        CELL *Prev;	/* Prev cell in list */
};

 
LIST *list_Create(void);
int list_Empty(LIST *l);
POSITION *list_First(LIST *l);
POSITION *list_Last(LIST *l);
POSITION *list_Next(LIST *l, POSITION *c);
POSITION *list_Prev(LIST *l, POSITION *c);
int list_InsertBefore(LIST *l, POSITION *c, LIST_ATOM *a, int Type);
int list_InsertAfter(LIST *l, POSITION *c, LIST_ATOM *a, int Type);
LIST_ATOM *list_Retrieve(LIST *l, POSITION *c);
void list_Update(LIST *l, POSITION *c, LIST_ATOM *a);
int list_Length(LIST *l);
void list_Delete(LIST *l, POSITION *c);
int list_DataType(POSITION *c);

/* Stack macros based on list */
#define stack_Size(s) list_Length(s)
#define stack_Create() list_Create()
#define stack_Empty(l) list_Empty(l)
#define stack_Push(l, a, t) list_InsertBefore(l,list_First(l),a, t)
#define stack_Top(l) list_Retrieve(l, list_First(l))
#define stack_Pop(l) list_Delete(l, list_First(l))

#ifdef __cplusplus
}
#endif

#endif /* _list_ */

