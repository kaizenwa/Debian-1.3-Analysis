/************************************************************************
 * stack.c -- part of rpncalc.c						*
 *									*
 * Refer to rpncalc.c for copyrights and license permissions.           *
 ************************************************************************/

/* $Id: stack.c,v 1.5 1997/01/19 20:02:00 david Rel $
 * $Log: stack.c,v $
 * Revision 1.5  1997/01/19 20:02:00  david
 * Renamed sun_sunos[45] to sunos[45]
 *
 * Revision 1.4  1997/01/19 18:54:16  david
 * Some braces rearranged.
 *
 * Revision 1.4  1997/01/19 18:19:23  david
 * new command `char'.
 *
 * Revision 1.2  1996/09/13 20:21:29  david
 * lclint additions
 *
 * Revision 1.1  1996/07/13 20:58:08  david
 * Cosmetic changes due to linting.
 *
 * Revision 1.0  1995/12/31 18:18:55  david
 * Initial revision
 *
 * Revision 1.2  1995/11/25 21:45:20  david
 * Unlimited stack depth due to dynamical allocation of the stack
 * elements (linear linked list).
 *
 * The stack operations have now the right value in the cmd->argno field. This
 * is done even at the expense of superfluous push/pops (dup),
 * unfortunately.
 * */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <locale.h>

#ifdef sunos5
#include <ieeefp.h>
#endif
#include <limits.h>

#include "cmds.h"
#include "utils.h"
#include "stack.h"

struct elm *anchor=NULL;

int pushtostack=1;

enum BASE base=DEC;
int digits=3;			/* accuracy, number of digits */

/* Push an element on the stack */
double push(double elem)
{
  struct elm *next;

  if (pushtostack)
  {
    next=anchor;
    anchor=(struct elm *)xmalloc(sizeof(struct elm));
    anchor->value=elem;  anchor->next=next; anchor->prev=NULL;
    if (anchor->next != NULL) anchor->next->prev=anchor;
  }
  return 0.0; /* dummy */
}

/* Pop an element from the stack */
double pop(void)
{
  double value;

  if (anchor != NULL)
  {
    value=anchor->value; anchor=anchor->next; 
    if (anchor != NULL) {
	if (anchor->prev != NULL) free(anchor->prev);
    	anchor->prev=NULL;
    }
  }
  else
  {
    fprintf(stderr, "pop: stack empty.\n"); value=0.0; 
    pushtostack=0;
  }
  return value;
}

/* Returns the elm-th element from the stack */
double pick(double op1)
{
  long int n, i;
  struct elm *p;
  
  n=(long int)op1; p=anchor; i=1; 
  while ((p != NULL) && (p->next != NULL) && (i<n)) { p=p->next; i++; }

  if ((i==n) && (p!=NULL)) return p->value;
  else
  {
    fprintf(stderr, "pick: element not available.\n"); 
    pushtostack=0;
    return 0.0;
  }
}

/* Drop top element */
double drop(void)
{
  struct elm *top;
  if (anchor != NULL) 
  {
    top=anchor; anchor=top->next; 
    if (anchor != NULL) anchor->prev=NULL; 
    free(top);
  }
  else
  {
    fprintf(stderr, "drop: stack empty.\n"); pushtostack=0;
  }

  return 0.0; /* dummy value */
}

/* Duplicate top element */
double dupel(double d)
{
  (void)push(d); (void)push(d);
  return 0.0; /* dummy */
}

/* Duplicate top element two times */
double dupel2(double d)
{
  (void)push(d); (void)push(d); (void)push(d);
  return 0.0; /* dummy */
}

double dupn(double n, double d)
{
  long int i;

  for (i=1; i<=(long int)n; i++) (void)push(d);
  return 0.0; /* dummy value */
}

double dropn(double n)
{
  long int i;

  for (i=1; i<=(long int)n; i++) (void)drop();
  return 0.0; /* dummy value */
}

double clear(void)
{
  struct elm *p, *t;

  p=anchor;
  while (p != NULL) 
  {
    t=p; p=p->next; 
    free(t);
  }
  anchor=NULL;
  return 0.0; /* dummy value */
}

/* Push 2nd-top element on the stack */
double over(void)
{
  (void)push(pick(2));
  return 0.0; /* dummy value */
}

/* How many elements are on the stack ? */
double depth(void)
{
  struct elm *p;
  double n;

  p=anchor; n=0;
  while (p != NULL) { p=p->next; n++; }

  return n;
}

/* Swap the 1st and 2nd element on stack */
double swap(void)
{
  double tmp1, tmp2;

  tmp1 = pop(); tmp2 = pop();
  (void)push(tmp1); (void)push(tmp2);
  return 0.0; /* dummy value */
}

/* Roll top three elements: 1 2 3 -> 3 1 2 */
double roll(void)
{
  double tmp1, tmp2, tmp3;

  tmp1 = pop(); tmp2 = pop(); tmp3 = pop();
  
  (void)push(tmp1); (void)push(tmp3); (void)push(tmp2);
  return 0.0; /* dummy value */
}

/*
 * Show the contents of the stack. Depending on the base the stack contents 
 * will be shown in dec, hex or oct notation (integers only) or in the normal
 * way: Show integers as ii and floats as ff.ffff
 *
 * Infinite results and NaN (not a number) will be displayed by using
 * isinf() and isnan().
 */
double showstack()
{
  struct elm *p;
  double value;
  short int i;

  if (anchor != NULL)
  {
    p=anchor; i=1;
    while (p->next != NULL) { p=p->next; i++; }
    while (p != NULL)
    {
      printf("%2i: ", i);
      value=p->value;  
      switch (base)
      {
        case HEX:  printf("0x%lx", (unsigned long int)value); break;
        case OCT:  printf("0%lo",  (signed long int)value); break;
	case CHAR: {
			unsigned long int  v=floor(value);
			unsigned char c=(unsigned char)v;

			printf("%03ld 0%03lo 0x%02lx ",v,v,v);
			if (isprint(c)) printf("'%c'",c);
			else if (iscntrl(c)) {
			  if (c < 32)        printf("^%c", c+64);
			  else if (c == 127) printf("^?");
			  else               printf("M-^%c", c-64);
			}
			break;
		   }
        case DEC:
        default:
#ifdef sunos5
		  if (!finite(value)) {
#else
	          if (isinf(value)) {
#endif
	            printf("%cinfinity",
#ifdef sunos5
			   finite(value) == 0 ? '+' : '-');
#else
			   isinf(value) == 1 ? '+' : '-');
#endif
		  }
	          else if (isnan(value)) { printf("NaN"); }
	          else if ((floor(value) == value) && 
			  ((long int)value <= LONG_MAX)) {
		    printf("%.0f",value);
		  } else {
		    printf("%.*g", (digits+1), value);
		  }
	          break;
	  
      }
      printf("\n");
      p=p->prev; i--;
    }  
  }
  return 0.0; /* dummy value */
}

double sethex(void)
{
  base = HEX;
  return 0.0; /* dummy value */
}

double setdec(void)
{
  base = DEC;
  return 0.0; /* dummy value */
}
  
double setoct(void)
{
  base = OCT;
  return 0.0; /* dummy value */
}

double setchar(void)
{
  base = CHAR;
  return 0.0; /* dummy value */
}
