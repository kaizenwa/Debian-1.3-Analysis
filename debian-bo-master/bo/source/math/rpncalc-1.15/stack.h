/************************************************************************
 * stack.h -- part of rpncalc.c						*
 *									*
 * Refer to rpncalc.c for copyrights and license permissions.           *
 ************************************************************************/

/* $Id: stack.h,v 1.2 1997/01/19 18:56:40 david Rel $
 * $Log: stack.h,v $
 * Revision 1.2  1997/01/19 18:56:40  david
 * Declaration of all stack related functions.
 *
 * Revision 1.1  1997/01/19 18:19:23  david
 * Initial revision
 *
 */

struct elm
{
  double value;
  struct elm *prev, *next;
};

double push(double elem);
double pop(void);
double pick(double op1);
double drop(void);
double dupel(double d);
double dupel2(double d);
double dupn(double n, double d);
double dropn(double n);
double clear(void);
double over(void);
double depth(void);
double swap(void);
double roll(void);
double showstack(void);
double sethex(void);
double setdec(void);
double setoct(void);
double setchar(void);
