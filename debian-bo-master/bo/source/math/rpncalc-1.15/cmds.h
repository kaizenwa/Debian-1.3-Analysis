/************************************************************************
 * cmds.h -- part of rpncalc.c						*
 *									*
 * Refer to rpncalc.c for copyrights and license permissions.           *
 ************************************************************************/

/* $Id: cmds.h,v 1.4 1997/01/19 18:50:45 david Rel $
 * $Log: cmds.h,v $
 * Revision 1.4  1997/01/19 18:50:45  david
 * Declarations for cmds.c
 *
 * Revision 1.4  1997/01/19 18:19:23  david
 * Provided all declarations.
 *
 * Revision 1.2  1996/09/13 20:21:29  david
 * lclint additions
 *
 * Revision 1.1  1996/07/13 20:49:35  david
 * Cleanup and renaming due to linting of the source.
 *
 * Revision 1.0  1995/12/31 18:16:22  david
 * Initial revision
 * */

enum BASE { DEC, HEX, OCT, CHAR };	/* current base (dec,oct..) */

extern enum BASE base;
extern int digits;			/* accuracy, number of digits */

double showstack(void);
double pick(double elm);
double depth(void);

double pi(void);
double e(void);

#ifndef linux
double pow10(double y);
double pow2(double y);
#endif

double chs(double f);
double sqr(double f);
double inv(double f);
double log2(double f);
double fact(double d);
double prec(double p);
double not(double l);

double plus(double s1, double s2);
double minus(double s, double m);
double multiply(double f1, double f2);
double divide(double n, double d);

double idiv(double dn, double dd);

double mod(double dn, double dd);
long int stein(double d1, double d2);
double gcd(double dn, double dd);
double and(double l1, double l2);
double or(double l1, double l2);
double xor(double l1, double l2);

double sum(void);
double prod(void);
