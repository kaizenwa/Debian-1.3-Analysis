/* @(#) pair.h,v 1.1 1992/05/12 11:03:51 tron Exp */
#ifdef ANSI_C
#define P_(p) p
#else
#define P_(p) ()
#endif

extern int fitpair P_((char *, int));
extern void putpair P_((char *, datum, datum));
extern datum getpair P_((char *, datum));
extern int delpair P_((char *, datum));
extern int chkpage P_((char *));
extern datum getnkey P_((char *, int));
extern void splpage P_((char *, char *, long));
#ifdef SEEDUPS
extern int duppair P_((char *, datum));
#endif

#undef P_
