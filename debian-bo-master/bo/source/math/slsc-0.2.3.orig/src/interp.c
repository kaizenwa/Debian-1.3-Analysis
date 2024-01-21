/*	SC	A Spreadsheet Calculator
 *		Expression interpreter and assorted support routines.
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel, 
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 6.1 $
 */

#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#include <slang.h>

#include "sc.h"
#include "lex.h"
#include "cmds.h"
#include "interp.h"
#include "range.h"

static char *seval (struct enode *);

/* Use this structure to save the the last 'g' command */

struct go_save 
{
   int g_type;
   double g_n;
   char *g_s;
   int  g_row;
   int  g_col;
}
gs;

/* g_type can be: */

#define G_NONE 0			/* Starting value - must be 0*/
#define G_NUM 1
#define G_STR 2
#define G_CELL 3


#define CELL_IS_VALID(p, r, c)  ((NULL != (p = tbl[r][c])) && (p->flags & IS_VALID))


int	exprerr;	   /* Set by eval() and seval() if expression errors */
double  prescale = 1.0;    /* Prescale for constants in let() */
int	extfunc  = 0;	   /* Enable/disable external functions */
int     loading = 0;       /* Set when readfile() is active */

double fn1_eval (double (*)(double), double);
double fn2_eval( double (*)(double, double), double, double);

#ifndef PI
# define PI (double)3.14159265358979323846
#endif

#define dtr(x) ((x)*(PI/(double)180.0))
#define rtd(x) ((x)*(180.0/(double)PI))

static double finfunc (int fun, double v1, double v2, double v3)
{
   double answer = 0.0, p;
   
   p = fn2_eval(pow, 1 + v2, v3);
   
   switch(fun)
     {
      case PV:
	answer = v1 * (1 - 1/p) / v2;
	break;
      case FV:
	answer = v1 * (p - 1) / v2;
	break;
      case PMT:
	answer = v1 * v2 / (1 - 1/p);
	break;
     }
   return(answer);
}

static char *dostindex (double val, int minr, int minc, int maxr, int maxc)
{
   register r,c;
   register struct ent *p;
   char *pr;
   int x;
   
   x = (int) val;
   r = minr; c = minc;
   p = 0;
   if ( minr == maxr ) 
     {
	/* look along the row */
	c = minc + x - 1;
	if (c <= maxc && c >=minc)
	  p = tbl[r][c];
     }
   else if ( minc == maxc ) 
     {
	/* look down the column */
	r = minr + x - 1;
	if (r <= maxr && r >=minr)
	  p = tbl[r][c];
     }
   else 
     {
	slsc_error ("range specified to @stindex");
	return(0);
     }
   if (p && p->label) 
     {
	pr = SLMALLOC((unsigned)(strlen(p->label)+1));
	(void)strcpy(pr, p->label);
	return (pr);
     }
   else
     return(0);
}

static double doindex (double val, int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c;
   register struct ent *p;
   int x;
   
   x = (int) val;
   v = 0;
   r = minr; c = minc;
   if ( minr == maxr ) 
     {
	/* look along the row */
	c = minc + x - 1;
	if ((c <= maxc) && (c >= minc)
	    && CELL_IS_VALID(p, r, c))
	  return p->v;
     }
   else if ( minc == maxc )
     {
	/* look down the column */
	r = minr + x - 1;
	if ((r <= maxr) && (r >= minr)
	    && CELL_IS_VALID(p, r, c))
	  return p->v;
     }
   else slsc_error("Invalid range specified to @index");
   return v;
}

static double dolookupn (double val, int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c;
   register struct ent *p;
   
   v = 0;
   r = minr; c = minc;
   if ( minr == maxr )
     { /* look along the row */
	for ( c = minc; c <= maxc; c++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  if (p->v <= val)
		    {
		       if (CELL_IS_VALID(p, r + 1, c))
		       	 v = p->v;
		    }
		  else return v;
	       }
	  }
     }
   else if ( minc == maxc )
     { /* look down the column */
	for ( r = minr; r <= maxr; r++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  if(p->v <= val)
		    {
		       if (CELL_IS_VALID(p, r, c + 1))
		       	 v = p->v;
		    }
		  else return v;
	       }
	  }
     }
   else slsc_error("Invalid range specified to @lookup");
   return v;
}

static double dolookups(char *s, int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c;
   register struct ent *p;
   
   v = 0;
   r = minr; c = minc;
   if ( minr == maxr ) 
     {
	/* look along the row */
	for ( c = minc; c <= maxc; c++) 
	  {
	     if ((NULL != (p = tbl[r][c])) && (p->label != NULL)) 
	       {
		  if (strcmp(s,p->label) == 0)
		    {
		       SLFREE(s);
		       if (CELL_IS_VALID(p, r + 1, c))
			 return(p->v);
		    }
	       }
	  }
     }
   else if ( minc == maxc )
     { /* look down the column */
	for ( r = minr; r <= maxr; r++)
	  {
	     if ((NULL != (p = tbl[r][c])) && (p->label != NULL))
	       {
		  if (strcmp(s,p->label) == 0)
		    {
		       SLFREE(s);
		       if (CELL_IS_VALID(p, r, c + 1))
		       	 return(p->v);
		    }
	       }
	  }
     }
   else slsc_error("Invalid range specified to @lookup");
   SLFREE(s);
   return v;
}


static double dosum (int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c;
   register struct ent *p;
   
   v = 0;
   for (r = minr; r<=maxr; r++)
     {
	for (c = minc; c<=maxc; c++)
	  if (CELL_IS_VALID (p, r, c)) v += p->v;
     }
   return v;
}

static double doprod (int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c;
   register struct ent *p;
   
   v = 1.0;
   for (r = minr; r <= maxr; r++)
     {
	for (c = minc; c<=maxc; c++)
	  if (CELL_IS_VALID(p, r, c))  v *= p->v;
     }
   return v;
}

static double doavg (int minr, int minc, int maxr, int maxc)
{
   double v;
   register r,c,count;
   register struct ent *p;
   
   v = 0;
   count = 0;
   for (r = minr; r<=maxr; r++)
     {
	for (c = minc; c<=maxc; c++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  v += p->v;
		  count++;
	       }
	  }
     }
   if (count == 0) return ((double) 0.0);
   
   return (v / (double) count);
}

static double dostddev (int minr, int minc, int maxr, int maxc)
{
   double lp, rp, v, nd;
   register r,c,n;
   register struct ent *p;
   
   n = 0;
   lp = 0;
   rp = 0;
   for (r = minr; r<=maxr; r++)
     {
	for (c = minc; c<=maxc; c++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  v = p->v;
		  lp += v * v;
		  rp += v;
		  n++;
	       }
	  }
     }
   if ((n == 0) || (n == 1))
     return ((double) 0);
   nd = (double) n;
   return (sqrt((nd*lp-rp*rp)/(nd*(nd-1))));
}

static double domax (int minr, int minc, int maxr, int maxc)
{
   double v = 0.0;
   register r,c,count;
   register struct ent *p;
   
   count = 0;
   for (r = minr; r<=maxr; r++)
     {
	for (c = minc; c<=maxc; c++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  if (!count)
		    {
		       v = p->v;
		       count++;
		    }
		  else if (p->v > v)
		    v = p->v;
	       }
	  }
     }
   
   if (count == 0)
     return ((double) 0);
   
   return (v);
}

static double domin (int minr, int minc, int maxr, int maxc)
{
   double v = 0.0;
   register r,c,count;
   register struct ent *p;
   
   count = 0;
   for (r = minr; r <= maxr; r++)
     {
	for (c = minc; c <= maxc; c++)
	  {
	     if (CELL_IS_VALID(p, r, c))
	       {
		  if (!count)
		    {
		       v = p->v;
		       count++;
		    }
		  else if (p->v < v)
		    v = p->v;
	       }
	  }
     }
   
   if (count == 0)
     return ((double) 0);
   
   return (v);
}

static double dotime (int which, double when)
{
   static time_t t_cache;
   static struct tm *tp;
   time_t tloc;
   
   if (which == NOW)
     return (double)time (NULL);
   
   tloc = (time_t) when;
   
   if (tloc != t_cache) 
     {
	tp = localtime(&tloc);
	tp->tm_mon += 1;
	tp->tm_year += 1900;
	t_cache = tloc;
     }
   
   switch (which) 
     {
      case HOUR: return((double)(tp->tm_hour));
      case MINUTE: return((double)(tp->tm_min));
      case SECOND: return((double)(tp->tm_sec));
      case MONTH: return((double)(tp->tm_mon));
      case DAY: return((double)(tp->tm_mday));
      case YEAR: return((double)(tp->tm_year));
     }
   /* Safety net */
   return (0.0);
}

static double doston (char *s)
{
   double v;
   
   if (!s)
     return((double)0.0);
   
   (void) slsc_strtof(s, &v);
   SLFREE(s);
   return(v);
}

static double doeqs (char *s1, char *s2)
{
   double v;
   
   if (!s1 && !s2)
     return(1.0);
   
   if (!s1 || !s2)
     v = 0.0;
   else if (strcmp(s1, s2) == 0)
     v = 1.0;
   else
     v = 0.0;
   
   if (s1)
     SLFREE(s1);
   
   if (s2)
     SLFREE(s2);
   
   return(v);
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return a pointer to the selected cell's entry, if any, else 0.  Use
 * only the integer part of the column number.  Always free the string.
 */

static struct ent *getent (char *colstr, double rowdoub)
{
   int collen;		/* length of string */
   int row, col;	/* integer values   */
   struct ent *ep = 0;	/* selected entry   */
   
   if (((row = (int) floor (rowdoub)) >= 0)
       && (row < MAXROWS)				/* in range */
       && ((collen = strlen (colstr)) <= 2)	/* not too long */
       && ((col = sc_atocol (colstr, collen)) >= 0)
       && (col < MAXCOLS))			/* in range */
     {
	ep = tbl [row] [col];
     }
   
   SLFREE (colstr);
   return (ep);
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static double donval (char *colstr, double rowdoub)
{
   struct ent *ep;
   
   return ((ep = getent (colstr, rowdoub))
	   && (ep->flags & IS_VALID) ?
	   (ep -> v) : 0);
}


/*
 *	The list routines (e.g. dolmax) are called with an LMAX enode.
 *	The left pointer is a chain of ELIST nodes, the right pointer
 *	is a value.
 */
static double dolmax(struct enode *ep)
{
   register int count = 0;
   register double maxval = 0; /* Assignment to shut up lint */
   register struct enode *p;
   register double v;
   
   for (p = ep; p; p = p->e.o.left) 
     {
	v = eval(p->e.o.right);
	if (!count || v > maxval) 
	  {
	     maxval = v; count++;
	  }
     }
   if (count) return maxval;
   else return 0.0;
}

static double dolmin (struct enode *ep)
{
   register int count = 0;
   register double minval = 0; /* Assignment to shut up lint */
   register struct enode *p;
   register double v;
   
   for (p = ep; p; p = p->e.o.left) 
     {
	v = eval(p->e.o.right);
	if (!count || v < minval) 
	  {
	     minval = v; count++;
	  }
     }
   if (count) return minval;
   else return 0.0;
}

double eval(register struct enode *e)
{
   int fix_for_region = 0;
   
   if (e==0) return 0;
   switch (e->op) 
     {
      case '+':	return (eval(e->e.o.left) + eval(e->e.o.right));
      case '-':	return (eval(e->e.o.left) - eval(e->e.o.right));
      case '*':	return (eval(e->e.o.left) * eval(e->e.o.right));
      case '/':     	return (eval(e->e.o.left) / eval(e->e.o.right));
      case '%':     
	  {
	     double num, denom;
	     num = floor(eval(e->e.o.left));
	     denom = floor(eval (e->e.o.right));
	     return denom ? num - floor(num/denom)*denom : 0; 
	  }
      case '^':	return (fn2_eval(pow,eval(e->e.o.left),eval(e->e.o.right)));
      case '<':	return (eval(e->e.o.left) < eval(e->e.o.right));
      case '=':	return (eval(e->e.o.left) == eval(e->e.o.right));
      case '>':	return (eval(e->e.o.left) > eval(e->e.o.right));
      case '&':	return (eval(e->e.o.left) && eval(e->e.o.right));
      case '|':	return (eval(e->e.o.left) || eval(e->e.o.right));
      case '?':	return eval(e->e.o.left) ? eval(e->e.o.right->e.o.left)
	  : eval(e->e.o.right->e.o.right);
      case 'm':	return (-eval(e->e.o.right));
      case 'f':	return (eval(e->e.o.right));
      case '~':	return (eval(e->e.o.right) == 0.0);
      case 'k':	return (e->e.k);
      case 'v':	return (e->e.v.vp->v);
      case INDEX:
      case LOOKUP:
	  {	register r,c;
	     register maxr, maxc;
	     register minr, minc;
	     maxr = e->e.o.right->e.r.right.vp -> row;
	     maxc = e->e.o.right->e.r.right.vp -> col;
	     minr = e->e.o.right->e.r.left.vp -> row;
	     minc = e->e.o.right->e.r.left.vp -> col;
	     if (minr>maxr) r = maxr, maxr = minr, minr = r;
	     if (minc>maxc) c = maxc, maxc = minc, minc = c;
	     switch(e->op)
	       {
		case LOOKUP:
		  if (sc_etype(e->e.o.left) == NUM)
	            return dolookupn(eval(e->e.o.left), minr, minc, maxr, maxc);
		  else
	            return dolookups(seval(e->e.o.left),minr, minc, maxr, maxc);
		case INDEX:
		  return doindex(eval(e->e.o.left), minr, minc, maxr, maxc);
	       }
	  }
       /* These are region functions */
      case REDUCE | 'A':
      case REDUCE | 'P':
      case REDUCE | 'S':
      case REDUCE | 'D':
      case REDUCE | 'M':
      case REDUCE | 'm':
	
	fix_for_region = 1;
       /* drop */
       /* these are range functions */
      case REDUCE | '+':
      case REDUCE | '*':
      case REDUCE | 'a':
      case REDUCE | 's':
      case REDUCE | MAX_FUN:
      case REDUCE | MIN_FUN:
	  {	register r,c;
	     register maxr, maxc;
	     register minr, minc;
	     maxr = e->e.r.right.vp -> row;
	     maxc = e->e.r.right.vp -> col;
	     minr = e->e.r.left.vp -> row;
	     minc = e->e.r.left.vp -> col;
	     if (minr>maxr) r = maxr, maxr = minr, minr = r;
	     if (minc>maxc) c = maxc, maxc = minc, minc = c;
	     
	     if (fix_for_region)
	       {
		  if (maxr > minr) maxr--;
		  if (maxc > minc) maxc--;
	       }
	     
	     switch (e->op)
	       {
		case REDUCE | MAX_FUN:
		case REDUCE | 'M':
		  return domax(minr, minc, maxr, maxc);
		  
		case REDUCE | MIN_FUN:
		case REDUCE | 'm':
		  return domin(minr, minc, maxr, maxc);
		  
		case REDUCE | '+':
		case REDUCE | 'S':
		  return dosum(minr, minc, maxr, maxc);
		  
		case REDUCE | 'a':
		case REDUCE | 'A':
		  return doavg(minr, minc, maxr, maxc);
		  
		case REDUCE | 's':
		case REDUCE | 'D':
		  return dostddev(minr, minc, maxr, maxc);
		  
		case REDUCE | '*':
		case REDUCE | 'P':
		  return doprod(minr, minc, maxr, maxc);
	       }
	  }
      case ACOS:	 return (fn1_eval( acos, eval(e->e.o.right)));
      case ASIN:	 return (fn1_eval( asin, eval(e->e.o.right)));
      case ATAN:	 return (fn1_eval( atan, eval(e->e.o.right)));
      case ATAN2:	 return (fn2_eval( atan2, eval(e->e.o.left), eval(e->e.o.right)));
      case CEIL:	 return (fn1_eval( ceil, eval(e->e.o.right)));
      case COS:	 return (fn1_eval( cos, eval(e->e.o.right)));
      case EXP:	 return (fn1_eval( exp, eval(e->e.o.right)));
      case FABS:	 return (fn1_eval( fabs, eval(e->e.o.right)));
      case FLOOR:	 return (fn1_eval( floor, eval(e->e.o.right)));
      case HYPOT:	 return (fn2_eval( hypot, eval(e->e.o.left), eval(e->e.o.right)));
      case LOG:	 return (fn1_eval( log, eval(e->e.o.right)));
      case LOG10:	 return (fn1_eval( log10, eval(e->e.o.right)));
      case POW:	 return (fn2_eval( pow, eval(e->e.o.left), eval(e->e.o.right)));
      case SIN:	 return (fn1_eval( sin, eval(e->e.o.right)));
      case SQRT:	 return (fn1_eval( sqrt, eval(e->e.o.right)));
      case TAN:	 return (fn1_eval( tan, eval(e->e.o.right)));
      case DTR:	 return (dtr(eval(e->e.o.right)));
      case RTD:	 return (rtd(eval(e->e.o.right)));
      case RND:	 
	  {
	     double temp;
	     temp = eval(e->e.o.right);
	     return(temp-floor(temp) < 0.5 ?
		    floor(temp) : ceil(temp));
	  }
      case FV:
      case PV:
      case PMT:	return(finfunc(e->op,eval(e->e.o.left),
			       eval(e->e.o.right->e.o.left),
			       eval(e->e.o.right->e.o.right)));
      case HOUR:	 return (dotime(HOUR, eval(e->e.o.right)));
      case MINUTE:	 return (dotime(MINUTE, eval(e->e.o.right)));
      case SECOND:	 return (dotime(SECOND, eval(e->e.o.right)));
      case MONTH:	 return (dotime(MONTH, eval(e->e.o.right)));
      case DAY:	 return (dotime(DAY, eval(e->e.o.right)));
      case YEAR:	 return (dotime(YEAR, eval(e->e.o.right)));
      case NOW:	 return (dotime(NOW, (double)0.0));
      case STON:	 return (doston(seval(e->e.o.right)));
      case EQS:        return (doeqs(seval(e->e.o.right),seval(e->e.o.left)));
      case LMAX:	 return dolmax(e);
      case LMIN:	 return dolmin(e);
      case NVAL:       return (donval(seval(e->e.o.left),eval(e->e.o.right)));
     }
   
   slsc_error ("Illegal numeric expression");
   exprerr = 1;
   return((double)0.0);	/* Quiet a questionable compiler complaint */
}

static int Float_Exception_Error;

static void slsc_eval_fpe (int sig) /* Trap for FPE errors in eval */
{
   Float_Exception_Error = 1;
   signal (SIGFPE, slsc_eval_fpe);
}


double fn1_eval (double (*fn)(double), double arg)
{
   double res;
   errno = 0;
   res = (*fn)(arg);
   if(errno)
     slsc_eval_fpe (0);
   
   return res;
}

double fn2_eval( double (*fn)(double, double), double arg1, double arg2)
{
   double res;
   errno = 0;
   res = (*fn)(arg1, arg2);
   if(errno)
     slsc_eval_fpe (0);
   
   return res;
}

/* 
 * Rules for string functions:
 * Take string arguments which they SLFREE.
 * All returned strings are assumed to be xalloced.
 */
static char *docat (char *s1, char *s2)
{
   register char *p;
   char *arg1, *arg2;
   
   if (!s1 && !s2)
     return(0);
   arg1 = s1 ? s1 : "";
   arg2 = s2 ? s2 : "";
   p = SLMALLOC((unsigned)(strlen(arg1)+strlen(arg2)+1));
   (void) strcpy(p, arg1);
   (void) strcat(p, arg2);
   if (s1)
     SLFREE(s1);
   if (s2)
     SLFREE(s2);
   return(p);
}

static char *dodate (long tloc)
{
   char *tp;
   char *p;
   
   tp = ctime(&tloc);
   tp[24] = 0;
   p = SLMALLOC((unsigned)25);
   (void) strcpy(p, tp);
   return(p);
}


static char *dofmt (char *fmtstr, double v)
{
   char buff[1024];
   char *p;
   
   if (!fmtstr)
     return(0);
   (void)sprintf(buff, fmtstr, v);
   p = SLMALLOC((unsigned)(strlen(buff)+1));
   (void) strcpy(p, buff);
   SLFREE(fmtstr);
   return(p);
}


/*
 * Given a command name and a value, run the command with the given value and
 * read and return its first output line (only) as an allocated string, always
 * a copy of prevstr, which is set appropriately first unless external
 * functions are disabled, in which case the previous value is used.  The
 * handling of prevstr and freeing of command is tricky.  Returning an
 * allocated string in all cases, even if null, insures cell expressions are
 * written to files, etc.
 */

#ifdef VMS
char *doext (char *command, double value)
{
   slsc_error("Warning: External functions unavailable on VMS");
   if (command)
     SLFREE(command);
   return (strcpy (SLMALLOC((unsigned) 1), "\0"));
}

#else /* VMS */

static char *doext (char *command, double value)
{
   static char *prevstr = 0;	/* previous result */
   char buff[1024];		/* command line/return, not permanently alloc */
   
   if (!prevstr) 
     {
	prevstr = SLMALLOC((unsigned)1);
	*prevstr = 0;
     }
   if (!extfunc)    
     {
	slsc_error ("Warning: external functions disabled; using %s value",
		    prevstr ? "previous" : "null");
	
	if (command) SLFREE (command);
     }
   else 
     {
	if (prevstr) SLFREE (prevstr);		/* no longer needed */
	prevstr = 0;
	
	if ((! command) || (! *command)) 
	  {
	     slsc_error ("Warning: external function given null command name");
	     if (command) SLFREE (command);
	  }
	else 
	  {
	     FILE *pp;
	     
	     (void) sprintf (buff, "%s %g", command, value); /* build cmd line */
	     SLFREE (command);
	     
	     slsc_error ("Running external function...");
	     (void) SLsmg_refresh ();
	     
	     if ((pp = popen (buff, "r")) == (FILE *) NULL)	/* run it */
	       slsc_error ("Warning: running \"%s\" failed", buff);
	     else 
	       {
		  if (fgets (buff, 1024, pp) == NULL)	/* one line */
		    slsc_error ("Warning: external function returned nothing");
		  else 
		    {
		       char *cp;
		       
		       sc_clear_message ();				/* erase notice */
		       buff[1023] = 0;
		       
		       if (NULL != (cp = strchr (buff, '\n'))) *cp = 0;
		   /* contains newline -- end string there */
		       
		       (void) strcpy (prevstr =
				      SLMALLOC ((unsigned) (strlen (buff) + 1)), buff);
			 /* save alloc'd copy */
		    }
		  (void) pclose (pp);
		  
	       }
	     /* else */
	  }
	/* else */
     }
   /* else */
   return (strcpy (SLMALLOC ((unsigned) (strlen (prevstr) + 1)), prevstr));
}

#endif /* VMS */


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's string value, if any.  Even if none,
 * still allocate and return a null string so the cell has a label value so
 * the expression is saved in a file, etc.
 */

static char *dosval (char *colstr, double rowdoub)
{
   struct ent *ep;
   char *label;
   
   label = (ep = getent (colstr, rowdoub)) ? (ep -> label) : "";
   return (strcpy (SLMALLOC ((unsigned) (strlen (label) + 1)), label));
}


/*
 * Substring:  Note that v1 and v2 are one-based to users, but zero-based
 * when calling this routine.
 */

static char *dosubstr (char *s, int v1, int v2)
{
   register char *s1, *s2;
   char *p;
   
   if (!s)
     return(0);
   
   if (v2 >= strlen (s))		/* past end */
     v2 =  strlen (s) - 1;		/* to end   */
   
   if (v1 < 0 || v1 > v2) 
     {
	/* out of range, return null string */
	SLFREE(s);
	p = SLMALLOC((unsigned)1);
	p[0] = 0;
	return(p);
     }
   s2 = p = SLMALLOC((unsigned)(v2-v1+2));
   s1 = &s[v1];
   for(; v1 <= v2; s1++, s2++, v1++)
     *s2 = *s1;
   *s2 = 0;
   SLFREE(s);
   return(p);
}

static char *seval (struct enode *se)
{
   register char *p;
   
   if (se==0) return 0;
   switch (se->op) 
     {
      case O_SCONST: p = SLMALLOC((unsigned)(strlen(se->e.s)+1));
	(void) strcpy(p, se->e.s);
	return(p);
      case O_VAR:    
	  {
	     struct ent *ep;
	     ep = se->e.v.vp;
	     
	     if (!ep->label)
	       return(0);
	     p = SLMALLOC((unsigned)(strlen(ep->label)+1));
	     (void) strcpy(p, ep->label);
	     return(p);
	  }
      case '#':    return(docat(seval(se->e.o.left), seval(se->e.o.right)));
      case 'f':    return(seval(se->e.o.right));
      case '?':    return(eval(se->e.o.left) ? seval(se->e.o.right->e.o.left)
			  : seval(se->e.o.right->e.o.right));
      case DATE:   return(dodate((long)(eval(se->e.o.right))));
      case FMT:    return(dofmt(seval(se->e.o.left), eval(se->e.o.right)));
      case STINDEX:
	  {	register r,c;
	     register maxr, maxc;
	     register minr, minc;
	     maxr = se->e.o.right->e.r.right.vp -> row;
	     maxc = se->e.o.right->e.r.right.vp -> col;
	     minr = se->e.o.right->e.r.left.vp -> row;
	     minc = se->e.o.right->e.r.left.vp -> col;
	     if (minr>maxr) r = maxr, maxr = minr, minr = r;
	     if (minc>maxc) c = maxc, maxc = minc, minc = c;
	     return dostindex(eval(se->e.o.left), minr, minc, maxr, maxc);
	  }
      case EXT:    return(doext(seval(se->e.o.left), eval(se->e.o.right)));
      case SVAL:   return(dosval(seval(se->e.o.left), eval(se->e.o.right)));
      case SUBSTR: return(dosubstr(seval(se->e.o.left),
				   (int)eval(se->e.o.right->e.o.left) - 1,
				   (int)eval(se->e.o.right->e.o.right) - 1));
      default:
	slsc_error ("Illegal string expression");
	exprerr = 1;
	return(0);
     }
}

/*
 * The graph formed by cell expressions which use other cells's values is not
 * evaluated "bottom up".  The whole table is merely re-evaluated cell by cell,
 * top to bottom, left to right, in RealEvalAll().  Each cell's expression uses
 * constants in other cells.  However, RealEvalAll() notices when a cell gets a
 * new numeric or string value, and reports if this happens for any cell.
 * EvalAll() repeats calling RealEvalAll() until there are no changes or the
 * evaluation count expires.
 */

int propagation = 3;	/* max number of times to try calculation */

void sc_set_iterations (int i)
{
   if(i < 1)
     {
	slsc_error("iteration count must be at least 1");
	propagation = 1;
     }
   else propagation = i;
}


static void sc_real_eval_one (register struct ent *p, int i, int j, int *chgct)
{
   if (p->flags & IS_STREXPR)
     {
	char *v = seval(p->expr);
	
	if (((v == NULL) && (p->label == NULL))
	    || Float_Exception_Error)
	  return;
	
	/* The code originally contained || instead of && but it makes no
	 * sense to me. --JED */
	if ((p->label != NULL) && (v != NULL) && strcmp(v, p->label))
	  {
	     (*chgct)++;
	     p->flags |= IS_CHANGED;
	  }
	/* apparantly seval mallocs a new result so we have to free current
	 * one even though they might be identical */
	if(NULL != p->label) SLFREE(p->label);
	p->label = v;
     }
   else
     {
	double v;
	
	v = eval (p->expr);
	if (Float_Exception_Error) return;
	
	if ((fabs (v - p->v) > Sc_Epsilon) && !Float_Exception_Error)
	  {
	     p->v = v; (*chgct)++;
	     p->flags |= IS_CHANGED | IS_VALID;
	  }
     }
}

/*
 * Evaluate all cells which have expressions and alter their numeric or string
 * values.  Return the number of cells which changed.
 */

static int sc_real_eval_all (void)
{
   register int i,j;
   int chgct = 0;
   register struct ent *p;
   
   (void) signal(SIGFPE, slsc_eval_fpe);
   
   if (calc_order == BYROWS)
     {
	for (i=0; i <= maxrow; i++)
	  {
	     for (j=0; j <= maxcol; j++)
	       {
		  p = tbl[i][j];
		  if ((p != NULL) && (p->expr != NULL))
		    sc_real_eval_one(p,i,j, &chgct);
		  if (Float_Exception_Error)
		    {
		       slsc_error("Floating point exception %s", sc_v_name( i, j));
		       goto done;
		    }
	       }
	  }
     }
   else if ( calc_order == BYCOLS )
     {
	for (j=0; j<=maxcol; j++)
	  {
	     for (i=0; i<=maxrow; i++)
	       {
		  if ((p=tbl[i][j]) && p->expr) sc_real_eval_one(p,i,j, &chgct);
		  if (Float_Exception_Error)
		    {
		       slsc_error("Floating point exception %s", sc_v_name( i, j));
		       goto done;
		    }
	       }
	  }
     }
   else slsc_error("Internal error calc_order");
   
   done:
   Float_Exception_Error = 0;
   
   (void) signal(SIGFPE, sc_quit);
   return chgct;
}

void sc_eval_all (void)
{
   int lastcnt = 1, repct = 0;
   
   while ((lastcnt > 0) && (repct++ < propagation))
     {
	lastcnt = sc_real_eval_all();
     }
   
   if ((propagation > 1) && (lastcnt > 0))
     slsc_error("Still changing after %d iterations", propagation-1);
}



struct enode *sc_new_enode (int op, struct enode *a1, struct enode *a2)
{
   register struct enode *p;
   p = (struct enode *) SLMALLOC ((unsigned)sizeof (struct enode));
   p->op = op;
   p->e.o.left = a1;
   p->e.o.right = a2;
   return p;
}

struct enode *new_var(int op, struct ent_ptr a1)
{
   register struct enode *p;
   p = (struct enode *) SLMALLOC ((unsigned)sizeof (struct enode));
   p->op = op;
   p->e.v = a1;
   return p;
}

struct enode *new_range(int op, struct range_s a1)
{
   register struct enode *p;
   p = (struct enode *) SLMALLOC ((unsigned)sizeof (struct enode));
   p->op = op;
   p->e.r = a1;
   return p;
}

struct enode *new_const(int op, double a1)
{
   register struct enode *p;
   p = (struct enode *) SLMALLOC ((unsigned)sizeof (struct enode));
   p->op = op;
   p->e.k = a1;
   return p;
}

struct enode *new_str (char *s)
{
   register struct enode *p;
   
   p = (struct enode *) SLMALLOC ((unsigned)sizeof(struct enode));
   p->op = O_SCONST;
   p->e.s = s;
   return(p);
}

static void copyrtv (int vr, int vc, int minsr, int minsc, int maxsr, int maxsc)
{
   register struct ent *p;
   register struct ent *n;
   register int sr, sc;
   register int dr, dc;
   
   for (dr=vr, sr=minsr; sr<=maxsr; sr++, dr++)
     for (dc=vc, sc=minsc; sc<=maxsc; sc++, dc++) 
     {
	n = lookat (dr, dc);
	(void) sc_clearent(n);
	if (NULL != (p = tbl[sr][sc]))
	  copyent( n, p, dr - sr, dc - sc);
     }
}

void sc_copy (struct ent *dv1, struct ent *dv2, struct ent *v1, struct ent *v2)
{
   int minsr, minsc;
   int maxsr, maxsc;
   int mindr, mindc;
   int maxdr, maxdc;
   int vr, vc;
   int r, c;
   
   mindr = dv1->row;
   mindc = dv1->col;
   maxdr = dv2->row;
   maxdc = dv2->col;
   if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
   if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
   maxsr = v2->row;
   maxsc = v2->col;
   minsr = v1->row;
   minsc = v1->col;
   if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
   if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
   if (maxdr >= MAXROWS  ||
       maxdc >= MAXCOLS) 
     {
	slsc_error ("The table can't be any bigger");
	return;
     }
   erase_area(mindr, mindc, maxdr, maxdc);
   if (minsr == maxsr && minsc == maxsc) 
     {
	/* Source is a single cell */
	for(vr = mindr; vr <= maxdr; vr++)
	  for (vc = mindc; vc <= maxdc; vc++)
	    copyrtv(vr, vc, minsr, minsc, maxsr, maxsc);
     }
   else if (minsr == maxsr) 
     {
	/* Source is a single row */
	for (vr = mindr; vr <= maxdr; vr++)
	  copyrtv(vr, mindc, minsr, minsc, maxsr, maxsc);
     }
   else if (minsc == maxsc) 
     {
	/* Source is a single column */
	for (vc = mindc; vc <= maxdc; vc++)
	  copyrtv(mindr, vc, minsr, minsc, maxsr, maxsc);
     }
   else 
     {
	/* Everything else */
	copyrtv(mindr, mindc, minsr, minsc, maxsr, maxsc);
     }
   sync_refs();
}


void sc_eraser(struct ent *v1, struct ent *v2)
{
   FullUpdate++;
   sc_flush_saved();
   erase_area(v1->row, v1->col, v2->row, v2->col);
   sync_refs();
}

/* Goto subroutines */

static void g_free (void)
{
   switch (gs.g_type) 
     {
      case G_STR: SLFREE(gs.g_s); break;
      default: break;
     }
   gs.g_type = G_NONE;
}

static void sc_generic_search (int type, double n, char *str)
{
   register struct ent *p;
   register int r,c;
   char *err;
   
   g_free();
   
   if ((gs.g_type = type) == G_NUM)
     {
	gs.g_n = n;
	err = "Number not found.";
     }
   else
     {
	gs.g_s = str;
	err = "String not found.";
     }
   
   r = currow;
   c = curcol;
   
   while (1)
     {
	if (c < maxcol)
	  c++;
	else
	  {
	     if (r < maxrow)
	       {
		  while ((++r < maxrow) && row_hidden[r]) /* */;
		  c = 0;
	       }
	     else  /* At end of so go back to the top */
	       {
		  r = 0;
		  c = 0;
	       }
	  }
	
	if ((r == currow) && (c == curcol))
	  {
	     slsc_error (err);
	     return;
	  }
	
	if (col_hidden[c] || !CELL_IS_VALID(p, r, c)) continue;
	if (type == G_NUM)
	  {
	     if (p->v == n) break;
	  }
	else if (strcmp (p->label, str) == 0) break;
     }
   
   currow = r;
   curcol = c;
}

void sc_num_search (double n)
{
   sc_generic_search (G_NUM, n, NULL);
}


void sc_str_search (char *s)
{
   sc_generic_search (G_STR, 0.0, s);
}


void sc_moveto(int row, int col)
{
   currow = row;
   curcol = col;
   g_free();
   gs.g_type = G_CELL;
   gs.g_row = currow;
   gs.g_col = curcol;
}

void sc_go_last (void)
{
   switch (gs.g_type) 
     {
      case G_NONE:
	slsc_error("Nothing to repeat"); break;
      case G_NUM:
	sc_num_search(gs.g_n);
	break;
      case  G_CELL:
	sc_moveto(gs.g_row, gs.g_col);
	break;
      case  G_STR:
	gs.g_type = G_NONE;	/* Don't free the string */
	sc_str_search(gs.g_s);
	break;
	
      default: slsc_error("go_last: internal error");
     }
}



void sc_fill (struct ent *v1, struct ent *v2, double start, double inc)
{
   register r,c;
   register struct ent *n;
   int maxr, maxc;
   int minr, minc;
   
   maxr = v2->row;
   maxc = v2->col;
   minr = v1->row;
   minc = v1->col;
   if (minr>maxr) r = maxr, maxr = minr, minr = r;
   if (minc>maxc) c = maxc, maxc = minc, minc = c;
   if (maxr >= MAXROWS) maxr = MAXROWS-1;
   if (maxc >= MAXCOLS) maxc = MAXCOLS-1;
   if (minr < 0) minr = 0;
   if (minr < 0) minr = 0;
   
   FullUpdate++;
   if( calc_order == BYROWS ) 
     {
	for (r = minr; r<=maxr; r++)
	  for (c = minc; c<=maxc; c++) 
	  {
	     n = lookat (r, c);
	     (void) sc_clearent(n);
	     n->v = start;
	     start += inc;
	     n->flags |= (IS_CHANGED | IS_VALID);
	  }
     }
   else if ( calc_order == BYCOLS ) 
     {
	for (c = minc; c<=maxc; c++)
	  for (r = minr; r<=maxr; r++) 
	  {
	     n = lookat (r, c);
	     (void) sc_clearent(n);
	     n->v = start;
	     start += inc;
	     n->flags |= (IS_CHANGED | IS_VALID);
	  }
     }
   else slsc_error(" Internal error calc_order");
}
static int constant (register struct enode *e)
{
   return ((e == 0)
	   || ((e -> op) == O_CONST)
	   || ((e -> op) == O_SCONST)
	   || (((e -> op) != O_VAR)
	       && (((e -> op) & REDUCE) != REDUCE)
	       && constant (e -> e.o.left)
	       && constant (e -> e.o.right)
	       && (e -> op != EXT)	 /* functions look like constants but aren't */
	       && (e -> op != NVAL)
	       && (e -> op != SVAL)
	       && (e -> op != NOW)));
}

void sc_let (struct ent *v, struct enode *e)
{
   double val;
   unsigned int isconst = constant(e);
   
   /* If we are loading, we do not want to evaluate any formulas.  However,
    * if the expression is a constant one, set it. */
   if (loading && !isconst) val = 0.0;
   else
     {
	exprerr = 0;
	
	Float_Exception_Error = 0;
	(void) signal(SIGFPE, slsc_eval_fpe);
	
	val = eval(e);
	if (Float_Exception_Error)
	  {
	     slsc_error ("Floating point exception in cell %s", sc_v_name(v->row, v->col));
	     Float_Exception_Error = 0;
	     val = 0.0;
	  }
	
	(void) signal(SIGFPE, sc_quit);
	if (exprerr) 
	  {
	     sc_efree(e);
	     return;
	  }
     }
   
   if (isconst)
     {
	if (!loading)
	  v->v = val * prescale;
	else
	  v->v = val;
	
	if (!(v->flags & IS_STREXPR))
	  {
	     sc_efree (v->expr);
	     v->expr = 0;
	  }
	sc_efree(e);
     }
   else
     {
	sc_efree (v->expr);
	v->expr = e;
	v->flags &= ~IS_STREXPR;
     }
   
   v->flags |= (IS_CHANGED | IS_VALID);
   Sc_Changed++;
   modflg++;
}

static void label (struct ent *v, char *s, int flushdir)
{
   if (v) 
     {
	if (flushdir==0 && v->flags & IS_VALID) 
	  {
	     register struct ent *tv;
	     if (v->col>0 && ((tv=lookat(v->row,v->col-1))->flags&IS_VALID)==0)
	       v = tv, flushdir = 1;
	     else if (((tv=lookat (v->row,v->col+1))->flags&IS_VALID)==0)
	       v = tv, flushdir = -1;
	     else flushdir = -1;
	  }
	if (v->label) SLFREE((char *)(v->label));
	if (s && s[0]) 
	  {
	     v->label = SLMALLOC ((unsigned)(strlen(s)+1));
	     (void) strcpy (v->label, s);
	  }
	else
	  v->label = 0;
	if (flushdir<0) v->flags |= IS_LEFTFLUSH;
	else v->flags &= ~IS_LEFTFLUSH;
	FullUpdate++;
	modflg++;
     }
}

void sc_slet (struct ent *v, struct enode *se, int flushdir)
{
   char *p;
   
   exprerr = 0;
   (void) signal(SIGFPE, slsc_eval_fpe);
   
   p = seval(se);
   if (Float_Exception_Error)
     {
	slsc_error ("Floating point exception in cell %s", sc_v_name(v->row, v->col));
	p = "";
	Float_Exception_Error = 0;
     }
   
   (void) signal(SIGFPE, sc_quit);
   if (exprerr) 
     {
	sc_efree(se);
	return;
     }
   if (constant(se)) 
     {
	label(v, p, flushdir);
	if (p) SLFREE(p);
	sc_efree(se);
	if (v->flags & IS_STREXPR) 
	  {
	     sc_efree (v->expr);
	     v->expr = NULL;
	     v->flags &= ~IS_STREXPR;
	  }
	return;
     }
   sc_efree (v->expr);
   v->expr = se;
   v->flags |= (IS_CHANGED | IS_STREXPR);
   if (flushdir<0) v->flags |= IS_LEFTFLUSH;
   else v->flags &= ~IS_LEFTFLUSH;
   FullUpdate++;
   Sc_Changed++;
   modflg++;
}

void sc_hide_row (int arg)
{
   if (arg < 0) 
     {
	slsc_error("Invalid Range");
	return;
     }
   if (arg > MAXROWS-2) 
     {
	slsc_error("You can't hide the last row");
	return;
     }
   FullUpdate++;
   row_hidden[arg] = 1;
}

void sc_hide_col (int arg)
{
   if (arg < 0) 
     {
	slsc_error("Invalid Range");
	return;
     }
   if (arg > MAXCOLS-2) 
     {
	slsc_error("You can't hide the last col");
	return;
     }
   FullUpdate++;
   col_hidden[arg] = 1;
}

void sc_clearent (struct ent *v)
{
   if (!v)
     return;
   label(v,"",-1);
   v->v = 0;
   if (v->expr)
     sc_efree(v->expr);
   v->expr = 0;
   v->flags |= (IS_CHANGED);
   v->flags &= ~(IS_VALID);
   Sc_Changed++;
   modflg++;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */


void sc_efree (register struct enode *e)
{
   if (e) 
     {
	if (e->op != O_VAR && e->op !=O_CONST && e->op != O_SCONST
	    && (e->op & REDUCE) != REDUCE) 
	  {
	     sc_efree(e->e.o.left);
	     sc_efree(e->e.o.right);
	  }
	if (e->op == O_SCONST && e->e.s)
	  SLFREE(e->e.s);
	SLFREE ((char *)e);
     }
}


static void decodev (struct ent_ptr v)
{
   register struct range *r;
   
   if (!v.vp) (void)sprintf (line+linelim,"VAR?");
   else if (NULL != (r = sc_find_range((char *)0, 0, v.vp, v.vp)))
     (void)sprintf(line+linelim, "%s", r->r_name);
   else
     (void)sprintf (line+linelim, "%s%s%s%d",
		    v.vf & FIX_COL ? "$" : "",
		    sc_coltoa(v.vp->col),
		    v.vf & FIX_ROW ? "$" : "",
		    v.vp->row);
   linelim += strlen (line+linelim);
}

char *sc_coltoa(int col)
{
   static char rname[3];
   register char *p = rname;
   
   if (col > 25) 
     {
	*p++ = col/26 + 'A' - 1;
	col %= 26;
     }
   *p++ = col+'A';
   *p = 0;
   return(rname);
}

static void decompile(struct enode *, int);

static void one_arg (char *s, struct enode *e)
{
   while ((line[linelim] = *s++) != 0) linelim++;
   
   decompile (e->e.o.right, 0);
   line[linelim++] = ')';
}

static void two_arg (char *s, struct enode *e)
{
   while ((line[linelim] = *s++) != 0) linelim++;
   
   decompile (e->e.o.left, 0);
   line[linelim++] = ',';
   decompile (e->e.o.right, 0);
   line[linelim++] = ')';
}

static void three_arg(char *s, struct enode *e)
{
   while ((line[linelim] = *s++) != 0) linelim++;
   decompile (e->e.o.left, 0);
   line[linelim++] = ',';
   decompile (e->e.o.right->e.o.left, 0);
   line[linelim++] = ',';
   decompile (e->e.o.right->e.o.right, 0);
   line[linelim++] = ')';
}

static void decompile_list (struct enode *);

static void list_arg (char *s, struct enode *e)
{
   while ((line[linelim] = *s++) != 0) linelim++;
   
   decompile (e->e.o.right, 0);
   line[linelim++] = ',';
   decompile_list(e->e.o.left);
   line[linelim - 1] = ')';
}

static void range_arg (char *s, struct enode *e)
{
   struct range *r;
   
   while ((line[linelim] = *s++) != 0) linelim++;
   
   if (NULL != (r = sc_find_range((char *)0, 0, e->e.r.left.vp,
				  e->e.r.right.vp))) 
     {
	(void)sprintf(line+linelim, "%s", r->r_name);
	linelim += strlen(line+linelim);
     }
   else 
     {
	decodev (e->e.r.left);
	line[linelim++] = ':';
	decodev (e->e.r.right);
     }
   line[linelim++] = ')';
}


/*
 *	To make list elements come out in the same order
 *	they were entered, we must do a depth-first eval
 *	of the ELIST tree
 */
static void decompile_list (struct enode *p)
{
   if (p == NULL) return;
   decompile_list(p->e.o.left);	/* depth first */
   decompile(p->e.o.right, 0);
   line[linelim++] = ',';
}


static void index_arg(char *s, struct enode *e)
{
   while ((line[linelim] = *s++) != 0) linelim++;
   
   decompile( e-> e.o.left, 0 );
   range_arg(", ", e->e.o.right);
}

static void decompile(struct enode *e, int priority)
{
   register char *s;
   if (e) 
     {
	int mypriority;
	switch (e->op) 
	  {
	   default: mypriority = 99; break;
	   case '?': mypriority = 1; break;
	   case ':': mypriority = 2; break;
	   case '|': mypriority = 3; break;
	   case '&': mypriority = 4; break;
	   case '<': case '=': case '>': mypriority = 6; break;
	   case '+': case '-': case '#': mypriority = 8; break;
	   case '*': case '/': case '%': mypriority = 10; break;
	   case '^': mypriority = 12; break;
	  }
	if (mypriority<priority) line[linelim++] = '(';
	switch (e->op) 
	  {
	   case 'f':
	     s = "fixed ";
	     while ((line[linelim] = *s++) != 0) linelim++;
	     decompile (e->e.o.right, 30);
	     break;
	   case 'm':	line[linelim++] = '-';
	     decompile (e->e.o.right, 30);
	     break;
	   case '~':	line[linelim++] = '~';
	     decompile (e->e.o.right, 30);
	     break;
	   case 'v':	decodev (e->e.v);
	     break;
	   case 'k':	(void)sprintf (line+linelim,"%.15g",e->e.k);
	     linelim += strlen (line+linelim);
	     break;
	   case '$':	(void)sprintf (line+linelim, "\"%s\"", e->e.s);
	     linelim += strlen(line+linelim);
	     break;
	     
	   case REDUCE | '+': range_arg( "@sum(", e); break;
	   case REDUCE | 'S': range_arg( "@rsum(", e); break;
	   case REDUCE | '*': range_arg( "@prod(", e); break;
	   case REDUCE | 'P': range_arg( "@rprod(", e); break;
	   case REDUCE | 'a': range_arg( "@avg(", e); break;
	   case REDUCE | 'A': range_arg( "@ravg(", e); break;
	   case REDUCE | 's': range_arg( "@stddev(", e); break;
	   case REDUCE | 'D': range_arg( "@rstddev(", e); break;
	   case REDUCE | 'M': range_arg( "@rmax(", e); break;
	   case REDUCE | 'm': range_arg( "@rmin(", e); break;
	   case REDUCE | MAX_FUN: range_arg( "@max(", e); break;
	   case REDUCE | MIN_FUN: range_arg( "@min(", e); break;
	     
	   case ACOS:	one_arg( "@acos(", e); break;
	   case ASIN:	one_arg( "@asin(", e); break;
	   case ATAN:	one_arg( "@atan(", e); break;
	   case ATAN2:	two_arg( "@atan2(", e); break;
	   case CEIL:	one_arg( "@ceil(", e); break;
	   case COS:	one_arg( "@cos(", e); break;
	   case EXP:	one_arg( "@exp(", e); break;
	   case FABS:	one_arg( "@fabs(", e); break;
	   case FLOOR:	one_arg( "@floor(", e); break;
	   case HYPOT:	two_arg( "@hypot(", e); break;
	   case LOG:	one_arg( "@ln(", e); break;
	   case LOG10:	one_arg( "@log(", e); break;
	   case POW:	two_arg( "@pow(", e); break;
	   case SIN:	one_arg( "@sin(", e); break;
	   case SQRT:	one_arg( "@sqrt(", e); break;
	   case TAN:	one_arg( "@tan(", e); break;
	   case DTR:	one_arg( "@dtr(", e); break;
	   case RTD:	one_arg( "@rtd(", e); break;
	   case RND:	one_arg( "@rnd(", e); break;
	   case HOUR:	one_arg( "@hour(", e); break;
	   case MINUTE:	one_arg( "@minute(", e); break;
	   case SECOND:	one_arg( "@second(", e); break;
	   case MONTH:	one_arg( "@month(", e); break;
	   case DAY:	one_arg( "@day(", e); break;
	   case YEAR:	one_arg( "@year(", e); break;
	   case DATE:	one_arg( "@date(", e); break;
	   case STON:	one_arg( "@ston(", e); break;
	   case FMT:	two_arg( "@fmt(", e); break;
	   case EQS:	two_arg( "@eqs(", e); break;
	   case NOW:
	     s = "@now";
	     while ((line[linelim] = *s++) != 0) linelim++;
	     break;
	     
	   case LMAX:	list_arg("@max(", e); break;
	   case LMIN: 	list_arg("@min(", e); break;
	   case FV:	three_arg("@fv(", e); break;
	   case PV:	three_arg("@pv(", e); break;
	   case PMT:	three_arg("@pmt(", e); break;
	   case NVAL:	two_arg("@nval(", e); break;
	   case SVAL:	two_arg("@sval(", e); break;
	   case EXT:	two_arg("@ext(", e); break;
	   case SUBSTR:	three_arg("@substr(", e); break;
	   case STINDEX:	index_arg("@stindex(", e); break;
	   case INDEX:	index_arg("@index(", e); break;
	   case LOOKUP:	index_arg("@lookup(", e); break;
	     
	   default:	decompile (e->e.o.left, mypriority);
	     line[linelim++] = e->op;
	     decompile (e->e.o.right, mypriority+1);
	     break;
	     
	  }
	if (mypriority<priority) line[linelim++] = ')';
     }
   else line[linelim++] = '?';
}



void editv (int row, int col)
{
   register struct ent *p;
   
   p = lookat (row, col);
   (void)sprintf (line, "let %s = ", sc_v_name(row, col));
   linelim = strlen(line);
   if ((p->flags & IS_STREXPR) || (p->expr == NULL))
     {
	(void)sprintf (line+linelim, "%.15g", p->v);
	linelim += strlen (line+linelim);
     }
   else 
     {
        editexp(row,col);
     }
}

void editexp(int row, int col)
{
   register struct ent *p;
   
   p = lookat (row, col);
   decompile (p->expr, 0);
   line[linelim] = 0;
}

void edits (int row, int col)
{
   register struct ent *p;
   
   p = lookat (row, col);
   (void)sprintf (line, "%sstring %s = ",
		  ((p->flags&IS_LEFTFLUSH) ? "left" : "right"),
		  sc_v_name(row, col));
   linelim = strlen(line);
   if (p->flags & IS_STREXPR && (p->expr != NULL)) 
     {
	editexp(row, col);
     }
   else if (p->label) 
     {
        (void)sprintf (line+linelim, "\"%s\"", p->label);
        linelim += strlen (line+linelim);
     }
   else 
     {
        (void)sprintf (line+linelim, "\"");
        linelim += 1;
     }
}
