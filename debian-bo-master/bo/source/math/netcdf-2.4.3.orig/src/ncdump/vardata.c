/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncdump/vardata.c,v 1.43 1996/06/07 14:46:14 steve Exp $
 *********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#ifndef NO_FLOAT_H
#include <float.h>		/* for FLT_EPSILON, DBL_EPSILON */
#endif /* NO_FLOAT_H */

#include <netcdf.h>
#include "ncdump.h"
#include "dumplib.h"
#include "vardata.h"

#ifndef NO_HAVE_PROTOTYPES 
static float float_epsilon(void);
static double double_epsilon(void);
static void init_epsilons(void);
static void printval(char* sout, char* fmt, struct ncvar* varp, void* valp);
static void pr_vals(struct ncvar* vp, long len, char* fmt, boolean more,
		    boolean lastrow, void* vals);
static void lastdelim(boolean  more, boolean lastrow);
static void annotate(struct ncvar* vp, struct fspec* fsp, long* cor, long iel);
static void pr_cvals(struct ncvar* vp, long len, char* fmt, boolean more,
		     boolean lastrow, void* vals, struct fspec* fsp, long* cor);
static int upcorner(long* dims, int ndims, long* odom, long* add);
#endif

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

static float float_eps;
static double double_eps;

static float
float_epsilon()
{
    float float_eps;
#ifndef NO_FLOAT_H
#ifdef FLT_EPSILON
    float_eps = FLT_EPSILON;
#define UD_HAVE_FLT_EPSILON
#endif
#endif
#ifndef UD_HAVE_FLT_EPSILON
    {
	float etop, ebot, eps;
	float one = 1.0;
	float two = 2.0;
	etop = 1.0;
	ebot = 0.0;
	eps = ebot + (etop - ebot)/two;
	while (eps != ebot && eps != etop) {
	    float epsp1;

	    epsp1 = one + eps;
	    if (epsp1 > one)
		etop = eps;
	    else
		ebot = eps;
	    eps = ebot + (etop - ebot)/two;
	}
	float_eps = two * etop;
    }
#endif /* UD_HAVE_FLT_EPSILON */
    return float_eps;
}


static double
double_epsilon()
{
    double double_eps;
#ifndef NO_FLOAT_H
#ifdef DBL_EPSILON
    double_eps = DBL_EPSILON;
#define UD_HAVE_DBL_EPSILON
#endif
#endif
#ifndef UD_HAVE_DBL_EPSILON
    {
	double etop, ebot, eps;
	double one = 1.0;
	double two = 2.0;
	etop = 1.0;
	ebot = 0.0;
	eps = ebot + (etop - ebot)/two;
	while (eps != ebot && eps != etop) {
	    double epsp1;

	    epsp1 = one + eps;
	    if (epsp1 > one)
		etop = eps;
	    else
		ebot = eps;
	    eps = ebot + (etop - ebot)/two;
	}
	double_eps = two * etop;
    }
#endif /* UD_HAVE_DBL_EPSILON */
    return double_eps;
}


static void
init_epsilons()
{
    float_eps = float_epsilon();
    double_eps = double_epsilon();
}


/*
 * Output a single value of a variable, except if there is a fill value for
 * the variable and the value is the fill value, print the fill-value string
 * instead.  Floating-point fill values need only be within machine epsilon of
 * defined fill value.  This never gets called for NC_CHAR data, so it doesn't
 * handle that.
 */
static void
printval(sout, fmt, varp, valp)
    char *sout;			/* string where output goes */
    char *fmt;			/* printf format used for value */
    struct ncvar *varp;		/* variable */
    void *valp;			/* value, interpreted using varp->type */
{
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } vp, fillp;

    switch (varp->type) {
    case NC_BYTE:
	fillp.cp = &varp->fillval.charv;
	vp.cp = (char *)valp;
	if (varp->has_fillval && *fillp.cp == *vp.cp) {
	    (void) sprintf(sout, FILL_STRING);
	} else {
	    (void) sprintf(sout, fmt, *vp.cp);
	}
	break;
      case NC_SHORT:
	fillp.sp = &varp->fillval.shortv;
	vp.sp = (short *)valp;
	if (varp->has_fillval && *fillp.sp == *vp.sp) {
	      (void) sprintf(sout, FILL_STRING);
	} else {
	      (void) sprintf(sout, fmt, *vp.sp);
	}
	break;
      case NC_LONG:
	fillp.lp = &varp->fillval.longv;
	vp.lp = (nclong *)valp;
	if (varp->has_fillval && *fillp.lp == *vp.lp) {
	      (void) sprintf(sout, FILL_STRING);
	} else {
	      (void) sprintf(sout, fmt, *vp.lp);
	}
	break;
      case NC_FLOAT:
	fillp.fp = &varp->fillval.floatv;
	vp.fp = (float *)valp;
#define absval(x)  ( (x) < 0 ? -(x) : (x) )
	if(varp->has_fillval &&
	   (*vp.fp > 0) == (*fillp.fp > 0) && /* prevents potential overflow */
	   (absval(*vp.fp - *fillp.fp) <= absval(float_eps * *fillp.fp))) {
	      (void) sprintf(sout, FILL_STRING);
	} else {
	      (void) sprintf(sout, fmt, *vp.fp);
	}
	break;
      case NC_DOUBLE:
	fillp.dp = &varp->fillval.doublev;
	vp.dp = (double *)valp;
	if(varp->has_fillval &&
	   (*vp.dp > 0) == (*fillp.dp > 0) && /* prevents potential overflow */
	   (absval(*vp.dp - *fillp.dp) <= absval(double_eps * *fillp.dp))) {
	      (void) sprintf(sout, FILL_STRING);
	} else {
	      (void) sprintf(sout, fmt, *vp.dp);
	}
	break;
      default:
	error("printval: bad type");
    }
}


/*
 * Print a row of variable values.  Makes sure output lines aren't too long
 * by judiciously inserting newlines. 
 */
static void
pr_vals(vp, len, fmt, more, lastrow, vals)
     struct ncvar *vp;		/* variable */
     long len;			/* number of values to print */
     char *fmt;			/*
				 * printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as strings
				 * enclosed in quotes.
				 */
     boolean more;		/*
				 * true if more data will follow, so add
				 * trailing comma
				 */
     boolean lastrow;		/*
				 * true if this is the last row for this
				 * variable, so terminate with ";" instead of
				 * ","
				 */
     void *vals;		/* pointer to block of values */
{
    long iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;
    char *sp;
    unsigned char uc;
    char sout[100];		/* temporary string for each encoded output */

    switch (vp->type) {
      case NC_BYTE:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.cp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	printval(sout, fmt, vp, gp.cp++);
	lput(sout);
	break;
      case NC_CHAR:
	gp.cp = (char *) vals;
	if (fmt == 0 || STREQ(fmt,"%s") || STREQ(fmt,"")) { /* as string */
	    Printf("\"");
	    /* adjust len so trailing nulls don't get printed */
	    sp = gp.cp + len;
	    while (len > 0 && *--sp == '\0')
	      len--;
	    for (iel = 0; iel < len; iel++)
	      switch (uc = *gp.cp++ & 0377) {
		case '\b':
		  Printf("\\b");
		  break;
		case '\f':
		  Printf("\\f");
		  break;
		case '\n':	/* generate linebreaks after new-lines */
		  Printf("\\n\",\n    \"");
		  break;
		case '\r':
		  Printf("\\r");
		  break;
		case '\t':
		  Printf("\\t");
		  break;
		case '\v':
		  Printf("\\v");
		  break;
		case '\\':
		  Printf("\\\\");
		  break;
		case '\'':
		  Printf("\\\'");
		  break;
		case '\"':
		  Printf("\\\"");
		  break;
		default:
		  if (isprint(uc))
		    Printf("%c",uc);
		  else
		    Printf("\\%.3o",uc);
		  break;
	      }
	    Printf("\"");
	} else {		/* use format from C_format attribute */
	    for (iel = 0; iel < len-1; iel++) {
		(void) sprintf(sout, fmt, *gp.cp++);
		(void) strcat(sout, ", ");
		lput(sout);
	    }
	    (void) sprintf(sout, fmt, *gp.cp++);
	    lput(sout);
	}
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.sp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	printval(sout, fmt, vp, gp.sp++);
	lput(sout);
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.lp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	printval(sout, fmt, vp, gp.lp++);
	lput(sout);
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.fp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	printval(sout, fmt, vp, gp.fp++);
	lput(sout);
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.dp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	printval(sout, fmt, vp, gp.dp++);
	lput(sout);
	break;
      default:
	error("pr_vals: bad type");
    }
    if (more) {
	lput(", ");
    } else {
	if(lastrow) {
	    lput(" ;");
	    lput("\n");
	} else {
	    lput(",\n");
	    lput("  ");
	}
    }
}


/*
 * print last delimiter in each line before annotation (, or ;)
 */
static void
lastdelim (more, lastrow)
     boolean more;
     boolean lastrow;
{
    if (more) {
	Printf(", ");
    } else {
	if(lastrow) {
	    Printf(";");
	} else {
	    Printf(",");
	}
    }
}


/*
 * Annotates a value in data section with var name and indices in comment
 */
static void
annotate(vp, fsp, cor, iel)
     struct ncvar *vp;		/* variable */
     struct fspec* fsp;		/* formatting specs */
     long cor[];		/* corner coordinates */
     long iel;			/* which element in current row */
{
    int vrank = vp->ndims;
    int id;
    
    /* print indices according to data_lang */
    (void) printf("  // %s(", vp->name);
    switch (fsp->data_lang) {
      case LANG_C:
	/* C variable indices */
	for (id = 0; id < vrank-1; id++)
	  Printf("%d,", cor[id]);
	Printf("%d", cor[id] + iel);
	break;
      case LANG_F:
	/* Fortran variable indices */
	Printf("%d", cor[vrank-1] + iel + 1);
	for (id = vrank-2; id >=0 ; id--) {
	    Printf(",%d", 1 + cor[id]);
	}
	break;
    }
    Printf(")\n    ");
}


/*
 * Print a number of commented variable values, where the comments for each
 * value identify the variable, and each dimension index.
 */
static void
pr_cvals(vp, len, fmt, more, lastrow, vals, fsp, cor)
     struct ncvar *vp;		/* variable */
     long len;			/* number of values to print */
     char *fmt;			/*
				 * printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as strings
				 * enclosed in quotes.
				 */
     boolean more;		/*
				 * true if more data for this row will follow,
				 * so add trailing comma
				 */
     boolean lastrow;		/*
				 * true if this is the last row for this
				 * variable, so terminate with ";" instead of
				 * ","
				 */
     void *vals;		/* pointer to block of values */
     struct fspec* fsp;		/* formatting specs */
     long cor[];		/* corner coordinates */
{
    long iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;
    char *sp;
    unsigned char uc;
    char sout[100];		/* temporary string for each encoded output */

    switch (vp->type) {
      case NC_BYTE:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.cp++);
	    Printf(sout);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	printval(sout, fmt, vp, gp.cp++);
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_CHAR:
	gp.cp = (char *) vals;
	if (fmt == 0 || STREQ(fmt,"%s") || STREQ(fmt,"")) { /* as string */
	    Printf("\"");
	    /* adjust len so trailing nulls don't get printed */
	    sp = gp.cp + len;
	    while (len > 0 && *--sp == '\0')
	      len--;
	    for (iel = 0; iel < len; iel++)
	      switch (uc = *gp.cp++ & 0377) {
		case '\b':
		  Printf("\\b");
		  break;
		case '\f':
		  Printf("\\f");
		  break;
		case '\n':	/* generate linebreaks after new-lines */
		  Printf("\\n\",\n    \"");
		  break;
		case '\r':
		  Printf("\\r");
		  break;
		case '\t':
		  Printf("\\t");
		  break;
		case '\v':
		  Printf("\\v");
		  break;
		case '\\':
		  Printf("\\\\");
		  break;
		case '\'':
		  Printf("\\\'");
		  break;
		case '\"':
		  Printf("\\\"");
		  break;
		default:
		  if (isprint(uc))
		    Printf("%c",uc);
		  else
		    Printf("\\%.3o",uc);
		  break;
	      }
	    Printf("\"");
	    lastdelim (more, lastrow);
	    annotate (vp, fsp, cor, 0L);
	} else {		/* use format from C_format attribute */
	    for (iel = 0; iel < len-1; iel++) {
		Printf(fmt, *gp.cp++);
		Printf(", ");
		annotate (vp, fsp, cor, iel);
	    }
	    Printf(fmt, *gp.cp++);
	    lastdelim (more, lastrow);
	    annotate (vp, fsp, cor, iel);
	}
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.sp++);
	    Printf(sout);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	printval(sout, fmt, vp, gp.sp++);
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.lp++);
	    Printf(sout);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	printval(sout, fmt, vp, gp.lp++);
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.fp++);
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	}
	printval(sout, fmt, vp, gp.fp++);
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    printval(sout, fmt, vp, gp.dp++);
	    Printf(sout);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	}
	printval(sout, fmt, vp, gp.dp++);
	Printf(sout);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      default:
	error("pr_vals: bad type");
    }
}


/*
 * Updates a vector of ints, odometer style.  Returns 0 if odometer
 * overflowed, else 1.
 */
static int
upcorner(dims,ndims,odom,add)
     long *dims;		/* The "odometer" limits for each dimension  */
     int ndims;			/* Number of dimensions */
     long* odom;		/* The "odometer" vector to be updated */
     long* add;			/* A vector to "add" to odom on each update */
{
    int id;
    int ret = 1;

    for (id = ndims-1; id > 0; id--) {
	odom[id] += add[id];
	if(odom[id] >= dims[id]) {
	    odom[id-1]++;
	    odom[id] -= dims[id];
	}
    }
    odom[0] += add[0];
    if (odom[0] >= dims[0])
      ret = 0;
    return ret;
}


int
vardata(vp, vdims, ncid, varid, fsp)
     struct ncvar *vp;		/* variable */
     long vdims[];		/* variable dimension sizes */
     int ncid;			/* netcdf id */
     int varid;			/* variable id */
     struct fspec* fsp;		/* formatting specs */
{
    long cor[MAX_NC_DIMS];	/* corner coordinates */
    long edg[MAX_NC_DIMS];	/* edges of hypercube */
    long add[MAX_NC_DIMS];	/* "odometer" increment to next "row"  */
#define VALBUFSIZ 8192
    double vals[VALBUFSIZ/sizeof(double)] ; /* aligned buffer */
    int gulp = VALBUFSIZ/nctypelen(vp->type);

    int id;
    int ir;
    long nels;
    long ncols;
    long nrows;
    int vrank = vp->ndims;
    static int initeps = 0;

    /* printf format used to print each value */
    char *fmt = get_fmt(ncid, varid, vp->type);

    if (!initeps) {		/* make sure epsilons get initialized */
	init_epsilons();
	initeps = 1;
    }

    nels = 1;
    for (id = 0; id < vrank; id++) {
	cor[id] = 0;
	edg[id] = 1;
	nels *= vdims[id];	/* total number of values for variable */
    }

    if (vrank <= 1) {
	Printf("\n %s = ", vp->name);
	set_indent (strlen(vp->name) + 4);
    } else {
	Printf("\n %s =\n  ", vp->name);
	set_indent (2);
    }

    if (vrank < 1) {
	ncols = 1;
    } else {
	ncols = vdims[vrank-1];	/* size of "row" along last dimension */
	edg[vrank-1] = vdims[vrank-1];
	for (id = 0; id < vrank; id++)
	  add[id] = 0;
	if (vrank > 1)
	  add[vrank-2] = 1;
    }
    nrows = nels/ncols;		/* number of "rows" */
    
    for (ir = 0; ir < nrows; ir++) {
	/*
	 * rather than just printing a whole row at once (which might exceed
	 * the capacity of MSDOS platforms, for example), we break each row
	 * into smaller chunks, if necessary.
	 */
	long corsav;
	long left = ncols;
	boolean lastrow;

	if (vrank > 0) {
	    corsav = cor[vrank-1];
	    if (fsp->brief_data_cmnts != false
		&& vrank > 1
		&& left > 0) {	/* print brief comment with indices range */
		Printf("// %s(",vp->name);
		switch (fsp->data_lang) {
		  case LANG_C:
		    /* print brief comment with C variable indices */
		    for (id = 0; id < vrank-1; id++)
		      Printf("%d,", cor[id]);
		    if (vdims[vrank-1] == 1)
		      Printf("0");
		    else
		      Printf(" 0-%d", vdims[vrank-1]-1);
		    break;
		  case LANG_F:
		    /* print brief comment with Fortran variable indices */
		    if (vdims[vrank-1] == 1)
		      Printf("1");
		    else
		      Printf("1-%d ",vdims[vrank-1]);
		    for (id = vrank-2; id >=0 ; id--) {
			Printf(",%d", 1 + cor[id]);
		    }
		    break;
		}
		Printf(")\n    ");
		set_indent(4);
	    }
	}
	lastrow = (boolean)(ir == nrows-1);
	while (left > 0) {
	    long toget = left < gulp ? left : gulp;
	    if (vrank > 0)
	      edg[vrank-1] = toget;
	    (void) ncvarget (ncid, varid, cor, edg, (void *) vals);
	    if (fsp->full_data_cmnts)
	      pr_cvals(vp, toget, fmt, left > toget, lastrow, (void *) vals,
		       fsp, cor);
	    else
	      pr_vals(vp, toget, fmt, left > toget, lastrow, (void *) vals);
	    left -= toget;
	    if (vrank > 0)
	      cor[vrank-1] += toget;
	}
	if (vrank > 0)
	  cor[vrank-1] = corsav;
	if (ir < nrows-1)
	  if (!upcorner(vdims,vp->ndims,cor,add))
	    error("vardata: odometer overflowed!");
	set_indent(2);
    }

    return 0;
}
