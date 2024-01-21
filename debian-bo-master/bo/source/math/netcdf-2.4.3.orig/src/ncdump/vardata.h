/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncdump/vardata.h,v 1.4 1995/10/06 19:54:01 russ Exp $
 *********************************************************************/

extern char *progname;		/* for error messages */

/* Display for user-defined fill values and default floating-point fill
   values; should match what ncgen looks for in ../ncgen/ncgen.l */
#define FILL_STRING "_"

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Output the data for a single variable, in CDL syntax. */
extern int	vardata		PROTO((
				       struct ncvar*, /* variable */
				       long [], /* variable dimension sizes */
				       int, /* netcdf id */
				       int, /* variable id */
				       struct fspec* /* formatting specs */
				       ));

#ifdef __cplusplus
}
#endif
