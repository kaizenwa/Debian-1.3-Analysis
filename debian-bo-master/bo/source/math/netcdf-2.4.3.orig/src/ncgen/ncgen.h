/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncgen/ncgen.h,v 1.19 1996/02/26 19:24:04 steve Exp $
 *********************************************************************/

#define MAX_NC_ATTSIZE    20000	/* max size of attribute (for ncgen) */
#define MAXTRST		  5000	/* max size of string value (for ncgen) */

#include "generic.h"

extern int ncid;		/* handle for netCDF */
extern int ndims;		/* number of dimensions declared for netcdf */
extern int nvars;		/* number of variables declared for netcdf */
extern int natts;		/* number of attributes */
extern int nvdims;		/* number of dimensions for variables */
extern int dimnum;		/* dimension number index for variables */
extern int varnum;		/* variable number index for attributes */
extern int valnum;		/* value number index for attributes */
extern int rec_dim;		/* number of the unlimited dimension, if any */
extern long var_len;		/* variable length (product of dimensions) */
extern int var_size;		/* size of each element of variable */
extern long netcdf_record_number; /* current record number for variables */

extern struct dims {
    long size;
    char *name;
} *dims;			/* table of dimensions */

extern struct vars {
    char *name;
    nc_type type;
    int ndims;
    int *dims;			/* array of dimension ids */
    union generic fill_value;	/* set to value of _FillValue attribute */
    int has_data;		/* 1 if data specified, 0 otherwise */
} *vars;			/* table of variables */

extern struct atts {
    int var;			/* number of variable for this attribute */
    char *name;
    nc_type type;
    int len;
    void *val;
} *atts;			/* table of variable and global attributes */
