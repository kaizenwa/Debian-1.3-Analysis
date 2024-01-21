/*
 *	Copyright 1990, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: jackets.c,v 1.7 1996/05/02 15:38:52 steve Exp $ */
/*
 * OVERVIEW
 *
 * This file contains jacket routines written in C for interfacing Fortran
 * netCDF function calls to the actual C binding for the NetCDF.  This code
 * is written explicitly for VMS.  In general, these functions handle
 * character-string parameter conventions, convert between
 * column-major-order arrays and row-major-order arrays, and map between
 * array indices beginning at one and array indices beginning at zero.
 *
 */

/* LINTLIBRARY */
#include	<ctype.h>
#include        <string.h>
#include	<stdlib.h>
#include	<stdio.h>
#include	<local_nc.h>	/* for NCadvise() prototype and <netcdf.h> */

#include descrip




/*
 * global integer used for suppressing error messages and determining
 * the fatality of errors.
 */
extern int ncopts;		/* default is (NC_FATAL | NC_VERBOSE) */

/* global integer that contains a netCDF-specific error code */
extern int ncerr;

/* blank fill C string to make FORTRAN string */
static void
fcdcpy (fstring, fslen, sstring)
    char *fstring;		/* output string to be blank-filled */
    int fslen;			/* length of output string */
    char *sstring;		/* input string, null-terminated */
{
    int i, len = strlen(sstring);

    for (i = 0; i < len; i++)
	*(fstring + i) = *(sstring + i);
    for (i = len; i < fslen; i++)
	*(fstring + i) = ' ';
}


static void
reverse (array, length)
    int array[];		/* array to be reversed */
    int length;			/* length of array */
{
    int temp, i, j;

    for (i = 0, j = length - 1; i < j; i++, j--) {
	temp = array[i];
	array[i] = array[j];
	array[j] = temp;
    }
}


static void
revlongs (array, length)
    long array[];		/* array to be reversed */
    int length;			/* length of array */
{
    int i, j;
    long temp;

    for (i = 0, j = length - 1; i < j; i++, j--) {
	temp = array[i];
	array[i] = array[j];
	array[j] = temp;
    }
}


/* error handling function */
static void
handle_err (pname, rcode)
    char *pname;		/* procedure name */
    int rcode;			/* error return */
{
    extern char *cdf_routine_name; /* routine name in error messages */

    cdf_routine_name = pname;
    NCadvise(rcode, "string won't fit in CHARACTER variable provided");
}

/* copy function used to copy strings with embedded blanks */
static void
fstrncpy (target, source, maxlen)
    char *target;		/* space to be copied into */
    char *source;		/* string to be copied */
    int maxlen;			/* maximum length of *source */
{
    while (maxlen-- && *source != '\0')
	*target++ = *source++;
    *target = '\0';
}

/* copy function used to copy strings terminated with blanks */
static void
nstrncpy (target, source, maxlen)
    char *target;		/* space to be copied into */
    char *source;		/* string to be copied */
    int maxlen;			/* maximum length of *source */
{
    while (maxlen-- && *source != ' ')
	*target++ = *source++;
    *target = '\0';
}


/*
 * Compute product of dimensions.
 */
static long
dimprod (dims, ndims)
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long *ip;
    long prod = 1;

    for (ip = dims; ip < &dims[ndims]; ip++)
      prod *= *ip;
    return prod;
}


#ifdef FORTRAN_HAS_NO_BYTE
/*
 * Convert multi-dimensional array of bytes stored in ints to packed array of
 * bytes, in malloc'ed space.  Returns pointer to bytes or NULL if malloc
 * failed.
 */
static char *
itob(ints, dims, ndims)
     int *ints;			/* multi-dimensional array of integers */
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    char *bytes = (char *) malloc (iocount * sizeof (char));
    int *ip;
    char *bp = bytes;

    if (bytes != NULL)
      for (ip = ints; iocount > 0; iocount--)
	*bp++ = (char) *ip++;
    return bytes;
}

/*
 * Convert a generalized multi-dimensional array of bytes stored in ints to 
 * packed array of bytes, in malloc'ed space.  Returns pointer to bytes or 
 * NULL if malloc failed.
 */
static char *
itobg(ints, dims, basis, ndims)
     int *ints;			/* multi-dimensional array of integers */
     long *dims;			/* list of dimensions */
     long *basis;			/* memory access basis vector */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    char *bytes = (char *) malloc (iocount * sizeof (char));

    if (bytes != NULL && iocount > 0) {
	int	idim;
	char	*bp	= bytes;
	char	*ip	= (char*)ints;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *bp++	= (char)*(int*)ip;
	    idim	= ndims - 1;
	carry:
	    ip	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }

    return bytes;
}

/*
 * Convert a packed array of bytes into a generalized multi-dimensional array
 * of ints.
 */
static void
btoig(bytes, ints, dims, basis, ndims)
     char *bytes;		/* packed array of bytes */
     int *ints;			/* multi-dimensional array of integers */
     long *dims;		/* list of dimensions */
     long *basis;		/* memory access basis vector */
     int ndims;			/* number of dimensions in list */
{
    if (dimprod (dims, ndims) > 0) {
	int	idim;
	char	*bp	= bytes;
	char	*ip	= (char*)ints;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *(int*)ip	= *bp++;
	    idim	= ndims - 1;
	carry:
	    ip	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }
}
#endif /* FORTRAN_HAS_NO_BYTE */

#ifdef FORTRAN_HAS_NO_SHORT
/*
 * Convert multi-dimensional array of shorts stored in ints to packed array of
 * shorts, in malloc'ed space.  Returns pointer to shorts or NULL if malloc
 * failed.
 */
static short *
itos(ints, dims, ndims)
     int *ints;		/* multi-dimensional array of ints */
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    short *shorts = (short *) malloc (iocount * sizeof (short));
    int *ip;
    short *sp = shorts;

    if (shorts != NULL)
      for (ip = ints; iocount > 0; iocount--)
	*sp++ = (short) *ip++;
    return shorts;
}

/*
 * Convert a generalized multi-dimensional array of shorts stored in ints to 
 * packed array of shorts, in malloc'ed space.  Returns pointer to shorts or 
 * NULL if malloc failed.
 */
static short *
itosg(ints, dims, basis, ndims)
     int *ints;			/* multi-dimensional array of integers */
     long *dims;			/* list of dimensions */
     long *basis;			/* memory access basis vector */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    short *shorts = (short *) malloc (iocount * sizeof (short));

    if (shorts != NULL && iocount > 0) {
	int	idim;
	char	*ip	= (char*)ints;
	short	*sp	= shorts;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *sp++	= (short)*(int*)ip;
	    idim	= ndims - 1;
	carry:
	    ip	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }

    return shorts;
}

/*
 * Convert a packed array of shorts into a generalized multi-dimensional array
 * of ints.
 */
static void
stoig(shorts, ints, dims, basis, ndims)
     short *shorts;		/* packed array of shorts */
     int *ints;			/* multi-dimensional array of integers */
     long *dims;		/* list of dimensions */
     long *basis;		/* memory access basis vector */
     int ndims;			/* number of dimensions in list */
{
    if (dimprod (dims, ndims) > 0) {
	int	idim;
	short	*sp	= shorts;
	char	*ip	= (char*)ints;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *(int*)ip	= *sp++;
	    idim	= ndims - 1;
	carry:
	    ip	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }
}
#endif /* FORTRAN_HAS_NO_SHORT */

#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
/*
 * Convert multi-dimensional array of NCLONGs stored in ints to packed
 * array of longs, in malloc'ed space.  Returns pointer to longs or NULL
 * if malloc failed.
 */
static long *
itol(ints, dims, ndims)
    int		*ints;		/* multi-dimensional array of ints */
    long	*dims;		/* list of dimensions */
    int		ndims;		/* number of dimensions in list */
{
    long	iocount = dimprod (dims, ndims);
    long	*longs = (long *) malloc (iocount * sizeof (long));
    int		*ip;
    long	*lp = longs;

    if (longs != NULL)
	for (ip = ints; iocount > 0; iocount--)
	    *lp++ = (long) *ip++;
    return longs;
}

/*
 * Convert a generalized multi-dimensional array of longs stored in ints to 
 * packed array of longs, in malloc'ed space.  Returns pointer to longs or 
 * NULL if malloc failed.
 */
static long *
itolg(ints, dims, imap, ndims)
    int		*ints;		/* multi-dimensional array of integers */
    long	*dims;		/* list of dimensions */
    long	*imap;		/* memory access index mapping vector */
    int		ndims;		/* number of dimensions in list */
{
    long	iocount = dimprod (dims, ndims);
    long	*longs = (long *) malloc (iocount * sizeof (long));

    if (longs != NULL && iocount > 0) {
	int	idim;
	char	*ip	= (char*)ints;
	long	*lp	= longs;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*imap[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *lp++	= (long)*(int*)ip;
	    idim	= ndims - 1;
	carry:
	    ip	+= imap[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }

    return longs;
}

/*
 * Convert a packed array of longs into a generalized multi-dimensional array
 * of ints.
 */
static void
ltoig(longs, ints, dims, imap, ndims)
    long	*longs;		/* packed array of longs */
    int		*ints;		/* multi-dimensional array of integers */
    long	*dims;		/* list of dimensions */
    long	*imap;		/* memory access index mapping vector */
    int		ndims;		/* number of dimensions in list */
{
    if (dimprod (dims, ndims) > 0) {
	int	idim;
	long	*lp	= longs;
	char	*ip	= (char*)ints;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*imap[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *(int*)ip	= *lp++;
	    idim	= ndims - 1;
	carry:
	    ip	+= imap[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		ip		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }
}
#endif	/* sizeof(int) < sizeof(nclong) above */

#ifdef FORTRAN_HAS_NO_FLOAT
/*
 * Convert multi-dimensional array of floats stored in doubles to packed array
 * of floats, in malloc'ed space.  Returns pointer to floats or NULL if malloc
 * failed.
 */
static float *
dtof(doubles, dims, ndims)
     double *doubles;	/* multi-dimensional array of doubles */
     long *dims;	/* list of dimensions */
     int ndims;		/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    float *floats = (float *) malloc (iocount * sizeof (float));
    double *dp;
    float *fp = floats;

    if (floats != NULL)
      for (dp = doubles; iocount > 0; iocount--)
	*fp++ = (float) *dp++;
    return floats;
}

/*
 * Convert a generalized multi-dimensional array of floats stored in doubles to 
 * packed array of floats, in malloc'ed space.  Returns pointer to floats or 
 * NULL if malloc failed.
 */
static float *
dtofg(doubles, dims, basis, ndims)
     double *doubles;	/* multi-dimensional array of doubles */
     long *dims;	/* list of dimensions */
     long *basis;	/* memory access basis vector */
     int ndims;		/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    float *floats = (float *) malloc (iocount * sizeof (float));

    if (floats != NULL && iocount > 0) {
	int	idim;
	char	*dp	= (char*)doubles;
	float	*fp	= floats;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *fp++	= (float)*(double*)dp;
	    idim	= ndims - 1;
	carry:
	    dp	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		dp		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }

    return floats;
}

/*
 * Convert a packed array of floats into a generalized multi-dimensional array
 * of doubles.
 */
static void
ftodg(floats, doubles, dims, basis, ndims)
     float *floats;		/* packed array of floats */
     double *doubles;		/* multi-dimensional array of doubles */
     long *dims;		/* list of dimensions */
     long *basis;		/* memory access basis vector */
     int ndims;			/* number of dimensions in list */
{
    if (dimprod (dims, ndims) > 0) {
	int	idim;
	float	*fp	= floats;
	char	*dp	= (char*)doubles;
	long	length[MAX_NC_DIMS];
	long	coords[MAX_NC_DIMS];

	for (idim = 0; idim < ndims; ++idim) {
	    length[idim]	= dims[idim]*basis[idim];
	    coords[idim]	= 0;
	}

	for (;;) {
	    *(double*)dp	= *fp++;
	    idim	= ndims - 1;
	carry:
	    dp	+= basis[idim];
	    if (++coords[idim] >= dims[idim]) {
		coords[idim]	= 0;
		dp		-= length[idim];
		if (--idim < 0)
		    break;
		goto carry;
	    }
        }
    }
}
#endif /* FORTRAN_HAS_NO_FLOAT */

/* ------------ VMS FORTRAN jackets for netCDF Functions ------------ */

/* used to set the C global variable ncopts from Fortran */
void
ncpopt(val)
    int		*val;	
{
    ncopts = *val;
}


/* used to get the C global variable ncopts from Fortran */
void
ncgopt(val)
    int		*val;	
{
    *val = ncopts;
}

/*
 * creates a new netCDF file, returning a netCDF ID.  New netCDF
 * file is placed in define mode.
 */
int
nccre(pathnamed, clobmode, rcode)
    struct dsc$descriptor_s * pathnamed;	
    int		*clobmode;	
    int		*rcode;	
{
    char	*pathname	= pathnamed->dsc$a_pointer;
    int		pathnamelen	= pathnamed->dsc$w_length;
    char name[256];
    extern char *cdf_routine_name; /* routine name in error messages */
    int ncid;

    nstrncpy (name, pathname, pathnamelen);

    cdf_routine_name = "NCCRE";
    if (*clobmode != NC_CLOBBER && *clobmode != NC_NOCLOBBER) {
        *rcode = NC_EINVAL;
        NCadvise(*rcode, "bad flag, did you forget to include netcdf.inc?");
    } else if ((ncid = nccreate (name, *clobmode)) != -1) {
	*rcode = 0;
	return (ncid);
    }
    *rcode = ncerr;
    return (-1);
}


/* opens an existing netCDF file for access */
int
ncopn(pathnamed, rwmode, rcode)
    struct dsc$descriptor_s * pathnamed;	
    int		*rwmode;	
    int		*rcode;	
{
    char	*pathname	= pathnamed->dsc$a_pointer;
    int		pathnamelen	= pathnamed->dsc$w_length;
    char name[256];
    extern char *cdf_routine_name; /* routine name in error messages */
    int ncid;

    nstrncpy (name, pathname, pathnamelen);
    cdf_routine_name = "NCOPN";
    if (*rwmode != NC_NOWRITE && *rwmode != NC_WRITE) {
        *rcode = NC_EINVAL;
        NCadvise(*rcode, "bad flag, did you forget to include netcdf.inc?");
    } else if ((ncid = ncopen (name, *rwmode)) != -1) {
	*rcode = 0;
	return (ncid);
    }
    *rcode = ncerr;
    return (-1);
}


/* adds a new dimension to an open netCDF file in define mode */
int
ncddef(ncid, dimnamed, dimlen, rcode)
    int		*ncid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*dimlen;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid;

    nstrncpy (name, dimname, dimnamelen);
    if ((dimid = ncdimdef (*ncid, name, (long)*dimlen)) != -1) {
	*rcode = 0;
	return (dimid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/*
 * returns the ID of a netCDF dimension, given the name of the
 * dimension
 */
int
ncdid(ncid, dimnamed, rcode)
    int		*ncid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid;

    nstrncpy (name, dimname, dimnamelen);
    if ((dimid = ncdimid (*ncid, name)) != -1) {
	*rcode = 0;
	return (dimid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* adds a new variable to an open netCDF file in define mode */
int
ncvdef(ncid, varnamed, datatype, ndims, dimarray, rcode)
    int		*ncid;	
    struct dsc$descriptor_s * varnamed;	
    int		*datatype;	
    int		*ndims;	
    int		*dimarray;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    int varid, i, dimid[MAX_VAR_DIMS];
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, varname, varnamelen);
    for (i = 0; i < *ndims; i++)
	dimid[i] = dimarray[i] - 1;
    reverse (dimid, *ndims);
    if ((varid = ncvardef (*ncid, name, (nc_type) *datatype, *ndims,
			   dimid)) != -1) {
	*rcode = 0;
	return (varid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* returns the ID of a netCDF variable given its name */
int
ncvid(ncid, varnamed, rcode)
    int		*ncid;	
    struct dsc$descriptor_s * varnamed;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    int varid;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, varname, varnamelen);
    if ((varid = ncvarid (*ncid, name)) != -1) {
	*rcode = 0;
	return (varid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* returns number of bytes per netCDF data type */
int
nctlen(datatype, rcode)
    int		*datatype;	
    int		*rcode;	
{
    int itype;

    if ((itype = nctypelen ((nc_type) *datatype)) != -1) {
	*rcode = 0;
	return (itype);
    }
    *rcode = ncerr;
    return (-1);
}

/* closes an open netCDF file */
void
ncclos(ncid, rcode)
    int		*ncid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncclose (*ncid) == -1)
	*rcode = ncerr;
}

/* puts an open netCDF into define mode */
void
ncredf(ncid, rcode)
    int		*ncid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncredef (*ncid) == -1)
	*rcode = ncerr;
}

/* takes an open netCDF out of define mode */
void
ncendf(ncid, rcode)
    int		*ncid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncendef (*ncid) == -1)
	*rcode = ncerr;
}

/* returns information about an open netCDF file given its netCDF ID */
void
ncinq(ncid, ndims, nvars, natts, recdim, rcode)
    int		*ncid;	
    int		*ndims;	
    int		*nvars;	
    int		*natts;	
    int		*recdim;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncinquire (*ncid, ndims, nvars, natts, recdim) == -1) {
	*rcode = ncerr;
	return;
    }
    if (*recdim != -1)
	(*recdim)++;
}

/*
 * makes sure that the disk copy of a netCDF file open for writing
 * is current
 */
void
ncsnc(ncid, rcode)
    int		*ncid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncsync (*ncid) == -1)
	*rcode = ncerr;
}

/*
 * restores the netCDF to a known consistent state in case anything
 * goes wrong during the definition of new dimensions, variables
 * or attributes
 */
void
ncabor(ncid, rcode)
    int		*ncid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncabort (*ncid) == -1)
	*rcode = ncerr;
}

/* returns the name and size of a dimension, given its ID */
void
ncdinq(ncid, dimid, dimnamed, size, rcode)
    int		*ncid;	
    int		*dimid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*size;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    long siz;
    char name[MAX_NC_NAME + 1];

    *rcode = 0;
    if (ncdiminq (*ncid, *dimid - 1, name, &siz) == -1) {
	*rcode = ncerr;
	return;
    }
    *size = siz;
    if ((int) strlen (name) > dimnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCDINQ", *rcode);
	return;
    }
    /* blank fill the input character string */
    fcdcpy (dimname, dimnamelen, name);
}

/* renames an existing dimension in a netCDF open for writing */
void
ncdren(ncid, dimid, dimnamed, rcode)
    int		*ncid;	
    int		*dimid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, dimname, dimnamelen);
    *rcode = 0;
    if (ncdimrename (*ncid, *dimid - 1, name) == -1)
	*rcode = ncerr;
}

/* returns information about a netCDF variable, given its ID */
void
ncvinq(ncid, varid, varnamed, datatype, ndims, dimarray, natts, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * varnamed;	
    int		*datatype;	
    int		*ndims;	
    int		*dimarray;	
    int		*natts;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid[MAX_VAR_DIMS], i;

    *rcode = 0;
    if (ncvarinq (*ncid, *varid - 1, name, (nc_type *) datatype, ndims, dimid,
		  natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < *ndims; i++)
	dimarray[i] = dimid[i] + 1;
    reverse (dimarray, *ndims);
    if ((int) strlen (name) > varnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCVINQ", *rcode);
	return;
    }
    fcdcpy (varname, varnamelen, name);
}

/* puts a single numeric data value into a variable of an open netCDF */
void
ncvpt1(ncid, varid, indices, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*indices;	
    void	*value;	
    int		*rcode;	
{
    int ndims, natts, i;
    nc_type datatype;
    long nindices[MAX_VAR_DIMS];
    int dimid[MAX_VAR_DIMS];

    if (ncvarinq (*ncid, *varid - 1, (char *) 0,
		  & datatype, &ndims, dimid, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++)
	nindices[i] = indices[i] - 1;
    revlongs (nindices, ndims);
    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {	/* pack ints into bytes */
	char           bytes = *(int *) value;
	if (ncvarput1(*ncid, *varid - 1, nindices,
		      (ncvoid *) &bytes) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) {	/* pack ints into shorts */
	short          shorts = *(int *)value;
	if (ncvarput1(*ncid, *varid - 1, nindices, (ncvoid *) &shorts) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	nclong        nclong = *(int *)value;
	if (ncvarput1(*ncid, *varid - 1, nindices, (ncvoid *) &nclong) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) {	/* pack doubles into floats */
	float          floats = *(double *)value;
	if (ncvarput1(*ncid, *varid - 1, nindices, (ncvoid *) &floats) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvarput1 (*ncid, *varid - 1, nindices, value) == -1) {
	*rcode = ncerr;
    }
}

/* puts a single character into an open netCDF file */
void
ncvp1c(ncid, varid, indices, chvald, rcode)
    int		*ncid;	
    int		*varid;	
    int		*indices;	
    struct dsc$descriptor_s * chvald;	
    int		*rcode;	
{
    char	*chval	= chvald->dsc$a_pointer;
    int		chvallen	= chvald->dsc$w_length;
    int ndims, natts, i;
    nc_type datatype;
    long nindices[MAX_VAR_DIMS];
    int dimid[MAX_VAR_DIMS];

    if (ncvarinq (*ncid, *varid - 1, (char *) 0,
		  & datatype, &ndims, dimid, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++)
	nindices[i] = indices[i] - 1;
    revlongs (nindices, ndims);
    *rcode = 0;
    if (ncvarput1 (*ncid, *varid - 1, nindices, (ncvoid *) chval) == -1) {
	*rcode = ncerr;
    }
}

/*
 * writes a hypercube of numeric values into a netCDF variable of an open
 * netCDF file
 */
void
ncvpt(ncid, varid, start, count, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    int ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {	/* pack ints into bytes */
	char *bytes = itob (value, ncount, ndims);
	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*ncid, *varid - 1, nstart, ncount,
	              (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	}
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) { /* pack ints into shorts */
	short *shorts = itos (value, ncount, ndims);
	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	}
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	nclong *nclongs = itol (value, ncount, ndims);
	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*ncid, *varid - 1, nstart, ncount,
	              (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	}
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) { /* pack doubles into floats */
	float *floats = dtof (value, ncount, ndims);
	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	}
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvarput (*ncid, *varid - 1, nstart, ncount, value) == -1) {
	*rcode = ncerr;
    }
}

/* writes a hypercube of character values into an open netCDF file */
void
ncvptc(ncid, varid, start, count, stringd, lenstr, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    int ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0,
		  & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    if (dimprod(ncount,ndims) > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCVPTC", *rcode);
	return;
    }
    *rcode = 0;
    if (ncvarput (*ncid, *varid - 1, nstart, ncount, (ncvoid *) string) == -1) {
	*rcode = ncerr;
    }
}

/*
 * writes a generalized hypercube of numeric values into a netCDF variable of 
 * an open netCDF file
 */
void
ncvptg(ncid, varid, start, count, stride, basis, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    int		*stride;	
    int		*basis;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    long nstride[MAX_VAR_DIMS], nbasis[MAX_VAR_DIMS];
    long tmpbasis;
    int ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_CHAR || datatype == NC_BYTE)
	tmpbasis	= nctypelen(NC_LONG);
    else
#endif
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT)
	tmpbasis	= nctypelen(NC_LONG);
    else
#endif
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG)
	tmpbasis	= sizeof(int);
    else
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT)
	tmpbasis	= nctypelen(NC_DOUBLE);
    else
#endif
	tmpbasis	= nctypelen(datatype);
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	nstride[i] = stride[0] == 0 ? 1 : stride[i];
	nbasis[i] = basis[0] == 0 ? tmpbasis : basis[i];
	tmpbasis *= count[i];
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    revlongs (nstride, ndims);
    revlongs (nbasis, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {	/* pack ints into bytes */
	/*
	 * Release 2.3.1 had a bug in the following line: it used count
	 * rather than ncount.
	 */
	char *bytes = itobg (value, ncount, nbasis, ndims);
	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride, 
		       (long*)NULL, (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	}
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) { /* pack ints into shorts */
	/*
	 * Release 2.3.1 had a bug in the following line: it used count
	 * rather than ncount.
	 */
	short *shorts = itosg (value, ncount, nbasis, ndims);
	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride,
		      (long*)NULL, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	}
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	nclong *nclongs = itolg (value, ncount, nbasis, ndims);
	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride,
		      (long*)NULL, (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	}
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) { /* pack doubles into floats */
	float *floats = dtofg (value, ncount, nbasis, ndims);
	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride,
		      (long*)NULL, (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	}
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride, nbasis,
		   value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * writes a generalized hypercube of character values into a netCDF variable of 
 * an open netCDF file
 */
void
ncvpgc(ncid, varid, start, count, stride, basis, stringd, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    int		*stride;	
    int		*basis;	
    struct dsc$descriptor_s * stringd;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    long nstride[MAX_VAR_DIMS], nbasis[MAX_VAR_DIMS];
    long tmpbasis;
    int ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    tmpbasis	= nctypelen(datatype);
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	nstride[i] = stride[0] == 0 ? 1 : stride[i];
	nbasis[i] = basis[0] == 0 ? tmpbasis : basis[i];
	tmpbasis *= count[i];
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    revlongs (nstride, ndims);
    revlongs (nbasis, ndims);

    *rcode = 0;
    if (ncvarputg (*ncid, *varid - 1, nstart, ncount, nstride, nbasis,
		   (ncvoid*)string) == -1) {
	*rcode = ncerr;
    }
}

/* gets a single numeric value from a variable of an open netCDF file */
void
ncvgt1(ncid, varid, indices, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*indices;	
    void	*value;	
    int		*rcode;	
{
    long nindices[MAX_VAR_DIMS], i;
    int ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	nindices[i] = indices[i] - 1;
    }
    revlongs (nindices, ndims);
    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {
	char           bytes;
	int            *ip = (int *) value;
	char           *bp = &bytes;

	if (ncvarget1(*ncid, *varid - 1, nindices, (ncvoid *) &bytes) == -1) {
	    *rcode = ncerr;
	    return;
	}
	*ip = *bp;
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) {
	short          shorts;
	int            *ip = (int *) value;
	short          *sp = &shorts;

	if (ncvarget1(*ncid, *varid - 1, nindices, (ncvoid *) &shorts) == -1) {
	    *rcode = ncerr;
	    return;
	}
	*ip = *sp;
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	nclong         nclongs;
	int           *ip = (int *) value;

	if (ncvarget1(*ncid, *varid - 1, nindices, (ncvoid *) &nclongs) == -1)
	{
	    *rcode = ncerr;
	    return;
	}
	*ip = nclongs;
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) {
	float          floats;
	double         *dp = (double *) value;
	float          *fp = &floats;

	if (ncvarget1(*ncid, *varid - 1, nindices, (ncvoid *) &floats) == -1) {
	    *rcode = ncerr;
	    return;
	}
	*dp = *fp;
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvarget1 (*ncid, *varid - 1, nindices, value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets a single character data value from a variable of an open
 * netCDF file
 */
void
ncvg1c(ncid, varid, indices, chvald, rcode)
    int		*ncid;	
    int		*varid;	
    int		*indices;	
    struct dsc$descriptor_s * chvald;	
    int		*rcode;	
{
    char	*chval	= chvald->dsc$a_pointer;
    int		chvallen	= chvald->dsc$w_length;
    long nindices[MAX_VAR_DIMS];
    int i, ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0,
		  & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = 0; i < ndims; i++) {
	nindices[i] = indices[i] - 1;
    }
    revlongs (nindices, ndims);
    *rcode = 0;
    if (ncvarget1 (*ncid, *varid - 1, nindices, (ncvoid *) chval) == -1) {
	*rcode = ncerr;
    }
}

/*
 * reads a hypercube of numeric values from a netCDF variable of an open
 * netCDF file
 */
void
ncvgt(ncid, varid, start, count, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    int i, ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	char *bytes = (char *) malloc (iocount * sizeof (char));
	int *ip;
	char *bp = bytes;

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	    free (bytes);
	    return;
	}
	for (ip = (int *) value; iocount > 0; iocount--)
	  *ip++ = *bp++;
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	short *shorts = (short *) malloc (iocount * sizeof (short));
	int *ip;
	short *sp = shorts;

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	    free (shorts);
	    return;
	}
	for (ip = (int *) value; iocount > 0; iocount--)
	    *ip++ = *sp++;
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	nclong *nclongs = (nclong *) malloc (iocount * sizeof (nclong));
	int *ip;
	nclong *lp = nclongs;

	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	    free (nclongs);
	    return;
	}
	for (ip = (int *) value; iocount > 0; iocount--)
	    *ip++ = *lp++;
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	float *floats = (float *) malloc (iocount * sizeof (float));
	double *dp;
	float *fp = floats;

	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*ncid, *varid - 1, nstart, ncount,
		      (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	    free (floats);
	    return;
	}
	for (dp = (double *) value; iocount > 0; iocount--)
	    *dp++ = *fp++;
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvarget (*ncid, *varid - 1, nstart, ncount, value) == -1) {
	*rcode = ncerr;
    }
}

/* reads a hypercube of character values from a netCDF variable */
void
ncvgtc(ncid, varid, start, count, stringd, lenstr, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    int i, ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;
    int prod = 1;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0,
		  & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	prod *= count[i];
    }
    if (prod > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCVGTC", *rcode);
	return;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    *rcode = 0;
    if (ncvarget (*ncid, *varid - 1, nstart, ncount, (ncvoid *) string) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = prod; i < *lenstr; i++)
	string[i] = ' ';
}

/*
 * reads a generalized hypercube of numeric values from a netCDF variable of an 
 * open netCDF file
 */
void
ncvgtg(ncid, varid, start, count, stride, basis, value, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    int		*stride;	
    int		*basis;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    long nstride[MAX_VAR_DIMS], nbasis[MAX_VAR_DIMS];
    long tmpbasis;
    int i, ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_CHAR || datatype == NC_BYTE)
	tmpbasis	= nctypelen(NC_LONG);
    else
#endif
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT)
	tmpbasis	= nctypelen(NC_LONG);
    else
#endif
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG)
	tmpbasis	= sizeof(int);
    else
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT)
	tmpbasis	= nctypelen(NC_DOUBLE);
    else
#endif
	tmpbasis	= nctypelen(datatype);
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	nstride[i] = stride[0] == 0 ? 1 : stride[i];
	nbasis[i] = basis[0] == 0 ? tmpbasis : basis[i];
	tmpbasis *= count[i];
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    revlongs (nstride, ndims);
    revlongs (nbasis, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	char *bytes = (char *) malloc (iocount * sizeof (char));
	int *ip;
	char *bp = bytes;

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride,
		      (long*)NULL, (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	    free (bytes);
	    return;
	}
	/*
	 * Release 2.3.1 had a bug in the following line: it used basis
	 * rather than nbasis.
	 */
	btoig(bytes, (int*)value, ncount, nbasis, ndims);
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	short *shorts = (short *) malloc (iocount * sizeof (short));
	int *ip;
	short *sp = shorts;

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride, 
		       (long*)NULL, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	    free (shorts);
	    return;
	}
	/*
	 * Release 2.3.1 had a bug in the following line: it used basis
	 * rather than nbasis.
	 */
	stoig(shorts, (int*)value, ncount, nbasis, ndims);
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	nclong *nclongs = (nclong *) malloc (iocount * sizeof (nclong));

	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride, 
		       (long*)NULL, (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	    free (nclongs);
	    return;
	}
	ltoig(nclongs, (int*)value, ncount, nbasis, ndims);
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) {
	long iocount = dimprod (ncount, ndims);	/* product of dimensions */
	float *floats = (float *) malloc (iocount * sizeof (float));

	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride, 
		       (long*)NULL, (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	    free (floats);
	    return;
	}
	ftodg(floats, (double*)value, ncount, nbasis, ndims);
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride,
		   nbasis, value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * reads a generalized hypercube of character values from a netCDF variable 
 * of an open netCDF file
 */
void
ncvggc(ncid, varid, start, count, stride, basis, stringd, rcode)
    int		*ncid;	
    int		*varid;	
    int		*start;	
    int		*count;	
    int		*stride;	
    int		*basis;	
    struct dsc$descriptor_s * stringd;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    long nstride[MAX_VAR_DIMS], nbasis[MAX_VAR_DIMS];
    long tmpbasis;
    int i, ndims, dimarray[MAX_VAR_DIMS], natts;
    nc_type datatype;

    if (ncvarinq (*ncid, *varid - 1, (char *) 0, & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    tmpbasis	= nctypelen(datatype);
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	nstride[i] = stride[0] == 0 ? 1 : stride[i];
	nbasis[i] = basis[0] == 0 ? tmpbasis : basis[i];
	tmpbasis *= count[i];
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    revlongs (nstride, ndims);
    revlongs (nbasis, ndims);

    *rcode = 0;
    if (ncvargetg (*ncid, *varid - 1, nstart, ncount, nstride,
		   nbasis, (ncvoid*)string) == -1) {
	*rcode = ncerr;
    }
}

/* changes the name of a netCDF variable in an open netCDF file */
void
ncvren(ncid, varid, varnamed, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * varnamed;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, varname, varnamelen);
    *rcode = 0;
    if (ncvarrename (*ncid, *varid - 1, name) == -1) {
	*rcode = ncerr;
    }
}

/*
 * adds or changes a numeric variable or global attribute of an open
 * netCDF file
 */
void
ncapt(ncid, varid, attnamed, datatype, attlen, value, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*attlen;	
    void	*value;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, attname, attnamelen);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) *datatype == NC_BYTE) {	/* pack ints into bytes */
	long ncount = *attlen;

	char *bytes = itob (value, &ncount, 1);

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	}
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) *datatype == NC_SHORT) {	/* pack ints into shorts */
	long ncount = *attlen;

	short *shorts = itos (value, &ncount, 1);

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	}
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if ((nc_type) *datatype == NC_LONG) {
	long ncount = *attlen;
	nclong *nclongs = itol (value, &ncount, 1);

	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	}
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if ((nc_type) *datatype == NC_FLOAT) {	/* pack ints into floats */
	long ncount = *attlen;

	float *floats = dtof (value, &ncount, 1);

	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	}
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *attlen,
		  value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * adds or changes a character variable or global attribute
 * of an open netCDF file
 */
void
ncaptc(ncid, varid, attnamed, datatype, lenstr, stringd, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*lenstr;	
    struct dsc$descriptor_s * stringd;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    char *value;

    nstrncpy (name, attname, attnamelen);
    if (((value = (char *)malloc ((unsigned) *lenstr + 1)) == NULL) || (*lenstr == 0)) {
	*rcode = NC_ESTS;
	handle_err ("NCAPTC", *rcode);
	return;
    }
    (void) fstrncpy (value, string, *lenstr);
    *rcode = 0;
    if (ncattput (*ncid, *varid - 1, name, (nc_type) *datatype, *lenstr,
		  (ncvoid *) value) == -1) {
	*rcode = ncerr;
    }
    free (value);
}

/*
 * returns information about a netCDF attribute given its variable
 * ID and name
 */
void
ncainq(ncid, varid, attnamed, datatype, attlen, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*attlen;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattinq (*ncid, *varid - 1, name, (nc_type *) datatype, attlen) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the value of a netCDF attribute given its variable ID
 * and name
 */
void
ncagt(ncid, varid, attnamed, value, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    void	*value;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    nc_type datatype;
    int attlen;

    nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattinq (*ncid, *varid - 1, name, &datatype, &attlen)
	    == -1) {
	*rcode = ncerr;
	return;
    }
#ifdef FORTRAN_HAS_NO_BYTE
    if (datatype == NC_BYTE) {
	char *bytes = (char *) malloc (attlen);
	int *ip;
	char *bp = bytes;

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*ncid, *varid - 1, name, (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	    free (bytes);
	    return;
	}
	for (ip = (int *) value; attlen > 0; attlen--)
	    *ip++ = *bp++;
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if (datatype == NC_SHORT) {
	short *shorts = (short *) malloc (attlen * sizeof (short));
	int *ip;
	short *sp = shorts;

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*ncid, *varid - 1, name, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	    free (shorts);
	    return;
	}
	for (ip = (int *) value; attlen > 0; attlen--)
	    *ip++ = *sp++;
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
#if SIZEOF_INT == 4 && SIZEOF_INT < SIZEOF_NCLONG
    if (datatype == NC_LONG) {
	nclong *nclongs = (nclong *) malloc (attlen * sizeof (nclong));
	int *ip;
	nclong *lp = nclongs;

	if (nclongs == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*ncid, *varid - 1, name, (ncvoid *) nclongs) == -1) {
	    *rcode = ncerr;
	    free (nclongs);
	    return;
	}
	for (ip = (int *) value; attlen > 0; attlen--)
	    *ip++ = *lp++;
	free (nclongs);
	return;
    }				/* else */
#endif
#ifdef FORTRAN_HAS_NO_FLOAT
    if (datatype == NC_FLOAT) {
	float *floats = (float *) malloc (attlen * sizeof (float));
	double *dp;
	float *fp = floats;

	if (floats == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*ncid, *varid - 1, name, (ncvoid *) floats) == -1) {
	    *rcode = ncerr;
	    free (floats);
	    return;
	}
	for (dp = (double *) value; attlen > 0; attlen--)
	    *dp++ = *fp++;
	free (floats);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_FLOAT */
    if (ncattget (*ncid, *varid - 1, name, value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the value of a netCDF character attribute given its variable
 * ID and name
 */
void
ncagtc(ncid, varid, attnamed, stringd, lenstr, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    nc_type datatype;
    int attlen;
    int i;

    nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattinq (*ncid, *varid - 1, name, &datatype, &attlen) == -1) {
	*rcode = ncerr;
	return;
    }
    if (attlen > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCAGTC", *rcode);
	return;
    }
    if (ncattget (*ncid, *varid - 1, name, (ncvoid *) string) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = attlen; i < *lenstr; i++)
	string[i] = ' ';
}

/* copies an attribute from one open netCDF file to another */
void
ncacpy(inncid, invarid, attnamed, outncid, outvarid, rcode)
    int		*inncid;	
    int		*invarid;	
    struct dsc$descriptor_s * attnamed;	
    int		*outncid;	
    int		*outvarid;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattcopy (*inncid, *invarid - 1, name,
		   *outncid, *outvarid - 1) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the name of an attribute given its variable ID and number
 * as an attribute of that variable
 */
void
ncanam(ncid, varid, attnum, attnamed, rcode)
    int		*ncid;	
    int		*varid;	
    int		*attnum;	
    struct dsc$descriptor_s * attnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    *rcode = 0;
    if (ncattname (*ncid, *varid - 1, *attnum - 1, name) == -1) {
	*rcode = ncerr;
	return;
    }
    if ((int) strlen (name) > attnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCANAM", *rcode);
	return;
    }
    fcdcpy (attname, attnamelen, name);
}


/* renames an attribute in an open netCDF file */
void
ncaren(ncid, varid, attnamed, newnamed, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    struct dsc$descriptor_s * newnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*newname	= newnamed->dsc$a_pointer;
    int		newnamelen	= newnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1], nname[MAX_NC_NAME + 1];

    nstrncpy (name, attname, attnamelen);
    nstrncpy (nname, newname, newnamelen);
    *rcode = 0;
    if (ncattrename (*ncid, *varid - 1, name, nname) == -1) {
	*rcode = ncerr;
    }
}

/*
 * deletes an attribute from an open netCDF file given the attribute
 * name
 */
void
ncadel(ncid, varid, attnamed, rcode)
    int		*ncid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattdel (*ncid, *varid - 1, name) == -1) {
	*rcode = ncerr;
    }
}


/*
 * sets the fill mode of a netCDF file open for writing
 */
int
ncsfil(ncid, fillmode, rcode)
    int		*ncid;	
    int		*fillmode;	
    int		*rcode;	
{
    int retval;

    if ((retval = ncsetfill (*ncid, *fillmode)) != -1) {
	*rcode = 0;
	return retval;
    }
    *rcode = ncerr;
    return (-1);
}
