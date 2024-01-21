/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: error.h,v 1.10 1993/03/18 05:47:41 russ Exp $ */
#ifndef _NC_ERROR_
#define _NC_ERROR_

#ifdef __STDC__
void nc_serror(char *fmt, ...) ;
void NCadvise(int err, char *fmt,...) ;
#else
void nc_serror() ;
void NCadvise() ;
#endif /* __STDC__ */

#endif /* _NC_ERROR_ */
