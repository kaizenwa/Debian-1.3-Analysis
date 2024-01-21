/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncgen/generic.h,v 1.4 1996/02/26 19:23:48 steve Exp $
 *********************************************************************/

#ifndef UD_GENERIC_H
#define UD_GENERIC_H

union generic {			/* used to hold any kind of fill_value */
    double doublev;
    float floatv;
    nclong longv;
    short shortv;
    char charv;
};

#endif
