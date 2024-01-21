/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /upc/share/CVS/netcdf/ncgen/close.c,v 1.14 1995/10/10 15:29:08 russ Exp $
 *********************************************************************/

#include <stdio.h>
#include <netcdf.h>
#include "generic.h"
#include "ncgen.h"
#include "genlib.h"

extern int netcdf_flag;
extern int c_flag;
extern int fortran_flag;

void close_netcdf	PROTO((
    void
    ));
static void cl_netcdf	PROTO((
    void
    ));
static void cl_c	PROTO((
    void
    ));
static void cl_fortran	PROTO((
    void
    ));

static void
cl_netcdf()
{
    if (ncclose(ncid) == -1)
      derror ("error closing netcdf");
}

#define fpr    (void) fprintf

static void
cl_c()
{
    cline("   ncclose (ncid);");
#ifndef vms
    cline("   return 0;");
#else
    cline("   return 1;");
#endif
    cline("}");
}


static void
cl_fortran()
{
    fline("call ncclos (ncid, iret)");
    fline("end");
}



void
close_netcdf()
{
    if (netcdf_flag)
      cl_netcdf();		/* close netcdf */
    if (c_flag)			/* create C code to close netcdf */
      cl_c();
    if (fortran_flag)		/* create Fortran code to close netcdf */
      cl_fortran();
}

