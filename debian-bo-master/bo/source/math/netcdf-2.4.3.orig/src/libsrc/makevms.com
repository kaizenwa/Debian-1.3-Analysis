$! --------------------------------------------------------------------------
$! For making the netCDF library and CDFTEST.EXE on VMS
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.19 1996/03/18 23:31:40 steve Exp $
$!
$! You must create the netcdf library, NETCDF.OLB, from the XDR directory,
$! [-.xdr], before executing this procedure.
$
$
$ macro :== macro/nolist
$ ccc := cc/vax/opt/nodebug/include=([-.xdr])/nolist/define=(stdc_includes,swap)
$ librep := library/replace [--.LIB]NETCDF.OLB
$
$ define rpc sys$disk:[-.xdr]
$ define sys sys$library
$
$ copy local_nc.gen local_nc.h
$ purge local_nc.h
$
$ ccc ARRAY.C
$ purge ARRAY.OBJ
$ ccc ATTR.C
$ purge ATTR.OBJ
$ ccc CDF.C
$ purge CDF.OBJ
$ ccc CDFTEST.C
$ purge CDFTEST.OBJ
$ ccc DIM.C
$ purge DIM.OBJ
$ ccc ERROR.C
$ purge ERROR.OBJ
$ ccc FILE.C
$ purge FILE.OBJ
$ ccc GLOBDEF.c
$ purge GLOBDEF.OBJ
$ ccc IARRAY.C
$ purge IARRAY.OBJ
$ ccc PUTGET.C
$ purge PUTGET.OBJ
$ ccc PUTGETG.C
$ purge PUTGETG.OBJ
$ ccc SHARRAY.C
$ purge SHARRAY.OBJ
$ ccc STRING.C
$ purge STRING.OBJ
$ ccc VAR.C
$ purge VAR.OBJ
$ ccc XDRPOSIX.C
$ purge XDRPOSIX.OBJ
$ macro HTONS.MAR
$ purge HTONS.OBJ
$ macro NTOHS.MAR
$ purge NTOHS.OBJ
$
$ librep ARRAY, ATTR, CDF, DIM, FILE, GLOBDEF, IARRAY, ERROR, -
    PUTGET, PUTGETG, SHARRAY, STRING, VAR, XDRPOSIX, HTONS, NTOHS
$
$ link/nodebug/exec=CDFTEST.exe -
    cdftest.obj, -
    [--.lib]netcdf/library, -
    sys$input/opt
	sys$library:vaxcrtl/share
$ purge CDFTEST.exe
$
$ create/dir [--.include]
$
$ copy netcdf.h [--.include]
$ purge [--.include]netcdf.h
$
