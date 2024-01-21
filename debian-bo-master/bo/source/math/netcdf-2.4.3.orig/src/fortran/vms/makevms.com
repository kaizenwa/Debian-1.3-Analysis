$! --------------------------------------------------------------------------
$! For making FTEST.EXE on VMS
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.4 1996/03/18 23:30:51 steve Exp $
$
$ ccc := cc/vax/opt/nodebug/nolist/include=([--.libsrc])/define=(stdc_includes,swap)
$
$ define rpc sys$disk:[--.xdr]
$ define sys sys$library
$
$ copy [-]fills.nc fills.nc
$ purge fills.nc
$
$ ccc JACKETS.C
$ purge JACKETS.OBJ
$ fort FTEST.FOR
$ purge FTEST.OBJ
$
$ link/nodebug/notraceback/exec=FTEST.exe -
    ftest.obj, -
    jackets.obj, -
    [---.lib]netcdf.olb/lib, -
    sys$input/opt
	sys$library:vaxcrtl.exe/share
$ purge FTEST.exe
$
$ library/replace [---.lib]NETCDF.OLB JACKETS
$
$ copy netcdf.inc [---.include]
$ purge [---.include]netcdf.inc
$
$ run ftest
$
