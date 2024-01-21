$! Tests NCGEN on VMS.
$
$! Assumptions:
$!   You are in the NCGEN source directory.
$!   You have already made the NCGEN and NCDUMP executables from their
$!   respective sources. 
$
$ ncgen == "$ sys$disk:[]ncgen.exe"
$ ncdump == "$ sys$disk:[-.ncdump]ncdump.exe"
$
$! Test "-n" option of ncgen
$
$! Create test0.nc from test0.cdl
$ ncgen -o test0.nc test0.cdl
$
$! Create test1.cdl from test0.nc
$ define/user sys$output test1.cdl
$ ncdump -n test1 test0.nc
$
$! Create test1.nc from test1.cdl
$ ncgen -o test1.nc test1.cdl
$
$! Create test2.cdl from test1.nc
$ define/user sys$output test2.cdl
$ ncdump -n test1 test1.nc
$
$! Compare test1.cdl and test2.cdl.  They should be identical.
$ difference test1.cdl test2.cdl	
$
$! Test "-c" option of ncgen
$
$! Create C program TEST0.C from test0.cdl
$ set message/noident/nofacility/notext/noseverity
$ define/user sys$output test0.c
$ ncgen -c test0.cdl
$ set message/ident/facility/text/severity
$
$! Compile generated C program
$ cc/vax/include_dir=[--.include]/nodebug test0
$ link /exe=test0 test0,sys$input/opt
	[--.lib]netcdf/library,sys$library:vaxcrtl/share
$
$! Run generated C program to create netCDF file ctest0.nc
$ run test0
$ rename test0.nc ctest0.nc
$
$! Dump ctest0.nc into CDL file ctest1.cdl
$ define/user sys$output ctest1.cdl
$ ncdump -n test1 ctest0.nc
$
$! Compare test1.cdl and ctest1.cdl.  They should be identical.
$ difference test1.cdl ctest1.cdl	
$
$! Test "-f" option of ncgen
$
$! Create FORTRAN program TEST0.FOR from test0.cdl
$ set message/noident/nofacility/notext/noseverity
$ define/user sys$output test0.for
$ ncgen -f test0.cdl
$ set message/ident/facility/text/severity
$
$! Compile generated FORTRAN program
$ copy [--.include]netcdf.inc netcdf.inc
$ fortran/nodebug test0
$ link /exe=test0 test0.obj,sys$input/opt
	[--.lib]netcdf/library,sys$library:vaxcrtl/share
$
$! Run generated FORTRAN program to create netCDF file ftest0.nc
$ run test0
$ rename test0.nc ftest0.nc
$
$! Dump ftest0.nc into CDL file ftest1.cdl
$ define/user sys$output ftest1.cdl
$ ncdump -n test1 ftest0.nc
$
$! Compare test1.cdl and ftest1.cdl.  They should be identical.
$ difference test1.cdl ftest1.cdl	
$
