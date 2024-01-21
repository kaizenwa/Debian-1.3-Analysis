$! --------------------------------------------------------------------------
$! For making NCTEST.EXE on VMS
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.9 1996/02/27 23:19:08 steve Exp $
$
$ ccc := cc /opt/nodebug/include=([--.include])/nolist
$
$ ccc ADD.C
$ purge ADD.OBJ
$ ccc ATTTESTS.C
$ purge ATTTESTS.OBJ
$ ccc CDFTESTS.C
$ purge CDFTESTS.OBJ
$ ccc DIMTESTS.C
$ purge DIMTESTS.OBJ
$ ccc DRIVER.C
$ purge DRIVER.OBJ
$ ccc EMALLOC.C
$ purge EMALLOC.OBJ
$ ccc ERROR.C
$ purge ERROR.OBJ
$ ccc MISCTEST.C
$ purge MISCTEST.OBJ
$ ccc REC.C
$ purge REC.OBJ
$ ccc SLABS.C
$ purge SLABS.OBJ
$ ccc VAL.C
$ purge VAL.OBJ
$ ccc VARDEF.C
$ purge VARDEF.OBJ
$ ccc VARGET.C
$ purge VARGET.OBJ
$ ccc VARPUT.C
$ purge VARPUT.OBJ
$ ccc VPUTGET.C
$ purge VPUTGET.OBJ
$ ccc VARGETG.C
$ purge VARGETG.OBJ
$ ccc VARPUTG.C
$ purge VARPUTG.OBJ
$ ccc VPUTGETG.C
$ purge VPUTGETG.OBJ
$ ccc VARTESTS.C
$ purge VARTESTS.OBJ
$
$ link/nodebug/notraceback/exe=NCTEST.exe -
    add.obj, -
    atttests.obj, -
    cdftests.obj, -
    dimtests.obj, -
    driver.obj, -
    emalloc.obj, -
    error.obj, -
    misctest.obj, -
    rec.obj, -
    slabs.obj, -
    val.obj, -
    vardef.obj, -
    varget.obj, -
    varput.obj, -
    vputget.obj, -
    vargetg.obj, -
    varputg.obj, -
    vputgetg.obj, -
    vartests.obj, -
    [--.lib]netcdf/library, -
    sys$input/opt
	sys$library:vaxcrtl.exe/share
$ purge NCTEST.exe
$
$ run nctest
$
