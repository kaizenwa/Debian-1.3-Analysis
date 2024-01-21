$! --------------------------------------------------------------------------
$! For making NCDUMP.EXE on VMS.
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.11 1996/03/18 23:32:14 steve Exp $
$
$ ccc := cc/vax/opt/nodebug/include=([--.include])/nolist
$
$ ccc DUMPLIB.C
$ purge DUMPLIB.OBJ
$ ccc NCDUMP.C
$ purge NCDUMP.OBJ
$ ccc VARDATA.C
$ purge VARDATA.OBJ
$ ccc [-.util]GETOPT.C
$ purge GETOPT.OBJ
$
$ link/nodebug/notraceback/exe=NCDUMP.exe -
    dumplib.obj, -
    ncdump.obj, -
    vardata.obj, -
    getopt.obj, -
    [--.lib]netcdf/library, -
    sys$input/opt
        sys$library:vaxcrtl/share
$ purge NCDUMP.exe
$
