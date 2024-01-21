$! --------------------------------------------------------------------------
$! For making NCGEN.EXE on VMS.
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.20 1996/03/18 23:33:43 steve Exp $
$
$ ccc := cc/vax/opt/nodebug/incl=([--.include])/nolist
$
$ copy vmstab.c ncgentab.c
$ purge ncgentab.c
$ copy vmstab.h ncgentab.h
$ purge ncgentab.h
$ copy vms_yy.c ncgenyy.c
$ purge ncgenyy.c
$
$ ccc MAIN.C
$ purge MAIN.OBJ
$ ccc GENLIB.C
$ purge GENLIB.OBJ
$ ccc LOAD.C
$ purge LOAD.OBJ
$ ccc NCGENTAB.C
$ purge NCGENTAB.OBJ
$ ccc ESCAPES.C
$ purge ESCAPES.OBJ
$ ccc GETFILL.C
$ purge GETFILL.OBJ
$ ccc INIT.C
$ purge INIT.OBJ
$ ccc CLOSE.C
$ purge CLOSE.OBJ
$ ccc [-.util]GETOPT.C
$ purge GETOPT.OBJ
$
$ link/nodebug/notraceback/exe=NCGEN.exe -
    getfill.obj, -
    close.obj, -
    genlib.obj, -
    escapes.obj, -
    init.obj, -
    load.obj, -
    main.obj, -
    ncgentab.obj, -
    getopt.obj, -
    [--.lib]netcdf/library, -
    sys$input/opt
	sys$library:vaxcrtl/share
$ purge NCGEN.exe
