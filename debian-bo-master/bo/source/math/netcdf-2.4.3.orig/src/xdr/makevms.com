$! --------------------------------------------------------------------------
$! For making XDRTEST.EXE on VMS
$! --------------------------------------------------------------------------
$!
$! $Id: makevms.com,v 1.12 1996/02/27 23:19:49 steve Exp $
$
$ macro :== macro/nolist
$ ccc := cc /opt/nodebug/nolist/include=[]
$
$ define sys sys$library
$ define netinet sys$library
$
$ ccc XDR.C
$ purge XDR.OBJ
$ ccc XDRTEST.C
$ purge XDRTEST.OBJ
$ ccc XDRARRAY.C
$ purge XDRARRAY.OBJ
$ ccc XDRFLOAT.C
$ purge XDRFLOAT.OBJ
$ ccc XDRSTDIO.C
$ purge XDRSTDIO.OBJ
$ macro HTONL.MAR
$ purge HTONL.OBJ
$ macro NTOHL.MAR
$ purge NTOHL.OBJ
$
$ create/dir [--.LIB]
$
$ library/create [--.LIB]NETCDF.OLB
$ purge [--.LIB]NETCDF.OLB
$ library/replace [--.LIB]NETCDF.OLB -
    XDR, XDRARRAY, XDRFLOAT, XDRSTDIO, HTONL, NTOHL
$
$ link/nodebug/notraceback/exec=XDRTEST.exe -
    xdrtest.obj, -
    [--.lib]netcdf.olb/lib, -
    sys$input/opt
	sys$library:vaxcrtl.exe/share
$ purge XDRTEST.exe
