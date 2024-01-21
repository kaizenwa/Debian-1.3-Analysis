$! COMPILE.COM
$! compiler for VMS-FSP V2.7.0
$! 03-JAN-93 First version for FSP V2.6.5 <S.A.Pechler@bdk.tue.nl>
$! 07-MAY-93 Modified for VMS-FSP V2.7.0 <S.A.Pechler@bdk.tue.nl>
$!
$! Original routine from VMS-IRC-Client DCL MAKE file
$! Copyright 1990 by Very Mad Students, University of Karlsruhe, FRG
$!
$!------------------------------------------------------------------
$! @Compile <File> <Definitions> <Options> <Options>
$!------------------------------------------------------------------
$!set verify
$CompileIt:
$   Source = F$CVTIME(F$FILE_ATTR("''P1'.C","CDT"))
$   IF F$SEARCH("''P1'.OBJ") .EQS. "" THEN GOTO DoObject
$   Object = F$CVTIME(F$FILE_ATTR("''P1'.OBJ","CDT"))
$   Header = F$CVTIME(F$FILE_ATTR("[.vms]ucxshare.opt","CDT"))
$   IF Source .LTS. Header THEN GOTO DoObject1
$   IF Source .LTS. Object THEN GOTO NoObject
$   GOTO DoObject
$DoObject1:
$   COPY/NOCONF/NOLOG 'P1'.c 'P1'.c
$   PURGE/NOLOG 'P1'.c
$DoObject:
$   WRITE sys$output "...compiling ''P1'"
$   CC/nodebug /object='P1'.obj /include=[.include.vms] 'P1' 'P2' 'P3' 'P4' 'P5'
$   GOTO EndSub
$NoObject:
$   WRITE sys$output "...''P1' already up to date"
$EndSub:
$ EXIT
