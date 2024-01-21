$! MAKE_SERVER.COM
$! compiler/linker for VMS-fspd V2.7.0
$! 03-JAN-93 First version <S.A.Pechler@bdk.tue.nl>
$! 28-JAN-93 Modified for use with Multinet <S.A.Pechler@bdk.tue.nl>
$! 12-MAR-93 Fixed for Multinet-implementations which don't have a
$!           [.multinet.include] directory. <S.A.Pechler@bdk.tue.nl>
$! 10-MAY-93 Modified for FSP V2.7.0        <S.A.Pechler@bdk.tue.nl>
$!
$echo:== write sys$output
$on CONTROL_Y then goto STOPPED
$on CONTROL_C then goto STOPPED
$on WARNING then goto WARNED
$multinet = F$LENGTH(F$TRNLNM("MULTINET_ROOT","LNM$SYSTEM"))
$IF ( multinet .EQ. 0 ) .OR. -
    ( F$SEARCH("MULTINET_ROOT:[multinet.include]*.*") .EQS. "" )
$THEN
$ define /nolog SYS sys$library
$ define /nolog NETINET sys$library
$ELSE
$ define /nolog SYS multinet_root:[multinet.include.sys]
$ define /nolog NETINET multinet_root:[multinet.include.netinet]
$ENDIF
$!
$set def [-]
$def/nolog c$include [.include]
$comp = "@[.vms]compile"
$ln = "@[.vms]link"
$!
$echo "Compiling network routines.."
$'COMP' [.common]udp_io
$'COMP' [.common]random
$'COMP' [.common]strdup
$!
$echo "Compiling vms emulation routines.."
$'COMP' [.vms_src]miscvms
$'COMP' [.vms_src]vmsreaddir
$'COMP' [.vms_src]getopt
$'COMP' [.vms_src]convpath
$'COMP' [.vms_src]vmsmain
$!
$echo "Compiling server routines.."
$'COMP' [.server]main
$'COMP' [.server]file
$'COMP' [.server]host
$'COMP' [.server]lib
$'COMP' [.server]filecache
$'COMP' [.server]conf
$!
$echo "Linking server routines.."
$'LN' fspd
$!
$goto endok

$STOPPED:
$echo "Compiling/Linking cancelled by CTRL/Y."
$exit 1

$WARNED:
$echo "Warnings/Errors occured during compilation/linking."
$exit 1

$endok:
$echo "VMS-fspd V2.7.0 compiled and linked OK!"
$exit
