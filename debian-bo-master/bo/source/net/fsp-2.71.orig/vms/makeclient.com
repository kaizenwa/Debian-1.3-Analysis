$! MAKECLIENT.COM
$! compiler/linker for VMS-fsp V2.7.0 for those who don't have VAX/MMS
$! 03-JAN-93 First version <S.A.Pechler@bdk.tue.nl>
$! 28-JAN-93 Modified for use with Multinet <S.A.Pechler@bdk.tue.nl>
$! 12-MAR-93 Fixed for Multinet-implementations which don't have a
$!           [.multinet.include] directory. <S.A.Pechler@bdk.tue.nl>
$! 07-MAY-93 Modified for FSP V2.7.0 <S.A.Pechler@bdk.tue.nl>
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
$LN = "@[.vms]link"
$!
$echo "Compiling bsd routines.."
$'comp' [.bsd_src]glob
$'comp' [.bsd_src]cmp
$'comp' [.bsd_src]ls
$'comp' [.bsd_src]print
$'comp' [.bsd_src]util
$'comp' [.bsd_src]find
$'comp' [.bsd_src]option
$'comp' [.bsd_src]operator
$'comp' [.bsd_src]function
$'comp' [.bsd_src]fnmatch
$!
$echo "Compiling common routines.."
$'comp' [.common]udp_io
$!
$echo "Compiling client routines.."
$'comp' [.client]lib
$'comp' [.client]util
$'comp' [.client]lock
$!
$echo "Compiling vms emulation routines.."
$'comp' [.vms_src]miscvms
$'comp' [.vms_src]getopt
$'comp' [.vms_src]convpath
$'comp' [.vms_src]vmsmain
$!
$echo "Compiling client commands.."
$'comp' [.clients]flscmd
$'comp' [.clients]fcdcmd
$'comp' [.clients]fgetcmd
$'comp' [.clients]frmcmd
$'comp' [.clients]frmdircmd
$'comp' [.clients]fprocmd
$'comp' [.clients]fmkdir
$'comp' [.clients]fput
$'comp' [.clients]fver /def=(VERSION_STR="""VMS-FSP Caltech version 2.7.0, May 8 1993""")
$'comp' [.clients]fcatcmd
$'comp' [.clients]fgrabcmd
$'comp' [.clients]fducmd
$'comp' [.clients]ffindcmd
$'comp' [.clients]fhostcmd
$!
$echo "linking client commands..."
$'LN' fls
$'LN' fcd
$'LN' fget
$'LN' frm
$'LN' frmdir
$'LN' fpro
$'LN' fmkdir
$'LN' fput
$'LN' fver
$'LN' fcat
$'LN' fgrab
$'LN' fdu
$'LN' ffind
$'LN' fhost
$!
$goto endok

$STOPPED:
$echo "Compiling/Linking cancelled by CTRL/Y."
$exit 1

$WARNED:
$exit 1

$endok:
$echo "VMS-fsp V2.7.0 clients compiled and linked OK!"
$exit
