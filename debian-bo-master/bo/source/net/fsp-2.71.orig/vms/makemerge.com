$! MAKE_MERGE.COM
$!
$! build a fsp_merge file with all fsp-client commands included, to save
$! diskspace.
$!
$! 03-JAN-93 First version                  <S.A.Pechler@bdk.tue.nl>
$! 28-JAN-93 Modified for use with Multinet <S.A.Pechler@bdk.tue.nl>
$! 12-MAR-93 Fixed for Multinet-implementations which don't have a
$!           [.multinet.include] directory. <S.A.Pechler@bdk.tue.nl>
$! 08-MAY-93 Modified for FSP V2.7.0        <S.A.Pechler@bdk.tue.nl>
$!
$echo:== write sys$output
$ON CONTROL_Y then goto STOPPED
$ON CONTROL_C then goto STOPPED
$ON WARNING then goto WARNED
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
$! get the directory where the fsp-sources are located.
$!FSPDIR=F$ENVIRONMENT("DEFAULT")
$!
$echo "Building FSP_MERGE.."
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
$call buildmerge fgetcmd
$call buildmerge fgrabcmd
$call buildmerge fcatcmd
$call buildmerge fput
$call buildmerge frmcmd
$call buildmerge flscmd
$! You've still to use fcd.com when using fmerge
$call buildmerge fcdcmd
$call buildmerge frmdircmd
$call buildmerge fprocmd
$call buildmerge fmkdir
$call buildmerge fver
$call buildmerge fducmd
$call buildmerge ffindcmd
$call buildmerge fhostcmd
$!
$echo "...Compiling fmerge"
$cc [.clients]merge /nodebug /include=[.include.vms]
$!
$'ln' fmerge
$inquire/nopunct flag "Delete *_merge object files ? [Y/N]"
$ if (flag .EQS. "Y" ) THEN $delete/log/noconf *_merge.obj;
$goto MERGEOK

$buildmerge:
$! compiles the client command P1 into a merge object.
$SUBROUTINE
$P1 = F$EDIT(P1,"LOWERCASE")
$echo "...Compiling: ''P1'"
$! create *_merge.c file
$create 'P1'_merge.c
$open/append buf 'P1'_merge.c
$write buf "#define main ''P1'_main"
$write buf "#include ""clients/''P1'.c"" "
$close buf
$!
$ cc 'P1'_merge /nodebug /def=(MERGE=1) /include=[.include.vms]
$ delete/nolog/noconf 'P1'_merge.c;
$exit
$ENDSUBROUTINE

$STOPPED:
$echo "Compiling/Linking cancelled by CTRL/Y."
$exit 1

$WARNED:
$exit 1

$MERGEOK:
$echo "VMS-fsp V2.7.0 mergefile compiled and linked OK!"
$exit
