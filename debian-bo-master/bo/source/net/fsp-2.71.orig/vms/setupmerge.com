$! Sample dcl initialization file for VMS-fsp V2.6.5 mergefile
$! 03-JAN-93 S.A.Pechler <S.A.Pechler@bdk.tue.nl>
$!
$! change the following name to the directory where your fsp-exes
$! are located.  Don't forget to copy the [.vms]fcd.com and
$! [.vms]fsp.com files to this directory.
$!
$FSPHOME :== pool:[bdaasp.fsp]
$!
$! default address for an FSP-server. Change to any address you want.
$define/nolog FSP_HOST "131.155.2.71"
$define/nolog FSP_PORT "21"
$define/nolog FSP_DIR "/"
!$define/nolog FSP_LOCALPORT "1192"
$define/nolog FSP_TRACE "1"
$!
$fls    :== $'FSPHOME'fmerge flscmd """
$fcd    :== @'FSPHOME'fsp fcdcmd """
$fcdcmd :== $'FSPHOME'fmerge fcdcmd """
$fver   :== $'FSPHOME'fmerge fver """
$fget   :== $'FSPHOME'fmerge fgetcmd """
$fcat   :== $'FSPHOME'fmerge fcatcmd """
$fput   :== $'FSPHOME'fmerge fput """
$frmdir :== $'FSPHOME'fmerge frmdircmd """
$fpro   :== $'FSPHOME'fmerge fprocmd """
$fmkdir :== $'FSPHOME'fmerge fmkdir """
$frm    :== $'FSPHOME'fmerge frmcmd """
$fgrab  :== $'FSPHOME'fmerge fgrabcmd """
$ffind	:== $'FSPHOME'fmerge ffindcmd """
$fdu	:== $'FSPHOME'fmerge fducmd """
$fhost	:== @'FSPHOME'fsp fhostcmd """
$fhostcmd:==$'FSPHOME'fmerge fhostcmd """
$fspd   :== $'FSPHOME'fspd.exe
