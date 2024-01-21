$! Sample dcl initialization file for VMS-fsp V2.6.5
$! 03-JAN-93 S.A.Pechler <S.A.Pechler@bdk.tue.nl>
$!
$! default address for an FSP-server. Change to any address you want.
$define/nolog FSP_HOST "131.155.2.71"
$define/nolog FSP_PORT "21"
$define/nolog FSP_DIR "/"
$define/nolog FSP_TRACE "1"
$define/nolog FSP_DELAY 3000
$define/nolog FSP_TIMEOUT 5
$! change the following name to the directory where your fsp-executables
$! are located.
$! Don't forget to copy the [.vms]fsp.com file to this directory!!
$!
$FSPHOME :== pool:[bdaasp.fsp]
$!
$fls   :== $'FSPHOME'flscmd.exe """
$fcd   :== @'FSPHOME'fsp.com fcdcmd """
$fcdcmd:== $'FSPHOME'fcdcmd.exe
$fver  :== $'FSPHOME'fver.exe """
$fget  :== $'FSPHOME'fgetcmd.exe """
$fcat  :== $'FSPHOME'fcatcmd.exe """
$fput  :== $'FSPHOME'fput.exe """
$frmdir:== $'FSPHOME'frmdircmd.exe """
$fpro  :== $'FSPHOME'fprocmd.exe """
$fmkdir:== $'FSPHOME'fmkdir.exe
$frm   :== $'FSPHOME'frmcmd.exe """
$fgrab :== $'FSPHOME'fgrabcmd.exe """
$ffind :== $'FSPHOME'ffindcmd.exe """
$fdu   :== $'FSPHOME'fducmd.exe """
$fhost :== @'FSPHOME'fsp.com fhostcmd """
$fhostcmd:==$'FSPHOME'fhostcmd.exe """
$fspd  :== $'FSPHOME'fspd.exe """
