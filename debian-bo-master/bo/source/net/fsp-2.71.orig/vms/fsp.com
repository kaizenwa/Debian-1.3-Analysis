$! FSP.COM
$! Puts the output of a fsp command to a file and changes the FSP variables 
$! to the content of this file.
$!
$! THIS FILE MUST BE COPIED TO THE SAME DIRECTORY WHERE THE FSP-EXECUTABLES
$! ARE, OTHERWISE YOU'LL GET AN ERRORMESSAGE WHEN USING THE 'FCD' AND 'FHOST'
$! COMMANDS.
$!
$on CONTROL_Y then goto end
$on CONTROL_C then goto end
$on ERROR then goto end
$def sys$output sys$scratch:temp_fspcmd0.com
$'P1' "''P2'"
$deassign sys$output
$if "''P1'" .EQS. "FHOSTCMD" then goto fhost
$!
$! Read the output of the fcd command, and define the new FSP_DIR
$! variable.
$fcd:
$open/read temp sys$scratch:temp_fspcmd0.com
$!
$pwd = ""
$loop:
$read/error=endfcd/end_of_file=endfcd temp pwd
$write sys$output "''pwd'"
$define/nolog fsp_dir "''pwd'"
$goto loop
$!
$endfcd:
$close temp
$goto end
$!
$! The fhost command produces an executable .COM file.
$fhost:
$@sys$scratch:temp_fspcmd0
$!
$end:
$deassign sys$output
$delete/nolog/noconfirm sys$scratch:temp_fspcmd0.com;*
