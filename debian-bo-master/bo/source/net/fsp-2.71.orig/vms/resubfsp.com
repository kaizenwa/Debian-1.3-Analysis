$ on warning then continue
$ time  = f$time()
$ date  = f$extract(0,11,time)
$ hour   = f$integer(f$cvtime(time,,"hour"))
$ day   = f$cvtime(time,,"weekday")
$!
$ write sys$output "date: ''date', day: ''day', hour: ''hour' "
$!
$ set def sys$login
$ if hour .ge. 18
$ then
$   stop AWAYD
$   @[.fsp.vms]RUNFSPD
$!
$!  Resubmit next day, on 09:00
$   submit/noprint/after="''date'+1-09:00:00" user2:[bdaasp.fsp.vms]resubfsp.com
$ else
$   show proc fspd /acc
$   stop FSPD
$   @[.fsp.away]RUNAWAY user2:[bdaasp.fsp.away]awainput.txt
$!
$!  Resubmit same day, on 18:00
$   submit/noprint/after="+09:00:00" user2:[bdaasp.fsp.vms]resubfsp.com
$ endif
